options(digits = 3)
options(scipen = 1000000)
set.seed(0203)
library(tidyverse)
# Ye, un-comment the following line and all path should be corected
#@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
#setwd('C:/Users/21983/OneDrive - ICF/ADIA/study 2/')#@@@@@@@@@@@@@@@@@@@@@@@@@@@
#@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
dat <- read.csv('data/LS_MH_PCE_anyt.csv')
head(dat)
colnames(dat)

# YE: As you said, in Study 2 we are ultimately interested in effect modification.
# This suggest an alternative to the 'case-control' design
#( i.e., compare expousure to PC among  those  exposed to ACEs with and without ann outcome)
# We can instead use a 'cohort' desing (i.e., compare an outcome between those exposed and not to ACE
# and check if the estiamted effect vary by PC expousure
#===============================================================================
### . Compute some varaibles
#===============================================================================
dat$cohort <- as.numeric(substr(dat$ChildDOB,6,9))
dat$cohort <- factor(dat$cohort)
summary(dat$cohort)

dat$anyACE <- apply(
    dat[, c('M','MP','MS','MN','MF','ML','ME','MM','MD')], 1, sum) >= 1
table(dat$anyACE)




#===============================================================================
### . Find balancing weights among those with and without any ACEs
#===============================================================================
#Approximate Balancing Weights (these are an alternative to entropy balancing)
#Reference:  https://doi.org/10.48550/arXiv.2007.09056
source('R/find.weights.r')

# In orther to balance the missing pattern we need to
# for categorical variables, create an NA category (addNA)
# this should inlcude binary variables (even if not declared as factors)
# for continuous, add indicator is.na and impute mean

x <- c('center', 'childrace_bsl','cohort','childsex_bsl','caregiver_married16','hh_income16')
dat$X <-
  dat[,x] %>%
  mutate(across(where(is.factor),addNA,ifany=T))  %>%
  mutate(across(where(~ is.numeric(.x) && any(is.na(.x)) && n_distinct(.x)==2), ~addNA(factor(.x)))) %>%
  mutate(across(where(~ is.numeric(.x) && any(is.na(.x))), is.na,.names = 'NA_{.col}')) %>%
  mutate(across(where(is.numeric),~ replace(., is.na(.),mean(., na.rm=T))))  %>%
  model.matrix(~., .) %>%
  .[, -1]


#weights add up to 1 in each group
tgt  <- colMeans(dat$X)
sd2  <- apply(dat$X, 2, function(v) if(mean(v) < .05) {sqrt(.05 * .95)} else {sd(v)})
phiX <- scale(dat$X, center = tgt, scale = sd2)
dat$w[dat$anyACE == 1] <- find.weights(phiX[dat$anyACE == 1, ], lambda = 0.01)
dat$w[dat$anyACE == 0] <- find.weights(phiX[dat$anyACE == 0, ], lambda = 0.01)
# Ye: lambda=0 give maximumn similarity
# at the cost of more varaibility of the weights (i.e.,less effective sample size)
# you coudl increase a bit, e.g. 0.05, to reduce weight  variability,
#if you still get
tapply(dat$anyMH , dat$anyACE, mean)
tapply(dat$anyMH * dat$w, dat$anyACE, sum)


with(dat, data.frame(X,anyACE,w)) %>%
  group_by(anyACE) %>%
  summarize(across(everything()&!w, list(
  mean,
  ~ weighted.mean(.,w))
  )) %>% t

library(cobalt)
bal.tab(dat$X, treat = dat$anyACE, weights = dat$w,
  s.d.denom='treated',un = TRUE,disp = c("means", "sds"))
love.plot(dat$X, treat = dat$anyACE, weights = dat$w , 
  thresholds = c(m = .1), binary = "std",s.d.denom='treated')



#===============================================================================
### . define a training sample
#===============================================================================
#stratfied random sample
dat <-
  dat %>%
  mutate(strata = paste0(x, anyACE)) %>%
  group_by(strata) %>%
  mutate(training.sample = if_else(id %in% 
    sample(unique(id), round(n_distinct(id)/2)), 1, 0)) %>%
  ungroup
table(dat$strata, dat$training.sample)

saveRDS(dat, file = "data/LS.w.Rds")

#Ye: ALL the code above needs to be run once (it appplies to all outcomes).
# the code below, in contrast, vary by outome

#===============================================================================
### . find effect modifiers
#===============================================================================
dat <- readRDS('data/LS.w.Rds')

library(causalTree)
#Reference https://doi.org/10.48550/arXiv.1504.01132

dat$y <- factor(dat$anyMH)
table(dat$y, useNA = "ifany")



#PCE across time indicators
pf <-  c('anysupadu', 'anysupparent', 'anysuprelative', 'anysupnonfam', 'fam_sat', 'home_safety', 'prrelation',
          'bestfriend', 'socialpart', 'parent_involv', 'resid_stab', 'neighborhood_safety', 'neighborhood_exp',
          'school_safety_y', 'srvc_use', 'childcare')

summary(dat[, pf])
dat$PF <- dat[, pf]
# sdat$PF <- ifelse(runif(length(sdat$y ))>0.25,!sdat$y,sdat$y)
# table(sdat$PF, sdat$y)

dat$cw <- dat$w * nrow(dat)
df0 <-
  dat %>%
  filter(training.sample == TRUE)
dim(df0)
dim(dat)



###Causal Trees (CT)
tree <- with(df0, causalTree(y ~ .,
              data = cbind(y, PF), treatment = anyACE, weights = cw,
              split.Rule = "CT", cv.option = "CT", xval = 5,
              split.Honest = TRUE, cv.Honest = TRUE, cp = 0)
            )
tree$cptable

opcp <- tree$cptable[, 1][which.min(tree$cptable[, 4])]
ptree_causal <- prune(tree, cp = opcp)

rpart.plot(ptree_causal) #Ye : chose the plot you like to save below
plot(as.party(ptree_causal))


png("output/tree.plot.causal.png",width = 480*4, heigh = 480*4, res = 300)
rpart.plot(ptree_causal)
dev.off()


dat$node.cau  <- factor(
                  predict(partykit:::as.party(ptree_causal),
                  type = "node", newdata = dat))
table(dat$node.cau)


#' predict node is supposed to predict node membership even
#' when some of the variables defining primary splits are missing
#' using "surrogate splits"
#' I am not sure it is doing a good job
#' Thus I propose to use only cases with no missing
#' on spliting variables
partykit:::.list.rules.party(partykit:::as.party(ptree_causal))
dat <-
  dat %>%
  mutate(pf =
    case_when(
    socialpart >= 0.5 & prrelation < 0.5 ~ 3
    , socialpart >= 0.5 & prrelation >= 0.5 ~ 4
    , socialpart < 0.5 ~ 5
    )
    , pf = factor(pf)
    )

table(dat$pf, dt$node.cau, useNA = "ifany")

rpart.rules(ptree_causal, roundint = FALSE)

with(dat[dat$training.sample == 1, ],
  weighted.mean(y[anyACE], cw[anyACE]) -
  weighted.mean(y[!anyACE], cw[!anyACE])
  )

sapply(split(dat[dat$training.sample == 1, ]
  , dat$pf[dat$training.sample == 1]), \(data){ with(data, 
  weighted.mean(y[anyACE], cw[anyACE]) -
  weighted.mean(y[!anyACE], cw[!anyACE])
)})

sapply(split(dat[dat$training.sample == 1, ]
  , dat$node.cau[dat$training.sample == 1]), \(data){ with(data, 
  weighted.mean(y[anyACE], cw[anyACE]) -
  weighted.mean(y[!anyACE], cw[!anyACE])
)})

#===============================================================================
### . Inferece
#===============================================================================
library(survey)
dat$id <- 1:nrow(dat)
df1 <-
  dat %>%
  filter(training.sample == 0)

sdw    <- svydesign(id = ~id, weights = ~ w, data = df1)
fit0   <- svyglm(as.numeric(y) ~ anyACE, 
          design = sdw, family = gaussian) #!!use gaussian even for binnary
coef(summary(fit0))

fit1 <- svyglm(as.numeric(y) ~ anyACE:node.cau + node.cau  -1,
          design = sdw, family = gaussian)#use gaussian even for binnary
tb   <- coef(summary(fit1))
tb[grep("anyACETRUE:", rownames(tb)), ]

anova(fit0, fit1)