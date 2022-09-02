options(digits=3)
options(scipen=1000000)
set.seed(0203)
library(tidyverse)
#setwd('C:/LOCAL/Example_Public_Health_Repo')
dat <- readRDS('data/LS_SUD.Rds')
head(dat)
colnames(dat)

# YE: As you said, in Study 2 we are ultimately interested in effect modification.
# This suggest an alternative to the 'case-control' design
#( i.e., compare expousure to PC among  those  exposed to ACEs with and without ann outcome)
# We can instead use a 'cohort' desing (i.e., compare an outcome between those exposed and not to ACE
# and check if the estiamted effect vary by PC expousure

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
dat$cohort <- factor(dat$cohort)
dat$X <-
  dat[,x] %>%
  mutate(across(where(is.factor),addNA,ifany=T))  %>%
  mutate(across(where(~ is.numeric(.x) && any(is.na(.x)) && n_distinct(.x)==2), ~addNA(factor(.x)))) %>%
  mutate(across(where(~ is.numeric(.x) && any(is.na(.x))), is.na,.names = 'NA_{.col}')) %>%
  mutate(across(where(is.numeric),~ replace(.,is.na(.),mean(.,na.rm=T))))  %>%
  model.matrix(~.,.) %>%
  .[,-1]


#weights add up to 1 in each group
tgt  <- colMeans(dat$X)
sd2  <- apply(dat$X,2,function(v) if(mean(v)<.05) {sqrt(.05*.95)} else {sd(v)})
phiX <- scale(dat$X, center=tgt, scale=sd2)
dat$w[dat$anyACE==1] <- find.weights(phiX[dat$anyACE==1,],lambda=0)
dat$w[dat$anyACE==0] <- find.weights(phiX[dat$anyACE==0,],lambda=0)
# Ye: lambda=0 give maximumn similarity
# at the cost of more varaibility of the weights (i.e.,less effective sample size)
# you coudl increase a bit, e.g. 0.05, to reduce weight  variability,
#if you still get


with(dat,data.frame(X,anyACE,w)) %>%
  group_by(anyACE) %>%
  summarize(across(everything()&!w, list(
  mean,
  ~ weighted.mean(.,w))
  ))%>%t

library(cobalt)
bal.tab(dat$X, treat = dat$anyACE, weights = dat$w,s.d.denom='treated',un = TRUE,disp = c("means", "sds"))
love.plot(dat$X, treat = dat$anyACE, weights = dat$w , thresholds = c(m = .1), binary = "std",s.d.denom='treated')



#===============================================================================
### . define a training sample
#===============================================================================
#stratfied random sample
dat <-
  dat %>%
  mutate(strata=paste0(x,anyACE)) %>%
  group_by(strata) %>%
  mutate(training.sample=if_else(id%in%sample(unique(id), round(n_distinct(id)/2)),1,0)) %>%
  ungroup
table(dat$strata, dat$training.sample)

saveRDS(dat, file='data/LS.w.Rds')

#Ye: ALL the code above needs to be run once (it appplies to all outcomes).
# the code below, in contrast, vary by outome

#===============================================================================
### . find effect modifiers
#===============================================================================
dat <- readRDS('data/LS.w.Rds')
library(causalTree)
#Reference https://doi.org/10.48550/arXiv.1504.01132

dat$y <- dat$anysud
table(dat$y,useNA='ifany')
#dat <- dat[!is.na(dat$y),]

pf <- c('comm_safety1416','comm_ce1416','fr_supportparent_cat1416')
summary(dat[,pf])
dat$PF <- dat[,pf]

df0 <-
  dat %>%
  filter(training.sample==1)

###Causal Trees (CT)
tree <- with(df0, causalTree(y ~ ., data = cbind(y,PF), treatment = anyACE, weights=w,
                   split.Rule = "CT", cv.option = "CT", split.Honest = T, cv.Honest = T,cp = 0))

opcp <- tree$cptable[,1][which.min(tree$cptable[,4])]
ptree.causal <- prune(tree, cp=opcp)

rpart.plot(ptree.causal)



png('output/tree.plot.causal.png',width = 480*4,heigh=480*4,res=300)
rpart.plot(tree.causal)
dev.off()


dat$node.cau  <- factor(predict(partykit:::as.party(ptree.causal),type='node',newdata=dat))
#===============================================================================
### . Inferece
#===============================================================================
library(survey)
dat$id <- 1:nrow(dat)
df1 <-
  dat %>%
  filter(training.sample==0)

sdw    <- svydesign(id = ~id, weights = ~w, data = df1)
fit0   <- svyglm(y~ anyACE, design=sdw,family=gaussian) #use gaussian even for binnary
coef(summary(fit0))

fit1 <- svyglm(y~ anyACE:node.cau + node.cau  -1, design=sdw,family=gaussian)#use gaussian even for binnary
tb   <- coef(summary(fit1))
tb[grep('anyACE:',rownames(tb)),]
