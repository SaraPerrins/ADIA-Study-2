options(digits=3)
options(scipen=1000000)
set.seed(0203)

library(tidyverse)

library(designmatch)
# Zubizarreta et al. (2011) https://doi.org/10.1198/tas.2011.11072
# Kelz et al. (2013) https://doi.org/10.1097/sla.0b013e31829654f3
library(cobalt) # to asesss/plot balance

#setwd('C:/LOCAL/Example_Public_Health_Repo')
#pathi   <- 'C:/Users/21983/OneDrive - ICF/ADIA/study 2/Data'
#dat <- read.csv('C:/Users/21983/OneDrive - ICF/ADIA/study 2/Data/LS_SUD.csv')

dat <- readRDS('data/LS_SUD.Rds')

head(dat)
colnames(dat)
#===============================================================================
### I.- Developing sample
#===============================================================================
### 1.- We focus on population expericing ACE during childhood
# (and before the otucome was asssess)

dat <- dat[dat$anyACE,]

### 2.- In that population, we want to campare those who develop a certain outcome to those who do not
# ()'case-control' design)
dat$y <- dat$anysud
table(dat$y,useNA='ifany')
dat <- dat[!is.na(dat$y),]


#-------------------------------------------------------------------------------
### 4.- Finding mathes

#i.- Group indicator
# in general 'treatment' indicator
# in 'case-control' studies, outcome indicator
# note that the data needs to be sorted in decreasing order
dat <- dat[order(dat$y,decreasing =T),]
t_ind = dat$y

#ii.- Distance matrix-----------------------------------------------------------
# A measure of similarity/disimilarity in the covariates
# to shose individual matches
x <- c('center', 'childrace_bsl','cohort','childsex_bsl','caregiver_married16','hh_income16')
X         <- model.matrix(~ .,data=lapply(dat[,x] ,addNA,ifany =T))[,-1]
dist_mat = distmat(t_ind, X)

#iii.  Moment balance-----------------------------------------------------------
# constrain differences in means to be at most .05 standard deviations apart
# this influence the marginal distribution not the individual matches
v <- c('center', 'childrace_bsl','cohort','childsex_bsl','caregiver_married16','hh_income16')
V       <- model.matrix(~ .,data=lapply(dat[,v] ,addNA,ifany =T))[,-1]
mom_tols = round(absstddif(V, t_ind, .05), 2)#if there is a rare factors (e.g., NA), either take it out from X, or change .05 to larger number .1, .15
mom = list(covs = V, tols = mom_tols)

#all(X==Z)
#iv.- Subset matching weight----------------------------------------------------
# this is a tunning parameter that regulate tredeoff between
# number of matches and quality of the matches
subset_weight =  median(dist_mat) #this parameter regulate matches, if qulaity getting bad or drop a lot of cases, can change

#! Ye: there are some other parameters that you can adjust
#! see help(bmatch) and/or reference
#! e.g., if we go for diffrent waves, we may want to match exactly on
#! hisotrical values of the outcome (e.g., y12 if we are looking at y14)

# Fine balance, same as moment blance, but for factors--------------------------

#fine_covs =  model.matrix(~ ., data=dat[,'center'])
#fine = list(covs = fine_covs)

# Exact matching
#exact_covs = cbind(black)
#exact = list(covs = exact_covs)

#v.- Solver options-------------------------------------------------------------
t_max = 60*5
solver = "glpk"
approximate = 1
solver = list(name = solver, t_max = t_max, approximate = approximate,
  round_cplex = 0, trace = 0)

#vi.-  Match
out <- bmatch(t_ind = t_ind, dist_mat = dist_mat, subset_weight = subset_weight,
  mom = mom,  solver = solver)


### 5.-  assess balance---------------------------------------------------------
bal.tab(out,treat=t_ind,covs=X,s.d.denom='treated',un = TRUE,disp = c("means", "sds"))

lplot <- love.plot(out,treat=t_ind,covs=X,thresholds = c(m = .1), binary = "std",s.d.denom='treated')
lplot
png('output/love.plot.png',width = 480*4,heigh=480*4,res=300)
lplot
dev.off()


table(out$group_id,useNA='ifany')
sdat <- dat[c(out$t_id,out$c_id	),]
dim(sdat)
sdat$id <- out$group_id
saveRDS(sdat, file='data/anySUD.m.Rds')
#write.csv(sdat,file=file.path(pathi,'anySUD_matched.csv'),na='')
#Ye: you may watn to save files and plots
table(sdat$id)
#===============================================================================
###II. Exploring possible protective factors
#===============================================================================
library(tidyverse)
sdat <- readRDS(file='data/anySUD.m.Rds')
#1. define a training sample
length(unique(sdat$id))
ts <- sample(unique(sdat$id),length(unique(sdat$id))/2)
sdat$ts <- sdat$id%in%ts
length(unique(sdat$id[sdat$ts]))


#2.- Protective factors
#Ye: this is only a place holder you can include many more
z.n <- c('comm_safety1416','comm_ce1416','fr_supportparent_cat1416')
summary(sdat[,z.n])

#2.1- dropping incomplete cases
# Ye: unlike covariates for matching, we do loose cases with no info on protective factors
# take into account this whne selecting protective factors

#sdat$cc <- ave(complete.cases(sdat[,z.n]),sdat$id,FUN=mean)
#sdat <- sdat[sdat$cc==1,]
#summary(sdat[,z.n])

###2.2- centering
# we need to center covariates at the pair-mean
# Stanfill et al. (2019) https://doi.org/10.1177/1179597219858954

#'old school' way
#sdat   <-  sdat[order(sdat$id),]
#sdat$Z <-  model.matrix(~ ., model.frame(~ ., sdat[,z.n], na.action=na.pass))[,-1]
#sdat$Z <-  do.call(rbind,lapply(split(data.frame(Z),sdat$id), function(d) apply(d,2,scale,scale=F) ))

#tider
sdat$Z <-
  sdat[,z.n] %>%
  with(., model.matrix(~ ., model.frame(~ ., data=.,na.action=na.pass))[,-1]) %>%
  data.frame ()%>%
  group_by(sdat$id) %>%
  mutate(across(everything(),scale,scale=F)) %>%
  ungroup %>%
  .[,-dim(.)[2]]


#test
sdat$x <- ifelse(runif(length(sdat$y ))>0.25,!sdat$y,sdat$y)
table(sdat$x,sdat$y)

sdat$Z <-
  sdat[,c(z.n,'x')] %>%
  with(., model.matrix(~ ., model.frame(~ ., data=.,na.action=na.pass))[,-1]) %>%
  data.frame ()%>%
  group_by(sdat$id) %>%
  mutate(across(everything(),scale,scale=F)) %>%
  ungroup %>%
  .[,-dim(.)[2]]

###3.- Run tree algorithm on training data
library(evtree)
efit <- with(sdat[sdat$ts,], evtree(factor(y)~., data=na.omit(data.frame(y,Z))))
plot(efit)


png('output/tree.plot.png',width = 480*4,heigh=480*4,res=300)
plot(efit)
dev.off()

###4.- Tradtional tree
# Advantage: can use cases with incomplete infromation
library(rpart)
library(rpart.plot)
#a.- grow large tree
tree <- with(sdat[sdat$ts,], rpart(y ~ . , data=cbind(y,Z),cp=-1,method = 'class'))
plotcp(tree,col='red' )

#b.- prune based on complexity
opcp <- tree$cptable[,'CP'][which.min(tree$cptable[,'xerror'])]
ptree <- prune(tree, cp =opcp)
plot(as.party(ptree))
rpart.plot(ptree)


png('output/rtree.plot.png',width = 480*4,heigh=480*4,res=300)
rpart.plot(ptree)
dev.off()


#===============================================================================
###.-inference (95% CI; p-values)
#===============================================================================

sdat$node.etree <- factor(predict(efit,type='node',newdata=sdat$Z))
table(sdat$node.etree)

sdat$node.rtree <- factor(predict(as.party(ptree),type='node',newdata=sdat$Z))
table(sdat$node.rtree)


#Ye this test overall association but tends to fail becouse of the zeros
mdat <- reshape(sdat[!sdat$ts,c('id','node.etree','y')], direction = "wide", idvar = 'id', timevar = 'y')
mcnemar.test(table(mdat[,2],mdat[,3]))


library(survival)
#sdat$node <- C(sdat$node,contr.treatment, base=4)
#sdat$node <- C(sdat$node,contr.sum) #Ye: each category Vs. mean
cfite <- clogit(y ~ node.etree  + strata(id), data=sdat[ !sdat$ts,])
anova(cfite,update(cfite,.~ 1 +  strata(id)))
summary(cfite)

t(with(sdat[sdat$ts,],tapply(y, node.etree,mean)))
t(with(sdat[!sdat$ts,],tapply(y, node.etree,mean)))

table(sdat$node.rtree)


cfitr <- clogit(y ~ node.rtree  + strata(id), data=sdat[!sdat$ts,])
summary(cfitr)

t(with(sdat[sdat$ts,],tapply(y, node.rtree,mean)))
t(with(sdat[!sdat$ts,],tapply(y, node.rtree,mean)))
