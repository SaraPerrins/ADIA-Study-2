options(digits=3)
options(scipen=1000000)
set.seed(0203)
#setwd('C:/LOCAL/Example_Public_Health_Repo')

dat <- read.csv('ADIA/data/LS_SUD.csv')
head(dat)
colnames(dat)
#===============================================================================
### I.- Developing sample
#===============================================================================
### 1.- We focus on population expericing ACE during childhood
# (and before the otucome was asssess)
dat$anyACE <- apply(dat[,c('M','MP','MS','MN','MF','ML','ME','MM','MD')],1,sum)>1
table(dat$anyACE )
dat <- dat[dat$anyACE,]

### 2.- In that population, we want to campare those who develop a certain outcome to those who do not
# ()'case-control' design)
dat$y <- dat$anysud
table(dat$y,useNA='ifany')
dat <- dat[!is.na(dat$y),]


### 3.- 'holding constant' some characteristics
#Ye: age with respect to a fix date does not make a lot of sense
# we could use 'age at FU' (at the time the outcome was assessed) OR
# we could use the year child was born as a cohort indicator
dat$cohort <- as.numeric(substr(dat$ChildDOB,6,9))
summary(dat$cohort)

x.n <- c('childrace_bsl','cohort','childsex_bsl','caregiver_married16','hh_income16')
summary(dat[,x.n])
# Ye: if we go for separate analysis by wave,
# we should also match on historical values of y

#3.1.- create indicators for categorical variables
#treat missing as a separate category
#(so we do not lose cases becosue of missing covariates here)
dat[,x.n] <- lapply(dat[,x.n] ,addNA,ifany =T)
X         <- model.matrix(~ .,data=dat[,x.n])[,-1]
summary(X)


### 4.- Finding mathes
library(designmatch)
# Zubizarreta et al. (2011) https://doi.org/10.1198/tas.2011.11072
# Kelz et al. (2013) https://doi.org/10.1097/sla.0b013e31829654f3

#i.- Group indicator
# in general 'treatment' indicator
# in 'case-control' studies, outcome indicator
# note that the data needs to be sorted in decreasing order
dat <- dat[order(dat$y,decreasing =T),]
t_ind = dat$y

#ii.- Distance matrix
# A measure of similarity/disimilarity in the covariates
# to shose individual matches
dist_mat = distmat(t_ind, X)

#iii.  Moment balance
# constrain differences in means to be at most .05 standard deviations apart
# this influence the marginal distribution not the individual matches
mom_tols = round(absstddif(X, t_ind, .05), 2)
mom = list(covs = X, tols = mom_tols)

#iv.- Subset matching weight
# this is a tunning parameter that regulate tredeoff between
# number of matches and quality of the matches
subset_weight =   median(dist_mat)

#! Ye: there are some other parameters that you can adjust
#! see help(bmatch) and/or reference
#! e.g., if we go for diffrent waves, we may want to match exactly on
#! hisotrical values of the outcome (e.g., y12 if we are looking at y14)

# Fine balance
#fine_covs = cbind(black, hispanic, married, nodegree)
#fine = list(covs = fine_covs)

# Exact matching
#exact_covs = cbind(black)
#exact = list(covs = exact_covs)

#v.- Solver options
t_max = 60*5
solver = "glpk"
approximate = 1
solver = list(name = solver, t_max = t_max, approximate = approximate,
  round_cplex = 0, trace = 0)

#vi.-  Match
out <- bmatch(t_ind = t_ind, dist_mat = dist_mat, subset_weight = subset_weight,
  mom = mom,  solver = solver)


### 5.-  assess balance
library(cobalt) # to asesss/plot balance

bal.tab(out,treat=t_ind,covs=X,s.d.denom='treated',un = TRUE,disp = c("means", "sds"))

lplot <- love.plot(out,treat=t_ind,covs=X,thresholds = c(m = .1), binary = "std",s.d.denom='treated')
lplot
png('ADIA/output/love.plot.png',width = 480*4,heigh=480*4,res=300)
lplot
dev.off()


table(out$group_id,useNA='ifany')
sdat <- dat[c(out$t_id,out$c_id	),]
dim(sdat)
sdat$id <- out$group_id

#Ye: you may watn to save files and plots

#===============================================================================
###II. Exploring possible protective factors
#===============================================================================
#1. define a training sample
length(unique(sdat$id))
ts0 <- sample(unique(sdat$id[dat$y==1]),length(unique(sdat$id[dat$y==1]))/2)
ts1 <- sample(unique(sdat$id[dat$y==0]),length(unique(sdat$id[dat$y==0]))/2)
ts  <- c(ts0,ts1)

#2.- Protective factors
#Ye: this is only a place holder you can include many more
z.n <- c('comm_safety1416','comm_ce1416','fr_supportparent_cat1416')
summary(sdat[,z.n])

#2.1- dropping incomplete cases
# Ye: unlike covariates for matching, we do loose cases with no info on protective factors
# take into account this whne selecting protective factors

sdat$cc <- ave(complete.cases(sdat[,z.n]),sdat$id,FUN=mean)
sdat <- sdat[sdat$cc==1,]
summary(sdat[,z.n])

#2.2- centering
# we need to center covariates at the pair-mean
# Stanfill et al. (2019) https://doi.org/10.1177/1179597219858954
sdat <- sdat[order(sdat$id),]

Z <-  model.matrix(~ .,data=sdat[,z.n])[,-1]
summary(Z)

Z <-  do.call(rbind,lapply(split(data.frame(Z),sdat$id), function(d) apply(d,2,scale,scale=F) ))


###3.- Run tree algorithm on training data
library(evtree)
efit <- evtree(factor(y)~., data=data.frame(y=sdat$y,Z)[sdat$id%in%ts,])
plot(efit)

png('ADIA/output/tree.plot.png',width = 480*4,heigh=480*4,res=300)
plot(efit)
dev.off()


#===============================================================================
###.-inference (95% CI; p-values)

sdat$node <- factor(predict(efit,type='node',newdata=data.frame(Z)))
table(sdat$node)



#Ye this test overall association but tends to fail becouse of the zeros
mdat <- reshape(sdat[!sdat$id%in%ts,c('id','node','y')], direction = "wide", idvar = 'id', timevar = 'y')
mcnemar.test(table(mdat[,2],mdat[,3]))


library(survival)
#sdat$node <- C(sdat$node,contr.treatment, base=4)
#sdat$node <- C(sdat$node,contr.sum) #Ye: each category Vs. mean
cfit <- clogit(y ~ node + strata(id), data=sdat[!sdat$id%in%ts,])
summary(cfit)
