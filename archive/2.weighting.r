options(digits=3)
options(scipen=1000000)
#setwd('C:/LOCAL/Example_Public_Health_Repo')
source('ADIA/R/find.weights.r')
library(cobalt) # to asesss/plot balance


dat <- read.csv('ADIA/data/LS_SUD.csv')
head(dat)
colnames(dat)
### 1.- We focus on population expericing ACE during childhood
# (and before the otucome was asssess)
dat$anyACE <- apply(dat[,c('M','MP','MS','MN','MF','ML','ME','MM','MD')],1,sum)>1
table(dat$anyACE )
dat <- dat[dat$anyACE,]

### 2.- In that populatuion, we want to campare those who develop  a certain outcome
dat$y <- dat$anysud
table(dat$y,useNA='ifany')
dat <- dat[!is.na(dat$y),]

### 3.- holding constant some characteristics
#Ye: age with respect to a fix date does not make a lot of sense
# we could use age at the FU (at the time the outcome was assessed
# if that is sort of fix, we woudl use the year child was born as a cohort indicator
dat$cohort <- as.numeric(substr(dat$ChildDOB,6,9))
summary(dat$cohort)

x.n <- c('childrace_bsl','cohort','childsex_bsl','caregiver_married16','hh_income16')
summary(dat[,x.n])

#create indicators for categorical variables
#treat missing as a separate category
dat[,x.n] <- lapply(dat[,x.n] ,addNA,ifany =T)
X         <- model.matrix(~ .,data=dat[,x.n])[,-1]
#summary(X)


#target or referece population (those who do develop the outcome, i.e., y==1)
# center covariates at target &
# scale covarites (i.e. divide by SD except for very small proportions )
tar  <- colMeans(X[dat$y==1,])
sd2  <- apply(X,2,function(v) if(mean(v)<.05) {sqrt(.05*.95)} else {sd(v)})
phiX <- scale(X, center=tar, scale=sd2 )

head(round(cbind(colMeans(phiX[dat$y==1,]),colMeans(phiX[dat$y==0,])),3))

###3.1.- compute weights
#weights add up to 1 in each group
dat$w[dat$y==1] <- 1/sum(dat$y)
dat$w[dat$y==0] <- find.weights(phiX[dat$y==0,],lambda=0)

###3.2.-  assess balance
#Ye: the effective sample size shows the cost
bal.tab(X, treat = dat$y, weights = dat$w,s.d.denom='treated',un = TRUE,disp = c("means", "sds"))


love.plot(X, treat = dat$y, weights = dat$w , thresholds = c(m = .1), binary = "std",s.d.denom='treated')

z.n <- c('comm_safety1416','comm_ce1416','fr_supportparent_cat1416')
summary(dat[,])




library(rpart)
#library(partykit)
set.seed(0203)
rtree <- rpart(y~ comm_safety1416+comm_ce1416+fr_supportparent_cat1416, data=dat,weights=dat$w,
  method = "class",cp=-1,)
cpdat <- data.frame(rtree$cptable)
min.e <- which.min(cpdat$xerror)
round(cpdat$CP[min.e],2)
round(cpdat$CP[cpdat$xerror<=(cpdat$xerror+cpdat$xstd)[min.e]][1],2)
ptree <- prune(rtree, cp =.05)
print(ptree)
plot(ptree)
rattle:::fancyRpartPlot(ptree)


#dat$w2[dat$y==1] <- 1
#dat$w2[dat$y==0] <- round(dat$w[dat$y==0]/sum(dat$w[dat$y==0]^2),0)
#love.plot(X, treat = dat$y, weights = dat$w2 , thresholds = c(m = .1), binary = "std",s.d.denom='treated')



#library(evtree)
#etree <- evtree(factor(y)~ comm_safety1416+comm_ce1416+fr_supportparent_cat1416, data=dat,weights=dat$w2)
#plot(etree)


table(cdat$y)
cfit <- ctree(factor(y)~ comm_safety1416+comm_ce1416+fr_supportparent_cat1416,data=cdat[!cdat$y==0&cdat$id%in%ts,],alpha = 0.5)
plot(cfit)


library(rpart)
rtree <- rpart(y~ comm_safety1416+comm_ce1416+fr_supportparent_cat1416, data=sdat[sdat$id%in%ts,],
  method = "class",cp=-1,)
cpdat <- data.frame(rtree$cptable)
min.e <- which.min(cpdat$xerror)
round(cpdat$CP[min.e],2)
round(cpdat$CP[cpdat$xerror<=(cpdat$xerror+cpdat$xstd)[min.e]][1],2)
ptree <- prune(rtree, cp =.04)
plot(as.party(ptree))

library(glmertree)
Htree <- glmertree(factor(y) ~ 1 | id | comm_safety1416+comm_ce1416+fr_supportparent_cat1416,
  data=sdat[sdat$id%in%ts,], family = "binomial", alpha = 1)
plot(Htree)

gtree <- glmtree(factor(y) ~1 | comm_safety1416+comm_ce1416+fr_supportparent_cat1416,
          cluster=sdat$id[sdat$id%in%ts],data=sdat[sdat$id%in%ts,],  family = 'binomial',alpha=100)

plot(gtree)
