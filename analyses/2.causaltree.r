#===============================================================================
### 3.- A casual tree
#===============================================================================
# A particular exposure (any traditional ACEs) has a special role
# Its eefct is estiamted adjustong bu covariates
# the regression tree is used to explore whter the effect is modified by other expousures

### a. Find balancing weights among those with and without any ACEs
#Entropy Balancing
#Reference: #https://web.stanford.edu/~jhain/Paper/eb.pdf
source('R/ebw.r')

# In orther to balance the missing pattern we need to
# for categorical variables, create an NA category (addNA)
# this should inlcude binary varaibles not declared as s
# for continuous, add indicator is.na and impute mean
x <- c('center', 'childrace_bsl','cohort','childsex_bsl','caregiver_married16','hh_income16')
X         <- model.matrix(~ .,data=lapply(dat[,x] ,addNA,ifany =T))[,-1]
dist_mat = distmat(t_ind, X)
dat$C <-
  dat$Z %>%
  mutate(across(where(is.factor),addNA,ifany=T))  %>%
  mutate(across(where(~ is.numeric(.x) && any(is.na(.x)) && n_distinct(.x)==2), ~addNA(factor(.x)))) %>%
  mutate(across(where(~ is.numeric(.x) && any(is.na(.x))), is.na,.names = 'NA_{.col}')) %>%
  mutate(across(where(is.numeric),~ replace(.,is.na(.),mean(.,na.rm=T))))  %>%
  model.matrix(~.,.) %>%
  .[,-1]

summary(dat)


tgt  <- colMeans(dat$C); tgt
ebw1 <- with(dat, ebw(id=id[anyACE==1], covariates=C[anyACE==1,], target.margins=tgt, base.weight = w[anyACE==1]))
ebw0 <- with(dat, ebw(id=id[anyACE==0], covariates=C[anyACE==0,], target.margins=tgt, base.weight = w[anyACE==0]))
dat  <- left_join(dat,rbind(ebw0,ebw1))

with(dat,data.frame(C,anyACE,w,wb)) %>%
  group_by(anyACE) %>%
  summarize(across(everything(), list(
  ~ weighted.mean(.,w),
  ~ weighted.mean(.,wb))
  ))
df0 <-
  dat %>%
  filter(training.sample==1)

### b. find effect modifiers
#library(devtools)
#install_github("susanathey/causalTree")
#Reference https://doi.org/10.48550/arXiv.1504.01132
library(causalTree)

###Transformed Outcome Trees (TOT), not the recomended approach
#tree <-  with(df0, causalTree(y~ ., data = cbind(y,X,Z), treatment =  anyACE, weights=wb))
#rpart.plot(tree,roundint=FALSE)

###Causal Trees (CT)
tree <- with(df0, causalTree(y~ ., data = cbind(y,X,Z), treatment = anyACE, weights=wb,
                   split.Rule = "CT", cv.option = "CT", split.Honest = T, cv.Honest = T,cp = 0))

opcp <- tree$cptable[,1][which.min(tree$cptable[,4])]
ptree.causal <- prune(tree, cp=opcp)

rpart.plot(ptree.causal)



png('output/tree.plot.causal.png',width = 480*4,heigh=480*4,res=300)
rpart.plot(tree.causal)
dev.off()

#===============================================================================
### 4. Save class membership for all cases
#===============================================================================

dat$node.cls  <- factor(predict(as.party(ptree),type='node',newdata=dat))
dat$node.cnd  <- factor(predict(as.party(ptree.cond),type='node',newdata=dat))
dat$node.cau  <- factor(predict(as.party(ptree.causal),type='node',newdata=dat))



saveRDS(dat,'data/test.tree.Rds')
