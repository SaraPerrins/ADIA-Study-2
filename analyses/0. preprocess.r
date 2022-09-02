library(tidyverse)

dat <- read.csv('data/LS_SUD.csv')

dat$anyACE <- apply(dat[,c('M','MP','MS','MN','MF','ML','ME','MM','MD')],1,sum)>1
table(dat$anyACE )

dat$cohort <- as.numeric(substr(dat$ChildDOB,6,9))
summary(dat$cohort)

saveRDS(dat, file='data/LS_SUD.Rds')
