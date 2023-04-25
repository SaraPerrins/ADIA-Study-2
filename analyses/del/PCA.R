options(digits = 3)
options(scipen = 1000000)
set.seed(0203)

library(tidyverse)

setwd('C:/Users/21983/OneDrive - ICF/ADIA/study 2') 
outv <- 'RB_deliq_hi16' # RB_deliq and RB_deliq_Mod do not enough outcome for match control

#===============================================================================

dat <- read.csv('data/LS_all_c3.csv')
head(dat)
colnames(dat)
summary(dat)
### 1.- We focus on population expericing ACE during childhood
# (and before the otucome was asssess)
dat$anyACE <- as.numeric(apply(
  dat[, c('M','MP','MS','MN','MF','ML','ME','MM','MD')], 1, sum) >= 1)
table(dat$anyACE) #0=383 1=971

#===============================================================================
### I.- scale all PCEs
#===============================================================================

zn <- c("anysupadu_s_1",	"anysupparent_s_1",	"anysuprelative_s_1",	"anysupnonfam_s_1",	"fam_sat_s_1",	"home_safety_s_1"
        ,	"prrelation_s_1",	"neighborhood_exp_s_1",	"school_safety_t_s_1",	"srvc_use_s_1",	"childcare_1",	"neighborhood_safety_s_2"
        ,	"neighborhood_exp_s_2",	"school_safety_y_s_2",	"school_safety_t_s_2",	"srvc_use_s_2",	"anysupadu_s_3",	"anysupparent_3"
        ,	"anysuprelative_3",	"anysupnonfam_3",	"prrelation_s_3",	"bestfriend_3",	"socialpart_s_3",	"parent_involv_s_3",	"resid_stab_3"
        ,	"neighborhood_safety_s_3",	"neighborhood_exp_s_3",	"srvc_use_s_3", "RB_deliq_hi")

summary(dat[, zn])

PF <- dat[, zn]

PF %>% 
  glimpse()

scaled_pf <- PF %>%
  drop_na()%>%
  mutate(RB_deliq_hi=factor(RB_deliq_hi)) %>%
  select(where(is.numeric)) %>%
  scale()

scaled_pf %>% 
  head()

which(apply(scaled_pf, 2, var)==0)

scaled_pf <- scaled_pf[ , which(apply(scaled_pf, 2, var) != 0)]

RB_deliq_hi <-  dat %>%
  drop_na() %>%
  pull(RB_deliq_hi)

pc <- prcomp(scaled_pf)

#pc <- prcomp(na.omit(dat[,zn]), center = TRUE, scale. = TRUE)

attributes(pc)
str(pc)
p_pca <- pc$x %>% 
  as.data.frame() %>%
  ggplot(aes(x=PC1, y=PC2,color=RB_deliq_hi))+
  geom_point()+
  labs(subtitle="PCA")

#===============================================================================
### II.- Conditional logistic regression
#===============================================================================

library(survival)

cfit2 <- clogit(RB_deliq_hi ~
                  PC1+PC2
                + strata(id)
                , data = dat)

summary(cfit2)
