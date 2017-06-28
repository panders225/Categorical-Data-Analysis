library("tidyverse")


#5.4A
# pearson chi-squared stat of 11.9756 on 11 df

1 - pchisq(10.9756, df=11)

# b
1 - pchisq(0.80, df=1)

# c
1 - pchisq(7.4091, df=5)

mbti <- read.csv("C:/Users/Philip/Schools/TAMU/STAT_659/homework/git_hw/mbti.csv")
sum(mbti$Freq)
sum(subset(mbti, Alc_flag==1)$Freq)
sum(subset(mbti, Alc_flag!=1)$Freq)

# 5.7
mbti %>% head()

# 5.10
crab_dat <- read.csv("C:/Users/Philip/Schools/TAMU/STAT_659/homework/git_hw/agresti_crab.csv")

# create a variable for satellite presence 
crab_dat$sat_pres <- ifelse(crab_dat$satell > 0, 1, 0)
table(crab_dat$sat_pres, crab_dat$satell)

crab_mod <- glm(formula=sat_pres ~ weight
                  , data=crab_dat
                 , family=binomial(link="logit") 
                )

help("confusionMatrix")
str(crab_mod)
crab_mod$fitted.values %>% summary()
crab_dat %>% head()
plot(density(crab_mod$fitted.values))

crab_pred <- cbind(crab_dat$sat_pres, crab_mod$fitted.values) %>% 
  data.frame()

names(crab_pred) <- c("sat_pres", "predict_val")
crab_pred$class <- ifelse(crab_pred$predict_val >= 0.642
                          , "sat_predicted"
                          , "sat_not_predicted") %>% 
  as.factor()
crab_tab <- table(crab_pred$class, crab_pred$sat_pres)
t(crab_tab)

# sensitivity
(68 / 111)
#specificity
(45 / 62)

# Form an ROC curve
library(ROCR)

pred <- ROCR::prediction(predictions=crab_mod$fitted.values
                 , labels=crab_dat$sat_pres
                 )

perf <- ROCR::performance(pred
                          , "tpr"
                          , "fpr"
                          )
plot(perf
     , main="ROC Curve for satellite presence"
)

#C conduct the Hosmer-Lemeshow statistic

library("ResourceSelection")
hlt <- ResourceSelection::hoslem.test(
    x=crab_dat$sat_pres
    , y=fitted(crab_mod)
    , g=10
)
hlt
head(cbind(hlt$observed, hlt$expected))

for (i in 5:15) {
  print(ResourceSelection::hoslem.test(
    x=crab_dat$sat_pres
    , y=fitted(crab_mod)
    , g=i
  ))
}

# we have no evidence of a poor fit by the hosmer-lemeshow test

# fit a new model, with a quadratic term


crab_mod2 <- glm(formula=sat_pres ~ weight + I(weight**2)
                  , data=crab_dat
                 , family=binomial(link="logit") 
                )
summary(crab_mod2)
summary(crab_mod)
1 - pchisq(deviance(crab_mod) - deviance(crab_mod2), df=1)

AIC(crab_mod2)
AIC(crab_mod)
# crab mod is superior

# 5.13
# import the credit data

credit <- read.csv("C:/Users/Philip/Schools/TAMU/STAT_659/homework/git_hw/german_credit.csv")
names(credit) <- tolower(names(credit))
names(credit)

# variables eligible for modeling
#account.balance, duration.of.credit..month
#payment.status.of.previous.credit
# purpose, sex...marital.status
head(credit)
summary(credit)


table(credit$account.balance)
# 1 = no money in account
# 2 = 0 - 200 DM
# 3 = > 200 DM
# 4 = no checking account - make this the base level

summary(credit$duration.of.credit..month.)
plot(density(credit$duration.of.credit..month.))
# use as-is

table(credit$payment.status.of.previous.credit)
#0 : no credits taken/ all credits paid back duly 
#1 : all credits at this bank paid back duly 
#2 : existing credits paid back duly till now 
#3 : delay in paying off in the past 
#4 : critical account/ other credits existing (not at this bank) 
# it's kindof gross but treat this as a linear variable


table(credit$purpose)
#0 : car (new) 
#1 : car (used) 
#2 : furniture/equipment 
#3 : radio/television 
#4 : domestic appliances 
#5 : repairs 
#6 : education 
#7 : (vacation - does not exist?) 
#8 : retraining 
#9 : business 
#10 : others 
# encode as factor with a reference level of other

table(credit$sex...marital.status)
#1 : male : divorced/separated 
#2 : female : divorced/separated/married 
#3 : male : single 
#4 : male : married/widowed 
#5 : female : single 

# this will require recoding - split the marital status 
# and gender 


credit2 <- credit %>%
  dplyr::select(creditability
             , account.balance, duration.of.credit..month.
             , payment.status.of.previous.credit
             , purpose, sex...marital.status
               )

credit2$male_fl <- ifelse(credit2$sex...marital.status==1 
                          | credit2$sex...marital.status==3
                          | credit2$sex...marital.status==4
                          , 1, 0 )

table(credit2$male_fl, credit2$sex...marital.status)

credit2$divorce_fl <- ifelse(credit2$sex...marital.status==1 | credit2$sex...marital.status==2, 1, 0)

credit2$single_fl <- ifelse(credit2$sex...marital.status==3 | credit2$sex...marital.status==5, 1, 0)

table(credit2$divorce_fl, credit2$sex...marital.status)
table(credit2$single_fl, credit2$sex...marital.status)

credit2 <- credit2 %>% 
  dplyr::select(-sex...marital.status)

table(credit$creditability)
credit_mod <- glm(formula=
                 creditability ~
                   relevel(as.factor(account.balance), ref=4) 
                 + duration.of.credit..month.
                 + payment.status.of.previous.credit
                + relevel(as.factor(purpose), ref= 10)
                + divorce_fl
                + single_fl
                + male_fl
                , data=credit2
                , family=binomial(link="logit")
                  )
AIC(credit_mod)
summary(credit_mod)

credit_mod2 <- glm(formula=
                 creditability ~
                   relevel(as.factor(account.balance), ref=4) 
                 + duration.of.credit..month.
                 + payment.status.of.previous.credit
#                + relevel(as.factor(purpose), ref= 10)
                + divorce_fl
                + single_fl
                + male_fl
                , data=credit2
                , family=binomial(link="logit")
                  )
AIC(credit_mod2)
summary(credit_mod2)

credit_mod3 <- glm(formula=
                 creditability ~
                   relevel(as.factor(account.balance), ref=4) 
                 + duration.of.credit..month.
                 + payment.status.of.previous.credit
                + relevel(as.factor(purpose), ref= 10)
                + divorce_fl
                + single_fl
#                + male_fl
                , data=credit2
                , family=binomial(link="logit")
                  )
AIC(credit_mod3)
summary(credit_mod3)

credit_mod4 <- glm(formula=
                 creditability ~
                   relevel(as.factor(account.balance), ref=4) 
                 + duration.of.credit..month.
                 + payment.status.of.previous.credit
                + relevel(as.factor(purpose), ref= 10)
#                + divorce_fl
                + single_fl
#                + male_fl
                , data=credit2
                , family=binomial(link="logit")
                  )
AIC(credit_mod4)
summary(credit_mod4)

credit_mod5 <- glm(formula=
                 creditability ~
                   relevel(as.factor(account.balance), ref=4) 
                 + duration.of.credit..month.
                 + payment.status.of.previous.credit
                + relevel(as.factor(purpose), ref= 10)
#                + divorce_fl
                + single_fl
#                + male_fl
                , data=credit2
                , family=binomial(link="logit")
                  )
AIC(credit_mod5)
summary(credit_mod5)



# 5.13B - get an overall goodness of fit
hlt <- ResourceSelection::hoslem.test(
    x=credit2$creditability
    , y=fitted(credit_mod5)
    , g=10
)
hlt


# 5.14
# input the data

long <- cbind(
c(rep(0, 4), rep(1,4), rep(2,4))
, c(rep(1,1), rep(0,3), rep(1,2), rep(0,2), rep(1,4))
) %>% data.frame()
names(long) <- c("x", "response")

short <- data.frame(
x=c(0,1,2)
, freq= c(4,4,4)
, response=c(0.25,0.5,1)
)

# long first, intercept-only
long_mod_int <- glm(
                  formula=response~1
                 , data=long
                 , family=binomial(link="logit")
                 )

long_mod_cov <- glm(
                  formula=response~x
                 , data=long
                 , family=binomial(link="logit")
                 )

short_mod_int <- glm(
                  formula=response~1
                  , data=short
                  , family=binomial(link="logit")
                  , weights=freq
                  )
short_mod_cov <- glm(
                  formula=response~x
                  , data=short
                  , family=binomial(link="logit")
                  , weights=freq
                  )

logLik(long_mod_int)
logLik(short_mod_int)
logLik(long_mod_cov)
logLik(short_mod_cov)

summary(long_mod_int)
summary(short_mod_int)

summary(long_mod_cov)
summary(short_mod_cov)



# 5.18
# input the data

smokes <- array(c(126, 35, 100, 61,
        908, 497,688,807,
        913, 336, 747, 598,
        235, 58, 172, 121,
        402, 121, 308, 215,
        182, 72, 156, 98,
        60, 11, 99, 43,
        104, 21, 89, 36
        )
      , dim=c(2,2,8)
      , dimnames=list(
        smoke=c("smoke", "non")
        , cancer=c("1","0")
        , study=c("Beijing", "Shanghai", "Shenyang", "Nanjing"
                  , "Harbin", "Zhengzhou", "Taiyuan", "Nanchang")
      )
)

smokes2 <- ftable(smokes) %>% data.frame()
smokes2$cancer_fl <- ifelse(smokes2$cancer=="1", 1,0)

smokes2$smoke_fl <- ifelse(smokes2$smoke=="smoke", 1, 0)


# construct a model
smoke_mod <- glm(
  formula=cancer_fl ~ study + smoke_fl
  , data=smokes2
  , family=binomial(link="logit")
  , weights=Freq
)

summary(smoke_mod)
exp(0.777062)

(pear_stat <- sum(residuals(smoke_mod, type = "pearson")^2))
1 - pchisq(pear_stat, df=sum(smokes2$Freq)-8 )

par(mfrow=c(2,2))
plot(smoke_mod)
par(mfrow=c(1,1))
plot(density(residuals(smoke_mod)))

qqnorm(residuals(smoke_mod))
qqline(residuals(smoke_mod))
shapiro.test(residuals(smoke_mod))




