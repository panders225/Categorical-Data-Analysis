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


summary(credit_mod)


# 5.13B - get an overall goodness of fit
hlt <- ResourceSelection::hoslem.test(
    x=credit2$creditability
    , y=fitted(credit_mod)
    , g=10
)
hlt
