
# work for homework three 

#3.4
# first, input the data

outcome <- c(
  rep(1,48), rep(0,17066)
  , rep(1,38), rep(0, 14464)
  , rep(1,5), rep(0,788)
  , rep(1,1), rep(0,126)
  , rep(1,1), rep(0,37)
  )

alc <- c(
          rep(0,17114)
         , rep(0.5, 14502)
         , rep(1.5, 793)
         , rep(4,127)
         , rep(7,38)
         )
(length(outcome) - length(alc))

ungrouped <- data.frame(cbind(outcome, alc))
table(ungrouped$outcome, ungrouped$alc)

test <- lm(outcome ~ alc, data=ungrouped)
summary(test)


# perform a sensitivity analysis by removing the observation
# where outcome == 1 and alc==7

ungrouped_sens <- ungrouped[!(ungrouped$outcome==1 & ungrouped$alc==7),]

nrow(ungrouped) - nrow(ungrouped_sens)

sens_lm <- lm(outcome ~ alc, data=ungrouped_sens)
summary(sens_lm)

re_score <- predict(
        sens_lm
        , data.frame(alc=c(0,7)) 
        ) %>%
  data.frame()

# re-calculate the relative risk
re_score[2,1] / re_score[1,1]

# the Relative Risk appears to have decreased with removing 
# the most extreme value.  Note though that the model is no
# longer significant

# part B - is the model sensitive to the choice of scores?

outcome <- c(
  rep(1,48), rep(0,17066)
  , rep(1,38), rep(0, 14464)
  , rep(1,5), rep(0,788)
  , rep(1,1), rep(0,126)
  , rep(1,1), rep(0,37)
  )

alc <- c(
          rep(0,17114)
         , rep(1, 14502)
         , rep(2, 793)
         , rep(3,127)
         , rep(4,38)
         )

score_sens <- cbind(outcome, alc) %>%
  data.frame()

score_sens_lm <- lm(outcome ~ alc, data=score_sens)
summary(score_sens_lm)

# it appears that the model is no longer significant, as it fails 
# the F-test.  Even if we had significance, the term on the alc
# variable is mchmuch lower.

re_score_sens <- predict(score_sens_lm
                    , data.frame(alc=c(0,7))) %>%
                  data.frame()

re_score_sens[2,1] / re_score_sens[1,1]

# Relative Risk of 2.92 - the score shift sensitivity analysis
# has roughly the same impact as removing the most extreme observation

# 3.3C - fit a logistic regression

outcome <- c(
  rep(1,48), rep(0,17066)
  , rep(1,38), rep(0, 14464)
  , rep(1,5), rep(0,788)
  , rep(1,1), rep(0,126)
  , rep(1,1), rep(0,37)
  )

alc <- c(
          rep(0,17114)
         , rep(0.5, 14502)
         , rep(1.5, 793)
         , rep(4,127)
         , rep(7,38)
         )

part_c <- data.frame(cbind(outcome, alc))

ungrouped_logit <- glm(
                        outcome ~ alc
                       , data = part_c
                       , family=binomial(link=logit) 
                       )

summary(ungrouped_logit)

# the sign on the alc variable is positive - this indicates 
# that we have a relationship between drinking and malformations


# 3.5 - snoring and heart disease problem

# first, load in the data
# we are going to be altering the scores
snore_i <- c(0,2,4,6)
snore_ii <- c(0,1,2,3)
snore_iii <- c(1,2,3,4)
disease <- c(24,35,21,30)
total <- c(1379,638,213,254)

logit_mod_score <- function(snore_n) 
  {
               glm(
                  formula=(disease / total) ~ snore_n
                  , weights=total
                  , family=binomial(link="logit")
                  )
  }

snore_glm_i <- logit_mod_score(snore_i)
snore_glm_ii <- logit_mod_score(snore_ii)
snore_glm_iii <- logit_mod_score(snore_iii)


summary(snore_glm_i)
summary(snore_glm_ii)
summary(snore_glm_iii)

# compare the parameters
cbind(snore_glm_i$coefficients
      , snore_glm_ii$coefficients
      , snore_glm_iii$coefficients
      ) %>%
  as.matrix(, nrow=2)
# estimated coefficients are identical

# compare the fitted values
cbind(snore_glm_i$fitted.values 
      , snore_glm_ii$fitted.values 
      , snore_glm_iii$fitted.values 
     ) %>%
  as.matrix(, nrow=4, ncol=3)
# the fitted values are identical

# as long as you maintain the relative spacings of the 
# variables, you are able to maintain the relative impact

# 3.11 - Poisson Regression

trt_a <- c(8,7,6,6,3,4,7,2,3,4)
trt_b <- c(9,9,8,14,8,13,11,5,7,6)
trt<- c(trt_a, trt_b)

trt_fl <- c(rep(0, 10), rep(1,10))

# 3.11 B - fit a poisson model to the above data

silicon_mod <- glm(formula=trt ~ trt_fl
                , family=poisson(link=log)
                   )
summary(silicon_mod)
paste(
  silicon_mod$coefficients[1]
  , '+'
  , silicon_mod$coefficients[2]
  , 'x'
  )

# get the average ratios
silicon_mod$coefficients[2] %>% 
  exp()
# notice that the above is the same as:
mean(trt_b) / mean(trt_a)

summary(silicon_mod)

# the reported wald value is 3.332, with a small p-value 
anova(silicon_mod)
# the anova function gives the LR statistic
test <- anova(silicon_mod)
1-pchisq(test$Deviance[2], df=test$Df[2])

# 3.11D - generate confidence intervals for mu_b / mu_a

c(
  0.5878 - 1.96*(0.1764)
 , 0.5878 + 1.96*(0.1764)
)%>%
  exp()


# 3.12 - add data to the model

trt_thic <- c(rep(0,5), rep(1,5), rep(0,5), rep(1,5))


silicon_mv <- glm(formula=trt ~ trt_fl + trt_thic  
                , family=poisson(link=log)
                   )

summary(silicon_mv)

exp(-0.2296)

# model is a good fit for the data
pchisq(deviance(silicon_mv), df.residual(silicon_mv), lower.tail = FALSE)

# high levels of wafer thickness are associated with lower rates
# of imperfections
# having a thicker wafer results in an odds ratio that is 80% lower
# than having a thin wafer

# Question 3.13 - HORSESHOE CRAB TIME
# import the data


crabby <- read.csv('C:/Users/Philip/Schools/TAMU/STAT_659/homework/git_hw/agresti_crab.csv')

# 3.13A - using x = weight and y = num(satellites), fit a Poisson
# loglinear regression

crab_model <- glm(
                  formula=satell ~ weight
                  , data=crabby
                  , family=poisson(link=log)
                  )

summary(crab_model)

#asses the fit of the model
pchisq(deviance(crab_model), df.residual(crab_model), lower.tail = FALSE)
# horrible, but we will proceed anyway

# 3.13B - estimate the mean of Y for female crabs, average weight 2.44kg

predict(crab_model, newdata = data.frame(weight=c(2.44)) ) %>%
  exp()

# 3.13C - use beta_hat to describe the weight effect 
summary(crab_model)
summary(crab_model)$coefficients[2,2]
(crab_bh <- summary(crab_model)$coefficients[2,1])
(crab_se <- summary(crab_model)$coefficients[2,2])

(lb <- crab_bh - qnorm(0.975)*crab_se)
(ub <- crab_bh + qnorm(0.975)*crab_se)
exp(lb);exp(ub)

# 3.13D - conduct a wald test on the weight coefficient
paste(crab_bh, "/", crab_se, "^2")
# the asymptotic wald statistic has a N(0,1) dist
# squaring it results in a chi-squared distribution with df=1
(crab_wald2 <- (crab_bh/crab_se)**2)
pchisq(crab_wald2, df=1, lower.tail=F)
#strong significance 

# 3.13E - conduct the likelihood ratio test 

(crab_lr <- anova(crab_model)$Deviance[2])
(crab_lrdf <- anova(crab_model)$Df[2])
pchisq(crab_lr, df=crab_lrdf, lower.tail=FALSE)
# strong significance of a relationship between the log(weight)

# 3.14 - still using the crabby dset, fit the same model
# but now assuming a negative binomial distribution for the response
library(MASS)
help(MASS::glm.nb())
crabby_nb <- MASS::glm.nb(
                          formula=satell ~ weight
                          , data=crabby
                          , link=log
                          )

summary(crabby_nb)

# 3.14A - report the prediction equation
summary(crabby_nb) %>%
  str()

# make this object actually useful
crabby_nb2 <- MASS::glm.convert(crabby_nb)

(crab_nb_coef <- summary(crabby_nb2)$coefficients)

crab_nb_coef[1,1]
crab_nb_coef[2,1]

# find the estimate of the dispersion parameter
crabby_nb2$theta
crabby_nb2$SE.theta

pchisq(deviance(crabby_nb2), df.residual(crabby_nb2), lower.tail=FALSE)
# much better evidence of this model fitting the data than Poisson via test

# we can use the Rootogram to visually assess fit
library("countreg")

countreg::rootogram(crabby_nb2, main="Neg. Binomial Fit")
countreg::rootogram(crab_model, main="Poisson Fit")

# 3.14B - construct a 95% confidence interval for Beta

summary(crabby_nb2)
(crab_nb_bh <- summary(crabby_nb2)$coefficients[2,1])
(crab_nb_se <- summary(crabby_nb2)$coefficients[2,2])

(lb_nb <- crab_nb_bh - qnorm(0.975)*crab_nb_se)
(ub_nb <- crab_nb_bh + qnorm(0.975)*crab_nb_se)

# compare to the poisson model's beta confidence interval
paste(lb_nb, ub_nb)
paste(lb, ub)
# interval is slightly wider under the negative binomial model
# this is likely because we are allowing for the possibility of 
# overdispersion in our data
  

# 3.18
# first, input the data
three_18 <- cbind(
            c(404, 286, 443, 169, 222
            , 150, 321, 189, 258, 223
            , 211, 215, 108, 210, 224
            , 211, 168, 185, 158, 429
            , 226, 150, 148)
            
            , c(308, 197, 184, 149, 132, 126, 110
            , 101, 99, 81, 79, 78, 68
            , 67, 60, 57, 55, 44, 38, 35, 29, 20, 19)
            ) %>%
  data.frame()
names(three_18) <- c("attend", "arrests")
# if the number of arrests is a low-variance proportion of the total
# attendance, modeling arrests as a function of attendance could make sense

# 3.18B
soccer <- glm(
              formula = arrests ~ attend
              , data=three_18
              , family=poisson(link="log")
              )
summary(soccer)

soccer_test <- glm(
                  formula=arrests/attend 
                  , data=three_18
                  , family=poisson(link="log")
                    )

summary(soccer_test)
exp(summary(soccer_test)$coefficients[2,1])  

# 3.18C - plot arrests against attendance 

plot(
    y=three_18$arrests
    , x=three_18$attend 
    , main = "Arrests by Attendance"
    , xlab="Attendance"
    , ylab="Arrests"
    )
summary(soccer_test)
soccer_test$coefficients[2]
x_var <- seq(min(three_18$attend), max(three_18$attend), by=1)
y_var <- (
            soccer_test$coefficients[1] + 
            exp(soccer_test$coefficients[2])*x_var
          )
lines(x_var, y_var)  
x_var  
y_var
three_18  


# 3.19
1 - pchisq(11.6, df=1)

2.59**2
1 - pchisq(2.59**2, df=1)

# 3.20
# first, input the data

age_range <- c("35-44", "35-44", "45-54", "45-54", "55-64"
      , "55-64", "65-74", "65-74", "75-84", "75-84")
age_scores <- seq(1,5,by=1) %>% rep(2)
years<- c(18793, 52407, 10673, 43248, 5710, 28612
        ,2585, 12663, 1462, 5317) / 1000
deaths <- c(2, 32, 12, 104, 28, 206, 28, 186, 31, 102)
smoke <- rep(seq(0,1,by=1), 5)

celebrate <- cbind(years, deaths, smoke, age_scores) %>%
  data.frame()
celebrate$age_range <- as.factor(age_range)

# 3.20A - compute the sample coronary death rate per 1000 person-years

(one<- with(
  dplyr::filter(celebrate, smoke==0)
  , tapply(deaths/years, age_range, function(x){
      sprintf("Mean=%1.5f", mean(x))
      }
    )
  )
)
(two <- with(
  dplyr::filter(celebrate, smoke==1)
  , tapply(deaths/years, age_range, function(x){
      sprintf("Mean=%1.5f", mean(x))
      }
    )
  )
)

# signal not consistent - smoking appears to be associated with 
# higher death rates at some ages and lower death rates at others

# 3.20D 
# specify a main-effects poisson model for log rates

celebrate_mod <- glm(
                    formula=deaths ~ age_range + smoke + offset(log(years))
                    , data=celebrate
                    , family=poisson(link="log")
                    )
summary(celebrate_mod)

# The model assumes a constant ratio of death rates because it doesnt
# allow for an interaction between age and smoking

celebrate_mod2 <- glm(
      formula=deaths ~ age_range + smoke + age_scores*smoke + offset(log(years))
                    , data=celebrate
                    , family=poisson(link="log")
                    )
summary(celebrate_mod2)

deviance(celebrate_mod) ; deviance(celebrate_mod2)
(dev_diff <- deviance(celebrate_mod) - deviance(celebrate_mod2))
1 - pchisq(dev_diff, df=1)

# term needed - deviance significant

# additional question number 2
help("zeroinfl")
# fire up the zip glm
zip1 <- pscl::zeroinfl(
            formula=satell ~ weight |  weight
              , data=crabby
              , dist="poisson"
                )
summary(zip1)
AIC(zip1)

zip2 <- pscl::zeroinfl(
            formula=satell ~ weight | 1
              , data=crabby
              , dist="poisson"
                )
summary(zip2)
AIC(zip2)

zip3 <- pscl::zeroinfl(
            formula=satell ~ weight |  weight
              , data=crabby
              , dist="negbin"
                )
summary(zip3)
AIC(zip3)

zip4 <- pscl::zeroinfl(
            formula=satell ~ weight |  1
              , data=crabby
              , dist="negbin"
                )
summary(zip4)
AIC(zip4)

# compare and contrast
AIC(crab_model)

print("____________")
paste("Model 1", AIC(crab_model))
print("____________")
paste("Model 2", AIC(crabby_nb2))
print("____________")
paste("Model 3", AIC(zip1))
print("____________")
paste("Model 4", AIC(zip2))
print("____________")
paste("Model 5", AIC(zip3))
print("____________")
paste("Model 6", AIC(zip4))

# The Negative Binomial Model with weight in the zero model 
# has the lowest AIC of the six models fit