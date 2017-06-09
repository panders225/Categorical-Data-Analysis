
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

# high levels of wafer thickness are associated with lower rates
# of imperfections
# having a thicker wafer results in an 80% chance of imperfection rate

