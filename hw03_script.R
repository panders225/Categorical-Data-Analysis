
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

