library("tidyverse")

# 4.2A
qnorm(0.95)
1 - pnorm(2.44, mean=0, sd=1)

1 - pchisq(8.30, df=1)

# 4.5
# first, input the data

temp <- c(66,70,69,68,67,72,73,70,57,63,70,78,67,53,67
          ,75,70,81,76,79,75,76,58)
response <- c(0,1,0,0,0,0,0,0,1,1,1,0,0,1,0,0,0,0,0,0,1,0,1)
oring <- data.frame(cbind(temp, response))

boxplot(temp ~ response, data=oring)
# looks like there is an association with lower temps and failure

# fit the regression
oring_mod <- glm(
                formula = response ~ temp
                  , data=oring
                , family=binomial(link="logit")
                )

summary(oring_mod)
anova(oring_mod)


# 4.7A
# input the data
k_pres <- c(12,15,42,52,59,73,82,91,96,105,114,120,121,128
            ,130,139,139,157)
k_abs <- c(1,1,2,8,11,18,22,31,37,61,72,81,97,112,118,127,131
           ,140,151,159,177,206)
k_tot <- c(k_pres, k_abs)

k_status <- c(rep(1,length(k_pres)), rep(0, length(k_abs)))

kyphosis <- cbind(k_tot, k_status) %>%
  data.frame()

# fit a logistic regression using age as the predictor

k_mod <- glm(formula = k_status ~ k_tot
    , data= kyphosis
    , family=binomial(link="logit"))

summary(k_mod)
anova(k_mod)
kyphosis$k_tot


ggplot(kyphosis, aes(factor(k_status), k_tot)) + 
  geom_boxplot() +
  xlab("Kyphosis Present") +
  ylab("Age (Months)") + 
  ggtitle("Age Dispersion Across Kyphosis Status")
  

# fit a logistic regression including a quadratic term

k_mod2 <- glm(
            formula= k_status ~ k_tot + I(k_tot^2)
              , data=kyphosis
            , family=binomial(link="logit")
              )

summary(k_mod2)
anova(k_mod2)

1 - pchisq(6.2762, df=1)

# create a plot and overlay the regression line
plot(kyphosis$k_tot
     , kyphosis$k_status
     , main="Kyphosis Status vs. Age"
     , xlab="Age (Months)"
     , ylab="Kyphosis Status"
     )
curve(predict(k_mod
              , data.frame(k_tot=x)
              , type="response"), add=TRUE)


# 4.8
# import the horseshoe crab data
crabby <- read.csv("C:/Users/Philip/Schools/TAMU/STAT_659/homework/git_hw/agresti_crab.csv")

# create a binary indicator for the presence of a satellite
crabby$sat_pres <-ifelse(crabby$satell > 0, 1, 0)

# fit a logistic model using weight as the predictor of satell presence
crab_mod <- glm(formula=sat_pres ~ weight
                , data=crabby
                , family=binomial(link="logit"))

summary(crab_mod)
anova(crab_mod)

predict(crab_mod
        , newdata = data.frame(weight=c(1.2, 2.44, 5.2))
        , type = "resp"
        )

predict(crab_mod
        , newdata=data.frame(weight=c(3.036, 2.136, 2.616))
        , type="response")


# 4.9
# A

crab_mod2 <- glm(
                formula=sat_pres ~ relevel(factor(color), ref = 4)
                  , data=crabby
                , family=binomial(link="logit")
                )

summary(crab_mod2)
anova(crab_mod2)  
1 - pchisq(13.698, df=1)

# 4.9C - treat color as a quantitative variable

crab_mod3 <- glm(
              formula=sat_pres ~ color
              , data=crabby
              , family=binomial(link="logit")
                )
summary(crab_mod3)
anova(crab_mod3)
1 - pchisq(12.461, df=1)


# 4.15D (do D first)
merit <-
  array(c(24, 47, 9, 12,
          10, 45, 3, 8,
          5, 57, 4, 9,
          16, 54, 7, 10,
          7, 59, 4, 12
          )
        , dim=c(2,2,5)
        , dimnames=list(
          Race=c("White", "Black")
          , Qualify=c("Y", "N")
          , District=c("NC", "NE", "NW", "SE", "SW")
        )
        )

(merit_df <- data.frame(ftable(merit)))
merit_df$qual_fl <- ifelse(merit_df$Qualify=="Y",1,0)
merit_df

# conduct a logistic regression for the combined data first 

logit_one <- glm(formula=qual_fl ~ Race*District 
             , data=merit_df
             , weights=Freq
             , family=binomial(link="logit")
             )
summary(logit_one)
anova(logit_one)

DescTools::BreslowDayTest(merit)



# 4.15A
mantelhaen.test(merit)

# 4.15B
logit_two <- glm(
          formula=qual_fl~Race + District
          , data=merit_df
          , weights = Freq
          , family=binomial(link="logit")
            )

summary(logit_two)
anova(logit_two)

# 4.16
# read in the data
mbti <- read.csv("C:/Users/Philip/Schools/TAMU/STAT_659/homework/git_hw/mbti.csv")

# fit a model using the four scales as predictors 
names(mbti) <- tolower(names(mbti))
mbti
mbti_mod <- glm(
                formula=alc_flag ~ e_flag + s_flag + t_flag + j_flag
                , data=mbti
                , family=binomial(link="logit")
                , weights=freq
                  )
summary(mbti_mod)

# 4.16B - prediction for someone with ESTJ

predict(mbti_mod
        , newdata=data.frame(e_flag=1, s_flag=1, t_flag=1, j_flag=1)
        ) 

predict(mbti_mod
        , newdata=data.frame(e_flag=1, s_flag=1, t_flag=1, j_flag=1)
        , type="response"
        ) 

#4.16C
predict(mbti_mod
        , newdata=data.frame(e_flag=1, s_flag=0, t_flag=1, j_flag=0)
        , type="response"
        ) 


# 4.17
mbti$i_flag <- abs(1-mbti$e_flag)

test <- glm(
  formula=alc_flag ~ i_flag + t_flag
  , data=mbti
  , family=binomial(link="logit")
  , weights=freq
  )
summary(test)


# 4.22
# going to be using the crab data again

crab_dat <- read.csv("C:/Users/Philip/Schools/TAMU/STAT_659/homework/git_hw/agresti_crab.csv")

#encode a dummy flag for satellite
crab_dat$sat_pres <- ifelse(crab_dat$satell > 0, 1, 0)

# 4.22A - fit the model
crab_dat_mod <- glm(
       formula=sat_pres ~ relevel(factor(color), ref = 4) + weight
                    , data=crab_dat
                    , family=binomial(link="logit")
                    )
summary(crab_dat_mod)
#4.22B - likelihood ratio test for effect of color
anova(crab_dat_mod)
1 - pchisq(13.698, df=3)

#4.22C

crab_dat_mod2 <- glm(
        formula= sat_pres ~ color + weight
          , data=crab_dat
        , family=binomial(link="logit")
                       )
summary(crab_dat_mod2)
anova(crab_dat_mod2)
1 - pchisq(12.461, df=1)

# 4.24
# input the data
duration <- c(45,15,40,83,90,25,35,65,95,35,75,45
              , 50, 75,30,25,20,60,70,30,60,61,65,15,
              20,45,15,25,15,30,40,15,135,20,40)
trach <- c(0,0,0,1,1,1,0,0,0,0,0,1
           , 1,1,0,0,1,1,1,0,0,0,0,1
           , 1,0,1,0,1,0,0,1,1,1,1)
y <- c(0,0,1,1,1,1,1,1,1,1,1,1
       ,0,1,0,1,0,1,1,1,1,0,1,0,
       0,1,0,1,0,1,1,0,1,0,0)
sore <- cbind(duration, trach, y) %>%
  data.frame()

sore_mod <- glm(
  formula= y ~ trach + duration
  , data=sore
  , family=binomial(link="logit")
                  )
summary(sore_mod)
anova(sore_mod)
1 - pchisq(12.44, df=1)
# fit a model with an interaction term
sore_mod <- glm(
  formula= y ~ trach + duration + trach*duration
  , data=sore
  , family=binomial(link="logit")
  )
summary(sore_mod)
anova(sore_mod)

1 - pchisq(1.82, df=1)


#4.30
# input the data

white_fl <- c(rep(1, 796+1625), rep(0,143+660))
female_fl <- c(rep(1,796), rep(0,1625), rep(1,143), rep(0,660))

grad_fl <- c(rep(1,498), rep(0,796-498)
             , rep(1,878), rep(0,1625-878)
             , rep(1,54), rep(0,143-54)
             , rep(1,197), rep(0,660-197)
             )
athlete <- cbind(white_fl, female_fl, grad_fl) %>%
  data.frame()

athlete_mod <- glm(
  formula= grad_fl ~ white_fl + female_fl
  , data=athlete
  , family=binomial(link="logit")
                     )

summary(athlete_mod)
anova(athlete_mod)

athlete_mod2 <- glm(
  formula= grad_fl ~ white_fl + female_fl + white_fl*female_fl
  , data=athlete
  , family=binomial(link="logit")
                     )
summary(athlete_mod2)
AIC(athlete_mod) - AIC(athlete_mod2)

# Additional Problem

berkeley <- array(c(512, 89, 313,19,
                    353, 17,207,8,
                    120,202,205,391,
                    138,131,279,244,
                    53,94,138,299,
                    22,557,1493,1278
                    )
                  , dim=c(2,2,6)
                  , dimnames=list(Gender=c("Male", "Female")
                                  , Admit=c("Yes", "No")
                                  , Department=c("1","2","3","4","5","6")
                                  )
                  )

# Part A - report the conditional odds ratio for each department
library(epitools)

for(i in 1:dim(berkeley)[3]) {
  obj <- berkeley[,,i] %>% epitools::epitab()
  print("_________________")
  print(paste("Department", i))
  print(obj$tab)
  print("__________________")
  }

# part B
# construct a logistic regression to match the Breslow-Day test
# build a model featuring the interaction term, grab the loglik and 
# the degrees of freedom from that 
# then build one without the interaction term, do the same thing 
# LR = LRreduced - LRfull 
# df = dffull - df reduced

male <- c(rep(1,12), rep(0,12))
dept <- c(rep(1,4), rep(2,4), rep(3,4), rep(4,4), rep(5,4), rep(6,4))
y_flag <- c(rep(1,6), rep(0,6), rep(1,6), rep(0,6))
freq <- c(512,353,120,138,53,22,313,207,205,279,138,351
          , 89,17,202,131,94,24,19,8,391,244,299,317)
berk_long <- cbind(male, dept, y_flag, freq) %>% data.frame()
berk_long$dept <- relevel(factor(berk_long$dept), ref = 6)

berk_modfull <- glm(
  formula=y_flag ~ male + dept + male*dept
                      , data=berk_long
                    , family=binomial(link="logit")
                    , weights=freq
                    )
berk_modred <- glm(formula=y_flag ~ male 
                      , data=berk_long
                    , family=binomial(link="logit")
                    , weights=freq
                    )

logLik(berk_modfull)
#295 on 6 df
logLik(berk_modred)
#2975 on 2 df

1 - pchisq( (2975-295), df=6-2 )
# very small p-value, odds ratios not significant

berk_modcmh <- glm(formula=y_flag ~ male + dept
                   , data=berk_long
                   , family=binomial(link="logit")
                   , weights=freq
                   )  
summary(berk_modcmh)
anova(berk_modcmh)
# when you control for department, the gender effect is wiped out

berkeley2 <- berkeley[,,2:6]
berkeley2
berk_long2 <- subset(berk_long, dept != 1)

BreslowDayTest(berkeley2)

berk_mod_final <- glm(formula=y_flag ~ male + dept
    , data=berk_long2
    , family=binomial(link="logit")
    , weights=freq
    )
summary(berk_mod_final)

  glm(formula=y_flag ~ male 
    , data=berk_long2
    , family=binomial(link="logit")
    , weights=freq
    ) %>%
summary()
  