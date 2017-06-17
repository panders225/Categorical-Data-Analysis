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

help(relevel)
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
        , type="response"
        ) 

#4.16C
predict(mbti_mod
        , newdata=data.frame(e_flag=1, s_flag=0, t_flag=1, j_flag=0)
        , type="response"
        ) 
