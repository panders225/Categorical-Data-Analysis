library("tidyverse")

# 7.9
# input the data SAS-style

admit <- rep(c(rep("Y",6), rep("N",6)), 2)
gender <- c(rep("M", 12), rep("F", 12))
dept <- rep(as.character(seq(1,6,by=1)), 4)
counts <- c(512,353,120,138,53,22,313,207,205,279,138,351,
            89,17,202,131,94,24,19,8,391,244,299,317)
berk <- data.frame(A=admit, G=gender, D=dept, counts=counts)

# fit the covfefe model of homogenous association
berk_mod <- glm(formula=counts ~ (admit+gender+dept)^2
                , data=berk
                , family=poisson(link="log"))
summary(berk_mod)
exp(-0.09987)

berk %>%
  dplyr::group_by(G,A) %>%
  dplyr::summarise(count_em=sum(counts))

1 - pchisq(20.204, df=5)
abs(residuals.glm(berk_mod)) 

admit <- rep(c(rep("Y",5), rep("N",5)), 2)
gender <- c(rep("M", 10), rep("F", 10))
dept <- rep(as.character(seq(2,6,by=1)), 4)
counts <- c(353,120,138,53,22,207,205,279,138,351,
            17,202,131,94,24,8,391,244,299,317)
berk2 <- data.frame(A=admit, G=gender, D=dept, counts=counts)

berk_mod2 <- glm(formula=counts ~ (admit+gender+dept)^2
                , data=berk2
                , family=poisson(link="log"))
summary(berk_mod2)
1 - pchisq(2.5564, df=4)

# rework the data
berk2$admit_flag <- ifelse(berk2$A=='Y',1,0)

berk_mod3 <- glm(formula=admit_flag~(G+D)
                 , weights=counts
                 ,data=berk2
                 , family=binomial(link="logit")
                  )
        
summary(berk_mod3)

berk_mod3 <- glm(formula=admit_flag~(D)
                 , weights=counts
                 ,data=berk2
                 , family=binomial(link="logit")
                  )


# 7.10
#input the data

seatbelt <- c(rep("sb", 4), rep("n_sb", 4))
eject <- rep(c("Y","Y", "N","N"),2)
injury <- rep(c("Nonfatal", "Fatal"),4)
counts <- c(1105,14,411111,483,4624,497,157342,1008)
auto <- data.frame(seatbelt=seatbelt, eject=eject, injury=injury, counts=counts)

# fit a loglinear model that describes the data well
auto_mod <- glm(formula=counts ~ (seatbelt+eject+injury)^2
                , data=auto
                , family=poisson(link="log"))
summary(auto_mod)

# fit an equivalent logistic regression model
auto_mod2 <- glm(formula=injury~ (seatbelt+eject)^2
                 , data=auto
                 ,weights=counts
                 ,family=binomial(link="logit")
                 )
summary(auto_mod2)

# calculate the dissimilarity index
sum(abs(auto_mod$fitted.values - auto$counts)) / 2

(1105 / (1105+14)) 
(411111 / (411111+483))
(4624 / (4624+497))
(157342 / (157342+1008))
auto_mod2$fitted.values

# 7.14
# input the data 

premarital <- c(rep("1",12), rep("2",12))
relig <- c(rep("1",6), rep("2",6), rep("1",6), rep("2",6))
birth <- rep(c("1","1","1","2","2","2"),4)
politics <- rep(c("1","2","3"),8)
counts <- c(99,73,51,15,20,19,73,87,51,25,37,36,
            8,20,6,4,13,12,24,50,33,22,60,88)
soc_dat <- data.frame(premarital=premarital
           , relig=relig
           , birth=birth
           , politics=politics
           , counts=counts
           )

soc_mod <- glm(
  formula=counts~ (premarital+relig+birth+politics)^2
               , data=soc_dat
               , family=poisson(link="log"))
summary(soc_mod)

1 - pchisq(6.9631, df=9)

reshape2::melt(exp(soc_mod$coefficients))


# fit a main effects loglinear model

soc_mod2 <- glm(
  formula=counts~ (premarital+relig+birth+politics)
               , data=soc_dat
               , family=poisson(link="log"))
summary(soc_mod2)
1 - pchisq(277.08, df=18)

exp(soc_mod2$coefficients) %>% reshape2::melt()




