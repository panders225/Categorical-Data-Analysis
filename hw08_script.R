
library("tidyverse")

# 8.2
(1 - pnorm(10.91))*2
(1 - pnorm(10.91))

sqrt( ((132+107) - (132-107)**2)/1144) 

sqrt((132+107) - ((132-107)**2)/1144)/1144
sqrt((125+2) - ((125-2)**2)/1120)/1120
qnorm(p = 0.95)

# 8.6
help("mcnemar.test")
mcnemar.test(matrix(c(114, 181, 11, 48), nrow=2, byrow=T))
help("mcnemar.test")

# 8.11
# read in some matrix functions
source("C:/Users/Philip/Schools/TAMU/STAT_659/homework/git_hw/Categorical-Data-Analysis/square_matrix_functions.R")  

freq <- c(1228, 39, 2, 158, 100, 649, 1, 107, 1, 0, 54, 9, 73, 12, 4, 137)
relig <- matrix(freq
               ,nrow=4
               ,byrow=TRUE
               ,dimnames=list(first=c("1","2","3","4"),second=c("1","2","3","4")))
relig_dat <- table2case(relig)

relig_mod <- glm(Count ~ factor(Rows) + factor(Cols)
                 ,family=poisson
                 ,data=relig_dat
                 )
summary(relig_mod)
reshape2::melt(relig_mod$residuals)
relig_dat

1 - pchisq(2.3, df=3)
1 - pchisq(148.3, df=3)

# 8.13D
# fit the marginal model

relig
rowSums(relig)
colSums(relig)
relig_dat

# 8.14
freq=c(425,17,80,36,10,555,74,47,7,34,771,33,5,14,29,452)

migration=matrix(freq,nrow=4,byrow=TRUE,dimnames=list(first=c("ne","mw","so","we"),second=c("ne","mw","so","we")))
mig_dat <- table2case(migration)

# get ready for the symmetry model
diags <- makediags(mig_dat)
symm <- makeoffdiags(mig_dat)

# fit the symmetry model
mig_mod <- glm(Count ~ diags + symm,family=poisson,data=mig_dat)
summary(mig_mod)
1 - pchisq(134.45, df=6)

# fit the quasi-symmetry model
mig_mod2 <- glm(Count ~ factor(Rows) + factor(Cols)+symm,family=poisson,data=mig.data)
summary(mig_mod2)
1 - pchisq(6.984, df=3)

deviance(mig_mod)
deviance(mig_mod2)

anova(mig_mod, mig_mod2)
1 - pchisq(127.47, df=3)


# 8.16
# input the data
freq <- c(66,39,3,227,359,48,150,216,108)
cycle <- matrix(data = freq, nrow=3
       , byrow=TRUE
       , dimnames = list(chem=c("1", "2", "3"), recycle=c("1", "2", "3"))
       )
cycle_dat <- table2case(cycle)

diags=makediags(cycle_dat)
symm=makeoffdiags(cycle_dat)

#A - fit the symmetry model

cycle_mod1 <- glm(Count ~ diags + symm
                  ,family=poisson
                  ,data=cycle_dat)
summary(cycle_mod1)
1 - pchisq(445.23, df=3)
# Fit quasi-symmetry model

cycle_mod2 <- glm(Count ~ factor(Rows) + factor(Cols)+symm
                  ,family=poisson
                  ,data=cycle_dat
                  )
summary(cycle_mod2)
1 - pchisq(1.2266, df=1)

# fit the model of ordinal quasi-symmetry

cycle_mod3 <- glm(Count ~ Rows + diags + symm
                  ,family=poisson
                  ,data=cycle_dat
                  )
summary(cycle_mod3)
1 - pchisq(2.4688, df=2)

anova(cycle_mod2, cycle_mod3)
1 - pchisq(2.4688 - 1.2266, df = 2 -1 )


# 8.17
freq <- c(95,72,32,8,66,129,116,13,31,101,233,82,5,4,24,26)
pollute <- matrix(freq, nrow=4, byrow=TRUE
       , dimnames=list(car=c("1", "2", "3", "4"), c("1", "2", "3", "4") ))
pollute_dat <- table2case(pollute)

# first fit the symmetry model to make sure that isnt appropriate

diags=makediags(pollute_dat)
symm=makeoffdiags(pollute_dat)

pollute_mod1 <- glm(Count ~ diags + symm
                  ,family=poisson
                  ,data=pollute_dat)
summary(pollute_mod1)
1 - pchisq(40.577, df=6)

# ordinal quasi-symmetry
pollute_mod2 <- glm(Count ~ Rows + diags + symm
                  ,family=poisson
                  ,data=pollute_dat
                  )
summary(pollute_mod2)
1 - pchisq(27.489, df=5)

#quasi-symmetry
pollute_mod3 <- glm(Count ~ factor(Rows) + factor(Cols)+symm
                    ,family=poisson
                    ,data=pollute_dat
                    )
summary(pollute_mod3)
1 - pchisq(2.2273, df=3)



# 8.19
# re-bring in the migration data
freq=c(425,17,80,36,10,555,74,47,7,34,771,33,5,14,29,452)

migration=matrix(freq,nrow=4,byrow=TRUE,dimnames=list(first=c("ne","mw","so","we"),second=c("ne","mw","so","we")))
mig_dat <- table2case(migration)

# get ready for the symmetry model
diags <- makediags(mig_dat)
symm <- makeoffdiags(mig_dat)

# fit the independence model
mig_mod1 <- glm(Count ~ factor(Rows) + factor(Cols)
                ,family=poisson
                ,data=mig_dat
                )
summary(mig_mod1)

# now fit the quasi-independence model
mig_mod2 <- glm(Count ~ factor(Rows) + factor(Cols)+diags
                ,family=poisson
                ,data=mig_dat
                )
summary(mig_mod2)
1 - pchisq(9.7032, df=5)
reshape2::melt(mig_mod1$residuals)
plot(density(mig_mod1$residuals))

reshape2::melt(mig_mod2$residuals)
plot(density(mig_mod2$residuals))


# 8.20
#input the data
freq <- c(38,5,0,1,33,11,3,0,10,14,5,6,3,7,3,10)
neuro <- matrix(freq, nrow=4, byrow=TRUE,
       dimnames=list(A=c("1", "2", "3","4"), B=c("1", "2", "3", "4")))
neuro_dat <- table2case(neuro)

diags <- makediags(neuro_dat)
symm <- makeoffdiags(neuro_dat)

# first fit the independence model
neuro_mod1 <- glm(Count ~ factor(Rows) + factor(Cols)
                ,family=poisson
                ,data=neuro_dat
                )
summary(neuro_mod1)
plot(density(neuro_mod1$residuals))
1 - pchisq(69, df=9)

# now fit the quasi-independence model
neuro_mod2 <- glm(Count ~ factor(Rows) + factor(Cols)+diags
                ,family=poisson
                ,data=neuro_dat
                )
summary(neuro_mod2)

# quasi-symmetry model
neuro_mod3 <- glm(Count ~ factor(Rows) + factor(Cols)+symm
                  ,family=poisson
                  ,data=neuro_dat
)
summary(neuro_mod3)
1 - pchisq(6.184, df=3)


# ordinal quasi-symmetry model
neuro_mod4 <- glm(Count ~ Rows + diags + symm
                  ,family=poisson
                  ,data=neuro_dat
                  )
summary(neuro_mod4)



# Fit the Bradley-Terry Model
# first arrange the data
library("BradleyTerry2")
data("citations", package = "BradleyTerry2")
citations.sf <- countsToBinomial(citations)
class(citations.sf)
names(citations.sf)[1:2] <- c("journal1", "journal2")

cite_model <-  BTm(cbind(win1, win2), journal1, journal2, ~ journal,
                   id = "journal", data = drinks)
cite_model
summary(cite_model)

drinks <- data.frame(
  journal1=as.factor(c("coke", "coke_classic", "coke", "pepsi"))
  , journal2=as.factor(c("pepsi", "pepsi", "coke_classic", "coke"))
  , win1=c(29,19,31, 0)
  , win2=c(20,28,19, 0)
            )

drinks <- drinks[1:3 , ]
str(drinks)


cite_model <-  BTm(cbind(win1, win2), journal1, journal2, ~ journal,
                   id = "journal", data = drinks)
cite_model



# 8.23
data("citations", package = "BradleyTerry2")
#(citations <- t(citations))
(citations.sf <- countsToBinomial(citations))

names(citations.sf)[1:2] <- c("journal1", "journal2")
citations.sf

cite_model <-  BTm(cbind(win1, win2), journal1, journal2, ~ journal,
                   id = "journal", data = citations.sf)
cite_model
1 - pchisq(4.293, df=3)

