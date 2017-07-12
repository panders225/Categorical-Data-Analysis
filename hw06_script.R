library("tidyverse")

# 6.1

prove_it <- function(x)
  {
return(exp(1 + (0.3*x)) > exp(3.3 - (0.2*x)))
}

prove_it(4.6)

# 6.3
# input the data

df_create <- function(nf, ni, nr, nb, no, size, lake_var)
  {
  fish <- c(rep(1,nf), rep(0,ni), rep(0,nr), rep(0,nb), rep(0,no))
  invert <- c(rep(0,nf), rep(1,ni), rep(0,nr), rep(0,nb), rep(0,no))
  rep <- c(rep(0,nf), rep(0,ni), rep(1,nr), rep(0,nb), rep(0,no))
  bird <- c(rep(0,nf), rep(0,ni), rep(0,nr), rep(1,nb), rep(0,no))
  other <- c(rep(0,nf), rep(0,ni), rep(0,nr), rep(0,nb), rep(1,no))
  df <- data.frame(fish=fish, invert=invert, rep=rep, bird=bird, other=other, low_size=size)
  df$lake=lake_var
  return(df)
  }
df1 <- df_create(nf=23, ni=4, nr=2, nb=2, no=8, size=1, lake_var="hancock")
df2 <- df_create(nf=7, ni=0, nr=1, nb=3, no=8, size=0, lake_var="hancock")
df3 <- df_create(nf=5, ni=11, nr=1, nb=0, no=3, size=1, lake_var="oklawaha")
df4 <- df_create(nf=13, ni=8, nr=6, nb=1, no=0, size=0, lake_var="oklawaha")
df5 <- df_create(nf=5, ni=11, nr=2, nb=1, no=5, size=1, lake_var="trafford")
df6 <- df_create(nf=8, ni=7, nr=6, nb=3, no=5, size=0, lake_var="trafford")
df7 <- df_create(nf=16, ni=19, nr=1, nb=2, no=3, size=1, lake_var="george")
df8 <- df_create(nf=17, ni=1, nr=0, nb=1, no=3, size=0, lake_var="george")

gator <- rbind(df1,df2,df3,df4,df5,df6,df7,df8)
gator$lake_fact <- relevel(factor(gator$lake), ref=4)

gator$food <- ifelse(gator$fish==1, 'f', 
                     ifelse(gator$invert==1, 'i', 
                            ifelse(gator$rep==1, 'r', 
                                   ifelse(gator$bird==1, 'b',
                                          ifelse(gator$other==1,'o', 'z')))))
  
gator$food <- factor(gator$food, levels=c('f', 'i', 'r', 'b', 'o'))
View(gator)
# now, finally create the model

library(nnet)

gator_mod <- nnet::multinom(food ~ low_size + lake
                            , data=gator)

summary(gator_mod)

# 6.3B

pred <- function(low=0, lh=0, lo=0, ls=0)
  {
  print( 1 / (1 + 
      exp(-1.52 + (1.4*low) + (-1.6*lh) + (0.92*lo) + (1.09*ls)) +
      exp(-3.3 + (-0.4*low) + (1.23*lh) + (2.45*lo) + (2.93*ls)) +
      exp(-2.09 + (-0.61*low) + (0.67*lh) + (-0.65*lo) + (1.09*ls)) +
      exp(-1.73 + (0.45*low) + (1.07*lh) + (-0.07*lo) + (1.45*ls))
     ))
}

#low size first
pred(low=1, lh=0, lo=1, ls=0)
# large size second
pred(low=0, lh=0, lo=1, ls=0)



# 6.6
# prediction function

pred_func <- function(income=0)
  {
print(1 / (1 +
         exp(-2.55-(0.275*income)) +
         exp(-2.1987-(0.1313*income))
       )
  )
}
pred_func(2)


#6.8
# input the data
therapy <- c(rep("Seq", 8), rep("Alt", 8))
response <- rep(c("prog", "NA", "partial", "complete"), 4)
gender <- c(rep("Male",4), rep("Female",4), rep("Male",4), rep("Female",4))
counts <- c(28,45,29,26,4,12,5,2,41,44,20,20,12,7,3,1)
trt_dat <- data.frame(
            therapy=therapy
            , response=response
            , gender=gender
            , counts=counts
  
)

trt_dat$therapy <- factor(trt_dat$therapy, levels=c("Seq", "Alt"))
trt_dat$response <- factor(trt_dat$response, levels=c("prog", "NA", "partial", "complete"))
trt_dat$gender <- factor(trt_dat$gender, levels=c("Male", "Female"))
trt_dat

library('MASS')

# fit the model

trt_fit <- MASS::polr(formula=response ~ therapy + gender
                      ,  data=trt_dat
                      , weights=counts)

summary(trt_fit)

# fit another model

trt_fit2 <- MASS::polr(formula=response ~ therapy + gender + therapy:gender
                      ,  data=trt_dat
                      , weights=counts)

summary(trt_fit2)

deviance(trt_fit)
deviance(trt_fit2)

1 - pchisq(deviance(trt_fit) - deviance(trt_fit2), df=1)
#model does not represent a significantly better fit

trt_dat$gender <- factor(trt_dat$gender, levels=c("Female", "Male"))
trt_fit3 <- MASS::polr(formula=response ~ therapy + gender + therapy:gender
                      ,  data=trt_dat
                      , weights=counts)

summary(trt_fit3)

# 6.10

exp(-0.2819 + (-0.3189*4.3) + 1.1112) / (1 + exp(-0.2819 + (-0.3189*4.3) + 1.1112))
exp(1.2128 + (-0.3189*4.3) + 1.1112) / (1 + exp(1.2128 + (-0.3189*4.3) + 1.1112))

exp(-0.2819 + (-0.3189*4.3)) / (1 + exp(-0.2819 + (-0.3189*4.3)))
exp(1.2128 + (-0.3189*4.3)) / (1 + exp(1.2128 + (-0.3189*4.3)))


# 6.13
# first, bring in the data 

gender <- c(rep("Female",4), rep("Male",4))
income <- rep(seq(1,4), 2)
y1 <- c(1,2,0,0,1,0,0,0)
y2 <- c(3,3,1,2,1,3,0,1)
y3 <- c(11,17,8,4,2,5,7,9)
y4 <- c(2,3,5,2,1,1,3,6)
satis <- data.frame(gender=gender, income=income, y1=y1, y2=y2, y3=y3, y4=y4) 
#satis$income <- factor(satis$income, levels=c("1", "2", "3", "4"))
satis$gender <- factor(satis$gender, levels=c("Female", "Male"))

satis_mod <- VGAM::vglm(cbind(y1, y2, y3, y4) ~ income 
                        , data=satis
                        , family=acat(parallel=TRUE)
)

summary(satis_mod)

satis_mod2 <- MASS::polr(formula=cbind)
  
trt_fit <- MASS::polr(formula=response ~ therapy + gender
                      ,  data=trt_dat
                      , weights=counts)
ideol.fit3 <- vglm(cbind(y1,y2,y3,y4,y5) ~ party
                   , family=acat(reverse=TRUE, parallel=TRUE)
                   ,data=ideology2)

ideol.fit2 <- vglm(cbind(y1,y2,y3,y4,y5) ~ party
                   , family=cumulative(parallel=FALSE)
                   , data=ideology2)

ideol.fit <- vglm(cbind(y1,y2,y3,y4,y5) ~ party
                  , family=cumulative(parallel=TRUE)
                  , data=ideology2)

# 6.17
# input the data

gender <- c(rep("Female", 4), rep("Male", 4))
loc <- c(rep("Urban",2), rep("Rural",2), rep("Urban",2), rep("Rural",2))
sb <- rep(c("No", "Yes"),4)
y1 <- c(7287, 11587,3246,6134,10381,10969,6123,6693)
y2 <- c(175,126,73,94,136,83,141,74)
y3 <- c(720,577,710,564,566,259,710,353)
y4 <- c(91,48,159,82,96,37,188,74)
y5 <- c(10,8,31,17,14,1,45,12)

accidents <- data.frame(gender=gender, loc=loc, sb=sb, y1=y1, y2=y2, y3=y3, y4=y4, y5=y5)
accidents

# create a long dataset
accidents_long <- rbind(
data.frame(gender=gender, loc=loc, sb=sb, accident_code=1, counts=y1)
, data.frame(gender=gender, loc=loc, sb=sb, accident_code=2, counts=y2)
, data.frame(gender=gender, loc=loc, sb=sb, accident_code=3, counts=y3)
, data.frame(gender=gender, loc=loc, sb=sb, accident_code=4, counts=y4)
, data.frame(gender=gender, loc=loc, sb=sb, accident_code=5, counts=y5)
)

accidents_long$gender <- factor(accidents_long$gender
                                , levels=c("Female", "Male"))

accidents_long$accident_code <- factor(accidents_long$accident_code
                                       , levels=c("1", "2", "3", "4", "5"))
accidents_long$sb <- factor(accidents_long$sb
                            , levels=c("No", "Yes"))
# fit the baseline category model
head(accidents_long, n=10)

a_mod1 <- nnet::multinom(accident_code ~ gender + loc + sb
                         , data=accidents_long
                         , weights=counts
                          )

summary(a_mod1)

# now fit the proportional odds model
a_mod2 <- MASS::polr(formula=accident_code ~ gender + loc + sb
                     , data=accidents_long
                     , weights=counts
                     )
summary(a_mod2)

a_mod3 <- VGAM::vglm(cbind(y1, y2, y3, y4, y5) ~ gender + loc + sb
                        , data=accidents
                        , family=acat(parallel=TRUE)
                          )
summary(a_mod3)


summary(a_mod1)
summary(a_mod2)
summary(a_mod3)

deviance(a_mod1)
deviance(a_mod2)
deviance(a_mod3)

AIC(a_mod1)
AIC(a_mod2)
AIC(a_mod3)
