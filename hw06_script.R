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
gender <- rep(c("M","F"), 8)
counts <- c(28,4,45,12,29,5,26,2,41,12,44,7,20,3,20,1)
trt_dat <- data.frame(
            therapy=therapy
            , response=response
            , gender=gender
            , counts=counts
  
)

trt_dat$therapy <- factor(trt_dat$therapy, levels=c("Seq", "Alt"))
trt_dat$response <- factor(trt_dat$response, levels=c("prog", "NA", "partial", "complete"))
trt_dat$gender <- factor(trt_dat$gender, levels=c("M", "F"))

library('MASS')

# fit the model

trt_fit <- MASS::polr(formula=response ~ therapy + gender
                      ,  data=trt_dat
                      , weights=counts)

summary(trt_fit)

# fit another model

trt_fit2 <- MASS::polr(formula=response ~ therapy + gender + therapy*gender
                      ,  data=trt_dat
                      , weights=counts)

summary(trt_fit2)

1 - pchisq(639.3827 - 633.9547, df=1)


# 6.10

exp(-0.2819 + (-0.3189*4.3) + 1.1112) / (1 + exp(-0.2819 + (-0.3189*4.3) + 1.1112))
exp(1.2128 + (-0.3189*4.3) + 1.1112) / (1 + exp(1.2128 + (-0.3189*4.3) + 1.1112))

exp(-0.2819 + (-0.3189*4.3)) / (1 + exp(-0.2819 + (-0.3189*4.3)))
exp(1.2128 + (-0.3189*4.3)) / (1 + exp(1.2128 + (-0.3189*4.3)))
