
install.packages("tidyverse")
library(tidyverse)


# x values (pi)
pi <- seq(0,1,by=0.01)
# y values (likelihood)
y_vec = 2 * pi * (1-pi)
plot(pi,y_vec, type="l"
     , xlab=expression(pi)
     , ylab="likelihood"
     , main="Likelihood Plot")

pi <- seq(0,1,by=0.01)
y_vec <- (1 - pi)**2
plot(pi, y_vec, type="l"
     , xlab=expression(pi)
     , ylab="likelihood"
     , main="Likelihood Function"
     )

library("binom")
binom.confint(x = 0, n = 25, conf.level = 0.95)


treat_a <- c(8,7,6,6,3,4,7,2,3,4)
treat_b <- c(9,9,8,14,8,13,11,5,7,6)

mean(treat_a)
var(treat_a)
mean(treat_b)
var(treat_b)

(5+((1.96**2)/20)) - (1.96/sqrt(10))*(sqrt(5+(1.96**2)/40))
(5+((1.96**2)/20)) + (1.96/sqrt(10))*(sqrt(5+(1.96**2)/40))



# 1.22

#  first install the package "maxLik"
library("maxLik")

# define the log likelihood function 
logLikFun <- function(theta){
  n[1]*log(.5+.25*theta) + 
  n[2]*log(.25-.25*theta) +
  n[3]*log(.25-.25*theta) +
  n[4]*log(.25*theta)
    }
# observation vector
n <- c(1997, 906, 904, 32)

# determine the MLE
mle <- maxLik(logLik=logLikFun, start=0.5)
summary(mle)

# Compute the estimated cell probabilities

theta <- coef(mle)
# create our expected values
p <- c(
  (0.5 + 0.25*theta)
  , (0.25 - 0.25*theta)
  , (0.25 - 0.25*theta)
  , (0.25*theta)
  )

#Carry out Pearson's chi-squared test
chitest <- chisq.test(n, y=NULL, p, correct=FALSE)

# Now compute correct the p-value for df=c-1-1
chitest
cat(c("The correct df equal ",length(n)-1-1),"\n")
cat(c("The correct p-value equals ",round(1-pchisq(chitest$statistic,length(n)-1-1),6)),"\n")

# now compute the LR stat

e <- p*sum(n)
lr <- 2*sum(n*log(n/e))
cat("LR=",lr,"\n")
cat("p-value=",1-pchisq(lr,length(x)-1-1),"\n")
