

# 2.15b
library(binom)
binom.confint(x = 189, n=11034)
binom.confint(x = 104, n=11037)

qnorm(p = 0.975, mean=0, sd=1)
qchisq(p=0.975, df=4)
1 - pchisq(q=73.4, df=4)

#2.16

smoke <- array(
  data=c(688,21,650,59)
  , dim=c(2,2)
)

odds.ratio(smoke)

# 2.18e
# first, we need to load our data

income <- array(
  data=c(21, 53, 94, 159, 372, 249, 110, 221, 83)
  , dim=c(3,3)
  , dimnames=list(income=c("Above Average", "Average", "Below Average")
                  , happiness=c("Not Too Happy", "Pretty Happy", "Very Happy"))
)
income

# using the Katari test for a linear trend- loaded separately
linear.trend(income, NI = 3, NJ = 3, 1:3, 1:3)

# we have strong evidence of a linear trend in the data
# M2 value of 55.93
    
1 - pchisq(q = 55.93, df=1)

# create a mosaic plot of the data
mosaicplot(t(income)
           , main="Income by Happiness Mosaic Plot"
           , xlab="Happiness Level"
           , ylab="Income Level"
           , color=TRUE
           )

# plot is showing general linear relationship between money and 
# happiness.  For relatively wealthy individuals, the boxes grow
# larger as we increase our happiness levels.  For relatively poor
# individuals, the boxes become larger as we decrease our 
# happiness levels.


# 2.19
# first, input the data
politics <- array(
  data = c(871, 302, 444, 80, 873, 43)
  , dim=c(2,3)
  , dimnames=list(Race=c("White", "Black")
                  , Party=c("Democrat", "Independent", "Republican")
                  )
    )
politics

# 2.19 A - test the null hypothesis between party identification
# and race
politics_test <- chisq.test(politics)
print(politics_test)
powdivind(politics, lambda=0, statonly=F)

# the test indicates that we have evidence of an association 
# between party and race

# 2.19 B
politics_test$observed
politics_test$stdres
# The standardized residuals indicate that White voters have a 
# larger proportion of self-identifying Republicans than we might
# expect, and a smaller proportion of democrats.  Black voters,
# on the other hand, have a larger proportion of Democrats and 
# smaller proportion of Republicans

# 2.19 C
# first examine democrats versus ind

partition_one <- politics[,1:2]
partition_one
part_one_test <- chisq.test(partition_one)
print(part_one_test)
lr_one <- powdivind(partition_one, lambda=0, statonly=F)
lr_one
#evidence of an association

#partition_two <- 

politics_two <- array(
  data = c(871+444, 302+80, 873, 43)
  , dim=c(2,2)
  , dimnames=list(Race=c("White", "Black")
                  , Party=c("Democrat+Ind", "Republican")
  )
)
politics_two
chisq.test(politics_two)
powdivind(politics_two, lambda=0, statonly=F)
# stronger evidence of an association



# 2.22

pysch <- array(
            data=c(105, 12, 18, 47,0, 8, 2, 19, 52, 13)
            , dim=c(5,2)
            , dimnames=list(Diag=c("Schiz."
                                   , "Affective"
                                   , "Neurosis"
                                   , "Person."
                                   , "Special")
                            , Drugs=c("Drugs", "No Drugs")
                            )
              )

# 2.22 A - conduct a test of independence
pysch
pysch_chisq <- chisq.test(pysch) 
print(pysch_chisq)

# this test won't work because some cells have fewer than 
# 5 observed

pysch_test <- fisher.test(pysch)
print(pysch_test)

# 2.2B - Obtain and interpret standardized residuals
pysch_chisq$observed
pysch_chisq$stdres

# standardized residuals suggest that more schizophrenic
# and affective disorder patients received drugs than we might
# have expected, and more neurotic, personality disorder, and 
# special symptom patients received no drugs that we may have expected

# 2.22 C - conduct partitionings of the chi-squared test

partition_one <- pysch[1:2,]
partition_one
chisq.test(partition_one)
fisher.test(partition_one)

partition_two <- pysch[3:4, ]
partition_two
chisq.test(partition_two)

partition_three <- array(
            data=c(105 +12, 18 + 47, 8 + 2, 19 + 52)
            , dim=c(2,2)
              )

partition_three  
chisq.test(partition_three)


# 2.27
education <- array(
  data=c(9,44,13,10, 11,52,23,22, 9,41,12,27)
  , dim=c(4,3)
  , dimnames = list(
    Aspirations=c("some hs", "hs grad", "some col.", "col grad")
  , Income=c("Low Income", "Middle Income", "High Income")
                )
  )
education
# 2.27A - test using the chi-squared test
chisq.test(education)
# this test is deficient because the data are ordinal 
# across both dimensions.  A linear trend test would provide
# more power 

chisq.test(education)$observed
chisq.test(education)$stdres
# the standardized residuals suggest a linear trend in the data
# as income increases, college aspirations increase as well

# 2.27C - conduct a more appropriate test
linear.trend(education, NI=4, NJ=3, x=1:4, y=1:3)

# 2.27D - Construct a mosaic plot for the data
mosaicplot(education
           , color=TRUE
           , main="Education by Family Income"
           , xlab="Educational Aspirations"
          )

# this plot confirms the linear qualities that I
# conjectured existed in the data


# 2.29
predno <- matrix(
              c(7,8,0,15)
              , nrow=2
              , byrow=FALSE
              , dimnames=list(Normalization=c("Y","N")
                              , Treatment=c("Treat", "Control"))
              )

predno
# conduct Fisher's exact test
predno_test <- fisher.test(predno, alternative = "greater")
predno_test


# 2.30 - radiation fisher question
surg <- matrix(
              c(21, 15,2,3)
              , nrow=2
              , byrow=FALSE
              , dimnames=list(Procedure=c("Surgery","Radiation")
                              , Outcome=c("Cancer Controlled", "Not Controlled"))
              )
surg
#surg OR
(21/15) / (2/3)

surg_test <- fisher.test(surg, alternative="greater")
surg_test

# 2.30A - for the above data, obtain and interpret a 
# two-sided exact P-value

surg_test_two <- fisher.test(surg, alternative="two.sided")
surg_test_two
mosaicplot(surg, color=TRUE)

#  install the epitools package for the mid p-value
library("epitools")
# consider surgery to be "exposed"
surg_test_three <- epitools::ormidp.test(a1=21, a0=15, b1=2, b0=3)
surg_test_three$one.sided


# Additional Problem A
# first, load the data

berkeley <- array(
          c(512,353,120,138,53,22
            ,313,207,205,279,138,351
            ,89,17,202,131,94,24
            ,19,8,391,244,299,317
            ) 
          , dim=c(6,2,2)
          , dimnames=list(Department=seq(1,6)
                          , Admit=c("Y","N")
                          , Gender=c("Male", "Female")
                          )
)

berkeley
# Additional A,a
# using the provided odds ratio package
for (i in seq(1,6))
  {
  print("_______________")
  print(paste("Dept", i))
  print(odds.ratio(berkeley[i,,]))
  print("_______________")
  }

# Additional A,b
# Perform the Breslow-Day and CMH tests

# the Breslow-Day Test requires a special load
DescTools::BreslowDayTest(berkeley)
# cmh test is pre-loaded in the stats package
mantelhaen.test(berkeley)
BDT(berkeley)
mantelhaen.test(berkeley[-1,,])

# Additional A,c
# we need the sum of all columns except the first

reduced_berkeley <- array(
  c(1198-512, 1493-313, 557-89, 1278-19)
  , dim=c(2,2)
)
reduced_berkeley
odds.ratio(reduced_berkeley, conf.level = 0.9)

full_berkeley <- array(
  c(1198, 1493, 557, 1278)
  , dim=c(2,2)
)
odds.ratio(full_berkeley, conf.level = 0.9)

# I mean, removing department 1 and creating the confidence 
# interval isn't producing different results


# Additional B
# input the data

merit <- array(
  data=c(24,10,5,16,7
         ,9,3,4,7,4
         ,47,45,57,54,59
         ,12,8,9,10,12)
      , dim=c(5,2,2)
      , dimnames=list(District=c("NC", "NE", "NW", "SE", "SW")
                      , Qualify=c("Y", "N")
                      , Race=c("Blacks", "Whites"))
  )
merit

# a - report the conditional odds ratios

for (i in seq(1,5))
{
  print("_______________")
  print(paste("District", i))
  print(odds.ratio(merit[i,,]))
  print("_______________")
  }


merit_tot <-   array(data=c(sum(24,10,5,16,7)
         ,sum(9,3,4,7,4)
         ,sum(47,45,57,54,59)
         ,sum(12,8,9,10,12)
        )
        , dim=c(2,2)
   )

merit_tot
odds.ratio(merit_tot)


# C
mantelhaen.test(merit)
