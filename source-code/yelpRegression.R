library(ggplot2)
library(stats)
library(sm)
library(car)
library(sandwich)
library(lmtest)
library(plm)
library(AER)
library(MASS)
library(nlme)

load("/Users/xanderhlim/Desktop/yelp.RData")

# Question we are interested in: 
# All things being equal, does an additional star on yelp increase business, and by how much?


# Variables:
# logrev: log of the revenue
# score: average of user's reviews
# stars: number of stars on yelp
# rest_id: restaruant id
# time_id: quarter identifier


# Naive regression, regress revenue on stars: logrev = β_0 + β_1 score + U_it
# Assumption: E(U|stars) = 0
m1 = lm(logrev ~ score, data = yelp)
summary(m1)

plot(yelp$logrev, yelp$stars)
abline(m1, col = "red")

# Interpretation: if a restaurant's star rating were to increase by 1, then the restaurant's
# logrev would increase by (1.73 * 100)% = 173%. 
# OVB (Omitted Variable Bias): 
# stars is most likely a function composed of the quality of the food as well as the price
# people more likely to judge quality relative to the price they pay


# rewrite regression using panel data: 
# logrev = α_i + α_t  + β score_it + U_it
# within -> fixed effects
# Assumption: E(U_it|stars_it) = 0 AND E(U_it|stars_i1, ..., stars_iT, α_i) = 0
# Second assumption: strict exogenity, no feedback from U_it and score across time.
m2 = plm(logrev ~ stars, data=yelp, index=c('rest_id','time'), method = 'within', effect="individual")
summary(m2)

plot(yelp$logrev, yelp$stars)
abline(m2, col = "red")

# Interpretation: Holding all control variables and state fixed effects constant, increasing
# the star rating by 1 will increase revenues by (1.72 * 100)% = 172%.

# Other factors might affect the revenue (OVB). To enforce causality, show that rounding scores
# up affects the revenue. Yelp rounds scores to the nearest half star: define a binary variable
# R that denotes restaurants that that is 1 when the score is rounded up, otherwise, it is rounded down. 

temp = yelp$score[   (yelp$score >= 0.25 & yelp$score <= 0.49) |
                  (yelp$score >= 0.75 & yelp$score <= 0.99) |
                  (yelp$score >= 1.25 & yelp$score <= 1.49) | 
                  (yelp$score >= 1.75 & yelp$score <= 1.99) |
                  (yelp$score >= 2.25 & yelp$score <= 2.49) |
                  (yelp$score >= 2.75 & yelp$score <= 2.99) |
                  (yelp$score >= 3.25 & yelp$score <= 3.49) |
                  (yelp$score >= 3.75 & yelp$score <= 3.99) |
                  (yelp$score >= 4.25 & yelp$score <= 4.49) |
                  (yelp$score >= 4.75 & yelp$score <= 4.99) ]

yelp$roundedUp <-  ifelse(yelp$score %in% temp, 1, 0)

# estimate treatment affect of restaurants recieving 0.5 star increment
m3 = plm(logrev ~ factor(roundedUp) + score, data = yelp, index=c("rest_id","time"), method= "within",effect="individual")
summary(m3)

plot(yelp$logrev, yelp$stars)
abline(m3, col = "red")

# Interpretation: Holding all control variables and state fixed effects constant, having a restaurant's
# score being rounded up will increasing will increase revenues by (1.65 * 100)% = 165%.




