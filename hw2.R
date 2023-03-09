# HW 2 - Jadon Fowler
# STA 570 Section 1
library(ggplot2)
library(dplyr)
library(mosaic)
library(mosaicData)
library(data.table)

# Problem 1
#      O    A     B    AB   Total
#White 36%  32.2% 8.8% 3.2%
#Black 7%   2.9%  2.5% 0.5%
#Asian 1.7% 1.2%  1%   0.3%
#Other 1.5% 0.8%  0.3% 0.1%
#Total                      100%

races <- c("White", "Black", "Asian", "Other")
blood.types <- data.frame(
  race=races,
  O=c(36, 7, 1.7, 1.5),
  A=c(32.2, 2.9, 1.2, 0.8),
  B=c(8.8, 2.5, 1, 0.3),
  AB=c(3.2, 0.5, 0.3, 0.1)
)

# 1.a
(blood.types$total <- blood.types$O + blood.types$A + blood.types$B + blood.types$AB)
(O.total <- sum(blood.types$O))
(A.total <- sum(blood.types$A))
(B.total <- sum(blood.types$B))
(AB.total <- sum(blood.types$AB))

# 1.b
# What is the probability that a randomly selected donor will be Asian and
# have Type O blood? That is to say, given a donor is randomly selected
# from the list of all donors, what is the probability that the selected donor
# will Asian with Type O?
.012

# 1.c
# What is the probability that a randomly selected donor is white? That is
# to say, given a donor is randomly selected from the list of all donors,
# what is the probability that the selected donor is white?
.802

# 1.d
# What is the probability that a randomly selected donor has Type A
# blood? That is to say, given a donor is selected from the list of all
# donors, what is the probability that the selected donor has Type A
# blood?
A.total #37.1

# 1.e
# What is the probability that a white donor will have Type A blood? That
# is to say, given a donor is randomly selected from the list of all the white
# donors, what is the probability that the selected donor has Type A
# blood? (Notice we already know the donor is white because we
#         restricted ourselves to that subset!)
.322 / .802 # .401

# 1.f
# Is blood type and ethnicity independent? Justify your response
# mathematically using your responses from the previous answers.
prob.of.black <- blood.types$total[blood.types$race=="Black"]
prob.of.white <- blood.types$total[blood.types$race=="White"]
# P(Black) * P(O) == P(Black ∩ O)
prob.of.black * O.total / 100 == blood.types$O[blood.types$race=="Black"] #FALSE
# P(White) * P(A) == P(White ∩ A)
prob.of.white * A.total / 100 == blood.types$A[blood.types$race=="White"] #FALSE
# These probabilities are not equal, so blood type and ethnicity are not independent.


# Problem 2
# a. Number of M&Ms I eat per hour while grading homework
#    Poisson
# b. The number of mornings in the coming 7 days that I change my son’s first diaper of the day.
#    Binomial
# c. The number of Manzanita bushes per 100 meters of trail.
#    Poisson


# Problem 3. During a road bike race, there is always a chance a crash will occur. Suppose
# the probability that at least one crash will occur in any race I’m in is π=0.2 and
# that races are independent.

# a. What is the probability that the next two races I’m in will both have crashes?
dbinom(x=2, size=2, prob=0.2) # 0.04
# b. What is the probability that neither of my next two races will have a crash?
dbinom(x=0, size=2, prob=0.2) # 0.64
# c. What is the probability that at least one of the next two races have a crash?
pbinom(0, size=2, prob=0.2, lower.tail=FALSE) # 0.36

# Problem 4. My cats suffer from gastric distress due to eating house plants and the number
# of vomits per week that I have to clean up follows a Poisson distribution with
# rate λ=1.2 pukes per week.

# a. What is the probability that I don’t have to clean up any vomits this coming week?
ppois(0, 1.2) # 0.301
# b. What is the probability that I must clean up 1 or more vomits?
ppois(0, 1.2, lower.tail=F) # 0.699
# c. If I wanted to measure this process with a rate per day, what rate should I use?
1.2 / 7 # 0.17 pukes/day

# Problem 5.
#         y 0    1    2    3  4+
#Probabilty 0.45 0.25 0.20    0.0

# a. What is the probability that I see 3 runners on a morning walk?
1 - (0.45 + 0.25 + 0.2) #0.1
# b. What is the expected number of runners that I will encounter?
0 # because it has the highest probability
# c. What is the variance of the number of runners that I will encounter?
var(c(0.45, 0.25, 0.2, 0.1, 0)) # 0.2875


# Problem 6. If Z∼N(μ=0,σ2=1), find the following probabilities:
# a. P(Z<1.58)=
# P(Z<=1.58) - P(Z=1.58)
pnorm(1.58, 0, 1) - dnorm(1.58, 0, 1) # 0.82844
# b. P(Z=1.58)=
dnorm(1.58, 0, 1) #0.1145
# c. P(Z>−.27)=
pnorm(-.27, 0, 1, lower.tail=F) # 0.60642
# d. P(−1.97<Z<2.46)=
# P(Z>-1.97) ∩ (P(Z<=2.46) - P(Z=2.46))
pnorm(-1.97, 0, 1, lower.tail=F) * (pnorm(2.46, 0, 1) - dnorm(2.46, 0, 1)) # 0.949


# Problem 7. Using the Standard Normal Table or the table functions in R, find
# z that makes the following statements true.
# a. P(Z<z)=.75
qnorm(.75) # z=0.67
# b. P(Z>z)=.4
qnorm(.4, lower.tail=F) # z=0.2533


# Problem 8. The amount of dry kibble that I feed my cats each morning can be well
# approximated by a normal distribution with mean μ=200 grams and standard
# deviation σ=30 grams.

# a. What is the probability that I fed my cats more than 250 grams of kibble this morning?
pnorm(250, 200, 30, lower.tail=FALSE) # 0.4779
# b. From my cats’ perspective, more food is better. How much would I have to
#    feed them for this morning to be among the top 10% of feedings?
qnorm(.1, 200, 30, lower.tail=F) # 238.4465


# Problem 9. Sea lion weight is well approximated by a normal distribution with a mean of
# 300 kg and standard deviation of 15 kg.

# a. Use R to find the probability of randomly sampling a sea lion with a
#    weight greater than 320 kg. Round your answer to 3 decimals.
pnorm(320, mean=300, sd=15, lower.tail=F) # 0.091

# b. Now suppose we independently sample 10 sea lions and we are
#    interested in how many of the 10 have a weight larger than 320 kg.
#    What distribution would we use to model this and what are the
#    parameters of that distribution?
# Binomial Distribution. n=10 pi=0.091

# c. Using a CALCULATOR AND PENCIL AND PAPER, calculate the probability of
#    observing only 1 sea lion with a weight greater than 320 kg.
dbinom(1, size=10, prob=0.091) # 0.385

# d. Use R to calculate the probability of all possible outcomes and produce
#    a graph the PMF of this distribution.
plot(0:10, dbinom(0:10, size=10, prob=0.091))
