library(ggplot2)
library(dplyr)
library(mosaic)
library(mosaicData)
library(data.table)
library(Lock5Data)

data("BodyTemp50",package = "Lock5Data")
data("EmployedACS",package = "Lock5Data")

# 1. Load the dataset BodyTemp50 from the Lock5Data package. This is a dataset of 50 healthy
# adults. One of the columns of this dataset is the Pulse of the 50 data points, which is the
# number of heartbeats per minute.

# a. Create a histogram of the observed pulse values. Comment on the graph and aspects
# of the graph that might be of scientific interest. Below will help you load the data, and
# we want to use the Pulse variable.

ggplot(BodyTemp50) +
  geom_histogram(aes(x=Pulse))
# The graph is slightly bell shaped.
# There are a few outliers that may be interesting to look at.

# b. Calculate the sample mean 𝑥 and sample standard deviation s of the pulses.
# Provide an interpretation of the standard deviation.

mean(BodyTemp50$Pulse) # 74.4
sd(BodyTemp50$Pulse) # 6.439673
# The typical amount a pulse differs from the sample mean is 6.44.

# c. Create a dataset of 10000 bootstrap replicates of 𝑥.
BootDist <- mosaic::do(10000) *
  mosaic::resample(BodyTemp50) %>% 
  summarise(boot.mean=mean(Pulse))

# d. Create a histogram of the bootstrap replicates. Report on the shape of the histogram.
# Calculate the mean and standard deviation of this distribution. Notice that the
# standard deviation of the distribution is often called the Standard Error of 𝑥 and we’ll
# denote it as 𝜎̂𝑥 . Provide an interpretation of 𝜎̂𝑥 .

ggplot(BootDist, aes(x=boot.mean)) +
  geom_histogram() +
  ggtitle('Bootstrap distribution of resampled means')
mean(BootDist$boot.mean) # 74.40115
sd(BootDist$boot.mean) # 0.9023292
# The typical amount that a sample mean differs from the population mean is 0.902.

# e. Using the bootstrap replicates and the quantile method, create a 95% confidence
# interval for μ, the average adult heart rate. Provide an interpretation of the interval.
quantile(BootDist$boot.mean, probs=c(0.025, 0.975))
#   2.5% 97.5%
#  72.62 76.14 
# I am 95% confident that the population mean of pulses falls between 72.62 76.14.

# f. Calculate the interval (𝑥 − 2𝜎̂𝑥 , 𝑥 + 2𝜎̂𝑥)
# and comment on its similarity to the interval you calculated in part (e).
original.sample.mean <- mean(BodyTemp50$Pulse)
std.error <- sd(BootDist$boot.mean)
c(original.sample.mean - 2*std.error, original.sample.mean + 2*std.error)
# It is almost the same as the interval calculated in part (e).

# 2. Load the dataset EmployedACS from the Lock5Data package. This is a dataset drawn from
# American Community Survey results which is conducted monthly by the US Census Bureau
# and should be representative of US workers. The column HoursWk represents the number of
# hours worked per week.
EmployedACS$HoursWk

# a. Create a histogram of the observed hours worked. Comment on the graph and
# aspects of the graph that might be of scientific interest.
ggplot(EmployedACS) +
  geom_histogram(aes(x=HoursWk), bins=30)
# Most people are working 40 hours a week.
# There are a few outliers working above 60 hours/week that may be interesting
# to look at.

# b. Calculate the sample mean 𝑥 and sample standard deviation s of the worked hours
# per week. Provide an interpretation of the standard deviation.

(sample.mean <- mean(EmployedACS$HoursWk)) # 37.86713
(stddev <- sd(EmployedACS$HoursWk)) # 12.94576
# The typical amount that the hours worked by one employee differs from the sample mean is 12.94 hours.

# c. Create a dataset of 10000 bootstrap replicates of 𝑥
hours.bootstrap <- mosaic::do(10000) *
  mosaic::resample(EmployedACS) %>% 
  summarise(boot.mean=mean(HoursWk))

# d. Create a histogram of the bootstrap replicates. Calculate the mean and standard
# deviation of this distribution. Provide an interpretation for the standard error.
ggplot(hours.bootstrap) +
  geom_histogram(aes(x=boot.mean))
mean(hours.bootstrap$boot.mean) # 37.8691
sd(hours.bootstrap$boot.mean) # 0.3642355
# The typical amount that a sample mean differs from the population mean is 0.36.

# e. Using the bootstrap replicates and the quantile method, create a 95% confidence 
# interval for μ, the average worked hours per week. Provide an interpretation of the
# interval.
quantile(hours.bootstrap$boot.mean, probs=c(0.025, 0.975))
#      2.5%   97.5% 
#   37.1406 38.5641
# I am 95% confident that the population mean of hours worked falls between 37.1 and 38.5.

# f.
std.error <- sd(hours.bootstrap$boot.mean)
c(sample.mean - 2*std.error, sample.mean + 2*std.error)
# It is almost the same as the interval calculated in part (e).

# 3. 
BootDist <- mosaic::do(10000) *
  mosaic::resample(BodyTemp50) %>% 
  summarise(boot.sd=sd(Pulse))
# b.
quantile(BootDist$boot.sd, probs=c(0.025, 0.975))
#      2.5%    97.5%
#  5.063878 7.647873
# I am 95% confident the population's standard deviation falls within 5.06 and 7.64.
