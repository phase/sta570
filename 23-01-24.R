# STA 570 - Day 2
# Jan 24th, 2023
# status: trying to be really productive today


# copied from CodeSamplesChap1.txt, License=ARR
install.packages("ggplot2")
install.packages("dplyr")
install.packages("mosaic")
install.packages("mosaicData")
library(ggplot2)
library(dplyr)
library(mosaic)
library(mosaicData)
DataSet <- data.frame(
  Measurement = c(5.73,4.54,5.17,4.06,5.48,4.06,4.56,5.69,5.47,5.49,4.62,5.15,6.47,6.69,6.24,6.1,7.51,6.33,6.24,7.49,7.17,7.99,6.51,7.15,7.08,7.28,7.61,6.84,7.62,6.83 ),
  Group  = c( rep('Group1',12), rep('Group2',18) ) )

head(DataSet)

ggplot(DataSet, aes(x=Group, y=Measurement)) + geom_boxplot()

ggplot(DataSet, aes(x=Measurement)) +
  geom_histogram() +
  facet_grid( Group ~ . )

mean(DataSet$Measurement)
median(DataSet$Measurement)
var(DataSet$Measurement)
sd(DataSet$Measurement)

DataSet %>% 
  group_by(Group) %>%
  summarise( SampleSize = length(Measurement),
             SampleMean = mean(Measurement),
             SampleStandardDeviation    = sd(Measurement),
             SampleVariance   = var(Measurement) ,
             SampleMedian   =  median(Measurement))
