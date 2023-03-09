# Jadon Fowler
# Chapter 1 Homework
# STA 570 - Section 1
library(ggplot2)
library(dplyr)
library(mosaic)
library(mosaicData)

# problem 3
PolutionRatios <- data.frame(
  Ratio = c(76.50, 6.03, 3.51, 9.96, 4.24, 7.74, 9.54, 41.70, 1.84, 2.5,
            1.54,
            0.27, 0.61, 0.54, 0.14, 0.63, 0.23, 0.56, 0.48, 0.16, 0.18
  ),
  Type = c( rep('Terrestrial',11), rep('Aquatic',10) ) )

mean(PolutionRatios$Ratio[PolutionRatios$Type == "Terrestrial"])
median(PolutionRatios$Ratio[PolutionRatios$Type == "Terrestrial"])
mean(PolutionRatios$Ratio[PolutionRatios$Type == "Aquatic"])
median(PolutionRatios$Ratio[PolutionRatios$Type == "Aquatic"])

# problem 4
Hotels <- data.frame(
  Price = c(175, 180, 120, 150, 120, 125, 50, 50, 49, 45, 36, 45, 50, 50,
            40),
  Type = c( rep('Luxury',6), rep('Budget', 9) ) )

mean(Hotels$Price[Hotels$Type == "Luxury"])
sd(Hotels$Price[Hotels$Type == "Luxury"])
mean(Hotels$Price[Hotels$Type == "Budget"])
sd(Hotels$Price[Hotels$Type == "Budget"])

# problem 5
ggplot(Hotels, aes(x=Type, y=Price)) + geom_boxplot()





# crap for work



ichor.startup <- data.frame(
  time = c(47.897,
            47.489,
            47.442,
            47.189,
            47.564,
            54.638,
            49.298,
            49.049,
            47.574,
            47.348
  ),
  type = c( rep('?',5), rep('1.19.3',5) ) )

ggplot(ichor.startup, aes(x=type, y=time)) + geom_boxplot()
