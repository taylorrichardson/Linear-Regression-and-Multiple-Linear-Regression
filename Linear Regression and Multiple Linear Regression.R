#
# Taylor Richardson 
# December 10, 2018
#
#
# I conducted Linear Regression and Multiple Linear Regression in order to determine if there are any variables that would help to predict the quality score
#
#
# 
# The R2 score suggests that the model is not effective at predicting the quality score
# The following variables are correlated with quality and should be kept; volatile acidity, pH, Sulphates, Chlorides, Free sulfur dioxide, alcohol, total sulfur dioxide.
#


#Load libraries
library(mosaic)
library(mosaicData)
library(mosaicCore)
library(lattice)
library(psych)
library(ggplot2)
library(dplyr)

#Histogram
histogram(winequality$quality, fit="normal")

#Dot graph
ggplot(winequality, aes(x = sqrt(winequality$alcohol), y = sqrt(winequality$quality))) + geom_point(colour = "blue") + geom_smooth(colour = "red") + labs(title = "Quality in comparison to alcohol (Square Root)", x = "Alcohol", y = "Quality")

#Overview of the variables
describe(winequality)

#Ho: The mean is equal to 5.6
#Ha: The mean does not equal 5.6
#Run a t-test
t.test(winequality$quality, mu = 5.6)

#Linear Regression model
lmodel <- lm(sqrt(quality) ~ sqrt(alcohol), data = winequality)
summary(lmodel)

#Multi linear Regression Model
multimodel <- lm(sqrt(quality) ~ sqrt(fixed_acidity) + sqrt(volatile_acidity) + sqrt(citric_acid) + sqrt(residual_sugar) + sqrt(chlorides) + sqrt(free_sulfur_dioxide) + sqrt(total_sulfur_dioxide) + sqrt(density) + sqrt(pH) + sqrt(sulphates) + sqrt(alcohol), data = winequality)
summary(multimodel)
