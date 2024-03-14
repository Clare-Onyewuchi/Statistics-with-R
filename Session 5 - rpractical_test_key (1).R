#########################
# Session 5 2021/22     #
# Correlation/regression#
#   R-practical Key     #
#   Statistics with R   #
#      Seda Erdem       #
#                       #
#########################

#~~~~~~~~~~~~~~~   SET-UP   ~~~~~~~~~~~~~~~~~
# Install and load packages needed for this session
library(tidyverse)
library(kableExtra)

# Load the data
bike_data <- read.csv("http://bit.ly/2M3taO8")

# Look at the data. By now you have several commands to choose from.
summary(bike_data)
glimpse(bike_data)
skimr::skim(bike_data)
summarytools::dfSummary(bike_data)

#~~~~~~~~~~~~~~~   ANALYSES   ~~~~~~~~~~~~~~~~~

# Q.1 What is the correlation between number of bike rentals and temperature?
# 0.63 -correct

# Method 1: 
cor(bike_data[,-1]) # [,-1] will remove first variable of dataset which is just an index variable

# Method 2: using tidyverse package
bike_data %>% select(-X) %>% cor()

# Method 3: Using kableExtra package to print out a table in the Viewer

bike_data %>% select(-X) %>% #remove unwanted variable (index of observations)
              cor() %>%
              kable(digits = 3,  caption = "Correlations between variables") %>%
              kable_styling("striped", full_width = F)


# Q.2 Which variable is strongly associated with the variable "count"?
# temp - correct
# Check the correlation matrix in the previous question Q1


# Q.3 What is the R-squared value of this regression?
# 0.79 - correct
#Fit the model

model <- lm(count ~ season + year + workingday + weathersit + temp + windspeed, data = bike_data)
summary(model) # Just read off from the results table OR: 
r2 <- summary(model)$r.squared 
r2
round(r2, digits = 3)
round(0.7861084, digits = 3)


# Q.4 How much variability in the count of bike rental is explained by the explanatory variables of this model?
# 79% - correct
# Same as Q3

# Q.5 Is this model statistically significant?
# Yes - correct
# Evident from the p-value of the F-statistics, which is less than any significance level.


# Q.6 What is the estimated coefficient of "windspeed"?
#  -2254.73 - correct

# Q.7 What is the estimate for the intercept?
# 1320.26 - correct

# Q.8 What is the confidence interval for "temp" at 90% confidence level?
# [4788.06, 5433.67] - correct
# Find confidence intervals
confint(model, level = 0.90)
confint(model, level = 0.95)
confint(model, level = 0.99)

# Q.9  What is the predicted bike rental count during a working day in 2011 when the weather is clear at 0.3 Celsius degree and windspeed is 0.20in winter?
#  2219 - correct
#Prediction
condition <- data.frame(season = 1, year = 0, workingday = 1, weathersit = 1, temp = 0.3, windspeed = 0.20)
predict(model, condition, interval = "prediction", level = 0.95)

# Q.10 All else being equal, what is the predicted change in bike rental counts if temperature goes up by 0.2 normalised Celsius?
# 10222 - correct
# Since everything else is the same, we can just use the coefficient for temperature.
# 5110.86 * 0.2
# Otherwise in R we would need to calculate 2  prediction with a difference of 0.2 in temp and then do the difference of the predictions.


# Q.11 All else being equal, what is the predicted difference between bike rental counts in an area with an average temperature of 0.22 normalised Celcius compared to an area with an average temperature of 0.26 normalised Celsius?

# 2044.4 - correct
# As in previous question we are looking only at a change in one variable and we can simply use the variable coefficient multiplyed by the change in temperature.
# 5110.86 * 0.04

# Q.12 What is the F statistic?
# 443.5 - correct

# Q.13 The distribution of residuals is
# Left skewed - Correct
hist(model$residuals)

# Q.14 What is the t-statistic of "workingday"?
# 2.638 - correct

# Q.15 Which one the following R codes can be used to test homoskedasticity?
plot(model$residuals ~ model$fitted)
abline(h = 0, col = "red", lty = 1, lwd = 3)
