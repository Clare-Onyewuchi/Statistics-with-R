#########################
#     Session 3         #
#   R-practical Key     #
#   Statistics with R   #
#      Seda Erdem       #
#                       #
#########################


gapminder <- read.csv("http://bit.ly/2GxjYOB")

# See variables and their central tendencies
summary(gapminder)


# Q1 - Which of the following is false about the variable 'lifeExp'?
# A:The middle 50% of individuals have a life expectancy of between approximately 24 and 83 years
# From the summary(gapminder) we can see that 24 and 83 are min and max a not the middle 50% delimited by 1st and 3rd quatiles.
summary(gapminder)


# Q2 - What is the 60th quantile of the variable 'lifeExp'?
# Calculate quantiles
quantile(gapminder$lifeExp, 0.60) # it is 66.08


# Q3 - What is the skewness of the variable 'gdpPercap'?
# Quickly view the skewness of the variables
hist(gapminder$gdpPercap)  #Skewed to the right

# Or by using ggplot2
library(ggplot2)
ggplot(data = gapminder, aes(x = gdpPercap)) +
  geom_histogram(binwidth = 1000)

#Q4 - Based on this boxplot, what is the interquartile range of life expectancy approximately?
# Question to be answered by looking at plot on test. A: 24


# Q5 - What is the difference between the average life expectancies measured for Bangladesh and Austria?


mb <- gapminder  %>% filter(country == "Bangladesh") %>% pull(lifeExp) %>% mean()
ma <- gapminder  %>% filter(country == "Austria") %>% pull(lifeExp) %>% mean()
mb - ma #A: Around 23

# Q6 - What is the difference between the average GDP per capita measured for  Europe and Asia?
# gdppercap for countinents

# using base R for calculations just to see a different syntax from previous question.
me <- mean(gapminder$gdpPercap[gapminder$continent == "Europe"])
ma <- mean(gapminder$gdpPercap[gapminder$continent == "Asia"])
me - ma # A:6567




