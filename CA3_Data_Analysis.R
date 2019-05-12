# Martin McParland - L00143723
# Msc Big Data Analytics Class A
# Data Science - CA3 Data Analysis - Health in Ireland
# Submitted 19th May 2019

# The dataset to be used includes Unemployment rates and death rates
# by month for the period 2005 to 2016. The years are categorized as
# Celtic_Tiger for years 2005 to 2008, Crash for years 2009 to 2012 and
# Recovery for years 2013 to 2016. The CSV file is read into a data frame.

unemployment_and_deaths <- read.csv("Unemployment and Death rates by Month 2005 - 2016.csv", 
                         header = TRUE, stringsAsFactors = FALSE)


# View the head and structure of unemployment_and_deaths
head(unemployment_and_deaths)
str(unemployment_and_deaths)


# Rename first column header to Period and convert Period to a Factor
names(unemployment_and_deaths)[1] <- "Period"
unemployment_and_deaths$Period <- factor(unemployment_and_deaths$Period)
str(unemployment_and_deaths)

# Before selecting the appropriate test, we need to check whether
# the data is normally distributed or not.

library("lattice")
# The histogram uses a 1 sided formula so we don't specify 
# anything on the left side of ~
# and on the right side we specify which variable is in the histogram

# The first histogram is for deaths across the three periods
histogram(~ deaths_per_capita | Period, data = unemployment_and_deaths)

# The second histogram is for unemployment rates across the three periods
histogram(~ unemployment_rate | Period, data = unemployment_and_deaths)

# Formal test of normality for deaths provided through the Shapiro-wilks test
normality_test <- shapiro.test(unemployment_and_deaths$deaths_per_capita)
normality_test$p.value
# p value tells us that the chances the sample comes from a normal distribution.
# In this example the p value is clearly lower than 0.01, so it is not normally distributed

# Formal test of normality for unemployment rate provided through the Shapiro-wilks test
normality_test <- shapiro.test(unemployment_and_deaths$unemployment_rate)
normality_test$p.value
# p value tells us that the chances the sample comes from a normal distribution.
# In this example the p value is clearly lower than 0.01, so it is not normally distributed

# To check the normality of deaths per capita for each of the periods use the tapply() function
with(unemployment_and_deaths, tapply(deaths_per_capita, Period, shapiro.test))
# The resulting p values for each period is lower than 0.01, so they are all not normally distributed

# To check the normality of unemployment rate for each of the periods use the tapply() function
with(unemployment_and_deaths, tapply(unemployment_rate, Period, shapiro.test))
# The resulting p values for each period is lower than 0.01, so they are all not normally distributed

### POWER TESTS ###
# The dataset contains 144 records, 48 for each period.
# Based on a 90% certainty, with an effect size of 0.8 and no more than 5% 
# chance of erroniously concluding that a difference exisits when it doesn't. 
# We can check for the optimal sampsize. 

install.packages("pwr")
library(pwr)

power_information <- pwr.t.test(power = 0.90, 
                                d = 0.8,
                                sig.level = 0.05,
                                n = NULL,
                                type = "two.sample",
                                alternative = "two.sided")
power_information
plot(power_information)
# Results suggest that we need 34 samples in each group. We have 48 in each period.





kruskal.test(deaths_per_capita ~ Period, data = unemployment_and_deaths)
kruskal.test(unemployment_rate ~ Period, data = unemployment_and_deaths)

# We want to model the means of variable deaths_count_thousands as a 
# function of the variable Period
aov_model <- aov(deaths_per_capita ~ Period, data = unemployment_and_deaths)
aov_model

summary(aov_model)
# p > 0.05 provides evidence that the 3 periods are not statistically different

# models.tables function allows us to examine the individual levels of factors.
# Creates 2 tables
model.tables(aov_model, type = "effects")

# Test the pair wise differences between periods.
# Pairwise comparison tests can be used to determine which group
# differences are statistically significant
par(mar=c(5,10,2,2))
plot(TukeyHSD(aov_model), las = 2)

# anova assumes that variances are equal across groups or samples
# The Bartlett test can verify this assumption
bartlett.test(deaths_per_capita ~ Period, data = unemployment_and_deaths)

# anova is sensitive to outliers
outlierTest(aov_model)

# Lets examine the output from the TukeyHSD() function
# for pairwise differences between group means
TukeyHSD(aov_model)


install.packages('gplots')
library(gplots)

plotmeans(deaths_per_capita ~ Period, data = unemployment_and_deaths,
          xlab = "Period",
          ylab = "Deaths",
          main = "Mean Plot with 95% CI")

plotmeans(unemployment_rate ~ Period, data = unemployment_and_deaths,
          xlab = "Period",
          ylab = "Unemployment Rate",
          main = "Mean Plot with 95% CI")
