# Martin McParland - L00143723
# Msc Big Data Analytics Class A
# Data Science - CA3 Data Analysis - Health in Ireland
# Submitted 19th May 2019

# There are two datasets to be used, Unemployment rates and death rates
# Both xls files are imported to data frames unemployment and deaths

unemployment <- read.csv("Unemployment rate by Month 2000 - 2018.csv", 
                         header = TRUE, stringsAsFactors = FALSE)

deaths <- read.csv("Deaths by Month by Year 2005 - 2016.csv", 
                   header = TRUE, stringsAsFactors = FALSE)

# View the head and structure of unemployment
head(unemployment)
str(unemployment)
# View the head and structure of deaths
head(deaths)
str(deaths)
