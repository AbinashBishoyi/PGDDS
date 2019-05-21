

## Loading libraries

library(caret)
library(plyr)
library(Information)
library(ggplot2)
library(devtools)
library(MASS)
library(car)
library(glmnet)
library(ROSE)
library(ggthemes)
library(ROCR)
library(grid)
library(dplyr)
library(scales)
library(gridExtra)
library(data.table)
library(tidyr)
library(broom)
library(Hmisc)
library(ROCR)



## Loading data from files
Demographic_data <- read.csv("Demographic data.csv", header = TRUE, stringsAsFactors = FALSE)
dim(Demographic_data) # 71295    12

Credit_Bureau_data <- read.csv("Credit Bureau data.csv", header = TRUE, stringsAsFactors = FALSE)
dim(Credit_Bureau_data) # 71295    19

# View(Demographic_data)
# View(Credit_Bureau_data)

## Merging both datasets
Master_Data <- merge(Demographic_data, Credit_Bureau_data, by = "Application.ID")

View(Master_Data)
summary(Master_Data)

# There are two columns for Performance Tag
# Let's check if they are redundant
sum(Master_Data$Performance.Tag.x - Master_Data$Performance.Tag.y, na.rm = TRUE) # 0 means they are identical
# Removing the redundant column
Master_Data <- Master_Data[-30]
names(Master_Data) # Renaming Performance.Tag.x as Performance.Tag
names(Master_Data)[which(names(Master_Data) == "Performance.Tag.x")] <- "Performance.Tag"

dim(Master_Data) # 71301    29
summary(Master_Data)
str(Master_Data)

# Let's check for NAs in all columns
sapply(Master_Data, function(x) (sum(is.na(x))))
sapply(Master_Data, function(x) (sum(is.na(x))/length(x))*100)

# ~2% of Performance.Tag is NA
# Performance tag NA are the rejected population (Credit application was rejected)
# Storing it as a separate dataframe
Rejected_Data <- Master_Data[which(is.na(Master_Data$Performance.Tag)), ]

# Removing Performance.Tag NAs from the dataset
Master_Data <- Master_Data[-which(is.na(Master_Data$Performance.Tag)), ]
dim(Master_Data) # 69867    29


# Let's check the default rate
sum(Master_Data$Performance.Tag)/nrow(Master_Data)*100 # ~4.2%

#### About the dataset ####
# Rejection Rate = 2%
# Approval Rate = 98%
# Default Rate = 4.2%

#### Since default rate is so low, that means our dataset is imbalanced


#### Let's check for individual columns ####
## Application.ID
# Check for duplicates
sum(duplicated(Master_Data$Application.ID)) # 9 duplicates

# Let's check the duplicates data
View(Master_Data[which(duplicated(Master_Data$Application.ID)), ])

# Removing duplicates Application.ID
Master_Data <- Master_Data[-which(duplicated(Master_Data$Application.ID)), ]
sum(duplicated(Master_Data$Application.ID)) # 0 duplicates

# Let's check for NA in all columns
sapply(Master_Data,function(x) sum(is.na(x)))
(sum(is.na(Master_Data))/nrow(Master_Data))*100 # 2.25%

# Dropping NAs from the dataset
delete.na <- function(DF, n=0) {
  DF[rowSums(is.na(DF)) <= n,]
}
Master_Data <- delete.na(Master_Data)
(sum(is.na(Master_Data))/nrow(Master_Data))*100 # 0

## Age
summary(Master_Data$Age)

# Looks like there are some -ve values are present
# Also as per RBI regulation, customer need to be at least 18 years to apply for Credit Card
nrow(Master_Data[which(Master_Data$Age < 18), ])
Master_Data <- Master_Data[-which(Master_Data$Age < 18), ]

nrow(Master_Data[which(Master_Data$Income < 0), ])/nrow(Master_Data)*100
