library(dplyr)
library(tidyr)
library(purrr)
library(ggplot2)
library(esquisse)
library(corrplot)
library(lubridate)
library(MASS)
library(car)


## 
employee_survey_data <- read.csv("employee_survey_data.csv", header = TRUE, stringsAsFactors = FALSE)
general_data <- read.csv("general_data.csv", header = TRUE, stringsAsFactors = FALSE)
in_time <- read.csv("in_time.csv", header = TRUE, stringsAsFactors = FALSE)
manager_survey_data <- read.csv("manager_survey_data.csv", header = TRUE, stringsAsFactors = FALSE)
out_time <- read.csv("out_time.csv", header = TRUE, stringsAsFactors = FALSE)

head(employee_survey_data)
str(employee_survey_data)

head(general_data)
str(general_data)

head(in_time)
str(in_time)

head(manager_survey_data)
str(manager_survey_data)

head(out_time)
str(out_time)

#### Data Preparation ####
# Let's start with in_time and out_time
# Rename X as EmployeeID for both dataset and remove Prefixed X from each column
colnames(in_time)[1] <- "EmployeeID"
colnames(in_time) <- gsub(pattern="X","",colnames(in_time))

colnames(out_time)[1] <- "EmployeeID"
colnames(out_time) <- gsub(pattern="X","",colnames(out_time))

# Check for any missing value or duplicate
length(unique(in_time$EmployeeID))
length(unique(out_time$EmployeeID))
# Great! No missing value or duplicate

# Check for NAs
sum(is.na(in_time))
sum(is.na(out_time))

# Let's check NAs at column level
map(in_time, ~ sum(is.na(.)))
map(out_time, ~ sum(is.na(.)))

# There are few columns with all NAs
colnames(in_time)[which(map(in_time, ~ sum(is.na(.))) == nrow(in_time))]
colnames(out_time)[which(map(out_time, ~ sum(is.na(.))) == nrow(out_time))]

# Columns are same for both in_time and out_time
# If we check calendar, these are all Indian Holidays
# So we can get rid of these columns, as they are all NAs
in_time <- in_time[-c(which(map(in_time, ~ sum(is.na(.))) == nrow(in_time)))]
out_time <- out_time[-c(which(map(out_time, ~ sum(is.na(.))) == nrow(out_time)))]

# Let's confirm if columns are same in both in_time and out_time
sum(colnames(in_time) != colnames(out_time))

# Convert to date timestamp
in_time[ , 2:250] <- map(in_time[ , 2:250], ~ ymd_hms(.))
out_time[ , 2:250] <- map(out_time[ , 2:250], ~ ymd_hms(.))

str(in_time)
str(out_time)

#### Derived Metrics ####
# Time Spent at Office
office_time <- out_time[ , 2:250] - in_time[ , 2:250]
office_time$EmployeeID <- in_time$EmployeeID

# Monthly 
