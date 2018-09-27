# Gramener Case Study
# Abinash Bishoyi, Karthik Vecham, Balaji Nagaraja

library(ggplot2)
library(dplyr)
library(ggthemes)
library(gridExtra)
library(lubridate)
library(gridExtra)
library(zoo)
library(readxl)
Data_Dictionary <- read_excel("Data_Dictionary.xlsx")


# For map
library(maps)
states_map <- map_data("state")



# Read Data from CSV
loans <- read.csv("loan.csv", header = TRUE, stringsAsFactors = FALSE)

#### Data Explanation ####
# View(Data_Dictionary)

#### Preparation ####
dim(loans)
# [1] 39717   111
str(loans)
summary(loans)
glimpse(loans)

# There are multiple colun with only NA or single unique value
# We can cleanse those column as they willnot be helpful for our analysis
col_one_unique <- which(sapply(loans, function(x) length(unique(x))==1))
unique(loans[col_one_unique])
# Values are NA, f, INDIVIDUAL, n, 1, 0

# Based on the data dictionaly these columns will not be helpful for our analysis
# So we can drop the columns from loan data set
loans <- loans[, -col_one_unique]

# We can also drop columns which only have 0 or NA's as their unique values
col_two_unique <- which(sapply(loans, function(x) length(unique(x)) == 2 & sum(unique(x) %in% c(0,NA))==2))
unique(loans[col_two_unique])

# Based on the data dictionaly these columns will also not be helpful for our analysis
# So we can drop the columns from loan data set
loans <- loans[, -col_two_unique]

dim(loans)
# [1] 39717    48
glimpse(loans)

# Transform Appropriate Columns To Categorical Varibales
loans$term <- as.factor(loans$term)
loans$grade <- as.factor(loans$grade)
loans$emp_length <- as.factor(loans$emp_length)
loans$home_ownership <- as.factor(loans$home_ownership)
loans$verification_status <- as.factor(loans$verification_status)
loans$loan_status <- as.factor(loans$loan_status)
loans$purpose <- as.factor(loans$purpose)
loans$addr_state <- as.factor(loans$addr_state)

# Parse Date variable properly
loans <- loans %>% mutate(issue_d = paste("01", issue_d, sep = "-"),
                          issue_d = parse_date_time(issue_d, orders = c("dmy"), locale = "EST"),
                          earliest_cr_line = paste("01", earliest_cr_line, sep = "-"),
                          earliest_cr_line = parse_date_time(earliest_cr_line, orders = c("dmy"), locale = "EST"))
head(loans$issue_d)
head(loans$earliest_cr_line)

# Count of NA values by column
loans %>% summarise_all(funs(sum(is.na(.))))
# mths_since_last_delinq and mths_since_last_record are having too many NA's

# Following columns are not helpful for our analysis
non_eda_col <- c("id", "member_id", "url", "desc", "title", "zip_code", "mths_since_last_delinq", "mths_since_last_record")

# Dropping these columns
loans <- loans[, !(colnames(loans) %in% non_eda_col)]

# Count of NA values by column: just to verify if the NA values cleared
loans %>% summarise_all(funs(sum(is.na(.))))
sum(is.na(loans)) # 0 

# Derived Metrics
# Lenght of Credit Line = earliest_cr_line - today
loans$len_of_cr_line <- as.integer(difftime(today("EST"), loans$earliest_cr_line)/365) # In year
