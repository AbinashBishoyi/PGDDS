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
# [1] 39717 111
str(loans)
summary(loans)
glimpse(loans)

# We are wants to understand the driving factors (or driver variables) behind loan default
# We are trying to perform EDA, so we will use "Fully paid" and "Charged off" for our analysis, 
# to understand the pattern of Good Loan vs Bad Loan
# "Current" will be part of predictive data analysis which is out of scope, so we are separating them
current_loan <- filter(loans, loan_status %in% c("Current"))
loans <- filter(loans, loan_status %in% c("Fully Paid","Charged Off"))

# Drop columns with all NA values
loans <- Filter(function(x)!all(is.na(x)), loans) 

# There are multiple colun with only single unique value
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
# [1] 39717  48
glimpse(loans)

# Transform Appropriate Columns To Categorical Varibales
loans$term <- as.factor(loans$term)
loans$grade <- as.factor(loans$grade)
loans$sub_grade <- as.factor(loans$sub_grade)
loans$emp_length <- as.factor(loans$emp_length)
loans$home_ownership <- as.factor(loans$home_ownership)
loans$verification_status <- as.factor(loans$verification_status)
loans$loan_status <- as.factor(loans$loan_status)
loans$purpose <- as.factor(loans$purpose)
loans$addr_state <- as.factor(loans$addr_state)

# Parse Date variable properly
loans$issue_d <- as.character(loans$issue_d)
loans$issue_d <- paste(loans$issue_d, "-01", sep = "")
loans$issue_d <- parse_date_time(loans$issue_d, "myd")

loans$earliest_cr_line <- as.character(loans$earliest_cr_line)
loans$earliest_cr_line <- paste(loans$earliest_cr_line, "-01", sep = "")
loans$earliest_cr_line <- parse_date_time(loans$earliest_cr_line, "myd")

loans$last_pymnt_d <- as.character(loans$last_pymnt_d)
loans$last_pymnt_d <- paste(loans$last_pymnt_d, "-01", sep = "")
loans$last_pymnt_d <- parse_date_time(loans$last_pymnt_d, "myd")

loans$last_credit_pull_d <- as.character(loans$last_credit_pull_d)
loans$last_credit_pull_d <- paste(loans$last_credit_pull_d, "-01", sep = "")
loans$last_credit_pull_d <- parse_date_time(loans$last_credit_pull_d, "myd")

head(loans$issue_d)
head(loans$earliest_cr_line)
head(loans$last_pymnt_d)
head(loans$last_credit_pull_d)

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
unique(loans$pub_rec_bankruptcies)
summary(as.factor(loans$pub_rec_bankruptcies))

# Clean addition character in columns

loans <- loans %>% mutate(int_rate = gsub("%", "", int_rate),
            int_rate = as.numeric(int_rate),
            term = gsub(" months", "", term),
            term = gsub(" ", "", term),
            term = as.numeric(term))
loans <- loans %>% mutate(emp_length = gsub("years", "", emp_length),
            emp_length = gsub("year", "", emp_length),
            emp_length = gsub("\\+", "", emp_length),
            emp_length = gsub("<", "", emp_length),
            emp_length = as.numeric(emp_length))
loans <- loans %>% mutate(revol_util = gsub("%", "", revol_util),
            revol_util = as.numeric(revol_util))


# # Outlier Analysis
# ggplot(data = loans, aes(x = loan_status, y = loan_amnt)) + geom_boxplot()
# ggplot(data = loans, aes(x = grade, y = loan_amnt)) + geom_boxplot()
# ggplot(data = loans, aes(x = verification_status, y = loan_amnt)) + geom_boxplot()
# ggplot(data = loans, aes(x = addr_state, y = loan_amnt)) + geom_boxplot()
# ggplot(data = loans, aes(fill = as.factor(pub_rec_bankruptcies), x = loan_status)) + geom_bar()

# Derived Metrics - Feature Engineering
# Lenght of Credit = isue_d - earliest_cr_line
# loans$time_since_first_credit <- (loans$issue_d - loans$earliest_cr_line)/365
# loans$time_since_first_credit <- as.integer(difftime(today("EST"), loans$earliest_cr_line)/365) # In year
# loans$time_since_first_credit <- as.numeric(loans$time_since_first_credit)
# loans <- loans %>% filter(time_since_first_credit > 0)
# head(loans$time_since_first_credit)
loans$year <- as.factor(format(loans$issue_d, "%Y"))


# ggplot(data = loans, aes(earliest_cr_line)) + 
# #geom_density(aes(colour = grade)) + 
# xlim(0,40) + 
# labs(title = 'Earliest credit line distribution by grade')
# 
# typeof(loans$earliest_cr_line)

##### Univariate & Segmented Univariate Analysis #####
# Most prominient factor is customer's income, customer with better income has better probality to paid off the loan
# Let's verify annual_inc
summary(loans$annual_inc)
# Min. 1st Qu.  Median  Mean 3rd Qu.  Max.
# 4000 40320 59000 68949 82145 6000000
# Max vaule is too high than 3rd quartile, thus outlier
Plot01 <- ggplot(loans, aes(x=annual_inc,fill=loan_status)) + 
  geom_histogram(bins = 60, position = "fill", col = "black") +
  #scale_y_continuous(breaks = seq(0,1,0.1)) +
  labs(x = "Annual Income", y= "Percentage", fill = "Loan Status")
Plot01
# Definition of outlier is any data point more than 1.5 interquartile ranges (IQRs) 
# below the first quartile or above the third quartile
# All High Income (above the third quartile) customer has paid off their loan, 
# so we can exclude them from our analysis
inc_outlier <- as.numeric(quantile(loans$annual_inc)[4] + 1.5 * IQR(loans$annual_inc))
loans <- loans %>% filter(loans$annual_inc < inc_outlier)
Plot02 <-  ggplot(loans, aes(x = annual_inc, fill = loan_status)) + 
  geom_histogram(bins = 60, position = "fill", col = "black") +
  scale_y_continuous(breaks = seq(0,1,0.1)) +
  labs(x = "Annual Income", y= "Percentage" , fill = "Loan Status")
Plot02
# Plot looks good witout outlier

# Verification Status - confirms the income
Plot03 <- ggplot(loans, aes(x = verification_status, fill = loan_status)) + geom_bar() +
  labs(x = "Verification Status", y = "Percentage", fill = "Loan Status") + 
  geom_text(aes(label = scales::percent((..count..)/sum(..count..))), stat="count", position=position_stack(vjust = 0.5))
Plot03

# Home Ownership - 
Plot04 <- ggplot(loans, aes(x = home_ownership, fill = loan_status)) + geom_bar() +
  labs(x = "Home Ownership", y = "Percentage", fill = "Loan Status") + 
  geom_text(aes(label = scales::percent((..count..)/sum(..count..))), stat="count", position=position_stack(vjust = 0.5))
Plot04

# Purpose of the loan
Plot05 <- ggplot(loans, aes(x = purpose, fill = loan_status)) + geom_bar() +
  labs(x = "Purpose", y = "Percentage", fill = "Loan Status") + coord_flip() + 
  geom_text(aes(label = scales::percent((..count..)/sum(..count..))), stat="count", position=position_stack(vjust = 0.5))
Plot05

# Let's look at major purpose
# Based on loan purpose distribution, we will only analyse the categories which contain more than 5% of observations.
# credit_card, debt_consolidation, home_improvement, major_purchase, other
loans <- loans %>% filter(purpose %in% c("credit_card","debt_consolidation","home_improvement","major_purchase", "other"))
Plot06 <- ggplot(loans, aes(x = purpose, fill = loan_status)) + geom_bar() +
  labs(x = "Purpose", y = "Percentage", fill = "Loan Status") + coord_flip() + 
  geom_text(aes(label = scales::percent((..count..)/sum(..count..))), stat="count", position=position_stack(vjust = 0.5))
Plot06

# Yearwise trend
Plot07 <- ggplot(loans, aes(x = year, fill = loan_status)) + geom_bar() +
  labs(x = "Year", y = "Percentage", fill = "Loan Status") + 
  geom_text(aes(label = scales::percent((..count..)/sum(..count..))), stat="count", position=position_stack(vjust = 0.5))
Plot07

# Yearwise Purpose Trend
Plot08 <- ggplot(loans, aes(x = year, fill = purpose)) + geom_bar() +
  labs(x = "Year", y = "Percentage", fill = "Loan Status") + 
  geom_text(aes(label = scales::percent((..count..)/sum(..count..))), stat="count", position=position_stack(vjust = 0.5))
Plot08

# Termwise Trend
Plot09 <- ggplot(loans, aes(x = term, fill = loan_status)) + geom_bar() +
  labs(x = "Term", y = "Percentage", fill = "Loan Status") + 
  geom_text(aes(label = scales::percent((..count..)/sum(..count..))), stat="count", position=position_stack(vjust = 0.5))
Plot09

# Gradewise Trend
Plot10 <- ggplot(loans, aes(x = grade, fill = loan_status)) + geom_bar() +
  labs(x = "Grade", y = "Percentage", fill = "Loan Status") + 
  geom_text(aes(label = scales::percent((..count..)/sum(..count..))), stat="count", position=position_stack(vjust = 0.5))
Plot10

# Subgradewise Trend
Plot11 <- ggplot(loans, aes(x = sub_grade, fill = loan_status)) + geom_bar() +
  labs(x = "Sub-Grade", y = "Percentage", fill = "Loan Status") + 
  geom_text(aes(label = scales::percent((..count..)/sum(..count..))), stat="count", position=position_stack(vjust = 0.5))
Plot11

# Statewise Trend
# Looking for statewise defaluter trend
loansDataStateWise <- loans %>% filter(loan_status %in% c("Charged Off")) %>% select(addr_state, loan_amnt) %>% 
  group_by(addr_state) %>% dplyr::summarise(total_no_loan = n(), sloan_amnt = sum(loan_amnt), mloan_amnt = mean(loan_amnt))

state_code=c("AK", "AL", "AR", "AZ", "CA", "CO", "CT", "DC", "DE", "FL", "GA", "HI",
     "IA", "ID", "IL", "IN", "KS", "KY", "LA", "MA", "MD", "ME", "MI", "MN",
     "MO", "MS",  "MT", "NC", "ND", "NE", "NH", "NJ", "NM", "NV", "NY", "OH",
     "OK", "OR", "PA", "PR", "RI", "SC", "SD", "TN", "TX", "UT", "VA", "VT",
     "WA", "WI", "WV", "WY")

state_name = c("Alaska","Alabama" ,  "Arkansas", "Arizona","California" , "Colorado" ,
     "Connecticut", "District of Columbia","Delaware" ,  "Florida" , "Georgia" ,
     "Hawaii","Iowa" ,"Idaho" , "Illinois" , "Indiana" ,  "Kansas" ,
     "Kentucky" , "Louisiana" , "Massachusetts", "Maryland" ,"Maine" ,
     "Michigan" , "Minnesota" , "Missouri" ,"Mississippi" ,  "Montana" ,
     "North Carolina","North Dakota", "Nebraska" , "New Hampshire" , "New Jersey" ,  "New Mexico" ,
     "Nevada" ,"New York" , "Ohio" , "Oklahoma" ,
     "Oregon" , "Pennsylvania" , "Puerto Rico", "Rhode Island" , "South Carolina", "South Dakota" ,
     "Tennessee" , "Texas" , "Utah" ,  "Virginia","Vermont" ,
     "Washington" , "Wisconsin", "West Virginia" , "Wyoming") 

states_data <- data.frame(state=tolower(state_name), state_code)
states_data_map <- aggregate(cbind(long, lat ) ~ region, data = states_map, FUN = function(x) mean(range(x)))
states_data <-  merge(states_data, states_data_map, by.x ="state", by.y = "region")

loansDataStateWise <-  merge(  loansDataStateWise, states_data,by.x = "addr_state", by.y = "state_code")
loanClubDataAbb <- aggregate(cbind(long, lat, sloan_amnt, total_no_loan) ~ state, data =  loansDataStateWise, FUN = function(x) mean(range(x)))

Plot12 <- ggplot( loansDataStateWise, aes(map_id = state, fill=total_no_loan)) + 
  geom_map(map = states_map, colour="black") + 
  scale_fill_gradient2(low="#01590b", mid="grey90", high="#f90202", midpoint = median(loansDataStateWise$total_no_loan)) +
  expand_limits(x = states_map$long, y = states_map$lat) + coord_map("polyconic") +
  labs(x = "", y = "", title = "Loan Defaulter Statewise", fill = "Defaulter Count") +
  geom_text(data=loanClubDataAbb, aes(long, lat, label = state), size=3, check_overlap = TRUE, vjust = 0, nudge_y = 0.5)
Plot12
# California, Florida, Texas and New York are having high loan defaulter
