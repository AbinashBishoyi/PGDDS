# Gramener Case Study
# Abinash Bishoyi, Karthik Vecham, Balaji Nagaraja

library(ggplot2)
library(dplyr)
library(ggthemes)
library(gridExtra)
library(lubridate)
library(gridExtra)
library(corrplot)
library(zoo)
library(readxl)
library(stringr)
Data_Dictionary <- read_excel("Data_Dictionary.xlsx")


# For map
library(maps)
states_map <- map_data("state")



# Read Data from CSV
loans <- read.csv("loan.csv", header = TRUE, stringsAsFactors = FALSE)

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
non_eda_col <- c("id", "member_id", "url", "desc", "title", "zip_code", "mths_since_last_delinq", "mths_since_last_record", "emp_title")

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
loans$year <- as.factor(format(loans$issue_d, "%Y"))
loans$sub_grade_num <- sapply(loans$sub_grade, function(x) str_split(x,"")[[1]][2])

##### Univariate & Segmented Univariate Analysis #####
# Loan Status
Plot01 <- ggplot(loans, aes(x = loan_status, fill = loan_status)) + 
  geom_bar() + labs(title = "Plot01 - Loan Status", x = "Loan Status", y= "Percentage", fill = "Loan Staus") +
  geom_text(aes(label = scales::percent((..count..)/sum(..count..))), stat="count", position=position_stack(vjust = 0.5))
Plot01
# Fully Paid - 85.7% and charged Off - 14.3%

# Most prominient factor is customer's income, customer with better income has better probality to paid off the loan
# Let's verify annual_inc
summary(loans$annual_inc)
# Min. 1st Qu.  Median  Mean 3rd Qu.  Max.
# 4000 40320 59000 68949 82145 6000000
# Max vaule is too high than 3rd quartile, thus outlier
Plot02 <- ggplot(loans, aes(x=annual_inc, fill=loan_status)) + 
  geom_histogram(bins = 60, position = "fill", col = "black") +
  scale_y_continuous(breaks = seq(0,1,0.1)) +
  labs(title = "Plot02 - Annual Income", x = "Annual Income", y= "Percentage", fill = "Loan Status")
Plot02
# Definition of outlier is any data point more than 1.5 interquartile ranges (IQRs) 
# below the first quartile or above the third quartile
# All High Income (above the third quartile) customer has paid off their loan, 
# so we can exclude them from our analysis
inc_outlier <- as.numeric(quantile(loans$annual_inc)[4] + 1.5 * IQR(loans$annual_inc))
loans <- loans %>% filter(loans$annual_inc < inc_outlier)
Plot03 <-  ggplot(loans, aes(x = annual_inc, fill = loan_status)) + 
  geom_histogram(bins = 60, position = "fill", col = "black") +
  scale_y_continuous(breaks = seq(0,1,0.1)) +
  labs(title = "Plot03 - Annual Income", x = "Annual Income", y= "Percentage" , fill = "Loan Status")
Plot03
# Plot looks good witout outlier and Annual Inconce is one of the factor as % of defaulter is going down with higher income

# Verification Status - confirms the income
Plot04 <- ggplot(loans, aes(x = verification_status, fill = loan_status)) + geom_bar() +
  labs(title = "Plot04 - Verification Status", x = "Verification Status", y = "Percentage", fill = "Loan Status") + 
  geom_text(aes(label = scales::percent((..count..)/sum(..count..))), stat="count", position=position_stack(vjust = 0.5))
Plot04
# Verification Status is also a driving factors as "Not Verified" as a greater probability to becone defaulter

# Purpose of the loan
Plot05 <- ggplot(loans, aes(x = purpose, fill = loan_status)) + geom_bar() +
  labs(title = "Plot05 - Purpose", x = "Purpose", y = "Percentage", fill = "Loan Status") + coord_flip() + 
  geom_text(aes(label = scales::percent((..count..)/sum(..count..))), stat="count", position=position_stack(vjust = 0.5))
Plot05
# Let's look at major purpose
# Based on loan purpose distribution, we will only analyse the categories which contain more than 5% of observations.
# credit_card, debt_consolidation, home_improvement, major_purchase, other
loans <- loans %>% filter(purpose %in% c("credit_card","debt_consolidation","home_improvement","major_purchase", "other"))
Plot06 <- ggplot(loans, aes(x = purpose, fill = loan_status)) + geom_bar() +
  labs(title = "Plot06 - Purpose", x = "Purpose", y = "Percentage", fill = "Loan Status") + coord_flip() + 
  geom_text(aes(label = scales::percent((..count..)/sum(..count..))), stat="count", position=position_stack(vjust = 0.5))
Plot06

# Yearwise trend
Plot07 <- ggplot(loans, aes(x = year, fill = loan_status)) + geom_bar() +
  labs(title = "Plot07 - Yearwise Trend", x = "Year", y = "Percentage", fill = "Loan Status") + 
  geom_text(aes(label = scales::percent((..count..)/sum(..count..))), stat="count", position=position_stack(vjust = 0.5))
Plot07
# Majority of loans have been provided in the year 2011(US debt ceiling crisis), there has been a significant increase YoY(Year on Year)
# Number of charged off loans has increased over the year,

# Yearwise Purpose Trend
Plot08 <- ggplot(loans, aes(x = year, fill = purpose)) + geom_bar() +
  labs(title = "Plot08 - Yearwise Purpose Trend", x = "Year", y = "Percentage", fill = "Purpose") + 
  geom_text(aes(label = scales::percent((..count..)/sum(..count..))), stat="count", position=position_stack(vjust = 0.5))
Plot08
# Credit card, Debt consolidation  and Home Improvement loans taken majorly in the years 2010 and 2011.

# Home Ownership Analysis
Plot09 <- ggplot(loans, aes(x = home_ownership, fill = purpose)) + geom_bar() +
  labs(title = "Plot09 - Home Ownership vs Purpose", x = "Home Ownership", y = "Percentage", fill = "Purpose") +
  geom_text(aes(label = scales::percent((..count..)/sum(..count..))), stat="count", position=position_stack(vjust = 0.5))
Plot09
# Customer with Rent/Mortgage houses have applied for Credit Card and Debt Consolidation loans the most.

# Termwise Trend
Plot10 <- ggplot(loans, aes(x = as.factor(term), fill = loan_status)) + geom_bar() +
  labs(title = "Plot10 - Termwise Trend", x = "Term", y = "Percentage", fill = "Loan Status") + 
  geom_text(aes(label = scales::percent((..count..)/sum(..count..))), stat="count", position=position_stack(vjust = 0.5))
Plot10
# Loan of 36 months term as defaluted the most

# Gradewise Trend
Plot11 <- ggplot(loans, aes(x = grade, fill = loan_status)) + geom_bar() +
  labs(title = "Plot11 - Gradewise Trend", x = "Grade", y = "Percentage", fill = "Loan Status") + 
  geom_text(aes(label = scales::percent((..count..)/sum(..count..))), stat="count", position=position_stack(vjust = 0.5))
Plot11
# Grade B, C and D has highest defulter

# Subgradewise Trend
Plot12 <- ggplot(loans %>% group_by(sub_grade, loan_status) %>% summarize(cnt = length(loan_status))) + 
  geom_col(aes(x=sub_grade, y=cnt, fill=loan_status), position="fill") +
  scale_y_continuous(breaks=seq(0,1,0.1)) +
  labs(title="Plot12 - Subgradewise Trend", x="Sub-Grade", y="Percentage", fill="Loan Status")
  # ggplot(loans, aes(x = sub_grade, fill = loan_status)) + geom_bar() +
  # labs(title = "Plot12 - Sub-gradewise Trend", x = "Sub-Grade", y = "Percentage", fill = "Loan Status") +
  # geom_text(aes(label = scales::percent((..count..)/sum(..count..))), stat="count", position=position_stack(vjust = 0.5))
Plot12

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

Plot13 <- ggplot( loansDataStateWise, aes(map_id = state, fill=total_no_loan)) + 
  geom_map(map = states_map, colour="black") + 
  scale_fill_gradient2(low="#01590b", mid="grey90", high="#f90202", midpoint = median(loansDataStateWise$total_no_loan)) +
  expand_limits(x = states_map$long, y = states_map$lat) + coord_map("polyconic") +
  labs(title = "Plot13 - Loan Defaulter Statewise", x = "", y = "", fill = "Defaulter Count") +
  geom_text(data=loanClubDataAbb, aes(long, lat, label = state), size=3, check_overlap = TRUE, vjust = 0, nudge_y = 0.5)
Plot13
# California, Florida, Texas and New York are having high loan defaulter

# Public Bankruptcy Records
Plot14 <- ggplot(data = loans %>% group_by(pub_rec_bankruptcies, loan_status) %>% summarize(cnt = length(loan_status))) + 
  geom_col(aes(x=as.factor(pub_rec_bankruptcies), y=cnt, fill=loan_status), position="fill") + 
  labs(title="Plot14 - Public Bankruptcy Record", x="No. of Records", y="Percentage", fill="Loan Status")
Plot14

# Revolving Utilization
Plot15 <- ggplot(loans) + geom_histogram(aes(x = revol_util, fill=loan_status), col="black", position="fill", bins=60) +
  scale_y_continuous(breaks=seq(0,1,0.1)) +
  labs(title="Plot15 - Revolving Utilization", x = "Revolving Utilization", y = "Percentage", fill = "Loan Status")
Plot15

# Delinquency in past 2 years
Plot16 <- ggplot(loans %>% group_by(delinq_2yrs, loan_status) %>% summarize(cnt = length(loan_status))) + 
  geom_col(aes(x=delinq_2yrs,y=cnt,fill=loan_status),position="fill") +
  scale_y_continuous(breaks=seq(0,1,0.1)) +
  scale_x_continuous(breaks=seq(0,12,1)) +
  labs(title="Plot16 - Delinquent in last 2 years", x="Number of delinquent", y="Percentage", fill="Loan Status")
Plot16

# Loan Amount
Plot17 <- ggplot(loans) + geom_histogram(aes(x = loan_amnt, fill = loan_status), col = "black", bins = 60, position = "fill") +
  scale_x_continuous(breaks = seq(0,36000,3000)) +
  scale_y_continuous(breaks = seq(0,1,0.1)) +
  labs(title="Plot17 - Loan Amounts", x="Loan Amount", y="Count", fill="Loan Status")
Plot17

# Inference:
# Variables that effect the charge off Vs Fully paid loans
# Annual Income
# Purpose
# Home Ownership
# Verification Status
# Public Bankruptcy Records
# Term
# Grade and Sub-grade
# Loan Amount
# Revolving Utilization

#### Bivariate Analysis ####
# Lets see different corelations between variables
cor_name= c("loan_amnt", "int_rate", "installment", "sub_grade", "annual_inc", "dti", "revol_util", "delinq_2yrs")

cor_var <- select(loans, one_of(cor_name))
cor_var <- cor_var[complete.cases(cor_var), ]

# Convert the character types to numeric types
cor_var$num_subgrade <- as.numeric(as.factor(cor_var$sub_grade))

# Remove the character 
cor_var <- select(cor_var, -sub_grade)

M <- cor(cor_var)
corrplot(M, method = "number", title = "Correlation Map of Variables", 
         type = "lower", order = "FPC", number.cex = 1, tl.cex = 0.9)

# Conclusion from Bivariate Analysis
# Based on customer's need, grade/sub-grade decides the interest rate/installment etc for the loan
# Therefore we should check the co-relation between grade/sub-grade and other parameters
# If a parameter gives strong relaion with grade/subgrade or other parameters then only
# one parameter may be considered as driving factor.

# Analyse Grade/SubGrade with different variables
# Grade vs Revolving Utilization
Plot18 <- ggplot(loans) + 
  geom_boxplot(aes(x = sub_grade, y= revol_util, fill = grade)) +
  geom_line(data = (loans %>% group_by(sub_grade) %>% summarize(avg = mean(revol_util, na.rm=TRUE))), aes(x = sub_grade, y = avg, group = 1)) +
  scale_y_continuous(breaks=seq(0,100,5)) +
  labs(title="Plot18 - Grade Vs Revolving Credit Utilization",x="Sub-Grade",y="Revolving Credit Utilization(%)",fill="Grade")
Plot18
# There seems to a relationship between the grade of the loan and the revolving utilization, but this isn't consistent

# Grade vs Loan Amount
# Note : the line signifies the mean of the dataset
Plot19 <- ggplot(loans) + geom_boxplot(aes(x = sub_grade, y = loan_amnt, fill = grade)) + 
  geom_line(data = (loans %>% group_by(sub_grade) %>% summarize(avg = mean(loan_amnt, na.rm=TRUE))),
            aes(x= sub_grade, y = avg, group = 1)) +
  scale_y_continuous(breaks=seq(0,36000,1000)) +
  labs(title="Plot19 - Grade Vs Loan Amount", x="Sub-Grade", y="Loan Amount", fill="Grade")
Plot19
# Grade/Sub-Grade does seem to be related to loan amount, but this isn't consistent

# Grade vs DTI
# Note : the line signifies the mean DTI in that sub_grade
Plot20 <- ggplot(data = loans %>% 
                group_by(grade, sub_grade_num) %>% 
                summarize(med_dti = median(dti, na.rm = TRUE)), aes(x = grade, y = sub_grade_num, value = med_dti)) + 
  geom_tile(aes(fill=med_dti)) +
  geom_text(aes(label = med_dti), col= "white") +
  labs(title="Plot20 - DTI Vs Grade", x = "Grade", y = "Sub-Grade", fill= "Median DTI")
Plot20

# DTI Vs Grade Vs Loan Status
Plot21 <- ggplot(data = loans %>% 
                group_by(grade, sub_grade_num, loan_status) %>% 
                summarize(med_dti = median(dti, na.rm = TRUE)), aes(x = grade, y = sub_grade_num, value = med_dti)) + 
  geom_tile(aes(fill = med_dti)) +
  geom_text(aes(label = med_dti), col = "white") + 
  labs(title="Plot21 - DTI Vs Grade Vs Loan Status", x = "Grade", y = "Sub-Grade", fill = "Median DTI") +
  facet_wrap(~loan_status)
Plot21

grid.arrange(Plot20, Plot21, nrow = 2, ncol = 1)
# There seems to be relationship between the DTI and the Grade, so,
# between DTI and Grade, Grade can be considered as a valid reflection for DTI data as well.

# # Grade Vs Sub_Grade Vs Median DTI  Vs Percentage Charged Off
# PLot22 <- ggplot() + 
#   geom_tile(data = loans %>% 
#               group_by(grade,sub_grade_num) %>% 
#               summarize(med_dti = median(dti,na.rm=TRUE)),aes(x=grade,y=sub_grade_num,fill=med_dti)) +
#   geom_text(data = (loans %>% 
#                       group_by(grade,sub_grade_num,loan_status) %>% 
#                       summarize(cnt=length(id)) %>% 
#                       mutate(ratio=paste("Charged Off =",round(cnt/sum(cnt),4)*100,"%")) %>% 
#                       filter(loan_status=="Charged Off")),
#             aes(x=grade,y=sub_grade_num,label=ratio),col="white") +
#   geom_text(data = (loans %>% 
#                       group_by(grade,sub_grade_num,loan_status) %>% 
#                       summarize(cnt=length(id)) %>% 
#                       mutate(ratio=paste("Fully Paid =",round(cnt/sum(cnt),4)*100,"%")) %>% 
#                       filter(loan_status=="Fully Paid")),
#             aes(x=grade,y=sub_grade_num,label=ratio),col="white",vjust=-1.0) +
#   geom_text(data = (loans %>% 
#                       group_by(grade,sub_grade_num) %>% 
#                       summarize(cnt=length(id)) %>%
#                       mutate(cnt2=paste("Total = ",cnt))),
#             aes(x=grade,y=sub_grade_num,label=cnt2),col="white",vjust=-2.2) +
#   labs(title="G18 - Grade Vs Sub-Grade Vs Median DTI\nWith percentage Charged off for each\nSub-Grade",
#        x="Grade",y="Sub-Grade",fill="Median DTI",label="Percentage of Charged Off Loans")
# 
# PLot22
# # G3 Grade level is a clear risk for LC #


# Influence of Grade on Interest Rate
# Note : the line signifies the mean interest rate in that sub_grade
Plot22 <- ggplot(loans) + geom_boxplot(aes(x = sub_grade, y = int_rate, fill = grade)) + 
  geom_line(data=(loans %>% group_by(sub_grade) %>% summarize(avg_dti = mean(int_rate, na.rm = TRUE))), 
            aes(x = sub_grade, y = avg_dti, group = 1)) +
  scale_y_continuous(breaks = seq(0, 25, 1)) +
  labs(title="Plot22 - Grade Vs Interest Rate", x="Sub-Grade", y="Interest Rate", fill="Grade")
Plot22

Plot23 <- ggplot(loans %>% 
                group_by(grade, sub_grade_num) %>% 
                summarize(med = median(int_rate)),
              aes(x = grade, y = sub_grade_num, value = med)) + 
  geom_tile(aes(fill = med)) +
  geom_text(aes(label = med), col = "white") +
  labs(title="Plot23 - Grade Vs Sub-Grade Vs Interest Rate", x = "Grade", y = "Sub-Grade", fill = "Median Interest Rate")
Plot23

Plot24 <- ggplot(data = loans %>% 
                group_by(grade,sub_grade_num,loan_status) %>% 
                summarize(med=median(int_rate)),
              aes(x=grade,y=sub_grade_num,value=med)) + 
  geom_tile(aes(fill=med)) +
  geom_text(aes(label=med),col="white") +
  facet_wrap(~loan_status) +
  labs(title="Plot24 - Grade Vs Sub-Grade Vs Interest Rate Vs Loan Status",x="Grade",y="Sub-Grade",fill="Median Interest Rate")
Plot24
grid.arrange(Plot23, Plot24, nrow = 2, ncol = 1)  

# There seems to be relationship between Interest Rate and the Grade, therefore,
# between interest rate and Grade, Grade can be considered as a valid reflection of interest data as well.

# Grade vs Verification Status
Plot25 <- ggplot(loans %>% group_by(verification_status, sub_grade) %>% summarize(cnt = length(sub_grade))) + 
  geom_col(aes(x = verification_status, y = cnt, fill = sub_grade), position = "fill") +
  scale_y_continuous(breaks = seq(0, 1, 0.1)) +
  labs(title="Plot25 - Grade vs Verification Status", x = "Verification Status", y="Percentage", fill="Grade")
Plot25

# Grade vs Home Ownership
Plot26 <- ggplot(loans %>% group_by(home_ownership, sub_grade) %>% summarize(cnt=length(sub_grade))) + 
  geom_col(aes(x = home_ownership, y = cnt, fill = sub_grade), position = "fill") + scale_y_continuous(breaks = seq(0, 1, 0.1)) +
  labs(title = "Plot26 - Grade vs Home Ownership", x = "Home Ownership", y = "Percentage", fill = "Grade")
Plot26
# There is no realationship between Grade and Home Ownership

