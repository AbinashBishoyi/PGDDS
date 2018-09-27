# Gramener Case Study
# Abinash Bishoyi, Karthik Vecham, Balaji Nagaraja

library(ggplot2)
library(dplyr)
library(ggthemes)
library(gridExtra)
library(lubridate)
library(gridExtra)

# For map
library(maps)
states_map <- map_data("state")



# Read Data from CSV
loans <- read.csv("loan.csv", header = TRUE, stringsAsFactors = FALSE)

##### Exploring The Data ####
# Structure and summary of Loan Data sets.
str(loan)

# Categorical Variable
# Following are the Categorical variables
# Term of loan ( "36 months", "60 months" are the two possible value),
# Grade (Loan grade with 7 possible values of "A", "B", "C", "D", "E", "F", "G"),
# Employment Length ( Possible values are between 0 and 10 where 0 means less than one year and 10 means ten or more years),
# Home_ownership (Possible values are "ANY", "MORTGAGE", "NONE", "OTHER", "OWN", "RENT"),
# Verification_status (possible values are "Not Verified", "Source Verified", "Verified"),
# Loan_status (Possible values are "Fully Paid", "Charged Off", "Current", "Default", "Does not meet Credit policy status:charged off", "Does not meet Credit policy status:fully paid", "In Grace Period", "Issued", "Late(16-30) days", "Late(31-120) days"),
# pymnt_plan (Indicates if a payment plan has been put in place for the loan with possible values of "y", "n")
# Purpose (Possible values are "Car", "Credit Card", "Debt Consolidation", "Educational", "Home Improvement", "House", "Major purchases", "Medical", "Moving", "Renewable Engery", "Small Business", "Vacation", "Wedding", "Other").
# application_type ( Possible values are "INDIVIDUAL and JOINT") , But we can ignore it because more 99% records belong to Individual , only 511 records are JOINT accounts out of 887379 records.
# addr_state (Possible values are "All State names")
# Transform Appropriate Columns To Categorical Varibales
loan$term <- as.factor(loan$term)
loan$grade <- as.factor(loan$grade)
loan$emp_length <- as.factor(loan$emp_length)
loan$home_ownership <- as.factor(loan$home_ownership)
loan$verification_status <- as.factor(loan$verification_status)
# Create new factor vairable loan_status_level from loan_status  categorical variable   
loan$loan_status_level <-  factor(loan$loan_status, levels = c("Issued", "Current", "Charged Off", "Charged Off Error", "Fully Paid", "Fully Paid Error", "In Grace Period", "Late (16-30 days)", "Late (31-120 days)", "Default"),labels = c("Issued", "Current", "ChargedOff", "ChargedOffError", "FullyPaid", "FullyPaidError", "GracePeriod", "OneMonthLate", "FourMonthLate", "Default"))
loan$pymnt_plan <- as.factor(loan$pymnt_plan)
loan$purpose <- as.factor(loan$purpose)
loan$application_type <- as.factor(loan$application_type)
loan$addr_state <- as.factor(loan$addr_state)
#Cuts the annual_inc column into Four segments: Low, Middle, Upper Middle, Upper 
#loan$income_class <- cut(loan$annual_inc,4,c("Low", "Middle", "Upper Middle", "Upper"))
# loan$int_rate_class<-cut(loan$int_rate, 3, labels = c("Normal", "High", "Very High"))

#Transform datetime to Date format
loan$issue_year <- year(as.yearmon(loan$issue_d,"%b-%Y"))

ggplot(data = loans, aes(x=verification_status,fill=loans$loan_status)) + geom_bar() + theme_fivethirtyeight()

ggplot(data = loans, aes(x=reorder(purpose,purpose,
                                          function(x)-length(x)),fill=loans$loan_status)) + geom_bar() + theme_solarized()   +coord_flip() + labs(y="Count",x="Loan Purpose") 

ggplot(data = loans,aes(x=reorder(purpose,purpose,
                                         function(x)-length(x)), y=loan_amnt, fill = loans$loan_status)) + stat_summary(fun.y = "sum",geom="bar")  + theme_solarized( )+ coord_flip() + labs(y="Loan Amount",x="Loan Purpose") 


ggplot(loans, aes(x = purpose, fill = loan_status)) + geom_bar() + coord_flip()

loans$loan_status <- as.factor(loans$loan_status)


#### Data Explanation ####



#### Preparation ####
dim(loans)
# [1] 39717   111
str(loans)
summary(loans)
glimpse(loans)


columns_with_single_unique <- as.vector(which(sapply(loans, function(x) length(unique(x))==1)))

columns_with_single_unique_c <- c(which(sapply(loans, function(x) length(unique(x))==1)))
unique(loans[columns_with_single_unique])


col_with_all_NA <- as.vector(which(sapply(loan_data, function(x) (is.na(unique(x))))))

levels(as.factor(loans$loan_status))
# [1] "Charged Off" "Current"     "Fully Paid" 
levels(as.factor(loans$purpose))
# [1] "car"                "credit_card"        "debt_consolidation" "educational"        "home_improvement"  
# [6] "house"              "major_purchase"     "medical"            "moving"             "other"             
# [11] "renewable_energy"   "small_business"     "vacation"           "wedding"  

loan <- loans
loan_data <- loans















