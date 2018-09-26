library(ggplot2)       #: For plotting functions
library(dplyr)         #: For data manipulation  
library(ggthemes)      #: For a variety of plot-themes
library(gridExtra)     #: Arranging ggplots in a grid 
library(data.table)    #
library(readr)        ?#: For read file through read_csv() function
library(lubridate)          # For date manipulation
library(zoo)
library(plyr)
library(gridExtra)
library(maps) # For map data
library(vcd)
states_map <- map_data("state")



# Read Data from CSV
loanClubData <-?read.csv("loan.csv", header = TRUE, stringsAsFactors = FALSE)

##### Exploring The Data ####
# Structure and summary of Loan Data sets.
str(loanClubData)

# Categorical Variable
# Following are the Categorical variables
# Term of loan ( "36 months", "60 mo?ths" are the two possible value),
# Grade (Loan grade with 7 possible values of "A", "B", "C", "D", "E", "F", "G"),
# Employment Length ( Possible values are between 0 and 10 where 0 means less than one year and 10 means ten or more years),
# Home_ownershi? (Possible values are "ANY", "MORTGAGE", "NONE", "OTHER", "OWN", "RENT"),
# Verification_status (possible values are "Not Verified", "Source Verified", "Verified"),
# Loan_status (Possible values are "Fully Paid", "Charged Off", "Current", "Default", "Does?not meet Credit policy status:charged off", "Does not meet Credit policy status:fully paid", "In Grace Period", "Issued", "Late(16-30) days", "Late(31-120) days"),
# pymnt_plan (Indicates if a payment plan has been put in place for the loan with possible v?lues of "y", "n")
# Purpose (Possible values are "Car", "Credit Card", "Debt Consolidation", "Educational", "Home Improvement", "House", "Major purchases", "Medical", "Moving", "Renewable Engery", "Small Business", "Vacation", "Wedding", "Other").
# applic?tion_type ( Possible values are "INDIVIDUAL and JOINT") , But we can ignore it because more 99% records belong to Individual , only 511 records are JOINT accounts out of 887379 records.
# addr_state (Possible values are "All State names")
# Transform Appro?riate Columns To Categorical Varibales
loanClubData$term <- as.factor(loanClubData$term)
loanClubData$grade <- as.factor(loanClubData$grade)
loanClubData$emp_length <- as.factor(loanClubData$emp_length)
loanClubData$home_ownership <- as.factor(loanClubData?home_ownership)
loanClubData$verification_status <- as.factor(loanClubData$verification_status)
# Create new factor vairable loan_status_level from loan_status  categorical variable   
loanClubData$loan_status_level <-  factor(loanClubData$loan_status, lev?ls = c("Issued", "Current", "Charged Off", "Charged Off Error", "Fully Paid", "Fully Paid Error", "In Grace Period", "Late (16-30 days)", "Late (31-120 days)", "Default"),labels = c("Issued", "Current", "ChargedOff", "ChargedOffError", "FullyPaid", "FullyP?idError", "GracePeriod", "OneMonthLate", "FourMonthLate", "Default"))
loanClubData$pymnt_plan <- as.factor(loanClubData$pymnt_plan)
loanClubData$purpose <- as.factor(loanClubData$purpose)
loanClubData$application_type <- as.factor(loanClubData$application_?ype)
loanClubData$addr_state <- as.factor(loanClubData$addr_state)
#Cuts the annual_inc column into Four segments: Low, Middle, Upper Middle, Upper 
#loanClubData$income_class <- cut(loanClubData$annual_inc,4,c("Low", "Middle", "Upper Middle", "Upper"))
# ?oanClubData$int_rate_class<-cut(loanClubData$int_rate, 3, labels = c("Normal", "High", "Very High"))

#Transform datetime to Date format
loanClubData$issue_year <- year(as.yearmon(loanClubData$issue_d,"%b-%Y"))

ggplot(data = loanClubData, aes(x=verificati?n_status,fill=term)) + geom_bar() + theme_fivethirtyeight()

ggplot(data = loanClubData, aes(x=reorder(purpose,purpose,
                                          function(x)-length(x)),fill=term)) + geom_bar() + theme_solarized()   +coord_flip() + labs(y="?ount",x="Loan Purpose") 

ggplot(data = loanClubData,aes(x=reorder(purpose,purpose,
                                         function(x)-length(x)), y=loan_amnt, fill = term)) + stat_summary(fun.y = "sum",geom="bar")  + theme_solarized( )+ coord_flip() + l?bs(y="Loan Amount",x="Loan Purpose") 

