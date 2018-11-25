#############################HR Analytics Case Study#################
# Submission by : Abinash Bishoyi, Balaji Nagaraja, Karthik Vecham
#####################################################################

### Business Understanding:

# Based on the past and current employee information,
# the company has maintained a database containing employee job satisfaction survey, manager/appriasal feedback  employee personal/demographic information,
# on their current/last role with company, education, distance from home, salary hike, number of companies they have worked with.

## AIM:

# The aim is to automate the process of predicting 
# if a employee would leave the company or not and to find the factors affecting the employee attrition 
# Whether a employee would leave the company or not will depend on data from the following three buckets:

# 1. Demographic Information -> general_data.csv 
# 2. Employee Job Satisfaction
# 3. Current Salary

################################################################

library(tidyr)
library(purrr)
library(MASS)
library(car)
library(e1071)
library(caret)
library(ggplot2)
library(cowplot)
library(caTools)
library(reshape)
library(reshape2)
library(magrittr)
library(lubridate)
library(dplyr)
library(ROCR)

## Load csv files
employee_survey_data <- read.csv("employee_survey_data.csv", header = TRUE, stringsAsFactors = FALSE)
general_data <- read.csv("general_data.csv", header = TRUE, stringsAsFactors = FALSE)
in_time <- read.csv("in_time.csv", header = TRUE, stringsAsFactors = FALSE)
manager_survey_data <- read.csv("manager_survey_data.csv", header = TRUE, stringsAsFactors = FALSE)
out_time <- read.csv("out_time.csv", header = TRUE, stringsAsFactors = FALSE)

#high_level overview of data at hand

str(general_data) #4410 obs. of  24 variables  #demographic data
str(employee_survey_data) #4410 obs. of  4 variables #employee job satisfcation
str(manager_survey_data)  #4410 obs. of  3 variables #Appraisal Data
str(in_time)  #4410 obs. of  262 variables # 1 year data of hours put in at work
str(out_time) #4410 obs. of  262 variables # 1 year data of hours put in at work

#high_level overview of data at hand shows equal number of observations.
#EmployeeID as column name for column 1  in in_time and out_time is missing to be fixed when cleaning data
#in_time and out_time columns where majority or all is NA to be considered as general holiday
# "Relationship Satisfaction" not found in any datasets https://learn.upgrad.com/v/course/163/question/95851

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
sapply(in_time, function(x) sum(is.na(x)))
sapply(out_time, function(x) sum(is.na(x)))

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

# str(in_time)
# str(out_time)

#### Derived Metrics ####
# Time Spent at Office
office_time <- out_time[ , 2:250] - in_time[ , 2:250]
office_time$EmployeeID <- in_time$EmployeeID

# Convert to long format
# ncol(office_time[,-1])
office_time.long <- gather(office_time, key = Date, values = c(1:ncol(office_time[,-1])))
dim(office_time.long)
# length(unique(office_time$EmployeeID))
# 249 * 4410, new columns counts looks good
# str(office_time)
# Rename value to Hours
colnames(office_time.long)[3] <- "Hours"
# Fix datatype for Date column
office_time.long$Date <- ymd(office_time.long$Date)
office_time.long$Hours <- as.double(office_time.long$Hours)
str(office_time.long)

# Derived Metrics - Add Month Column
# office_time.long$Month <- month.abb[month(office_time.long$Date)]
office_time.long$Month <- month(office_time.long$Date)

# write.csv(office_time.long, "office_time.csv")

# Summarise Monthly Hours and Leaves(NAs), WorkingDays
office_time.summarised <- office_time.long %>% group_by(EmployeeID, Month) %>%
                            mutate(MonthlyHours = sum(Hours, na.rm = TRUE), Leaves = sum(is.na(Hours))) %>%
                            distinct(EmployeeID, Month, MonthlyHours, Leaves) %>% arrange(EmployeeID, Month)

Days <- as.data.frame(office_time.long %>% group_by(EmployeeID, Month) %>% mutate(WorkingDays = length(Hours)) %>% 
          distinct(EmployeeID, Month, WorkingDays) %>% arrange(EmployeeID, Month))

office_time.summarised$WorkingDays <- Days$WorkingDays

# Assuming standard 8 hr/day, we can caluluate the Monthly Utilization
office_time.summarised <- office_time.summarised %>% group_by(EmployeeID, Month) %>% 
                            mutate(MonthlyUtil = MonthlyHours/(WorkingDays*8))

# We can add Yearly calucation
office_time.summarised <- office_time.summarised %>% group_by(EmployeeID) %>% 
                            mutate(YearlyHours = sum(MonthlyHours))

office_time.summarised <- office_time.summarised %>% group_by(EmployeeID) %>%
                            mutate(YearlyLeaves = sum(Leaves))

office_time.summarised <- office_time.summarised %>% group_by(EmployeeID) %>% 
                            mutate(YearlyAvgUtil = mean(MonthlyUtil))

# Converting Month to month.abb
office_time.summarised$Month <- month.abb[office_time.summarised$Month]
head(office_time.summarised)

# Converting office_time.summarised to wide format so that it can be merged with other dataset
# Dropping MonthlyHours, Leaves, YearlyHours in the new dataset
office_time.final <- spread(office_time.summarised[, c(1,2,6)], key = Month, value = MonthlyUtil)
office_time.final <- full_join(office_time.final, office_time.summarised %>% group_by(EmployeeID) %>%
                                 distinct(EmployeeID, YearlyLeaves, YearlyAvgUtil), by = "EmployeeID")

# We will use only Yerly data for the Modeling, we can drop monthly data
office_time.final <- office_time.final %>% dplyr::select(-c(month.abb[1:12]))

# Now, let's check employee_survey_data
sum(is.na(employee_survey_data))
map(employee_survey_data, ~ sum(is.na(.)))

# Based on the Data Dicitinary, these are all categorical variables
# So we can't get rid of NAs by substitution, since the count is very low we will leave them for now
# Once we will merge all the dataset, we can decide what to do with these NAs

# Let's check general_data
sum(is.na(general_data))
map(general_data, ~ sum(is.na(.)))
sapply(general_data, function(x) sum(is.na(x)))
# View(general_data[which(is.na(general_data$NumCompaniesWorked)), ])
# We can't substitute NAs for NumCompaniesWorked, as based on the current dataset we can't derived any metrics for the same

# View(general_data[which(is.na(general_data$TotalWorkingYears)), ])
# From current Age and TotalWorkingYears, we can deduct at what age someone started working
# It can help in sustituting NAs for TotalWorkingYears
general_data %>% filter(!is.na(TotalWorkingYears)) %>% summarise(mean(Age - TotalWorkingYears))
# 25.64054 ~ 26

# Substitue NAs
general_data[which(is.na(general_data$TotalWorkingYears)), "TotalWorkingYears"] <- general_data[which(is.na(general_data$TotalWorkingYears)), "Age"] - 26

# Let's check manager_survey_data
sum(is.na(manager_survey_data))
head(manager_survey_data)

# Now we can merge differnet dataset
employee_database <- full_join(employee_survey_data, general_data, by = "EmployeeID")
employee_database <- full_join(employee_database, manager_survey_data, by = "EmployeeID")
employee_database <- full_join(employee_database, office_time.final, by = "EmployeeID")

dim(employee_database)
str(employee_database)

# Follwoing are categorical variables based on the data dict
factor_cols <- c("Attrition", "BusinessTravel", "Department", "EducationField", "Gender", 
                 "JobRole", "MaritalStatus", "Over18")
employee_database[factor_cols] <- lapply(employee_database[factor_cols], factor)
sapply(employee_database, class)

# Let's check if there is any column which have the same data - no variance
novariance_cols <- colnames(employee_database)[which(sapply(employee_database, function(x) length(unique(x)) == 1))]
novariance_cols
# [1] "EmployeeCount" "Over18"        "StandardHours"
# We can remove these column, as they won't help in modelling
employee_database <- employee_database %>% dplyr::select(-c(novariance_cols))

# Let'e check for NAs
sum(is.na(employee_database))
sapply(employee_database, function(x) sum(is.na(x)))
colMeans(is.na(employee_database))

# Since the NAs population is too small, we can drop them
employee_database <- na.omit(employee_database)
dim(employee_database)

##### Univariate and Bivariate Analysis ####
## Outlier Treatment
# Continuous Variables 
con_vars <- c("EmployeeID", "MonthlyIncome", "Age", "DistanceFromHome", "PercentSalaryHike", 
              "TotalWorkingYears", "YearsAtCompany", "YearsWithCurrManager", "YearsSinceLastPromotion", 
              "YearlyLeaves","YearlyAvgUtil")

# Outlier check with box plot
# New copy for the modeling
emp_db <- employee_database
melt(data = emp_db[con_vars], id.vars = "EmployeeID") %>%  ggplot(aes(x = variable, y = value)) +
  geom_boxplot() +  facet_wrap(~variable, scales = "free")

# Variables with outliers are MonthlyIncome, TotalWorkingYears, YearsAtCompany, YearsWithCurrManager, 
# YearsSinceLastPromotion, YearlyAvgUtil
remove_outliers <- function(x) {
  qnt <- quantile(x, probs = c(.25, .75), na.rm = TRUE)
  caps <- quantile(x, probs = c(.05, .95), na.rm = TRUE)
  H <- 1.5 * IQR(x, na.rm = TRUE)
  y <- x
  y[x < (qnt[1] - H)] <- caps[1]
  y[x > (qnt[2] + H)] <- caps[2]
  y
}

emp_db$MonthlyIncome <- remove_outliers(emp_db$MonthlyIncome)
emp_db$TotalWorkingYears <- remove_outliers(emp_db$TotalWorkingYears)
emp_db$YearsAtCompany <- remove_outliers(emp_db$YearsAtCompany)
emp_db$YearsWithCurrManager <- remove_outliers(emp_db$YearsWithCurrManager)
emp_db$YearsSinceLastPromotion <- remove_outliers(emp_db$YearsSinceLastPromotion)
emp_db$YearlyAvgUtil <- remove_outliers(emp_db$YearlyAvgUtil)

# Post Outlier Treatment
melt(data = emp_db[con_vars], id.vars = "EmployeeID") %>%  ggplot(aes(x = variable, y = value)) +
  geom_boxplot() +  facet_wrap(~variable, scales = "free")

##### Model Preparation ####
str(emp_db)
categorial_cols <- c(6, 7, 8, 11, 12, 14, 15)
continuous_cols <- c(2:ncol(emp_db))[which(!c(2:ncol(emp_db)) %in% categorial_cols)]

# Scale all numerical variables
emp_db[, continuous_cols] <- data.frame(sapply(emp_db[, continuous_cols], function(x) scale(x)))

# Looks for factor variable with 2 levels
colnames(employee_database)[which(sapply(employee_database, function(x) length(unique(x)) == 2))]
# [1] "Attrition"         "Gender"            "PerformanceRating"
# converting 2-level data to c(0, 1) and to numeric data type
levels(emp_db$Attrition) <- c(0, 1) # No(0), Yes(1)
# emp_db$Attrition <- as.numeric(levels(emp_db$Attrition))[emp_db$Attrition]

levels(emp_db$Gender) <- c(0, 1) # Female(0), Male(1)
# emp_db$Gender <- as.numeric(levels(emp_db$Gender))[emp_db$Gender]

sapply(emp_db[categorial_cols], class)
sapply(emp_db[continuous_cols], class)

# Excluding Attritaion and Gender
categorial_cols <- c(7, 8, 11, 14, 15)

# Converting multi-level categorical variable to numeric dummy variable
dummies <- as.data.frame(sapply(emp_db[, categorial_cols], 
                  function(x) data.frame(model.matrix(~x, data = emp_db[, categorial_cols]))))
# colnames(dummies)
dummies <- dummies %>% dplyr::select(-c("BusinessTravel.X.Intercept.", "Department.X.Intercept.", 
                                        "EducationField.X.Intercept.", "JobRole.X.Intercept.", 
                                        "MaritalStatus.X.Intercept."))
emp_db <- cbind(emp_db %>% dplyr::select(-c("BusinessTravel", "Department", "EducationField", 
                                            "JobRole", "MaritalStatus")), dummies)

# We don't need EmployeeId for the Modeling
emp_db <- emp_db %>% dplyr::select(-EmployeeID)

#### Building Model ####
# separate training and testing data
set.seed(100)

trainindices = sample(1:nrow(emp_db), 0.7 * nrow(emp_db))
train = emp_db[trainindices, ]
test = emp_db[-trainindices, ]

# Build model 1 containing all variables
model_1 = glm(Attrition ~ ., data = train, family = "binomial")
summary(model_1)

#Now let's use stepAIC. 
model_2 <- stepAIC(model_1, direction = "both")
summary(model_2)
sort(vif(model_2), decreasing = TRUE)

# Removing Department.xResearch...Development
model_3 <- glm(formula = Attrition ~ EnvironmentSatisfaction + JobSatisfaction + 
                 WorkLifeBalance + Age + Education + Gender + MonthlyIncome + 
                 NumCompaniesWorked + TotalWorkingYears + TrainingTimesLastYear + 
                 YearsSinceLastPromotion + YearsWithCurrManager + JobInvolvement + 
                 YearlyLeaves + YearlyAvgUtil + BusinessTravel.xTravel_Frequently + 
                 BusinessTravel.xTravel_Rarely +  
                 Department.xSales + EducationField.xTechnical.Degree + JobRole.xManufacturing.Director + 
                 JobRole.xResearch.Director + JobRole.xResearch.Scientist + 
                 JobRole.xSales.Executive + MaritalStatus.xSingle, family = "binomial", 
               data = train)

summary(model_3)
sort(vif(model_3), decreasing = TRUE)

# Removing BusinessTravel.xTravel_Rarely
model_4 <- glm(formula = Attrition ~ EnvironmentSatisfaction + JobSatisfaction + 
                 WorkLifeBalance + Age + Education + Gender + MonthlyIncome + 
                 NumCompaniesWorked + TotalWorkingYears + TrainingTimesLastYear + 
                 YearsSinceLastPromotion + YearsWithCurrManager + JobInvolvement + 
                 YearlyLeaves + YearlyAvgUtil + BusinessTravel.xTravel_Frequently +
                 Department.xSales + EducationField.xTechnical.Degree + JobRole.xManufacturing.Director + 
                 JobRole.xResearch.Director + JobRole.xResearch.Scientist + 
                 JobRole.xSales.Executive + MaritalStatus.xSingle, family = "binomial", 
               data = train)

summary(model_4)
sort(vif(model_4), decreasing = TRUE)
# VIF ~ 2, we need to chk for high p-value

# Removing Department.xSales
model_5 <- glm(formula = Attrition ~ EnvironmentSatisfaction + JobSatisfaction + 
                 WorkLifeBalance + Age + Education + Gender + MonthlyIncome + 
                 NumCompaniesWorked + TotalWorkingYears + TrainingTimesLastYear + 
                 YearsSinceLastPromotion + YearsWithCurrManager + JobInvolvement + 
                 YearlyLeaves + YearlyAvgUtil + BusinessTravel.xTravel_Frequently +
                 EducationField.xTechnical.Degree + JobRole.xManufacturing.Director + 
                 JobRole.xResearch.Director + JobRole.xResearch.Scientist + 
                 JobRole.xSales.Executive + MaritalStatus.xSingle, family = "binomial", 
               data = train)

summary(model_5)
sort(vif(model_5), decreasing = TRUE)
# VIF ~ 2, we need to chk for high p-value

# Removing Gender
model_6 <- glm(formula = Attrition ~ EnvironmentSatisfaction + JobSatisfaction + 
                 WorkLifeBalance + Age + Education + MonthlyIncome + 
                 NumCompaniesWorked + TotalWorkingYears + TrainingTimesLastYear + 
                 YearsSinceLastPromotion + YearsWithCurrManager + JobInvolvement + 
                 YearlyLeaves + YearlyAvgUtil + BusinessTravel.xTravel_Frequently +
                 EducationField.xTechnical.Degree + JobRole.xManufacturing.Director + 
                 JobRole.xResearch.Director + JobRole.xResearch.Scientist + 
                 JobRole.xSales.Executive + MaritalStatus.xSingle, family = "binomial", 
               data = train)

summary(model_6)
sort(vif(model_6), decreasing = TRUE)

# Removing YearlyLeaves
model_7 <- glm(formula = Attrition ~ EnvironmentSatisfaction + JobSatisfaction + 
                 WorkLifeBalance + Age + Education + MonthlyIncome + 
                 NumCompaniesWorked + TotalWorkingYears + TrainingTimesLastYear + 
                 YearsSinceLastPromotion + YearsWithCurrManager + JobInvolvement + 
                 YearlyAvgUtil + BusinessTravel.xTravel_Frequently +
                 EducationField.xTechnical.Degree + JobRole.xManufacturing.Director + 
                 JobRole.xResearch.Director + JobRole.xResearch.Scientist + 
                 JobRole.xSales.Executive + MaritalStatus.xSingle, family = "binomial", 
               data = train)

summary(model_7)
sort(vif(model_7), decreasing = TRUE)

# Removing JobRole.xResearch.Scientist
model_8 <- glm(formula = Attrition ~ EnvironmentSatisfaction + JobSatisfaction + 
                 WorkLifeBalance + Age + Education + MonthlyIncome + 
                 NumCompaniesWorked + TotalWorkingYears + TrainingTimesLastYear + 
                 YearsSinceLastPromotion + YearsWithCurrManager + JobInvolvement + 
                 YearlyAvgUtil + BusinessTravel.xTravel_Frequently +
                 EducationField.xTechnical.Degree + JobRole.xManufacturing.Director + 
                 JobRole.xResearch.Director +
                 JobRole.xSales.Executive + MaritalStatus.xSingle, family = "binomial", 
               data = train)

summary(model_8)
sort(vif(model_8), decreasing = TRUE)

# Removing Education
model_9 <- glm(formula = Attrition ~ EnvironmentSatisfaction + JobSatisfaction + 
                 WorkLifeBalance + Age + MonthlyIncome + 
                 NumCompaniesWorked + TotalWorkingYears + TrainingTimesLastYear + 
                 YearsSinceLastPromotion + YearsWithCurrManager + JobInvolvement + 
                 YearlyAvgUtil + BusinessTravel.xTravel_Frequently +
                 EducationField.xTechnical.Degree + JobRole.xManufacturing.Director + 
                 JobRole.xResearch.Director +
                 JobRole.xSales.Executive + MaritalStatus.xSingle, family = "binomial", 
               data = train)

summary(model_9)
sort(vif(model_9), decreasing = TRUE)

# Removing JobInvolvement
model_10 <- glm(formula = Attrition ~ EnvironmentSatisfaction + JobSatisfaction + 
                 WorkLifeBalance + Age + MonthlyIncome + 
                 NumCompaniesWorked + TotalWorkingYears + TrainingTimesLastYear + 
                 YearsSinceLastPromotion + YearsWithCurrManager + 
                 YearlyAvgUtil + BusinessTravel.xTravel_Frequently +
                 EducationField.xTechnical.Degree + JobRole.xManufacturing.Director + 
                 JobRole.xResearch.Director +
                 JobRole.xSales.Executive + MaritalStatus.xSingle, family = "binomial", 
               data = train)

summary(model_10)
sort(vif(model_10), decreasing = TRUE)

# Removing MonthlyIncome
model_11 <- glm(formula = Attrition ~ EnvironmentSatisfaction + JobSatisfaction + 
                  WorkLifeBalance + Age + 
                  NumCompaniesWorked + TotalWorkingYears + TrainingTimesLastYear + 
                  YearsSinceLastPromotion + YearsWithCurrManager + 
                  YearlyAvgUtil + BusinessTravel.xTravel_Frequently +
                  EducationField.xTechnical.Degree + JobRole.xManufacturing.Director + 
                  JobRole.xResearch.Director +
                  JobRole.xSales.Executive + MaritalStatus.xSingle, family = "binomial", 
                data = train)

summary(model_11)
sort(vif(model_11), decreasing = TRUE)

# Removing EducationField.xTechnical.Degree
model_12 <- glm(formula = Attrition ~ EnvironmentSatisfaction + JobSatisfaction + 
                  WorkLifeBalance + Age + NumCompaniesWorked + TotalWorkingYears + 
                  TrainingTimesLastYear + YearsSinceLastPromotion + YearsWithCurrManager + 
                  YearlyAvgUtil + BusinessTravel.xTravel_Frequently +
                  JobRole.xManufacturing.Director + JobRole.xResearch.Director + 
                  JobRole.xSales.Executive + MaritalStatus.xSingle, family = "binomial", 
                data = train)

summary(model_12)
sort(vif(model_12), decreasing = TRUE)

# Removing JobRole.xSales.Executive
model_13 <- glm(formula = Attrition ~ EnvironmentSatisfaction + JobSatisfaction + 
                  WorkLifeBalance + Age + NumCompaniesWorked + TotalWorkingYears + 
                  TrainingTimesLastYear + YearsSinceLastPromotion + YearsWithCurrManager + 
                  YearlyAvgUtil + BusinessTravel.xTravel_Frequently +
                  JobRole.xManufacturing.Director + JobRole.xResearch.Director + 
                  MaritalStatus.xSingle, family = "binomial", 
                data = train)

summary(model_13)
sort(vif(model_13), decreasing = TRUE)

# Removing JobRole.xResearch.Director
model_14 <- glm(formula = Attrition ~ EnvironmentSatisfaction + JobSatisfaction + 
                  WorkLifeBalance + Age + NumCompaniesWorked + TotalWorkingYears + 
                  TrainingTimesLastYear + YearsSinceLastPromotion + YearsWithCurrManager + 
                  YearlyAvgUtil + BusinessTravel.xTravel_Frequently +
                  JobRole.xManufacturing.Director +
                  MaritalStatus.xSingle, family = "binomial", 
                data = train)

summary(model_14)
sort(vif(model_14), decreasing = TRUE)

# Removing JobRole.xManufacturing.Director
model_15 <- glm(formula = Attrition ~ EnvironmentSatisfaction + JobSatisfaction + 
                  WorkLifeBalance + Age + NumCompaniesWorked + TotalWorkingYears + 
                  TrainingTimesLastYear + YearsSinceLastPromotion + YearsWithCurrManager + 
                  YearlyAvgUtil + BusinessTravel.xTravel_Frequently +
                  MaritalStatus.xSingle, family = "binomial", data = train)

summary(model_15)
sort(vif(model_15), decreasing = TRUE)

# Model looks stable
final.model <- model_15

#### Model Evalution #####
# Predict Attrition for test data
test$AttritionPredict <- predict(final.model, 
                                     type = "response", 
                                     newdata = test %>% dplyr::select(-Attrition))
# Minimum value of Prediction Probability
min(test$AttritionPredict)
# [1] 0.000428976

# Maximum value of Prediction Probability
max(test$AttritionPredict)
# [1] 0.8829905

# Let's use the Prediction Probability cutoff of 60%
predict_attrition <- factor(ifelse(test$AttritionPredict >= 0.60, "Yes", "No"))
actual_attrition <- factor(ifelse(test$Attrition == 1, "Yes", "No"))

conf_Matrix <- confusionMatrix(predict_attrition, actual_attrition, positive = "Yes")
conf_Matrix

# Sensitivity is very low
# Let's find out the optimal probalility cutoff 

determine_cutoff <- function(cutoff) 
{
  predict <- factor(ifelse(test$AttritionPredict >= cutoff, "Yes", "No"))
  conf <- confusionMatrix(predict, actual_attrition, positive = "Yes")
  Accuracy <- conf$overall[1]
  Sensitivity <- conf$byClass[1]
  Specificity <- conf$byClass[2]
  t(as.matrix(c(Sensitivity, Specificity, Accuracy))) 
}

# Testing for  cutoff values from 0.01 to 0.90
s = seq(0.01, 0.90, length=100)
result = matrix(0, 100, 3)
for(i in 1:100)
{
  result[i, ] = determine_cutoff(s[i])
} 

# Plot the Cutoff values
plot(s, result[, 1], xlab = "Cutoff", ylab = "Value", cex.lab = 1.5,
     cex.axis=1.5, ylim=c(0,1), type="l", lwd = 2, axes = FALSE, col = 2)
axis(1, seq(0, 1, length=5), seq(0, 1, length = 5), cex.lab = 1.5)
axis(2, seq(0, 1, length=5), seq(0, 1, length = 5), cex.lab = 1.5)
lines(s, result[,2], col = "Green", lwd = 2)
lines(s, result[,3], col = 4, lwd = 2)
box()
legend(0, 0.50, col = c(2,"Green", 4, "Red"), lwd = c(2, 2, 2, 2), c("Sensitivity","Specificity","Accuracy"))

cutoff <- s[which(abs(result[, 1] - result[, 2]) < 0.01)]
cutoff

# Let's use cutoff value of 0.172 got from the plot
predict_attrition <- factor(ifelse(test$AttritionPredict >= 0.172, "Yes", "No"))
conf_Matrix.final <- confusionMatrix(predict_attrition, actual_attrition, positive = "Yes")
conf_Matrix.final

predict_attrition <- ifelse(predict_attrition == "Yes", 1, 0)
actual_attrition <- ifelse(actual_attrition == "Yes", 1, 0)

## KS Statistics
pred_test<- prediction(predict_attrition, actual_attrition)
performance_measures_test<- performance(pred_test, "tpr", "fpr")

ks_table_test <- attr(performance_measures_test, "y.values")[[1]] - (attr(performance_measures_test, "x.values")[[1]])

max(ks_table_test)


# Lift & Gain Chart 
lift <- function(labels, predicted_prob, groups = 10) {
  if(is.factor(labels)) labels  <- as.integer(as.character(labels ))
  if(is.factor(predicted_prob)) predicted_prob <- as.integer(as.character(predicted_prob))
  helper = data.frame(cbind(labels, predicted_prob))
  helper[, "bucket"] = ntile(-helper[, "predicted_prob"], groups)
  gaintable = helper %>% group_by(bucket)  %>%
                summarise_at(vars(labels), funs(total = n(), totalresp=sum(., na.rm = TRUE))) %>%
                mutate(Cumresp = cumsum(totalresp),
                       Gain=Cumresp/sum(totalresp)*100, 
                       Cumlift=Gain/(bucket*(100/groups))) 
  return(gaintable)
}

Attrition_decile = lift(actual_attrition, test$AttritionPredict, groups = 10)
Attrition_decile

Gain <- c(0, Attrition_decile$Gain)
Deciles <- c(0, Attrition_decile$bucket)

# Gain Chart
plot(y = Gain, x = Deciles, type = "l", lwd = 2, xlab = "Bucket", ylab = "Gain", main = "Gain Chart")
Random_Gain <- seq(from = 0, to = 100, by = 10)
lines(y = Random_Gain, x = Deciles, type = "l", lwd = 2, col = "Red")
Perfect_Gain <- vector(mode = "numeric", length = 11)
for (i in 2:11) {
  Perfect_Gain[i] <- 100 * min(1, 129 * (i - 1)/209)
}
lines(y = Perfect_Gain, x = Deciles, type = "l", lwd = 2, col = "Green")
legend("bottomright", col= c("Green", "Black", "Red"), lwd = c(2, 2, 2, 2), 
       c("Perfect Model", "Actual Model", "Random Model"), cex = 0.7)

# Lift chart
Lift <- Gain/Random_Gain
Random_Lift <- Random_Gain/Random_Gain

plot(y = Lift, x = Deciles, type = "l", ylim = c(0, 3.5), lwd = 2, 
     xlab = "Bucket", ylab = "Lift", main = "Lift Chart", ylim <- c())
lines(y = Random_Lift, x = Deciles, type = "l", lwd = 2, col = "Red")
legend("topright", col = c("Black", "Red"), lwd = c(2, 2, 2), c("Actual Model", "Random Model"), cex = 0.7)
