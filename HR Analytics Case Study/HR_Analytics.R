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

# head(employee_survey_data)
# str(employee_survey_data)
# 
# head(general_data)
# str(general_data)
# 
# head(in_time)
# str(in_time)
# 
# head(manager_survey_data)
# str(manager_survey_data)
# 
# head(out_time)
# str(out_time)

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


#################
#################
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
office_time.final <- full_join(office_time.final, office_time.summarised %>% group_by(EmployeeID) %>% distinct(EmployeeID, YearlyLeaves, YearlyAvgUtil), by = "EmployeeID")

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
View(general_data[which(is.na(general_data$NumCompaniesWorked)), ])
# We can't substitute NAs for NumCompaniesWorked, as based on the current dataset we can't derived any metrics for the same

View(general_data[which(is.na(general_data$TotalWorkingYears)), ])
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

sum(is.na(employee_database))
sapply(employee_database, function(x) sum(is.na(x)))
