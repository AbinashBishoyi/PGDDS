# Abinash Bishoyi
# abinash.bishoyi@gmail.com

library(tidyr)
library(dplyr)
library(car)
library(MASS)
library(ggplot2)
library(reshape2)
library(corrplot)
library(esquisse)

CarDB <- read.csv("CarPrice_Assignment.csv", header = TRUE, stringsAsFactors = FALSE)
# View(CarDB)
head(CarDB)
summary(CarDB)
str(CarDB)
dim(CarDB)

#### Data Preparation ####
CarDB_Orig <- CarDB

# Look for NAs 
sum(is.na(CarDB)) # Great! No NAs

head(CarDB)
summary(CarDB$car_ID)
# Min 1 Max 205 - It is just a unique identifier for the dataset, will not be helpful for modelling
CarDB <- CarDB[-1]

# Check for no of unique value in each column
sapply(names(CarDB), function(x) sum(!duplicated(CarDB[, x])))

# From the above result, let's look at the column where the unique values are < 10
unique(CarDB$symboling)
CarDB$symboling <- as.factor(CarDB$symboling)
str(CarDB$symboling)
summary(CarDB$symboling)
# -2 -1  0  1  2  3 
#  3 22 67 54 32 27
# Converting to numeric will casue issue, "-1" will become 1
# Assign a positive range of level
levels(CarDB$symboling) <- c(1:6)
CarDB$symboling <- as.factor(as.numeric(CarDB$symboling))
summary(CarDB$symboling)
# 1  2  3  4  5  6 
# 3 22 67 54 32 27

str(CarDB$symboling)
levels(CarDB$symboling)

unique(CarDB$fueltype)
CarDB$fueltype <- as.factor(CarDB$fueltype)

unique(CarDB$aspiration)
CarDB$aspiration <- as.factor(CarDB$aspiration)

unique(CarDB$doornumber)
CarDB$doornumber <- as.factor(CarDB$doornumber)

unique(CarDB$carbody)
CarDB$carbody <- as.factor(CarDB$carbody)

unique(CarDB$drivewheel)
CarDB$drivewheel <- as.factor(CarDB$drivewheel)

unique(CarDB$enginelocation)
CarDB$enginelocation <- as.factor(CarDB$enginelocation)

unique(CarDB$enginetype)
CarDB$enginetype <- as.factor(CarDB$enginetype)

unique(CarDB$cylindernumber)
CarDB$cylindernumber <- as.factor(CarDB$cylindernumber)

# CarName can be split into make and model
CarDB$make <- tolower(sapply(str_split(CarDB$CarName, " ", 2), head, n = 1))
CarDB$model <- tolower(sapply(str_split(CarDB$CarName, " ", 2), tail, n = 1))

unique(CarDB$make)
# Fixing few make name isue
CarDB[which(CarDB$make %in% c('maxda')), 'make'] <- 'mazda'
CarDB[which(CarDB$make %in% c('porcshce')), 'make'] <- 'porsche'
CarDB[which(CarDB$make %in% c('toyouta')), 'make'] <- 'toyota'
CarDB[which(CarDB$make %in% c('vokswagen', 'vw')), 'make'] <- 'volkswagen'
unique(CarDB$make)
CarDB$make <- as.factor(CarDB$make)

unique(CarDB$model) # Will leave it as it is
# CarName is not needed  anymore
head(CarDB, n=1)
CarDB <- CarDB[,-2]

##### Univariate Analysis ####
# make
# esquisser(CarDB)
Plot01 <- ggplot(data = CarDB) +
  aes(x = make) +
  geom_bar(fill = "#0c4c8a") +
  labs(x = "Make/Manufacture",
       y = "Count",
       caption = "Plot01 - Make") +
  theme_minimal() +
  coord_flip()
Plot01

# fueltype
ggplot(data = CarDB) +
  aes(x = fueltype, fill = fueltype) +
  geom_bar() +
  labs(x = "Fuel Type",
       y = "%",
       caption = "Plot02 - Fule Type") +
  geom_text(aes(label = scales::percent((..count..)/sum(..count..))), stat="count", position=position_stack(vjust = 0.5)) +
  theme_minimal()


Plot01 <- ggplot(loans, aes(x = loan_status, fill = loan_status)) + 
  geom_bar() + labs(title = "Plot01 - Loan Status", x = "Loan Status", y= "Percentage", fill = "Loan Staus") +
  geom_text(aes(label = scales::percent((..count..)/sum(..count..))), stat="count", position=position_stack(vjust = 0.5))
Plot01


