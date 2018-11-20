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
library(stringr)

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
# esquisser(CarDB, viewer = "browser")
Plot01 <- ggplot(data = CarDB) +
  aes(x = make) +
  geom_bar(fill = "#0c4c8a") +
  labs(x = "Make/Manufacture",
       y = "Count",
       caption = "Plot01 - Make") +
  geom_text(aes(label = (..count..)), stat="count", position = position_stack(vjust = 0.5)) +
  theme_minimal() +
  coord_flip()
Plot01

# fueltype
Plot02 <- ggplot(data = CarDB) +
  aes(x = fueltype) +
  geom_bar() +
  labs(x = "Fuel Type",
       y = "Count",
       caption = "Plot02 - Fule Type") +
  geom_text(aes(label = scales::percent((..count..)/sum(..count..))), stat="count", position = position_stack(vjust = 0.5)) +
  theme_minimal()
Plot02

# symboling
Plot03 <- ggplot(data = CarDB) +
  aes(x = symboling) +
  geom_bar(fill = "#0c4c8a") +
  labs(x = "Symboling",
       y = "Count",
       caption = "Plot03 - Symboling") +
  geom_text(aes(label = scales::percent((..count..)/sum(..count..))), stat="count", position = position_stack(vjust = 0.5)) +
  theme_minimal()
Plot03

# aspiration
Plot04 <- ggplot(data = CarDB) +
  aes(x = aspiration) +
  geom_bar(fill = "#0c4c8a") +
  labs(x = "Aspiration",
       y = "Count",
       caption = "Plot04 - Aspiration") +
  geom_text(aes(label = scales::percent((..count..)/sum(..count..))), stat="count", position = position_stack(vjust = 0.5)) +
  theme_minimal()
Plot04

# doornumber
Plot05 <- ggplot(data = CarDB) +
  aes(x = doornumber) +
  geom_bar(fill = "#0c4c8a") +
  labs(x = "No. of Door",
       y = "Count",
       caption = "Plot05 - No. of Door") +
  geom_text(aes(label = scales::percent((..count..)/sum(..count..))), stat="count", position = position_stack(vjust = 0.5)) +
  theme_minimal()
Plot05

# carbody
Plot06 <- ggplot(data = CarDB) +
  aes(x = carbody) +
  geom_bar(fill = "#0c4c8a") +
  labs(x = "Car Body",
       y = "Count",
       caption = "Plot06 - Car Body") +
  geom_text(aes(label = scales::percent((..count..)/sum(..count..))), stat="count", position = position_stack(vjust = 0.5)) +
  theme_minimal()
Plot06

# drivewheel
Plot07 <- ggplot(data = CarDB) +
  aes(x = drivewheel) +
  geom_bar(fill = "#0c4c8a") +
  labs(x = "Drivewheel",
       y = "Count",
       caption = "Plot07 - Drivewheel") +
  geom_text(aes(label = scales::percent((..count..)/sum(..count..))), stat="count", position = position_stack(vjust = 0.5)) +
  theme_minimal()
Plot07

# enginelocation
Plot08 <- ggplot(data = CarDB) +
  aes(x = enginelocation) +
  geom_bar(fill = "#0c4c8a") +
  labs(x = "Engine Location",
       y = "Count",
       caption = "Plot08 - Engine Location") +
  geom_text(aes(label = scales::percent((..count..)/sum(..count..))), stat="count", position = position_stack(vjust = 0.5)) +
  theme_minimal()
Plot08

# Data Preparation for modeling
# converting 2-level data to c(0,1) and to numeric data type
levels(CarDB$fueltype) <- c(0, 1) # diesel(0), gas(1)
CarDB$fueltype <- as.numeric(levels(CarDB$fueltype))[CarDB$fueltype]

levels(CarDB$aspiration) <- c(0, 1) # std(0), turbo(1)
CarDB$aspiration <- as.numeric(levels(CarDB$aspiration))[CarDB$aspiration]

levels(CarDB$doornumber) <- c(0, 1) # four(0), two (1)
CarDB$doornumber <- as.numeric(levels(CarDB$doornumber))[CarDB$doornumber]

levels(CarDB$enginelocation) <- c(0, 1) # front(0), rear(1)
CarDB$enginelocation <- as.numeric(levels(CarDB$enginelocation))[CarDB$enginelocation]

# Converting multi-level categorical variable to numeric dummy variable
head(CarDB)
# make
dummy_make <- data.frame(model.matrix( ~make, data = CarDB))
# View(dummy_make)
dummy_make <- dummy_make[,-1]

# carbody
dummy_carbody <- data.frame(model.matrix( ~carbody, data = CarDB))
# View(dummy_carbody)
dummy_carbody <- dummy_carbody[,-1]

# drivewheel
dummy_drivewheel <- data.frame(model.matrix( ~drivewheel, data = CarDB))
# View(dummy_drivewheel)
dummy_drivewheel <- dummy_drivewheel[,-1]

# enginetype
dummy_enginetype <- data.frame(model.matrix( ~enginetype, data = CarDB))
# View(dummy_enginetype)
dummy_enginetype <- dummy_enginetype[,-1]

# cylindernumber
dummy_cylindernumber <- data.frame(model.matrix( ~cylindernumber, data = CarDB))
# View(dummy_cylindernumber)
dummy_cylindernumber <- dummy_cylindernumber[,-1]

# fuelsystem
dummy_fuelsystem <- data.frame(model.matrix( ~fuelsystem, data = CarDB))
# View(dummy_fuelsystem)
dummy_fuelsystem <- dummy_fuelsystem[,-1]

head(CarDB)
length(unique(CarDB$model))
# Since model is having too many varities of data, and our dataset is too small to make any sense out of it
# we can drop model column
CarDB <- CarDB[, -26]

head(CarDB)
# removing the multi-level categorical column and adding their equivalent dummy variable to the dataset
delete <- c("make", "carbody", "drivewheel", "enginetype", "cylindernumber", "fuelsystem")
CarDB_Model <- CarDB[, !(colnames(CarDB) %in% delete), drop=FALSE]

CarDB_Model <- cbind(CarDB_Model, dummy_carbody, dummy_cylindernumber, 
                     dummy_drivewheel, dummy_enginetype, dummy_fuelsystem, dummy_make)


#### Building Model ####
# separate training and testing data
set.seed(100)
trainindices= sample(1:nrow(CarDB_Model), 0.7*nrow(CarDB_Model))
train = CarDB_Model[trainindices,]
test = CarDB_Model[-trainindices,]

# Build model 1 containing all variables
model_1 <- lm(price~., data = train)
summary(model_1)
# Residual standard error: 1436 on 83 degrees of freedom
# Multiple R-squared:  0.9819,	Adjusted R-squared:  0.9691

#Now let's use stepAIC. 
step <- stepAIC(model_1, direction = "both")
step

# Build next model base on the stpeAIC suggestion
model_2 <- lm(price ~ aspiration + enginelocation + carwidth + curbweight +
                enginesize + boreratio + stroke + horsepower + peakrpm +
                carbodyhardtop + carbodyhatchback + carbodysedan + carbodywagon + cylindernumberfive +
                cylindernumberthree + cylindernumbertwo + drivewheelrwd + enginetypedohcv + enginetypel +
                enginetypeohc + enginetypeohcf + fuelsystem2bbl + fuelsystemmpfi + makebmw +
                makebuick + makedodge + makehonda + makejaguar + makemazda +
                makemitsubishi + makenissan + makeplymouth + makerenault + makesaab +
                maketoyota + makevolkswagen, data = CarDB_Model)

summary(model_2)
# Residual standard error: 1875 on 168 degrees of freedom
# Multiple R-squared:  0.9546,	Adjusted R-squared:  0.9449
vif(model_2)

# Dropping based on the VIF and p-value - horsepower, stroke, drivewheelrwd, enginetypel, enginetypeohc, enginetypeohcf,
# fuelsystemmpfi
model_3 <- lm(price ~ aspiration + enginelocation + carwidth + curbweight +
               enginesize + boreratio + peakrpm +
               carbodyhardtop + carbodyhatchback + carbodysedan + carbodywagon + cylindernumberfive +
               cylindernumberthree + cylindernumbertwo + enginetypedohcv +
               fuelsystem2bbl + makebmw +
               makebuick + makedodge + makehonda + makejaguar + makemazda +
               makemitsubishi + makenissan + makeplymouth + makerenault + makesaab +
               maketoyota + makevolkswagen, data = CarDB_Model)

summary(model_3)
# Residual standard error: 1915 on 175 degrees of freedom
# Multiple R-squared:  0.9507,	Adjusted R-squared:  0.9425
vif(model_3)

# Dropping based on the VIF and p-value - curbweight, fuelsystem2bbl, makejaguar, makemazda, makesaab
model_4 <- lm(price ~ aspiration + enginelocation + carwidth +
                enginesize + boreratio + peakrpm +
                carbodyhardtop + carbodyhatchback + carbodysedan + carbodywagon + cylindernumberfive +
                cylindernumberthree + cylindernumbertwo + enginetypedohcv +
                makebmw +
                makebuick + makedodge + makehonda +
                makemitsubishi + makenissan + makeplymouth + makerenault +
                maketoyota + makevolkswagen, data = CarDB_Model)

summary(model_4)
# Residual standard error: 1972 on 180 degrees of freedom
# Multiple R-squared:  0.9463,	Adjusted R-squared:  0.9391
vif(model_4)

# Dropping based on the VIF and p-value - cylindernumberfive, cylindernumberthree, enginetypedohcv
model_5 <- lm(price ~ aspiration + enginelocation + carwidth +
                enginesize + boreratio + peakrpm +
                carbodyhardtop + carbodyhatchback + carbodysedan + carbodywagon +
                cylindernumbertwo +
                makebmw +
                makebuick + makedodge + makehonda +
                makemitsubishi + makenissan + makeplymouth + makerenault +
                maketoyota + makevolkswagen, data = CarDB_Model)

summary(model_5)
# Residual standard error: 2008 on 183 degrees of freedom
# Multiple R-squared:  0.9433,	Adjusted R-squared:  0.9368
vif(model_5)


# Dropping based on the VIF and p-value - boreratio, carbodyhardtop, makerenault, maketoyota, makevolkswagen
model_6 <- lm(price ~ aspiration + enginelocation + carwidth +
                enginesize + peakrpm +
                carbodyhatchback + carbodysedan + carbodywagon +
                cylindernumbertwo +
                makebmw +
                makebuick + makedodge + makehonda +
                makemitsubishi + makenissan + makeplymouth, data = CarDB_Model)

summary(model_6)
# Residual standard error: 2184 on 188 degrees of freedom
# Multiple R-squared:  0.9311,	Adjusted R-squared:  0.9252
vif(model_6)

# Dropping based on the VIF and p-value - carbodyhatchback, carbodysedan, carbodywagon, makedodge, makehonda, makeplymouth
model_7 <- lm(price ~ aspiration + enginelocation + carwidth +
                enginesize + peakrpm +
                cylindernumbertwo +
                makebmw +
                makebuick +
                makemitsubishi + makenissan, data = CarDB_Model)

summary(model_7)
# Residual standard error: 2247 on 194 degrees of freedom
# Multiple R-squared:  0.9248,	Adjusted R-squared:  0.9209
vif(model_7)

# Dropping based on the VIF and p-value - makenissan
model_8 <- lm(price ~ aspiration + enginelocation + carwidth +
                enginesize + peakrpm +
                cylindernumbertwo +
                makebmw +
                makebuick +
                makemitsubishi, data = CarDB_Model)

summary(model_8)
# Residual standard error: 2275 on 195 degrees of freedom
# Multiple R-squared:  0.9225,	Adjusted R-squared:  0.9189
vif(model_8)

# Dropping based on the VIF and p-value - enginesize
model_9 <- lm(price ~ aspiration + enginelocation + carwidth +
                peakrpm +
                cylindernumbertwo +
                makebmw +
                makebuick +
                makemitsubishi, data = CarDB_Model)

summary(model_9)
# Residual standard error: 3295 on 196 degrees of freedom
# Multiple R-squared:  0.8366,	Adjusted R-squared:  0.8299
vif(model_9)

# Dropping based on the VIF and p-value - aspiration, peakrpm, cylindernumbertwo, makemitsubishi
model_10 <- lm(price ~ enginelocation + carwidth +
                 makebmw +
                 makebuick, data = CarDB_Model)

summary(model_10)
# Residual standard error: 3313 on 200 degrees of freedom
# Multiple R-squared:  0.8314,	Adjusted R-squared:  0.828
vif(model_10)

# predicting the results for test dataset
head(test)
predict_1 <- predict(model_10, test[, -19])
test$test_price <- predict_1

# Now, we need to test the r square between actual and predicted price
r <- cor(test$price, test$test_price)
rsquared <- cor(test$price, test$test_price)^2
rsquared