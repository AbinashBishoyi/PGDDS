##################### Uber Supply-Demand Gap ##########################
## Abinash Bishoyi
## abinash.bishoyi@gmail.com

library(dplyr)
library(tidyr)
library(lubridate)
library(grid)
library(gridExtra)
library(ggplot2)

options(warn=0)
## Few part of my code will throw warning, which I need for my data transformation stratergy
## If you want to avoid those warning message please uncomment below line
# options(warn=-1)

## Input Data
Uber <- read.csv("Uber Request Data.csv", header = TRUE, stringsAsFactors = FALSE)

## Data Understanding
# Request id: A unique identifier of the request
# Time of request: The date and time at which the customer made the trip request
# Drop-off time: The drop-off date and time, in case the trip was completed 
# Pick-up point: The point from which the request was made
# Driver id: The unique identification number of the driver
# Status of the request: The final status of the trip, that can be either completed, cancelled by the driver or no cars available
head(Uber)
str(Uber)
summary(Uber)
# glimpse(Uber)

# ## Status of the request can be treated as factor
# Uber$Status <- as.factor(Uber$Status)
# levels(Uber$Status)
# 
# ## Driver ID can be treated as factor
# Uber$Driver.id <- as.factor(Uber$Driver.id)
# levels(Uber$Driver.id)


## Total number of NAs - 6564
sum(is.na(Uber))

## No. of NAs in each column - https://sebastiansauer.github.io/sum-isna/
Uber %>% select(everything()) %>% summarise_all(funs(sum(is.na(.))))
# Request.id Pickup.point Driver.id Status Request.timestamp Drop.timestamp
#          0            0      2650      0                 0           3914

## Check for blanks/whitespace in character type column
# Uber[sapply(Uber, is.character)] %>% select(everything()) %>% summarise_all(funs(sum(. == "City" || . == " "))) -- wrong implementation
char_col <- names(Uber[sapply(Uber, is.character)])
sapply(char_col, function(x) sum(Uber[, x] == "" || Uber[, x] == " "))
# Pickup.point Status Request.timestamp Drop.timestamp
#            0      0                 0              0

## Let's check few of the rows contains NA
head(Uber[which(is.na(Uber$Driver.id)),], n=10)
tail(Uber[which(is.na(Uber$Driver.id)),], n=10)
head(Uber[which(is.na(Uber$Drop.timestamp)),], n=10)
tail(Uber[which(is.na(Uber$Drop.timestamp)),], n=10)
## Based on the above data, we don't need to clean those NA values


## Check for no of unique value in each column
sapply(names(Uber), function(x) sum(!duplicated(Uber[, x])))
# Request.id      Pickup.point         Driver.id            Status Request.timestamp    Drop.timestamp 
#       6745                 2               301                 3              5618              2599

## From the above result we can set Status and Pickup.point as factor
Uber$Status <- as.factor(Uber$Status)
Uber$Pickup.point <- as.factor(Uber$Pickup.point)
str(Uber);  levels(Uber$Status)

## Convert Request.timestamp and Drop.timestamp to proper date type
# sum(is.na(as.Date(Uber$Request.timestamp))) != 0
# sum(is.na(as.Date(Uber$Drop.timestamp))) != 3914
# We need to format these two field
Uber <- mutate(Uber, Request.timestamp_2 = dmy_hms(Uber$Request.timestamp,tz="Asia/Calcutta"))
## Opps, 2674 failed to parse, as they don't have second - ss data, 6745 - 2674 = 4071 got parsed
Uber <- mutate(Uber, Request.timestamp_3 = dmy_hm(Uber$Request.timestamp,tz="Asia/Calcutta"))
## Again, 4071 failed to parse, but we're good as that matched with previous parsed timestamp. 
## Now we can merge these two new columns to one
## https://stackoverflow.com/questions/14563531/combine-column-to-remove-nas
Uber <-  mutate(Uber, Request.timestamp.new = coalesce(Request.timestamp_2, Request.timestamp_3))
sum(is.na(as.Date(Uber$Request.timestamp.new)))  # Same as before, no NA for Request.timestamp
## We are good now, we can delete those two temp columns
Uber <- subset(Uber, select = -c(Request.timestamp_2, Request.timestamp_3))

## Let's do the same activites for Drop.timestamp
Uber <- mutate(Uber, Drop.timestamp_2 = dmy_hms(Uber$Drop.timestamp,tz="Asia/Calcutta"))
Uber <- mutate(Uber, Drop.timestamp_3 = dmy_hm(Uber$Drop.timestamp,tz="Asia/Calcutta"))
Uber <-  mutate(Uber, Drop.timestamp.new = coalesce(Drop.timestamp_2, Drop.timestamp_3))
Uber <- subset(Uber, select = -c(Drop.timestamp_2, Drop.timestamp_3))
sum(is.na(as.Date(Uber$Drop.timestamp.new)))
## Great, same as before NAs count - 3914

## Helpful aggregate variable
cnt_Total <- as.numeric(nrow(Uber))
cnt_Airport <- as.numeric(nrow(Uber[Uber$Pickup.point == 'Airport',]))
cnt_City <- as.numeric(nrow(Uber[Uber$Pickup.point == 'City',]))
# cnt_Airport + cnt_City = 6745


## Now let's check the count of different Status
Uber %>% group_by(Status) %>% summarize(cnt=length(Request.id), pct = (cnt/cnt_Total)*100)
# Status              cnt   pct
# Cancelled          1264  18.7
# No Cars Available  2650  39.3
# Trip Completed     2831  42.0

## Let's look at the NA values for Drop.timestamp.new
Uber %>% filter(is.na(Drop.timestamp.new)) %>% group_by(Status) %>% summarize(cnt=length(Request.id))
# Status              cnt
# Cancelled          1264
# No Cars Available  2650
# So, Drop.timestamp.new is NA when cab is Cancelled or No Cars Available.

## Let's look at the NA values for Driver.id
Uber %>% filter(is.na(Driver.id)) %>% group_by(Status) %>% summarize(cnt=length(Request.id))
# Status              cnt
# No Cars Available  2650
# So, Driver.id is NA when No Cars Available


## Derived Metrics
## We can create derived values like Date, Day, Hour and Trip time from both timestamp data
Uber$Request.Date <- date(Uber$Request.timestamp.new)
Uber$Request.Day <- weekdays(Uber$Request.timestamp.new)
Uber$Request.Hour <- hour(Uber$Request.timestamp.new)
Uber$Drop.Date <- date(Uber$Drop.timestamp.new)
Uber$Drop.Day <- weekdays(Uber$Drop.timestamp.new)
Uber$Drop.Hour <- hour(Uber$Drop.timestamp.new)
Uber$Trip.length <- interval(Uber$Request.timestamp.new, Uber$Drop.timestamp.new)/dminutes(1)

## Univariate Analysis
## Request.id
length(unique(Uber$Request.id)) ## 6745 - No. of data point, as there is no NA value
summary(Uber$Request.id)
# Min. 1st Qu.  Median    Mean  3rd Qu.    Max.
# 1    1691     3387      3385    5080    6766
# Reqiest.id looks like sequential data, but the max value is 6766 instead of 6745.
# Which mean 6766 - 6745 + 1 = 22 data points are missing

## Pickup.point
summary(Uber$Pickup.point)
Uber %>% group_by(Pickup.point) %>% summarize(cnt=length(Request.id), pct = (cnt/cnt_Total)*100)


G1 <- ggplot(Uber, aes(x=Pickup.point, fill=Pickup.point)) + geom_bar() +
  labs(title="All Cab request by Pickup Point", x="Pickup Point", y="Count", fill="Pickup Point")
grid.arrange(G1)
# Data point are almost evenly distributed between City and Airport
# Let's look at overall status count for both Pickup.point
Uber %>% group_by(Pickup.point, Status) %>% summarize(cnt=length(Request.id))
# Pickup.point Status              cnt
# Airport      Cancelled           198
# Airport      No Cars Available  1713
# Airport      Trip Completed     1327
# City         Cancelled          1066
# City         No Cars Available   937
# City         Trip Completed     1504

G2 <- ggplot(Uber, aes(x=Pickup.point, fill=Status)) + geom_bar() +
  labs(title="All cab request Status by Pickup Point", x="Status", y="Count", fill="Status")
grid.arrange(G2)


## Status
summary(Uber$Status)
G3 <- ggplot(Uber, aes(x=Status, fill=Status)) + geom_bar()
ggplot(Uber, aes(x=1, fill=Status)) + geom_bar() + coord_polar("y", start=0) + theme_void()
grid.arrange(G3)

# There is an issue as lots of request are Cancelled or No Cars available

## Request.Date - Derived Metrics
summary(Uber$Request.Date)
summary(as.factor(Uber$Request.Date))
G4 <- ggplot(Uber, aes(x=Request.Date, fill=Request.Day)) + geom_bar() + 
  labs(title="All cab request by Date", x="Date", y="Count", fill="Day of Request")
grid.arrange(G4)
# Only 5 days data is present and they are all weekdays. Data is almost evenly distributed over all dates

## Request.Hour - Derived Metrics
summary(Uber$Request.Hour)
summary(as.factor(Uber$Request.Hour))
G5 <- ggplot(Uber, aes(x=Request.Hour)) + scale_x_continuous(breaks=seq(0,23,1)) + geom_bar() +
  labs(title="All cab request by Hour", x="Hour", y="Count")
grid.arrange(G5)
# There are two segment in which cab request is higher, Morning 4-10 and Evening 5-11

## Segemented Univarate Analysis
# We can divide the hours into following segements
Uber[which(Uber$Request.Hour %in% c(0:3)),'Day.segment'] <- "1 - Late Night" # Added Number for ordering
Uber[which(Uber$Request.Hour %in% c(4:5)),'Day.segment'] <- "2 - Early Morning"
Uber[which(Uber$Request.Hour %in% c(6:10)),'Day.segment'] <- "3 - Morning Rush"
Uber[which(Uber$Request.Hour %in% c(11:16)),'Day.segment'] <- "4 - Day Time"
Uber[which(Uber$Request.Hour %in% c(17:23)),'Day.segment'] <- "5 - Evening Rush"
Uber$Day.segment <- as.factor(Uber$Day.segment)
# levels(Uber$Day.segment) <- c("Late Night", "Early Morning", "Morning Rush", "Day Time", "Evening Rush")
levels(Uber$Day.segment)

########### Visually identify the most pressing problems for Uber ###########


########### Create plots to visualise the frequency of requests that get cancelled or show 'no cars available'; identify the most problematic types of requests (city to airport / airport to city etc.) and the time slots (early mornings, late evenings etc.) using plots ###########
G6 <- ggplot(subset(Uber, Uber$Pickup.point == "Airport"), aes(x=Day.segment, fill=Status)) + geom_bar(position = "dodge") +
  labs(title="Airport - Trip Status over the day", x="Segment of the day", y="Number of Trips", fill="Trip Status")
grid.arrange(G6)
# In Evening Rush and Late Night hours, there is lot of No Cars Available.

G7 <- ggplot(subset(Uber, Uber$Pickup.point == "City"), aes(x=Day.segment, fill=Status)) + geom_bar(position = "dodge") +
  labs(title="City - Trip Status over the day", x="Segment of the day", y="Number of Trips", fill="Trip Status")
grid.arrange(G7)
# There are high "Cancellations" & "No Cabs available" during "Early Morning" & "Morning Rush" hours
grid.arrange(G6, G7)

########### Find out the gap between supply and demand and show the same using plots ###########
# Demand and Supply at Airport and City
airport_demand <- ggplot(subset(Uber, Uber$Pickup.point == "Airport"), aes(x=Request.Hour)) +
  scale_x_continuous(breaks=seq(0,23,1)) + geom_bar(fill = "blue") +
  labs(title="Demand at Airport (Requests)", x="Request Hour", y="Count") + ylim(0,500)

airport_supply <- ggplot(subset(subset(Uber, Uber$Pickup.point == "Airport"), !is.na(Drop.Hour)), aes(x=Drop.Hour)) +
  scale_x_continuous(breaks=seq(0,23,1)) +
  geom_bar(fill = "red") + labs(title ="Supply at Airport (Drops)", x="Drop Hour", y="Count") + ylim(0,500)

city_demand <- ggplot(subset(Uber, Uber$Pickup.point == "City"), aes(Request.Hour)) +
  scale_x_continuous(breaks=seq(0,23,1)) +
  geom_bar(fill = "blue") + labs(title ="Demand at City (Requests)", x="Request Hour", y="Count") + ylim(0,500)

city_supply <- ggplot(subset(subset(Uber, Uber$Pickup.point == "City"), !is.na(Drop.Hour)), aes(Drop.Hour)) +
  scale_x_continuous(breaks=seq(0,23,1)) +
  geom_bar(fill = "red") + labs(title ="Supply at City (Drops)", x="Drop Hour", y="Count") + ylim(0,500)


# Overall Demand and Supply trend
grid.arrange(airport_demand, airport_supply, city_demand, city_supply, nrow = 2, ncol = 2)


########### Find the time slots when the highest gap exists ###########
G8 <- ggplot(Uber) + scale_x_continuous(breaks=seq(0,23,1)) +
  geom_bar(aes(Request.Hour), fill = "blue") + geom_bar(aes(Drop.Hour), fill = "red", position = "dodge")+
  ylim(0,500) + labs(title = "Demand-Supply Gap", x="Hour of the day", y="Count")
grid.arrange(G8)
# Demand-Supply gap is more between 4-10AM and 5-10PM

########### Find the types of requests (city-airport or airport-city) for which the gap is the most severe in the identified time slots ###########
G9 <- ggplot(subset(Uber, Uber$Pickup.point == "Airport")) + scale_x_continuous(breaks=seq(0,23,1)) +
  geom_bar(aes(Request.Hour), fill = "blue") + geom_bar(aes(Drop.Hour), fill = "red", position = "dodge")+
  ylim(0,500) + labs(title = "Airport: Demand vs Supply", x="Hour of the day", y="Count")
grid.arrange(G9)
# Demand-Supply gap is more between 5-10PM for Airport

G10 <- ggplot(subset(Uber, Uber$Pickup.point == "City")) + scale_x_continuous(breaks=seq(0,23,1)) +
  geom_bar(aes(Request.Hour), fill = "blue") + geom_bar(aes(Drop.Hour), fill = "red", position = "dodge")+
  ylim(0,500) + labs(title = "City: Demand vs Supply", x="Hour of the day", y="Count")
grid.arrange(G10)
# Demand-Supply gap is more between 4-10AM for City

# Combined Plot
grid.arrange(G9, G10)

########### What do you think is the reason for this issue for the supply-demand gap? Write the answer in less than 100 words. You may accompany the write-up with plot(s) ###########
# In PPT

########### Recommend some ways to resolve the supply-demand gap ###########
#In PPT
