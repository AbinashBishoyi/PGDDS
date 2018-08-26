# Spark Fund Case Study
# Abinash Bishoyi, Karthik Vecham, Balaji Nagaraja

# Set Working Directory to source file

library(dplyr)
library(tidyr)
library(countrycode)

####################  Checkpoint 1: Data Cleaning 1 #################### 
# Load the companies and rounds data (provided on the previous page) into two data frames 
# and name them companies and rounds2 respectively.

companies <- read.delim("companies.txt", sep="\t", header = TRUE, stringsAsFactors = FALSE)
rounds2 <- read.csv("rounds2.csv", header = TRUE, stringsAsFactors = FALSE)
# str(companies) 
# str(rounds2)

# Change permalink to lowercase for companies and rounds
companies$permalink <- tolower(companies$permalink)
rounds2$company_permalink <- tolower(rounds2$company_permalink)

# Results Expected: Table 1.1
# Table 1.1: Understand the Data Set 

# How many unique companies are present in rounds2?
# unique_rounds_comanies <- unique(rounds2$company_permalink, incomparables = FALSE)
n_distinct(rounds2$company_permalink)


# How many unique companies are present in companies?
# unique_comanies <- unique(companies$permalink)
n_distinct(companies$permalink)


# In the companies data frame, which column can be used as the unique key for each company?
# Write the name of the column.
# At first look, there are two candidates for unique key, permalink and name, 
# n_distinct(companies$name) = 66103, 
# n_distinct(companies$permalink) = 66368
# but upon further analysis, we can see that name is not unique, so permalink is our unique key
print("permalink")


# Are there any companies in the rounds2 file which are not present in companies? Answer yes or no: Y/N
if (sum(!(rounds2$company_permalink %in% companies$permalink))) {
  print("Y")
} else {
  print("N")
}

# Merge the two data frames so that all variables (columns) in the companies frame are added to the rounds2 data frame.
# https://stackoverflow.com/questions/6081439/changing-column-names-of-a-data-frame
# colnames(rounds2)[1] <- "permalink"
# master_frame <- inner_join(companies, rounds2, by.x = "permalink", by.y = "company_permalink")
master_frame <- merge(companies, rounds2, by.x = "permalink", by.y = "company_permalink", all = TRUE)


# Name the merged frame master_frame. How many observations are present in master_frame?
nrow(master_frame)


#################### Checkpoint 2: Funding Type Analysis ####################
# Average funding amount of venture type
# master_frame$raised_amount_usd[is.na(master_frame$raised_amount_usd)] <- 0
mean(master_frame[which(master_frame$funding_round_type == "venture"),]$raised_amount_usd, na.rm = TRUE)


# Average funding amount of angel type
mean(master_frame[which(master_frame$funding_round_type == "angel"),]$raised_amount_usd, na.rm = TRUE)


# Average funding amount of seed type
mean(master_frame[which(master_frame$funding_round_type == "seed"),]$raised_amount_usd, na.rm = TRUE)


# Average funding amount of private equity type
mean(master_frame[which(master_frame$funding_round_type == "private_equity"),]$raised_amount_usd, na.rm = TRUE)


# Considering that Spark Funds wants to invest between 5 to 15 million USD per investment round, 
# which investment type is the most suitable for it?
(master_frame %>% group_by(funding_round_type) %>% 
  summarise(raised_amount_usd = mean(raised_amount_usd, na.rm = TRUE)) %>%
  filter(raised_amount_usd > 5000000 & raised_amount_usd < 15000000))$funding_round_type


#################### Checkpoint 3: Country Analysis ####################

# Now that you know the type of investment suited for Spark Funds, let's narrow down the countries.
venture_master_frame <- filter(master_frame, funding_round_type == "venture")
country_group_amount <- summarise(group_by(venture_master_frame, country_code), total_amount = sum(raised_amount_usd, na.rm = TRUE))

# Removing Blank country
country_group_amount <- country_group_amount[country_group_amount$country_code != "",]
top9 <- head(arrange(country_group_amount, desc(total_amount)), 9)

# Collected data from http://www.emmir.org/fileadmin/user_upload/admission/Countries_where_English_is_an_official_language.pdf
english_speaking_country <- read.delim("English_Speaking_Country.txt", header = FALSE, stringsAsFactors = FALSE, sep = ",")

# Adding column country_name
top9$country_name <- countrycode(top9$country_code,'iso3c', 'country.name')
top9_english_speaking_country <- top9 %>% filter(country_name %in% english_speaking_country)


# 1. Top English-speaking country
top9_english_speaking_country$country_name[1]


# 2. Second English-speaking country
top9_english_speaking_country$country_name[2]


# 3. Third English-speaking country
top9_english_speaking_country$country_name[3]


#################### Checkpoint 4: Sector Analysis 1 ####################
# You discuss with the CEO and come up with the business rule that the first string before the vertical bar will be considered the primary sector.
# Extract the primary sector of each category list from the category_list column
# https://stackoverflow.com/questions/19320966/get-the-strings-before-the-comma-with-r
master_frame$category_list <- tolower(master_frame$category_list)
# primary_sector <- sapply(strsplit(master_frame$category_list, "|", fixed = TRUE), "[", 1)
# primary_sector <- vapply(strsplit(master_frame$category_list, "|", fixed = TRUE), "[", "", 1)
# sum(is.na(primary_sector))
master_frame <- mutate(master_frame, primary_sector = sapply(strsplit(master_frame$category_list, "|", fixed = TRUE), "[", 1))


# Use the mapping file 'mapping.csv' to map each primary sector to one of the eight main sectors
# (Note that 'Others' is also considered one of the main sectors)
mapping <- read.csv("mapping.csv", header = TRUE, stringsAsFactors = FALSE)

# Found that column name are getting collapsed, so using list with column name for mapping.csv
mapping_col <- c("category_list","Automotive & Sports","Blanks","Cleantech / Semiconductors","Entertainment","Health","Manufacturing","News, Search and Messaging","Others","Social, Finance, Analytics, Advertising")
names(mapping) <- mapping_col
mapping$category_list <- tolower(mapping$category_list)

# Search for category_list contains any alphanumeric characters.
mapping$category_list[which(grepl("[[:digit:]]", mapping$category_list) == TRUE)]

# Replacing erroneous '0' with 'na' will be the best approach
mapping$category_list <- gsub("0","na",mapping$category_list)

# Search again for category_list contains any alphanumeric characters.
mapping$category_list[which(grepl("[[:digit:]]", mapping$category_list) == TRUE)]

# Fixing invalid substitution for "enterprise 2.na"
mapping$category_list <- gsub("2.na", "2.0", mapping$category_list)
mapping$category_list[which(grepl("[[:digit:]]", mapping$category_list) == TRUE)]


# Convert wide to long format
mapping_long <- gather(mapping, main_sector, value, 2:10)
mapping_long <- mapping_long[!(mapping_long$value == 0),]
# head(mapping_long)
mapping_long <- mapping_long[,-3]

# Data cleaning needed before merging
sum(mapping$category_list == "")
sum(is.na(master_frame$primary_sector))
master_frame$primary_sector[which(is.na(master_frame$primary_sector))] <- ""


mapped_master_frame <- merge(master_frame, mapping_long, by.x = "primary_sector", by.y = "category_list", all.x = TRUE)
master_frame <- mapped_master_frame

# master_frame <- filter(master_frame, funding_round_type == "venture", master_frame$raised_amount_usd >= 5e+06 & master_frame$raised_amount_usd <= 15e+06, main_sector != "Blanks")
# master_frame_sectorwise_investment <- master_frame %>% group_by(main_sector) %>% summarize(sectorwise_count = n(), sectorwise_total_investment = sum(raised_amount_usd, na.rm = TRUE))
# master_frame <- merge(master_frame, master_frame_sectorwise_investment,by="main_sector")

master_frame <- filter(master_frame, master_frame$raised_amount_usd >= 5e+06 & master_frame$raised_amount_usd <= 15e+06)
# Wrtite master_frame into csv for Tableau
write.csv(master_frame, "master_frame.csv")

# levels(as.factor(master_frame$main_sector))

#################### Checkpoint 5: Sector Analysis 2 ####################

# Create three separate data frames D1, D2 and D3 for each of the three countries containing the observations of funding type FT falling within the 5-15 million USD range. The three data frames should contain:
# All the columns of the master_frame along with the primary sector and the main sector
# The total number (or count) of investments for each main sector in a separate column
# The total amount invested in each main sector in a separate column
# Excluding the 'Blank' sector
D1 <- filter(master_frame, country_code == "USA", funding_round_type == "venture", master_frame$raised_amount_usd >= 5e+06 & master_frame$raised_amount_usd <= 15e+06, main_sector != "Blanks")
D2 <- filter(master_frame, country_code == "GBR", funding_round_type == "venture", master_frame$raised_amount_usd >= 5e+06 & master_frame$raised_amount_usd <= 15e+06, main_sector != "Blanks")
D3 <- filter(master_frame, country_code == "IND", funding_round_type == "venture", master_frame$raised_amount_usd >= 5e+06 & master_frame$raised_amount_usd <= 15e+06, main_sector != "Blanks")

D1_sectorwise_investment <- D1 %>% group_by(main_sector) %>% summarize(sectorwise_count = n(), sectorwise_total_investment = sum(raised_amount_usd))
D1 <- merge(D1,D1_sectorwise_investment,by="main_sector")

D2_sectorwise_investment <- D2 %>% group_by(main_sector) %>% summarize(sectorwise_count = n(), sectorwise_total_investment = sum(raised_amount_usd))
D2 <- merge(D2,D2_sectorwise_investment,by="main_sector")

D3_sectorwise_investment <- D3 %>% group_by(main_sector) %>% summarize(sectorwise_count = n(), sectorwise_total_investment = sum(raised_amount_usd))
D3 <- merge(D3,D3_sectorwise_investment,by="main_sector")

# Table 5.1: Based on the analysis of the sectors, which main sectors and countries would you recommend Spark Funds to invest in? Present your conclusions in the presentation. The conclusions are subjective (i.e. there may be no 'one right answer'), but it should be based on the basic strategy - invest in sectors where most investments are occurring. 
# 1. Total number of investments (count)
nrow(D1)
nrow(D2)
nrow(D3)


# 2. Total amount of investment (USD)
# sum(is.na(D1$raised_amount_usd))
sum(D1$raised_amount_usd, na.rm = TRUE)
sum(D2$raised_amount_usd, na.rm = TRUE)
sum(D3$raised_amount_usd, na.rm = TRUE)


# 3. Top sector (based on count of investments)
# D1 %>% arrange(desc(sectorwise_count)) %>% select(main_sector) %>% unique()
top_sector_D1 <- slice(D1 %>% arrange(desc(sectorwise_count)) %>% select(main_sector) %>% unique(), 1)$main_sector; top_sector_D1
top_sector_D2 <- slice(D2 %>% arrange(desc(sectorwise_count)) %>% select(main_sector) %>% unique(), 1)$main_sector; top_sector_D2
top_sector_D3 <- slice(D3 %>% arrange(desc(sectorwise_count)) %>% select(main_sector) %>% unique(), 1)$main_sector; top_sector_D3

# 4. Second-best sector (based on count of investments)
second_sector_D1 <- slice(D1 %>% arrange(desc(sectorwise_count)) %>% select(main_sector) %>% unique(), 2)$main_sector; second_sector_D1
second_sector_D2 <- slice(D2 %>% arrange(desc(sectorwise_count)) %>% select(main_sector) %>% unique(), 2)$main_sector; second_sector_D2
second_sector_D3 <- slice(D3 %>% arrange(desc(sectorwise_count)) %>% select(main_sector) %>% unique(), 2)$main_sector; second_sector_D3


# 5. Third-best sector (based on count of investments)
third_sector_D1 <- slice(D1 %>% arrange(desc(sectorwise_count)) %>% select(main_sector) %>% unique(), 3)$main_sector; third_sector_D1
third_sector_D2 <- slice(D2 %>% arrange(desc(sectorwise_count)) %>% select(main_sector) %>% unique(), 3)$main_sector; third_sector_D2
third_sector_D3 <- slice(D3 %>% arrange(desc(sectorwise_count)) %>% select(main_sector) %>% unique(), 3)$main_sector; third_sector_D3


# 6. Number of investments in the top sector (refer to point 3)
(D1 %>% filter(main_sector == top_sector_D1) %>% select(sectorwise_count) %>% unique())$sectorwise_count
(D2 %>% filter(main_sector == top_sector_D2) %>% select(sectorwise_count) %>% unique())$sectorwise_count
(D3 %>% filter(main_sector == top_sector_D3) %>% select(sectorwise_count) %>% unique())$sectorwise_count


# 7. Number of investments in the second-best sector (refer to point 4)
(D1 %>% filter(main_sector == second_sector_D1) %>% select(sectorwise_count) %>% unique())$sectorwise_count
(D2 %>% filter(main_sector == second_sector_D2) %>% select(sectorwise_count) %>% unique())$sectorwise_count
(D3 %>% filter(main_sector == second_sector_D3) %>% select(sectorwise_count) %>% unique())$sectorwise_count


# 8. Number of investments in the third-best sector (refer to point 5)
(D1 %>% filter(main_sector == third_sector_D1) %>% select(sectorwise_count) %>% unique())$sectorwise_count
(D2 %>% filter(main_sector == third_sector_D2) %>% select(sectorwise_count) %>% unique())$sectorwise_count
(D3 %>% filter(main_sector == third_sector_D3) %>% select(sectorwise_count) %>% unique())$sectorwise_count


# 9. For the top sector count-wise (point 3), which company received the highest investment?
D1_top_permalink <- (D1 %>% filter(main_sector == top_sector_D1) %>% group_by(permalink) %>% 
  summarise(total_investment = sum(raised_amount_usd)) %>% arrange(desc(total_investment)) %>% slice(1))$permalink
(D1 %>% filter(permalink == D1_top_permalink) %>% select(name) %>% slice(1))$name
D2_top_permalink <- (D2 %>% filter(main_sector == top_sector_D1) %>% group_by(permalink) %>% 
                       summarise(total_investment = sum(raised_amount_usd)) %>% arrange(desc(total_investment)) %>% slice(1))$permalink
(D2 %>% filter(permalink == D2_top_permalink) %>% select(name) %>% slice(1))$name
D3_top_permalink <- (D3 %>% filter(main_sector == top_sector_D1) %>% group_by(permalink) %>% 
                       summarise(total_investment = sum(raised_amount_usd)) %>% arrange(desc(total_investment)) %>% slice(1))$permalink
(D3 %>% filter(permalink == D3_top_permalink) %>% select(name) %>% slice(1))$name


# 10. For the second-best sector count-wise (point 4), which company received the highest investment?
D1_second_permalink <- (D1 %>% filter(main_sector == second_sector_D1) %>% group_by(permalink) %>% 
                       summarise(total_investment = sum(raised_amount_usd)) %>% arrange(desc(total_investment)) %>% slice(1))$permalink
(D1 %>% filter(permalink == D1_second_permalink) %>% select(name) %>% slice(1))$name
D2_second_permalink <- (D2 %>% filter(main_sector == second_sector_D1) %>% group_by(permalink) %>% 
                       summarise(total_investment = sum(raised_amount_usd)) %>% arrange(desc(total_investment)) %>% slice(1))$permalink
(D2 %>% filter(permalink == D2_second_permalink) %>% select(name) %>% slice(1))$name
D3_second_permalink <- (D3 %>% filter(main_sector == second_sector_D1) %>% group_by(permalink) %>% 
                       summarise(total_investment = sum(raised_amount_usd)) %>% arrange(desc(total_investment)) %>% slice(1))$permalink
(D3 %>% filter(permalink == D3_second_permalink) %>% select(name) %>% slice(1))$name

