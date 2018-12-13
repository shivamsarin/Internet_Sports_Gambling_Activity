
##############################
#     1 -  Data Cleaning     #
##############################

# Load-in the libraries required
library(haven)
library(dplyr)
library(readr)
library(lubridate)

# Select the data Directory
data_directory <- "F:/IESEG Classwork/R Programming/group-assignment-open-source-programming-bwin-group-12/Group Assignment/"

# Read the AnalyticDataInternetGambling dataset
d1_Analytic_Data_Internet_Gambling <- read_sas(paste0(data_directory,"AnalyticDataInternetGambling.sas7bdat"))

# Read the RawDataIDemographics dataset
d2_Demographics <- read_sas(paste0(data_directory,"RawDataIDemographics.sas7bdat"))

# Read the RawDataIIIPokerChipConversions dataset
d3_Poker_Chip_Conversions <- read_sas(paste0(data_directory,"RawDataIIIPokerChipConversions.sas7bdat"))

# Read the RawDataIIUserDailyAggregation dataset
d4_User_Daily_Aggregation <- read_sas(paste0(data_directory,"RawDataIIUserDailyAggregation.sas7bdat"))

# Removing Duplicate Data to remove inconsistency
d1_Analytic_Data_Internet_Gambling <- unique(d1_Analytic_Data_Internet_Gambling) # No Duplicates
d2_Demographics <- unique(d2_Demographics) # No Duplicates
d3_Poker_Chip_Conversions <- unique(d3_Poker_Chip_Conversions) # 277907 - 277653 = 254 Duplicates removed
d4_User_Daily_Aggregation <- unique(d4_User_Daily_Aggregation) # 1740196 - 1740111 = 85 Duplicates removed

# Manually created a CSV for mapping the Application Codes to Application Descriptions
Application_Code_Comprehension <- read_csv(paste0(data_directory,"Application_Code_Comprehension.csv"))

# Manually created a CSV for mapping the Country Codes to Conntry Names
Country_Code_Comprehension <- read_csv(paste0(data_directory,"Country_Code_Comprehension.csv"))

# Manually created a CSV for mapping the Language Codes to Language Names
Language_Code_Comprehension <- read_csv(paste0(data_directory,"Language_Code_Comprehension.csv"))

# Manually created a CSV for mapping the Product Codes to Product Descriptions
Product_Code_Comprehension <- read_csv(paste0(data_directory,"Product_Code_Comprehension.csv"))

# Clean the Date format of TransDateTime
d3_Poker_Chip_Conversions$TransDateTime <- ymd_hms(d3_Poker_Chip_Conversions$TransDateTime)
# Map the transaction type to the buy/sell values
d3_Poker_Chip_Conversions$TransType <- ifelse(d3_Poker_Chip_Conversions$TransType == 124, 'Buy','Sell')

# Save the Data
# write.csv(d3_Poker_Chip_Conversions, file=paste0(data_directory,"d3_Poker_Chip_Conversions.csv"),row.names=F)

# Merge the Application description to the application codes
d2_Demographics <- merge(x=d2_Demographics, y=Application_Code_Comprehension, by.x = "ApplicationID",
              by.y = "ApplicationID", all.x = T)

# Merge the Country Names to the Country codes
d2_Demographics <- merge(x=d2_Demographics, y=Country_Code_Comprehension, by.x = "Country",
                         by.y = "Country", all.x = T)

# Merge the language Names to the language codes
d2_Demographics <- merge(x=d2_Demographics, y=Language_Code_Comprehension, by.x = "Language",
                         by.y = "Language", all.x = T)

# Map the Gender type to the male/female values
d2_Demographics$Gender <- ifelse(d2_Demographics$Gender == 1, 'Male','Female')

# Replace the Codes with actual values
d2_Demographics$Language <- d2_Demographics$`Language Description`
d2_Demographics$Country <- d2_Demographics$`Country Name`
d2_Demographics$ApplicationID <- d2_Demographics$`Application Description`

# Remove unnecessary data
d2_Demographics$`Language Description` <- NULL
d2_Demographics$`Country Name` <- NULL
d2_Demographics$`Application Description` <- NULL

# Fix all the date formats
d2_Demographics$RegDate <- as.Date(d2_Demographics$RegDate)
d2_Demographics$FirstPay <- as.Date(d2_Demographics$FirstPay, format = "%Y%m%d")
d2_Demographics$FirstAct <- as.Date(d2_Demographics$FirstAct, format = "%Y%m%d")
d2_Demographics$FirstSp <- as.Date(d2_Demographics$FirstSp, format = "%Y%m%d")
d2_Demographics$FirstCa <- as.Date(d2_Demographics$FirstCa, format = "%Y%m%d")
d2_Demographics$FirstGa <- as.Date(d2_Demographics$FirstGa, format = "%Y%m%d")
d2_Demographics$FirstPo <- as.Date(d2_Demographics$FirstPo, format = "%Y%m%d")

# Save the data
# write.csv(d2_Demographics, file=paste0(data_directory,"d2_Demographics.csv"),row.names=F)

# Merge the product Names to the product codes
d4_User_Daily_Aggregation <- merge(x=d4_User_Daily_Aggregation, y=Product_Code_Comprehension, by.x = "ProductID",
                         by.y = "ProductID", all.x = T)

# Replace the Codes with actual values
d4_User_Daily_Aggregation$ProductID <- d4_User_Daily_Aggregation$`Product Description`
d4_User_Daily_Aggregation$`Product Description` <- NULL

# Fix all the date formats
d4_User_Daily_Aggregation$Date <- as.Date(d4_User_Daily_Aggregation$Date, format = "%Y%m%d")

# Save the data
# write.csv(d4_User_Daily_Aggregation, file=paste0(data_directory,"d4_User_Daily_Aggregation.csv"),row.names=F)

# Merge the Country Names to the Country codes
d1_Analytic_Data_Internet_Gambling <- merge(x=d1_Analytic_Data_Internet_Gambling, y=Country_Code_Comprehension, by.x = "COUNTRY",
                                            by.y = "Country", all.x = T)

# Merge the language Names to the language codes
d1_Analytic_Data_Internet_Gambling <- merge(x=d1_Analytic_Data_Internet_Gambling, y=Language_Code_Comprehension, by.x = "LANGUAGE",
                                            by.y = "Language", all.x = T)

# Replace the Codes with actual values
d1_Analytic_Data_Internet_Gambling$LANGUAGE <- d1_Analytic_Data_Internet_Gambling$`Language Description`
d1_Analytic_Data_Internet_Gambling$COUNTRY <- d1_Analytic_Data_Internet_Gambling$`Country Name`

# Remove unnecessary data
d1_Analytic_Data_Internet_Gambling$`Language Description` <- NULL
d1_Analytic_Data_Internet_Gambling$`Country Name` <- NULL

# Map the Gender type to the male/female values
d1_Analytic_Data_Internet_Gambling$GENDER <- ifelse(d1_Analytic_Data_Internet_Gambling$GENDER == 1, 'Male','Female')

# Save the data
# write.csv(d1_Analytic_Data_Internet_Gambling, file=paste0(data_directory,"d1_Analytic_Data_Internet_Gambling.csv"),row.names=F)
