
##############################
#  2 - Base table Creation   #
##############################

options("scipen" = 100,"digits" = 2)

# Load-in the libraries required
library(dplyr)
library(lubridate)
library(tidyr)
library(readr)

# Select the data Directory
data_directory <- "F:/IESEG Classwork/R Programming/group-assignment-open-source-programming-bwin-group-12/Group Assignment/"

# Read the Cleansed AnalyticDataInternetGambling dataset
d1_Analytic_Data_Internet_Gambling <- read_csv(paste0(data_directory,"d1_Analytic_Data_Internet_Gambling.csv"), 
                                               col_types = cols(FOFirstActiveDate = col_date(format = "%Y-%m-%d"), 
                                                                FOLastActiveDate = col_date(format = "%Y-%m-%d"), 
                                                                FirstSportsActiveDate = col_date(format = "%Y-%m-%d"), 
                                                                LAFirstActiveDate = col_date(format = "%Y-%m-%d"), 
                                                                LALastActiveDate = col_date(format = "%Y-%m-%d"), 
                                                                RegistrationDate = col_date(format = "%Y-%m-%d")))

# Read the Cleansed RawDataIDemographics dataset
d2_Demographics <- read_csv(paste0(data_directory,"d2_Demographics.csv"), 
                            col_types = cols(FirstAct = col_date(format = "%Y-%m-%d"), 
                                             FirstCa = col_date(format = "%Y-%m-%d"), 
                                             FirstPay = col_date(format = "%Y-%m-%d"), 
                                             FirstPo = col_date(format = "%Y-%m-%d"), 
                                             FirstSp = col_date(format = "%Y-%m-%d"), 
                                             RegDate = col_date(format = "%Y-%m-%d")))

# Read the Cleansed RawDataIIIPokerChipConversions dataset
d3_Poker_Chip_Conversions <- read_csv(paste0(data_directory,"d3_Poker_Chip_Conversions.csv"), 
                                      col_types = cols(TransDateTime = col_datetime(format = "%Y-%m-%d %H:%M:%S")))

# Read the Cleansed RawDataIIUserDailyAggregation dataset
d4_User_Daily_Aggregation <- read_csv(paste0(data_directory,"d4_User_Daily_Aggregation.csv"), 
                                      col_types = cols(Date = col_date(format = "%Y-%m-%d")))

# Create a temporary dataframe for data manipulation/aggregation
d4 <- d4_User_Daily_Aggregation

# Stakes, winnings and bets can never be negative 
# So replace all negative values with lowest possible value i.e., zero
d4$Stakes[d4$Stakes < 0] <- 0
d4$Winnings[d4$Winnings < 0] <- 0
d4$Bets[d4$Bets < 0] <- 0

# Add the Stakes, Winning and the Bets based on the User, Product and Date
d4 <- d4 %>%
  group_by(UserID,ProductID,Date) %>%
  summarise(Stakes = sum(Stakes),
            Winnings = sum(Winnings),
            Bets = sum(Bets)) %>%
  arrange(UserID,Date)

# Create new variables:
#   Assumption- We have data till 2005-09-31 so we are calculating recency from 2005-10-01
#   Recency_lastwin Recent Days on which user won
#   Recency_lastloss Recent Days on which user lost
#   IPT_Days Difference in the successive gaming sessions
#   month_range Creating month range for first three months = 1, mid three months = 2 and the last two months as 3
d4 <- d4 %>%
  group_by(UserID,ProductID) %>%
  mutate(Recency_lastwin = ifelse(Winnings > 0,as.numeric(difftime(as.Date("2005-10-01"),Date, units="days")),0),
         Recency_lastloss = ifelse(Winnings == 0,as.numeric(difftime(as.Date("2005-10-01"),Date, units="days")),0),
         IPT_Days = as.numeric(difftime(Date, lag(Date), units="days")),
         month_range = ifelse(Date < as.Date("2005-05-01"),1,ifelse(Date < as.Date("2005-08-01"),2,3)))

# For User_Daily_Aggregation (UA)
#
# Recency:
#   UA_Recency - Number of days since last betting activity
#   UA_Recency_lastwin - Number of days since last win
#   UA_Recency_lastloss - Number of days since last loss
# Interpurchase time:
#   UA_IPT_std - Standard Deviation of the number of days between betting sessions
#   UA_IPT_min - Minimun number of days between betting sessions
#   UA_IPT_max - Maximum number of days between betting sessions
#   UA_IPT_mean - Average number of days between betting sessions
#   UA_IPT_CV - Coefficient of variation of interpurchase time (ratio of UA_IPT_std to UA_IPT_mean)
# Length of relationship:
#   UA_Lor_firstPlay - Number of days since first betting
# Frequency:
#   UA_Freq_bets - Total number of bets
#   UA_Freq_bets_first3Months - Total number of bets from Feb to April
#   UA_Freq_bets_mid3Months - Total number of bets from May to July
#   UA_Freq_bets_last2Months - Total number of bets from August to September
#   UA_Freq_sessions_won - Total number of bets where winning is greater than 0
#   UA_Freq_sessions_lost - Total number of bets where winning is equal to 0
#   UA_rFreq_bets - Total number of bets relative to the length of relationship (UA_Lor_firstPlay)
# Monetary:
#   UA_Mon_Stakes - Total monetary amount of stakes
#   UA_Mon_wins - Total monetary amount of wins
#   UA_Mon_profit - Total monetary amount of profit (wins - stakes)
#   UA_rMon_Stakes - Total monetary amount of stakes relative to the length of relationship (UA_Lor_firstPlay)
#   UA_rMon_wins - Total monetary amount of wins relative to the length of relationship (UA_Lor_firstPlay)
#   UA_rMon_profit - Total monetary amount of profit relative to the length of relationship (UA_Lor_firstPlay)
d4 <- d4 %>%
  group_by(UserID,ProductID) %>%
  summarise(UA_Recency = as.numeric(difftime(as.Date("2005-10-01"), max(Date), units="days")),
            UA_Recency_lastwin = min(Recency_lastwin[Recency_lastwin > 0]),
            UA_Recency_lastloss = min(Recency_lastloss),
            UA_IPT_std = sd(IPT_Days,na.rm = T),
            UA_IPT_min = min(IPT_Days,na.rm = T),
            UA_IPT_max = max(IPT_Days,na.rm = T),
            UA_IPT_mean = mean(IPT_Days,na.rm = T),
            UA_IPT_CV = UA_IPT_std/UA_IPT_mean,
            UA_Lor_firstPlay = as.numeric(difftime(max(Date), min(Date), units="days")),
            UA_Freq_bets = sum(Bets),
            UA_Freq_bets_first3Months = sum(Bets[month_range==1]),
            UA_Freq_bets_mid3Months = sum(Bets[month_range==2]),
            UA_Freq_bets_last2Months = sum(Bets[month_range==3]),
            UA_Freq_sessions_won = sum(Bets[Winnings > 0]),
            UA_Freq_sessions_lost = sum(Bets[Winnings == 0]),
            UA_rFreq_bets = UA_Freq_bets/UA_Lor_firstPlay,
            UA_Mon_Stakes = sum(Stakes),
            UA_Mon_wins = sum(Winnings),
            UA_Mon_profit = UA_Mon_wins - UA_Mon_Stakes,
            UA_rMon_Stakes = UA_Mon_Stakes/UA_Lor_firstPlay,
            UA_rMon_wins = UA_Mon_wins/UA_Lor_firstPlay,
            UA_rMon_profit = UA_Mon_profit/UA_Lor_firstPlay
            )

# Clean the variables created
d4$UA_Recency_lastwin[is.infinite(d4$UA_Recency_lastwin)] <- 0
d4$UA_IPT_min[is.infinite(d4$UA_IPT_min)] <- NA
d4$UA_IPT_max[is.infinite(d4$UA_IPT_max)] <- NA
d4$UA_IPT_mean[is.nan(d4$UA_IPT_mean)] <- NA
d4$UA_rFreq_bets[is.infinite(d4$UA_rFreq_bets)] <- NA
d4$UA_rMon_Stakes[is.infinite(d4$UA_rMon_Stakes)] <- NA
d4$UA_rMon_wins[is.infinite(d4$UA_rMon_wins)] <- NA
d4$UA_rMon_profit[is.infinite(d4$UA_rMon_profit)] <- NA

# Create a temporary dataframe for data manipulation/aggregation
d3 <- d3_Poker_Chip_Conversions

# As data is till 2005-09-31 so consider the data before 2005-10-01 only
d3 <- subset(d3, d3$TransDateTime < as.Date("2005-10-01"))

# Add the Transaction amount based on the User, TransDateTime and TransType
d3 <- d3 %>%
  group_by(UserID,TransDateTime,TransType) %>%
  summarise(TransAmount = sum(TransAmount)) %>%
  arrange(UserID,TransDateTime)

# Create new variables:
#   IPT_Days Difference in the successive gaming sessions
#   month_range Creating month range for first three months = 1, mid three months = 2 and the last two months as 3
d3 <- d3 %>%
  group_by(UserID) %>%
  mutate(IPT_Days = as.numeric(difftime(TransDateTime, lag(TransDateTime), units="days")),
         month_range = ifelse(TransDateTime < as.Date("2005-05-01"),1,ifelse(TransDateTime < as.Date("2005-08-01"),2,3)),
         num_of_records = 1)


# For Poker_Chips (PC)
# 
# Recency:
#   PC_Recency - Number of days since last betting activity
# Interpurchase time:
#   PC_IPT_std - Standard Deviation of the number of days between betting sessions
#   PC_IPT_min - Minimun number of days between betting sessions
#   PC_IPT_max - Maximum number of days between betting sessions
#   PC_IPT_mean - Average number of days between betting sessions
#   PC_IPT_CV - Coefficient of variation of interpurchase time (ratio of UA_IPT_std to UA_IPT_mean)
# Length of relationship:
#   PC_Lor_firstPlay - Number of days since first betting
# Frequency:
#   PC_Freq_sessions - Total number of sessions played
#   PC_Freq_sessions_first3Months - Total number of sessions from Feb to April
#   PC_Freq_sessions_mid3Months - Total number of sessions from May to July
#   PC_Freq_sessions_last2Months - Total number of sessions from August to September
#   PC_rFreq_sessions - Total number of sessions relative to the length of relationship (PC_Lor_firstPlay)
# Monetary:
#   PC_Mon_Buy - Total monetary amount of Buys
#   PC_Mon_Sell - Total monetary amount of Sells
#   PC_Mon_profit - Total monetary amount of profit (Sell - Buy)
#   PC_rMon_Buy - Total monetary amount of Buy relative to the length of relationship (PC_Lor_firstPlay)
#   PC_rMon_Sell - Total monetary amount of sell relative to the length of relationship (PC_Lor_firstPlay)
#   PC_rMon_profit - Total monetary amount of profit relative to the length of relationship (PC_Lor_firstPlay)
d3 <- d3 %>%
  group_by(UserID) %>%
  summarise(PC_Recency = as.numeric(difftime(as.Date("2005-10-01"), max(TransDateTime), units="days")),
            PC_IPT_std = sd(IPT_Days,na.rm = T),
            PC_IPT_min = min(IPT_Days,na.rm = T),
            PC_IPT_max = max(IPT_Days,na.rm = T),
            PC_IPT_mean = mean(IPT_Days,na.rm = T),
            PC_IPT_CV = PC_IPT_std/PC_IPT_mean,
            PC_Lor_firstPlay = as.numeric(difftime(max(TransDateTime), min(TransDateTime), units="days")),
            PC_Freq_sessions = sum(num_of_records),
            PC_Freq_sessions_first3Months = sum(num_of_records[month_range==1]),
            PC_Freq_sessions_mid3Months = sum(num_of_records[month_range==2]),
            PC_Freq_sessions_last2Months = sum(num_of_records[month_range==3]),
            PC_rFreq_sessions = PC_Freq_sessions/PC_Lor_firstPlay,
            PC_Mon_Buy = sum(TransAmount[TransType == "Buy"]),
            PC_Mon_Sell = sum(TransAmount[TransType == "Sell"]),
            PC_Mon_profit = PC_Mon_Sell - PC_Mon_Buy,
            PC_rMon_Buy = PC_Mon_Buy/PC_Lor_firstPlay,
            PC_rMon_Sell = PC_Mon_Sell/PC_Lor_firstPlay,
            PC_rMon_profit = PC_Mon_profit/PC_Lor_firstPlay
  )

# Clean the variables created
d3$PC_IPT_min[is.infinite(d3$PC_IPT_min)] <- NA
d3$PC_IPT_max[is.infinite(d3$PC_IPT_max)] <- NA
d3$PC_IPT_mean[is.nan(d3$PC_IPT_mean)] <- NA
d3$PC_rFreq_sessions[is.infinite(d3$PC_rFreq_sessions)] <- NA
d3$PC_rMon_Buy[is.infinite(d3$PC_rMon_Buy)] <- NA
d3$PC_rMon_Sell[is.infinite(d3$PC_rMon_Sell)] <- NA
d3$PC_rMon_profit[is.infinite(d3$PC_rMon_profit)] <- NA


# The data apart from AGE has been calculated from Raw datasets, 
# so we exclude all the data from Analytics set apart from AGE
d1 <- d1_Analytic_Data_Internet_Gambling
d1 <- d1[,c("USERID","AGE")]

# Removing all the date variables as we have already calculated RFM from Raw data sets
d2 <- d2_Demographics
d2 <- d2[,c("Language","Country","ApplicationID","UserID","RegDate","Gender")]

# Merging the data based in USERID
d12 <- merge(x=d1, y=d2, by.x = "USERID", by.y = "UserID", all.y = T)
d123 <- merge(x=d12, y=d3, by.x = "USERID", by.y = "UserID", all.x = T)
d1234 <- merge(x=d123, y=d4, by.x = "USERID", by.y = "UserID", all.x = T)

# Plot a Histogram for UA_IPT_CV
hist(d1234$UA_IPT_CV)

# Create a new variable churn based on following rulset-
# 1) If the UA_IPT_CV is NA it means the customer has gambled at most twice 
# in the entire data which means he is a tourist.
# 2) If the UA_IPT_CV is greater than 3 it means the customer has higher variability
# so he is a churner
# 3) If the UA_IPT_CV is less than 3 then the customer has low variability
# so he is a non-churner
#
#  Note: Value 3 has been selected based on the histogram plot for UA_IPT_CV
d1234$Churn <- ifelse(is.na(d1234$UA_IPT_CV),"Tourists",ifelse(d1234$UA_IPT_CV > 3,"Yes","No"))

# Save the Base Table
# write.csv(d1234,file=paste0(data_directory,"basetable.csv"), row.names = F)
