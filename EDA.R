## Columns 1,2,3,9,14,20
all_data = read.csv("data_for_daniel.csv", header = TRUE)
data = all_data[,c(1,2,3,9,14,20)]
install.packages("lubridate")
library(lubridate)
## Check lubridate is working
dmy(data$Date[1])
library(ggplot2)
## Find the NA values in the data and remove
which(is.na(data))
data[which(is.na(data)),]
which(is.na(data$Day))
which(is.na(data$Date))
which(is.na(data$NO_CWV))
which(is.na(data$NO_DM))
which(is.na(data$NE_CWV))
which(is.na(data$NE_DM))
data[3103,]
data = na.omit(data)
which(duplicated(data$Date))
## No duplicated dates in the data
## Set the Date column in the correct format
data$Date = dmy(data$Date)

## Create box plots of the data with the days in order Monday-Sunday
level_order = c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday")
ggplot(data = data) + 
  geom_boxplot(mapping = aes(x = factor(Day, level = level_order), y = NO_CWV)) +
  xlab("") + ylab("CWV Albermarle")
ggplot(data = data) + 
  geom_boxplot(mapping = aes(x = factor(Day, level = level_order), y = NE_CWV))+
  xlab("") + ylab("CWV Nottingham Watnall")
ggplot(data = data) + 
  geom_boxplot(mapping = aes(x = factor(Day, level = level_order), y = NO_DM)) + 
  xlab("") + ylab("DM Albermarle")
ggplot(data = data) + 
  geom_boxplot(mapping = aes(x = factor(Day, level = level_order), y = NE_DM)) + 
  xlab("") + ylab("DM Nottingham Watnall")

## Create scatter plots of relationships within variables (CWV and DM)
ggplot(data = data) + geom_point(mapping = aes(x = NO_CWV, y = NE_CWV))
ggplot(data = data) + geom_point(mapping = aes(x = NO_DM, y = NE_DM))
## Now within locations (NO and NE)
ggplot(data = data) + geom_point(mapping = aes(x = NO_CWV, y = NO_DM))
ggplot(data = data) + geom_point(mapping = aes(x = NE_CWV, y = NE_DM))

## Create some line plots of the data
ggplot(data = data, mapping = aes(x = Date, y = NO_CWV)) + geom_line()
ggplot(data = data, mapping = aes(x = Date, y = NE_CWV)) + geom_line()
ggplot(data = data, mapping = aes(x = Date, y = NO_DM)) + geom_line()
ggplot(data = data, mapping = aes(x = Date, y = NE_DM)) + geom_line()

## Import the holiday dates
holidays = read.csv("holidays.csv", header = TRUE)
which(is.na(holidays))  ## No missing values
bank_holidays = holidays[which(holidays$Bank.Holiday==TRUE),]
## Need to change the date column for the data frame so they are in the same format
bank_holidays$Date = ymd(bank_holidays$Date)
bank_hols_data = match_df(data, bank_holidays)
data = join(data, bank_holidays, by = "Date")
data$Holiday = NULL
data$Type = NULL
library(tidyr)
data$Bank.Holiday = replace_na(data$Bank.Holiday,"FALSE")

## Now can plot the bank holidays against other days
ggplot(data = data) + geom_point(mapping = aes(x = NO_CWV, y = NE_CWV, colour = factor(Bank.Holiday)))
ggplot(data = data) + geom_point(mapping = aes(x = NO_DM, y = NE_DM, colour = factor(Bank.Holiday)))
ggplot(data = data) + geom_point(mapping = aes(x = NO_CWV, y = NO_DM, colour = factor(Bank.Holiday)))
ggplot(data = data) + geom_point(mapping = aes(x = NE_CWV, y = NE_DM, colour = factor(Bank.Holiday)))

## Look at more ways to represent the differences (if any) with bank holidays
ggplot(data = data, mapping = aes(x = Date, y = NO_CWV, colour = factor(Bank.Holiday))) + geom_line()




## Extract the dates
index = dmy(data$Date)
## Create a time series object - need to check for leap years because of frequency
NO_CWV_ts = ts(data$NO_CWV, start = c(2008, as.numeric(format(inds[1], "%j"))),
               frequency = 365)
NO_CWV_ts

