## Columns 1,2,3,9,14,20
all_data = read.csv("data_for_daniel.csv", header = TRUE)
data = all_data[,c(1,2,3,9,14,20)]
install.packages("lubridate")
install.packages("zoo")
library(lubridate)
library(ggplot2)
library(tidyr)
library(plyr)
library(dplyr)
library(zoo)
## Check lubridate is working
dmy(data$Date[1])
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
## Check negative values
which(data$NO_CWV<0)
which(data$NO_DM<0)
which(data$NE_CWV<0)
which(data$NE_DM<0)
## Some values where the CWV is less than 0 in either region
data[which(data$NO_CWV<0),]
## All dates within November - March so likely just particularly bad winter weather
data[which(data$NE_CWV<0),]
## Again all dates fall within November - March
## Set the Date column in the correct format
data$Date = dmy(data$Date)

## Create box plots of the data with the days in order Monday-Sunday
level_order = c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday")
ggplot(data = data) + 
  geom_boxplot(mapping = aes(x = factor(Day, level = level_order), y = NO_CWV)) +
  xlab("")
ggplot(data = data) + 
  geom_boxplot(mapping = aes(x = factor(Day, level = level_order), y = NE_CWV))+
  xlab("")
ggplot(data = data) + 
  geom_boxplot(mapping = aes(x = factor(Day, level = level_order), y = NO_DM)) + 
  xlab("")
ggplot(data = data) + 
  geom_boxplot(mapping = aes(x = factor(Day, level = level_order), y = NE_DM)) + 
  xlab("")

## Create scatter plots of relationships within variables (CWV and DM)
ggplot(data = data) + geom_point(mapping = aes(x = NO_CWV, y = NE_CWV), size = 0.5)
ggplot(data = data) + geom_point(mapping = aes(x = NO_DM, y = NE_DM), size = 0.5)
## Now within locations (NO and NE)
ggplot(data = data) + geom_point(mapping = aes(x = NO_CWV, y = NO_DM), size = 0.4)
ggplot(data = data) + geom_point(mapping = aes(x = NE_CWV, y = NE_DM), size = 0.4)

## Perform K-means on these scatter plots (for DM)
NO_data = data.frame(cbind(data$NO_DM, data$NO_CWV))
colnames(NO_data) = c("NO_DM", "NO_CWV")
km_NO = kmeans(NO_data, 4, iter.max = 50, nstart = 20)
pca_km_NO = prcomp(x = NO_data)
plot(pca_km_NO$x[,1], pca_km_NO$x[,2], col = km_NO$cluster, pch = km_NO$cluster)
NO_data$cluster = km_NO$cluster
ggplot() + 
  geom_point(data = NO_data, 
             mapping = aes(x = NO_CWV, y = NO_DM, colour = factor(cluster))) + 
  scale_colour_brewer(name = "Cluster", palette = "RdGy")
  
NE_data = data.frame(cbind(data$NE_DM, data$NE_CWV))
colnames(NE_data) = c("NE_DM", "NE_CWV")
km_NE = kmeans(NE_data, 4, iter.max = 50, nstart = 20)
pca_km_NE = prcomp(x = NE_data)
plot(pca_km_NE$x[,1], pca_km_NE$x[,2], col = km_NE$cluster, pch = km_NE$cluster)
NE_data$cluster = km_NE$cluster
ggplot() + 
  geom_point(data = NE_data, 
             mapping = aes(x = NE_CWV, y = NE_DM, colour = factor(cluster))) + 
  scale_colour_brewer(name = "Cluster", palette = "RdGy")

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
data$Bank.Holiday = replace_na(data$Bank.Holiday,"FALSE")

## Now can plot the bank holidays against other days
ggplot(data = data) + geom_point(mapping = aes(x = NO_CWV, y = NE_CWV, colour = factor(Bank.Holiday)))
ggplot(data = data) + 
  geom_point(mapping = aes(x = NO_DM, y = NE_DM, colour = factor(Bank.Holiday)), size = 0.4) + 
  scale_colour_discrete(limits = c("FALSE", "TRUE"), labels = c("Non-bank holiday", "Bank holiday"),
                        type = c("skyblue", "black")) + 
  theme(legend.title = element_blank(), legend.position = "bottom")
ggplot(data = data) + 
  geom_point(mapping = aes(x = NO_CWV, y = NO_DM, colour = factor(Bank.Holiday)), size = 0.4) + 
  scale_colour_discrete(limits = c("FALSE", "TRUE"), labels = c("Non-bank holiday", "Bank holiday"),
                        type = c("skyblue", "black")) + 
  theme(legend.title = element_blank(), legend.position = "bottom")
ggplot(data = data) + 
  geom_point(mapping = aes(x = NE_CWV, y = NE_DM, colour = factor(Bank.Holiday)), size = 0.4) + 
  scale_colour_discrete(limits = c("FALSE", "TRUE"), labels = c("Non-bank holiday", "Bank holiday"),
                        type = c("skyblue", "black")) + 
  theme(legend.title = element_blank(), legend.position = "bottom")

## Line plots which separate bank holidays and non-bank holidays
ggplot(data = data, mapping = aes(x = Date, y = NO_DM, colour = factor(Bank.Holiday))) + 
  geom_line() + scale_colour_discrete(limits = c("FALSE", "TRUE"), labels = c("Non-bank holiday", "Bank holiday"),
                                      type = c("skyblue", "black")) + 
  theme(legend.title = element_blank(), legend.position = "bottom")
ggplot(data = data, mapping = aes(x = Date, y = NE_DM, colour = factor(Bank.Holiday))) + 
  geom_line() + scale_colour_discrete(limits = c("FALSE", "TRUE"), labels = c("Non-bank holiday", "Bank holiday"),
                                      type = c("skyblue", "black")) + 
  theme(legend.title = element_blank(), legend.position = "bottom")

## Change the black line to points for the bank holidays
ggplot(data = data, mapping = aes(x = Date, y = NO_DM)) + 
  geom_line(data = data[which(data$Bank.Holiday==FALSE),], aes(colour = factor(Bank.Holiday))) + 
  geom_point(data = data[which(data$Bank.Holiday==TRUE),], aes(shape = factor(Bank.Holiday))) + 
  theme(legend.position = "bottom") + 
  labs(colour = "legend") + 
  scale_colour_discrete(name = "", breaks = c("TRUE", "FALSE"), 
                        labels = c("Bank holiday", "Non-bank holiday"),
                        type = c("skyblue", "black")) +
  scale_shape_discrete(name = "", breaks = c("TRUE", "FALSE"), 
                        labels = c("Bank holiday", "Non-bank holiday"))

ggplot(data = data, mapping = aes(x = Date, y = NE_DM)) + 
  geom_line(data = data[which(data$Bank.Holiday==FALSE),], aes(colour = factor(Bank.Holiday))) + 
  geom_point(data = data[which(data$Bank.Holiday==TRUE),], aes(shape = factor(Bank.Holiday))) + 
  theme(legend.position = "bottom") + 
  labs(colour = "legend") + 
  scale_colour_discrete(name = "", breaks = c("TRUE", "FALSE"), 
                        labels = c("Bank holiday", "Non-bank holiday"),
                        type = c("skyblue", "black")) +
  scale_shape_discrete(name = "", breaks = c("TRUE", "FALSE"), 
                       labels = c("Bank holiday", "Non-bank holiday"))

## Plot each year over the top of each other
ggplot(data = data, aes(format(Date, format = "%m-%d"), NO_DM, group = factor(year(Date)),
       colour = factor(year(Date)))) + geom_line()
ggplot(data = data, aes(format(Date, format = "%m-%d"), NE_DM, group = factor(year(Date)),
                        colour = factor(year(Date)))) + geom_line()

## Box plots separating the bank holidays and non bank holidays
ggplot(data = data) + 
  geom_boxplot(mapping = aes(x = Bank.Holiday, y = NO_CWV), fill = c("skyblue", "darkgrey")) + xlab("") +  
  scale_x_discrete(labels = c("FALSE" = "Non-bank holiday", "TRUE" = "Bank holiday"))
ggplot(data = data) + 
  geom_boxplot(mapping = aes(x = Bank.Holiday, y = NE_CWV), fill = c("skyblue", "darkgrey")) + xlab("") + 
  scale_x_discrete(labels = c("FALSE" = "Non-bank holiday", "TRUE" = "Bank holiday"))

ggplot(data = data) + geom_boxplot(mapping = aes(x = Bank.Holiday, y = NO_DM),
                                   fill = c("skyblue", "darkgrey")) + xlab("") +
  scale_x_discrete(labels = c("FALSE" = "Non-bank holiday", "TRUE" = "Bank holiday"))
ggplot(data = data) + geom_boxplot(mapping = aes(x = Bank.Holiday, y = NE_DM),
                                   fill = c("skyblue", "darkgrey")) + xlab("") + 
  scale_x_discrete(labels = c("FALSE" = "Non-bank holiday", "TRUE" = "Bank holiday"))


bank_hols_vector = which(data$Bank.Holiday == "TRUE")
weekends_vector = sort(c(which(data$Day == "Saturday"), which(data$Day == "Sunday")))
data$day_type = NA
data$day_type[bank_hols_vector] = "Bank holiday"
data$day_type[weekends_vector] = "Weekend"
data$day_type = replace_na(data$day_type, "Weekday")

## Now can separate into weekdays, weekends and bank holidays
ggplot(data = data) + geom_boxplot(mapping = aes(x = day_type, y = NO_DM),
                                   fill = c("darkgrey", "gold", "darkorange")) + xlab("")
ggplot(data = data) + geom_boxplot(mapping = aes(x = day_type, y = NE_DM),
                                   fill = c("darkgrey", "gold", "darkorange")) + xlab("")

ggplot(data = data, mapping = aes(x = Date, y = NO_DM, colour = factor(day_type))) + 
  geom_line() + scale_colour_discrete(limits = c("Bank holiday", "Weekday", "Weekend"),
                                      type = c("black", "gold", "darkorange")) + 
  theme(legend.title = element_blank(), legend.position = "bottom")
ggplot(data = data, mapping = aes(x = Date, y = NE_DM, colour = factor(day_type))) + 
  geom_line() + scale_colour_discrete(limits = c("Bank holiday", "Weekday", "Weekend"),
                                      type = c("black", "gold", "darkorange")) + 
  theme(legend.title = element_blank(), legend.position = "bottom")

## Look at correlation coefficients
cor(data$NO_CWV, data$NO_DM)
#cor(data$NO_CWV, data$NO_DM, method = "kendall")
cor(data$NO_CWV, data$NO_DM, method = "spearman")
cor(data$NE_CWV, data$NE_DM)
#cor(data$NE_CWV, data$NE_DM, method = "kendall")
cor(data$NE_CWV, data$NE_DM, method = "spearman")
cor(data$NO_DM, data$NE_DM)
#cor(data$NO_DM, data$NE_DM, method = "kendall")
cor(data$NO_DM, data$NE_DM, method = "spearman")

cor.test(data$NO_CWV, data$NO_DM)
cor.test(data$NO_CWV, data$NO_DM, method = "spearman")
cor.test(data$NE_CWV, data$NE_DM)
cor.test(data$NE_CWV, data$NE_DM, method = "spearman")
cor.test(data$NO_DM, data$NE_DM)
cor.test(data$NO_DM, data$NE_DM, method = "spearman")


## More correlations based on "day type"
cor(data$NO_DM[which(data$day_type == "Bank holiday")], 
    data$NE_DM[which(data$day_type == "Bank holiday")])
cor(data$NO_DM[which(data$day_type == "Bank holiday")], 
    data$NE_DM[which(data$day_type == "Bank holiday")], method = "kendall")
cor(data$NO_DM[which(data$day_type == "Bank holiday")], 
    data$NE_DM[which(data$day_type == "Bank holiday")], method = "spearman")
cor(data$NO_DM[which(data$day_type == "Weekday")], 
    data$NE_DM[which(data$day_type == "Weekday")])
cor(data$NO_DM[which(data$day_type == "Weekday")], 
    data$NE_DM[which(data$day_type == "Weekday")], method = "kendall")
cor(data$NO_DM[which(data$day_type == "Weekday")], 
    data$NE_DM[which(data$day_type == "Weekday")], method = "spearman")
cor(data$NO_DM[which(data$day_type == "Weekend")], 
    data$NE_DM[which(data$day_type == "Weekend")])
cor(data$NO_DM[which(data$day_type == "Weekend")], 
    data$NE_DM[which(data$day_type == "Weekend")], method = "kendall")
cor(data$NO_DM[which(data$day_type == "Weekend")], 
    data$NE_DM[which(data$day_type == "Weekend")], method = "spearman")

## Correlations of "day type" for the North
cor(data$NO_CWV[which(data$day_type == "Bank holiday")], 
    data$NO_DM[which(data$day_type == "Bank holiday")])
cor(data$NO_CWV[which(data$day_type == "Bank holiday")], 
    data$NO_DM[which(data$day_type == "Bank holiday")], method = "kendall")
cor(data$NO_CWV[which(data$day_type == "Bank holiday")], 
    data$NO_DM[which(data$day_type == "Bank holiday")], method = "spearman")
cor(data$NO_CWV[which(data$day_type == "Weekday")], 
    data$NO_DM[which(data$day_type == "Weekday")])
cor(data$NO_CWV[which(data$day_type == "Weekday")], 
    data$NO_DM[which(data$day_type == "Weekday")], method = "kendall")
cor(data$NO_CWV[which(data$day_type == "Weekday")], 
    data$NO_DM[which(data$day_type == "Weekday")], method = "spearman")
cor(data$NO_CWV[which(data$day_type == "Weekend")], 
    data$NO_DM[which(data$day_type == "Weekend")])
cor(data$NO_CWV[which(data$day_type == "Weekend")], 
    data$NO_DM[which(data$day_type == "Weekend")], method = "kendall")
cor(data$NO_CWV[which(data$day_type == "Weekend")], 
    data$NO_DM[which(data$day_type == "Weekend")], method = "spearman")

## Perform cor.test on these
cor.test(data$NO_CWV[which(data$day_type == "Bank holiday")], 
    data$NO_DM[which(data$day_type == "Bank holiday")])
cor.test(data$NO_CWV[which(data$day_type == "Bank holiday")], 
    data$NO_DM[which(data$day_type == "Bank holiday")], method = "kendall")
cor.test(data$NO_CWV[which(data$day_type == "Bank holiday")], 
    data$NO_DM[which(data$day_type == "Bank holiday")], method = "spearman")
cor.test(data$NO_CWV[which(data$day_type == "Weekday")], 
    data$NO_DM[which(data$day_type == "Weekday")])
cor.test(data$NO_CWV[which(data$day_type == "Weekday")], 
    data$NO_DM[which(data$day_type == "Weekday")], method = "kendall")
cor.test(data$NO_CWV[which(data$day_type == "Weekday")], 
    data$NO_DM[which(data$day_type == "Weekday")], method = "spearman")
cor.test(data$NO_CWV[which(data$day_type == "Weekend")], 
    data$NO_DM[which(data$day_type == "Weekend")])
cor.test(data$NO_CWV[which(data$day_type == "Weekend")], 
    data$NO_DM[which(data$day_type == "Weekend")], method = "kendall")
cor.test(data$NO_CWV[which(data$day_type == "Weekend")], 
    data$NO_DM[which(data$day_type == "Weekend")], method = "spearman")

## Correlations of "day type" for the North East
cor(data$NE_CWV[which(data$day_type == "Bank holiday")], 
    data$NE_DM[which(data$day_type == "Bank holiday")])
cor(data$NE_CWV[which(data$day_type == "Bank holiday")], 
    data$NE_DM[which(data$day_type == "Bank holiday")], method = "kendall")
cor(data$NE_CWV[which(data$day_type == "Bank holiday")], 
    data$NE_DM[which(data$day_type == "Bank holiday")], method = "spearman")
cor(data$NE_CWV[which(data$day_type == "Weekday")], 
    data$NE_DM[which(data$day_type == "Weekday")])
cor(data$NE_CWV[which(data$day_type == "Weekday")], 
    data$NE_DM[which(data$day_type == "Weekday")], method = "kendall")
cor(data$NE_CWV[which(data$day_type == "Weekday")], 
    data$NE_DM[which(data$day_type == "Weekday")], method = "spearman")
cor(data$NE_CWV[which(data$day_type == "Weekend")], 
    data$NE_DM[which(data$day_type == "Weekend")])
cor(data$NE_CWV[which(data$day_type == "Weekend")], 
    data$NE_DM[which(data$day_type == "Weekend")], method = "kendall")
cor(data$NE_CWV[which(data$day_type == "Weekend")], 
    data$NE_DM[which(data$day_type == "Weekend")], method = "spearman")

## Perform cor.test on the above correlation coefficients
cor.test(data$NE_CWV[which(data$day_type == "Bank holiday")], 
    data$NE_DM[which(data$day_type == "Bank holiday")])
cor.test(data$NE_CWV[which(data$day_type == "Bank holiday")], 
    data$NE_DM[which(data$day_type == "Bank holiday")], method = "kendall")
cor.test(data$NE_CWV[which(data$day_type == "Bank holiday")], 
    data$NE_DM[which(data$day_type == "Bank holiday")], method = "spearman")
cor.test(data$NE_CWV[which(data$day_type == "Weekday")], 
    data$NE_DM[which(data$day_type == "Weekday")])
cor.test(data$NE_CWV[which(data$day_type == "Weekday")], 
    data$NE_DM[which(data$day_type == "Weekday")], method = "kendall")
cor.test(data$NE_CWV[which(data$day_type == "Weekday")], 
    data$NE_DM[which(data$day_type == "Weekday")], method = "spearman")
cor.test(data$NE_CWV[which(data$day_type == "Weekend")], 
    data$NE_DM[which(data$day_type == "Weekend")])
cor.test(data$NE_CWV[which(data$day_type == "Weekend")], 
    data$NE_DM[which(data$day_type == "Weekend")], method = "kendall")
cor.test(data$NE_CWV[which(data$day_type == "Weekend")], 
    data$NE_DM[which(data$day_type == "Weekend")], method = "spearman")


## Plot the CSV and DM together
ggplot(data = data, aes(x = Date)) + 
  geom_line(mapping = aes(y = NO_DM, colour = "NO_DM")) + 
  geom_line(mapping = aes(y = NO_CWV*2192.245, colour = "NO_CWV")) + 
  scale_y_continuous(sec.axis = sec_axis(trans = ~./2192.245, name = "NO_CWV")) +
  theme(legend.title = element_blank(), legend.position = "right") +
  scale_color_manual(values = c("NO_CWV" = "darkgrey", "NO_DM" = "purple"))

ggplot(data = data, aes(x = Date)) + 
  geom_line(mapping = aes(y = NE_DM, color = "NE_DM")) + 
  geom_line(mapping = aes(y = NE_CWV*2346.609, color = "NE_CWV")) + 
  scale_y_continuous(sec.axis = sec_axis(trans = ~./2346.609, name = "NE_CWV")) +
  theme(legend.title = element_blank(), legend.position = "right") +
  scale_color_manual(values = c("NE_CWV" = "darkgrey", "NE_DM" = "purple"))

data$NE_DM[1]/data$NE_CWV[1]
data$NO_DM[1]/data$NO_CWV[1]

