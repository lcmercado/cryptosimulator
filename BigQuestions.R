#Calculate if weightedaverage is lower than all runnning averages


#include only USDT tickers
FullDataUSDT <-  FullData %>% 
  filter(grepl('USDT', Ticker)) 

#Convert date to character
FullDataUSDT$Date <- as.character(FullDataUSDT$Date)

#Get Today date  
TodayDate <- paste(today(), "00:00:00")

#Subset FullDataUSDT to include only Today data
FullDataUSDTToday <- FullDataUSDT %>% 
  filter(Date==TodayDate) 

FullDataUSDTToday <- drop_na(FullDataUSDTToday)

#Boolean calculations
FullDataUSDTToday$Buy7 <- FullDataUSDTToday$weightedaverage < FullDataUSDTToday$RollingAvg7Days
FullDataUSDTToday$Buy30 <- FullDataUSDTToday$weightedaverage < FullDataUSDTToday$RollingAvg30Days
FullDataUSDTToday$Buy90 <- FullDataUSDTToday$weightedaverage < FullDataUSDTToday$RollingAvg90Days
FullDataUSDTToday$Buy180 <- FullDataUSDTToday$weightedaverage < FullDataUSDTToday$RollingAvg180Days
FullDataUSDTToday$Buy360 <- FullDataUSDTToday$weightedaverage < FullDataUSDTToday$RollingAvg360Days


#Compare current weighted price vs running averages. Higher scores reflects better buy price.
FullDataUSDTToday$Buy <- 
    FullDataUSDTToday$Buy7 +
    FullDataUSDTToday$Buy30 +
    FullDataUSDTToday$Buy90 +
    FullDataUSDTToday$Buy180 + 
    FullDataUSDTToday$Buy360


#DaysPerTicker
DaysPerTicker <- FullDataUSDT %>% 
  group_by(Ticker) %>% 
  summarise(DaysPerTicker=n())

View(DaysPerTicker)
View(FullDataUSDTToday)




