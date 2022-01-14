Sys.setenv(tz = "UTC")
from    <- as.POSIXct("2021-04-20 00:00:00 UTC")
TodayDate <- paste0(as.character(today()), " 22:00:00 UTC")
to      <- as.POSIXct(TodayDate)
period  <- "5M" #4H D


#Setup placeholder data frame
FullDataHourly <- data.frame(
  high = numeric(),
  low = numeric(),
  open = numeric(),
  close = numeric(),
  volume = numeric(),
  quotevolume = numeric(),
  weightdaverage = numeric(),
  Date = character(),
  Ticker = character()
)


#Pull tickers and rbind together
for (i in 1:length(tickerList)) {
  pair <- tickerList[i]
  print(paste(tickerList[i], i))
  tempData <- ReturnChartData(
    theObject = poloniex.public,
    pair      = pair,
    from      = from,
    to        = to,
    period    = period
  )
  tempDataDates <-  index(tempData)
  tempData <- as_tibble(tempData)
  tempData$Date <- tempDataDates
  tempData$Ticker <- tickerList[i]
  
  FullDataHourly <- rbind(FullDataHourly, tempData)
}

#Calculate daily change
FullDataHourlyDF <- FullDataHourly #Backup for dev
FullDataHourly <- as_tibble(FullDataHourly)

#Running sequential calculation per ticker. First one per pair goes blank.
FullDataHourly <- FullDataHourly %>%
  group_by(Ticker) %>%
  mutate(Change = (close / lag(close)) - 1) 



#Rolling averages

FullDataHourly <- FullDataHourly %>% 
  mutate(RollingAvg7Days=rollapply(weightedaverage,7,mean, align="right", fill=NA)) %>% 
  mutate(RollingAvg30Days=rollapply(weightedaverage,30,mean, align="right", fill=NA)) %>% 
  mutate(RollingAvg90Days=rollapply(weightedaverage,90,mean, align="right", fill=NA)) %>% 
  mutate(RollingAvg180Days=rollapply(weightedaverage,180,mean, align="right", fill=NA)) %>% 
  mutate(RollingAvg360Days=rollapply(weightedaverage,360,mean,align="right", fill=NA))


#Calculate range of price movement
FullDataHourly$Range <- FullDataHourly$high - FullDataHourly$low



write.csv(FullDataHourly, file="FullDataHourly.csv")
