#devtools::install_github("VermeirJellen/PoloniexR")
library(PoloniexR)
library(zoo)
library(tidyverse)
library(lubridate)
poloniex.public <- PoloniexPublicAPI()
ticker.info     <- ReturnTicker(poloniex.public)

ticker.info$Ticker <- rownames(ticker.info)
rownames(ticker.info) <- NULL
ticker.info <- ticker.info[, c("Ticker", "last", "percentChange")]

#Remove non USDT pairs
ticker.info <- ticker.info %>% 
  filter(stringr::str_detect(Ticker, 'USDT') ) %>% 
  filter(!stringr::str_detect(Ticker, 'BEAR') ) %>% 
  filter(!stringr::str_detect(Ticker, 'BULL') )

tickerList <- ticker.info$Ticker



Sys.setenv(tz = "UTC")
from    <- as.POSIXct("2017-01-01 00:00:00 UTC")
TodayDate <- paste0(as.character(today()), " 00:00:00 UTC")
to      <- as.POSIXct(TodayDate)
period  <- "D" #4H D


#Setup placeholder data frame
FullData <- data.frame(
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
  
  FullData <- rbind(FullData, tempData)
}

#Calculate daily change
FullDataDF <- FullData #Backup for dev
FullData <- as_tibble(FullData)

#Running sequential calculation per ticker. First one per pair goes blank.
FullData <- FullData %>%
  group_by(Ticker) %>%
  mutate(Change = (close / lag(close)) - 1) 
  


#Rolling averages
FullData <- FullData %>% 
  mutate(RollingAvg7Days=rollapply(weightedaverage,7,mean, align="right", fill=NA)) %>% #close
  mutate(RollingAvg30Days=rollapply(weightedaverage,30,mean, align="right", fill=NA)) %>% 
  mutate(RollingAvg90Days=rollapply(weightedaverage,90,mean, align="right", fill=NA)) %>% 
  mutate(RollingAvg180Days=rollapply(weightedaverage,180,mean, align="right", fill=NA)) %>% 
  mutate(RollingAvg360Days=rollapply(weightedaverage,360,mean,align="right", fill=NA))


#Calculate range of price movement
FullData$Range <- FullData$high - FullData$low
    
write.csv(FullData, file="FullData.csv")
