#####SETUP####
#devtools::install_github("VermeirJellen/PoloniexR")
library(PoloniexR)
library(zoo)
library(tidyverse)
library(lubridate)

#Cbind.Fill function
cbind.fill <- function(...){
  nm <- list(...) 
  nm <- lapply(nm, as.matrix)
  n <- max(sapply(nm, nrow)) 
  do.call(cbind, lapply(nm, function (x) 
    rbind(x, matrix(, n-nrow(x), ncol(x))))) 
}

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


####DOWNLOAD DATA####
#Make this a data frame containing "5M", "15M", "30M", "2H", "4H", "D"
Sys.setenv(tz = "UTC")
from    <- as.POSIXct("2021-04-01 00:00:00 UTC")
TodayDate <- paste0(as.character(today()), " 22:00:00 UTC")
to      <- as.POSIXct(TodayDate)
period  <- "4H" # "5M", "15M", "30M", "2H", "4H", "D"


#Setup placeholder data frame
FullDataTimely <- data.frame(
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
  
  FullDataTimely <- rbind(FullDataTimely, tempData)
}

#Change from UTC to Chicago time zone -5
FullDataTimely$Date <- with_tz(FullDataTimely$Date, tzone = "America/Chicago")

####Calculate changes####
#Calculate  change
FullDataTimelyDF <- FullDataTimely #Backup for dev
FullDataTimely <- as_tibble(FullDataTimely)

#Running sequential calculation per ticker. First one per pair goes blank.
FullDataTimely <- FullDataTimely %>%
  group_by(Ticker) %>%
  mutate(Change = (close / lag(close)) - 1) 


#Rolling averages
FullDataTimely <- FullDataTimely %>% 
  mutate(RollingAvg7Days=rollapply(weightedaverage,7,mean, align="right", fill=NA)) %>% 
  mutate(RollingAvg30Days=rollapply(weightedaverage,30,mean, align="right", fill=NA)) %>% 
  mutate(RollingAvg90Days=rollapply(weightedaverage,90,mean, align="right", fill=NA)) %>% 
  mutate(RollingAvg180Days=rollapply(weightedaverage,180,mean, align="right", fill=NA)) %>% 
  mutate(RollingAvg360Days=rollapply(weightedaverage,360,mean,align="right", fill=NA))


#Calculate range of price movement
FullDataTimely$Range <- FullDataTimely$high - FullDataTimely$low


####EXPORT DATA####
fileName <- paste0("data/FullDataTimely",period,".csv")
write.csv(FullDataTimely, fileName)


####SIMULATOR####

#Select entry point (date/time)
#EntryPoints <- unique(FullDataTimely$Date)

#This should be automatically selected by choosing the Change entry point
#SelectedEntryPoint <- EntryPoints[5799] 

#Start loop with different Entry Points

EntryPoints <- seq(-.05,.05,by=.01)
#ExitConditions <- ExitConditions <- seq(.01, .1, by=.001)
#Ticker <- tickerList
#StartPeriod
#EndPeriod

SimulationResults <- tibble(
  Ticker = character(),
  EntryPoint = numeric(),
  FinalProfit = numeric(),
  BuyTradeCount = numeric(),
  SellTradeCount = numeric()
)

MasterTradesMatrix <- tibble(
  EntryPoint = numeric(),
  BuyTradeDates = character(),
  SellTradeDates = character(),
  WaitingTimeHours = numeric(),
  BuyTrades = character(),
  SellTrades = character(),
  BuyPrices = character(),
  SellPrices = character(),
  Profits = character(),
  
)

for (j in 1:length(EntryPoints)){
  
EntryPoint <- EntryPoints[j]
SelectedTicker <- "USDT_XRP"
print(EntryPoint)


#Select condition for investment (1.5%,2%,2.5%, etc.)
ChosenExitCondition <- .015

#Input amount invested
AmountInvested <- 1000

#Select amount of reinvestments
Reinvestments <- 3

#Select time period
#StartDate <-  ymd_hms("2021-05-08 00:50:50")


#Calculate total profit/loss
SelectedData <- FullDataTimely %>% 
  filter(Ticker==SelectedTicker)


#Remove first row as it has NA change
SelectedData <- SelectedData[-1,]


##Remove everything
remove(SellTrade)
remove(SellPrice)
remove(BuyTrade)
remove(BuyPrice)

#Variables setup
FinalProfit <- 0
BuyTradeCount <- 0
SellTradeCount <- 0

BuyTrades <- c()
SellTrades <- c()

BuyPrices <- c()
SellPrices <- c()

BuyTradeDates <- c()
SellTradeDates <- c()

Profits <- c()


for (i in 1:nrow(SelectedData)){
  DaysPassed <- i
  print(DaysPassed)
  
  if (SelectedData$Change[i] <= EntryPoint & !exists("BuyTrade")){
    BuyTrade <- AmountInvested / SelectedData$close[i]
    BuyPrice <- SelectedData$close[i]
    BuyPrices <- c(BuyPrices, BuyPrice)
    SellPrice <- BuyPrice + (BuyPrice*ChosenExitCondition)
    BuyTradeCount <- BuyTradeCount+1
    BuyTradeDates <- c(BuyTradeDates,as.character(SelectedData$Date[i]))
  }
  
  if (exists("SellPrice")){
    if(SelectedData$close[i] >= SellPrice){
    SellTrade <- BuyTrade * SelectedData$close[i]
    SellPrices <- c(SellPrices,SelectedData$close[i])
    SellTradeCount <- SellTradeCount+1
    SellTradeDates <- c(SellTradeDates,as.character(SelectedData$Date[i]))
    BuyTrades <- c(BuyTrades,BuyTrade)
    remove(BuyTrade)
    remove(BuyPrice)
    if (exists("SellTrade")){
      SellTrades <- c(SellTrades,SellTrade)
      FinalProfit <- FinalProfit + (SellTrade-AmountInvested)
      Profits <- c(Profits,SellTrade-AmountInvested)
      remove(SellTrade)
      remove(SellPrice)
    }
    }
}
}


TradesMatrix <- as_tibble(cbind.fill(BuyTradeDates,SellTradeDates,BuyTrades,SellTrades,BuyPrices,SellPrices, Profits))
colnames(TradesMatrix) <- c("BuyTradeDates","SellTradeDates","BuyTrades","SellTrades","BuyPrices","SellPrices", "Profits")

TradesMatrix$WaitingTimeHours <- as.double(as_datetime(TradesMatrix$SellTradeDates)- as_datetime(TradesMatrix$BuyTradeDates), units = "hours")

TradesMatrix <- TradesMatrix[,c(1,2,8,3:7)]

TradesMatrix$EntryPoint <- EntryPoint

MasterTradesMatrix <- rbind(MasterTradesMatrix,TradesMatrix)


SimulationRecord <- c(EntryPoint,FinalProfit,BuyTradeCount,SellTradeCount)

SimulationResults <- rbind(SimulationResults,SimulationRecord)

FinalProfit
BuyTradeCount
SellTradeCount
BuyTradeDates
SellTradeDates
BuyTrades
SellTrades
BuyPrices
SellPrices
Profits
from
to

#View(TradesMatrix)



}


#ROADMAP
#Add stochastic waiting periods.
#Add recovery points at a loss
##Add random waiting times/skips for trades. Sometimes waiting is the move.
##Add indicator to take into account running averages crossing (30,90,360)
##Add indicator for previous supports/resistances
##Add indicator to double wait 2 entry points (-4% + -4%) when the weighted price is below 3 running averages
##Check and compare speed of down trend by the minute 
##Maybe wait to see if there's a flash deep to buy cheaper within the next 1-5 minutes after the entry point is reached
##Ability to iterate through multiple tickers
##Ability to iterate through multiple time periods (5M,15M,1H,4H,12H,D)
##Ability to iterate through multiple random waiting periods
##Ability to iterate through multiple exit conditions
##Ability to iterate through multiple time periods
##Ability to iterate through multiple running averages

####PROBABILITIES CALCULATOR####
##Ability to calculate likelyhood of success
