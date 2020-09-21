

###############################################################################################
#################Extracting technical indicators for IBM ######################################
###############################################################################################



library(quantmod)
getSymbols(Symbols = "IBM",
           src = "yahoo",
           from="2016-01-01",
           to="2019-10-01",
)
#################### Visualisation ###########################
barChart(IBM)
chartSeries(IBM)
addMACD()
addBBands(n=20,sd=2,ma='SMA',draw='bands',on=-1)
##############################################################

library("TTR")
IBM_df = data.frame(value=coredata(IBM),timestamp=index(IBM))
data = IBM_df
data$IBM_ret <- Delt(data$value.IBM.Adjusted)
library(dplyr)
data$Return_lead <-lead(data$IBM_ret,1)
head(data); nrow(data); colnames(data)
sma20 <- SMA(data[c('value.IBM.Close')],n=20) # 20-day moving average
head(sma20, n=50) # show first 50 values
ema14 = EMA(data[c('value.IBM.Close')],n=14) #14-day EMA
head(ema14,n=20)
bb20 = BBands(data[c('value.IBM.Close')], sd=2.0) #bolliger band, default is 20day
head(bb20,n=30)
dataPlusBB = data.frame(data,bb20) #create a new data frame with all input data, plus Bollinger Bands data.
plot(dataPlusBB$timestamp,data$value.IBM.Close)
lines(dataPlusBB$timestamp,dataPlusBB$value.IBM.Close, col = 'red')
lines(dataPlusBB$timestamp,dataPlusBB$up, col = 'purple')
lines(dataPlusBB$timestamp,dataPlusBB$dn, col = 'brown')
lines(dataPlusBB$timestamp,dataPlusBB$mavg, col = 'blue')
rsi14 = RSI(data[c('value.IBM.Close')], n=14)
head(rsi14,n=30)
macd = MACD(data[c('value.IBM.Close')], nFast=12, nSlow=26, nSig=9, maType=SMA)
head(macd,n=30)
allData = data.frame(data,sma20,ema14,bb20,rsi14,macd) # merge all data 
head(allData,n=20)
plot(allData$timestamp,allData$macd)
allData <- head(allData,-1)
write.table(allData, file="ibm_with_indicators.csv", na="", sep=",", row.names = FALSE)

###############################################################################################
#################Extracting fundamental data for IBM ######################################
###############################################################################################

library(readxl)
IBM_funda <- read_excel("IBM_fundamentals_edit.xlsx")
IBM_funda
allData$Year <-format(as.Date(allData$timestamp),"%Y")

################# Merging technical and fundamental data for IBM ######################################

IBM_total <-merge(x = allData, y = IBM_funda, by = "Year", all.x = TRUE)
IBM_total$ticker ='IBM'


IBM_total <- IBM_total[,c("Year","timestamp","Return_lead","sma20","ema14","dn","mavg","up","pctB","rsi14",
                       "macd","signal","EPS","Revenue","EBITDA","Net_Income","Income_Tax","ticker")]

###############################################################################################
#################Extracting technical factors for IRBT ######################################
###############################################################################################


###Repeating for IRobot
library(quantmod)
getSymbols(Symbols = "IRBT",
           src = "yahoo",
           from="2016-01-01",
           to="2019-10-01",
)

#################### Visualisation ###########################
barChart(IRBT)
chartSeries(IRBT)
addMACD()
addBBands(n=20,sd=2,ma='SMA',draw='bands',on=-1)
##############################################################

library("TTR")
IRBT_df = data.frame(value=coredata(IRBT),timestamp=index(IRBT))
data = IRBT_df
data$IRBT_ret <- Delt(data$value.IRBT.Adjusted)
library(dplyr)
data$Return_lead <-lead(data$IRBT_ret,1)
head(data); nrow(data); colnames(data)
sma20 <- SMA(data[c('value.IRBT.Close')],n=20) # 20-day moving average
head(sma20, n=50) # show first 50 values
ema14 = EMA(data[c('value.IRBT.Close')],n=14) #14-day EMA
head(ema14,n=20)
bb20 = BBands(data[c('value.IRBT.Close')], sd=2.0) #bolliger band, default is 20day
head(bb20,n=30)
dataPlusBB = data.frame(data,bb20) #create a new data frame with all input data, plus Bollinger Bands data.
plot(dataPlusBB$timestamp,data$value.IRBT.Close)
lines(dataPlusBB$timestamp,dataPlusBB$value.IRBT.Close, col = 'red')
lines(dataPlusBB$timestamp,dataPlusBB$up, col = 'purple')
lines(dataPlusBB$timestamp,dataPlusBB$dn, col = 'brown')
lines(dataPlusBB$timestamp,dataPlusBB$mavg, col = 'blue')
head(rsi14,n=30)
macd = MACD(data[c('value.IRBT.Close')], nFast=12, nSlow=26, nSig=9, maType=SMA)
head(macd,n=30)
allData = data.frame(data,sma20,ema14,bb20,rsi14,macd) # merge all data 
head(allData,n=20)
plot(allData$timestamp,allData$macd)
allData <- head(allData,-1)
write.table(allData, file="irbt_with_indicators.csv", na="", sep=",", row.names = FALSE)

###############################################################################################
#################Extracting fundamental data for IRBT ######################################
###############################################################################################

library(readxl)
IRBT_funda <- read_excel("IRBT_fundamentals_edit.xlsx")
IRBT_funda
allData$Year <-format(as.Date(allData$timestamp),"%Y")
head(k,10)

################# Merging technical and fundamental data for IRBT ######################################

IRBT_total <-merge(x = allData, y = IRBT_funda, by = "Year", all.x = TRUE)

IRBT_total$ticker ='IRBT'

IRBT_total <- IRBT_total[,c("Year","timestamp","Return_lead","sma20","ema14","dn","mavg","up","pctB","rsi14",
                          "macd","signal","EPS","Revenue","EBITDA","Net_Income","Income_Tax","ticker")]






###############################################################################################
#################Extracting technical factors for NFLX ######################################
###############################################################################################


####Repeating netflix group
library(quantmod)
getSymbols(Symbols = "NFLX",
           src = "yahoo",
           from="2016-01-01",
           to="2019-10-01",
)
#################### Visualisation ###########################
barChart(NFLX)
chartSeries(NFLX)
addMACD()
addBBands(n=20,sd=2,ma='SMA',draw='bands',on=-1)
##############################################################
library("TTR")
NFLX_df = data.frame(value=coredata(NFLX),timestamp=index(NFLX))
data = NFLX_df
data$NFLX_ret <- Delt(data$value.NFLX.Adjusted)
library(dplyr)
data$Return_lead <-lead(data$NFLX_ret,1)
head(data); nrow(data); colnames(data)
sma20 <- SMA(data[c('value.NFLX.Close')],n=20) # 20-day moving average
head(sma20, n=50) # show first 50 values
ema14 = EMA(data[c('value.NFLX.Close')],n=14) #14-day EMA
head(ema14,n=20)
bb20 = BBands(data[c('value.NFLX.Close')], sd=2.0) #bolliger band, default is 20day
head(bb20,n=30)
dataPlusBB = data.frame(data,bb20) #create a new data frame with all input data, plus Bollinger Bands data.
plot(dataPlusBB$timestamp,data$value.NFLX.Close)
lines(dataPlusBB$timestamp,dataPlusBB$value.NFLX.Close, col = 'red')
lines(dataPlusBB$timestamp,dataPlusBB$up, col = 'purple')
lines(dataPlusBB$timestamp,dataPlusBB$dn, col = 'brown')
lines(dataPlusBB$timestamp,dataPlusBB$mavg, col = 'blue')
rsi14 = RSI(data[c('value.NFLX.Close')], n=14)
head(rsi14,n=30)
macd = MACD(data[c('value.NFLX.Close')], nFast=12, nSlow=26, nSig=9, maType=SMA)
head(macd,n=100)
allData = data.frame(data,sma20,ema14,bb20,rsi14,macd) # merge all data 
head(allData,n=20)
plot(allData$timestamp,allData$macd)
allData <- head(allData,-1)
write.table(allData, file="nflx_with_indicators.csv", na="", sep=",", row.names = FALSE)

###############################################################################################
################# Extracting fundamental data for NFLX ######################################
###############################################################################################
library(readxl)
NFLX_funda <- read_excel("NFLX_fundamentals_edit.xlsx")
NFLX_funda
year(allData[1,"timestamp"])
allData$Year <-format(as.Date(allData$timestamp),"%Y")

################# Merging technical and fundamental data for NFLX ######################################
NFLX_total <-merge(x = allData, y = NFLX_funda, by = "Year", all.x = TRUE)
head(NFLX_total,10)
NFLX_total$ticker ='NFLX'

NFLX_total <- NFLX_total[,c("Year","timestamp","Return_lead","sma20","ema14","dn","mavg","up","pctB","rsi14",
                            "macd","signal","EPS","Revenue","EBITDA","Net_Income","Income_Tax","ticker")]


################# Merging all the 3 stocks into a dataframe ######################################
stocks <-rbind(IBM_total,IRBT_total,NFLX_total)
stocks$Return_lead <- as.numeric(stocks$Return_lead)
str(stocks)


################# Running panel data regression ######################################
library(plm)
fe <- plm(Return_lead ~sma20+ema14+dn+mavg+up+pctB+rsi14+
          macd+signal+EPS+Revenue+EBITDA+Net_Income+Income_Tax,
          data = stocks,model="within",index=c("ticker","timestamp") )


summary(fe)


###################################### Performance Analytics ################################
library(PerformanceAnalytics)
tickers <- c("IBM","IRBT","NFLX")
weights <- c(0.7,0,0.3)

portfolioPrices <- NULL
for(ticker in tickers){
  portfolioPrices <- cbind(portfolioPrices,getSymbols.yahoo(ticker,from="2016-01-01"
                                                            ,to="2019-09-01",auto.assign=FALSE)[,4])
}

benchmarkPrices <- getSymbols.yahoo('^GSPC',from="2016-01-01"
                                    ,to="2019-09-01",auto.assign=FALSE)[,4]

benchmarkReturns <- na.omit(ROC(benchmarkPrices))

portfolioReturns <- na.omit(ROC(portfolioPrices))

portfolioReturn <- Return.portfolio(portfolioReturns)

CAPM.beta(portfolioReturn,benchmarkReturns,0.035/252)

CAPM.jensenAlpha(portfolioReturn,benchmarkReturns,0.035/252)

SharpeRatio(portfolioReturn,0.035/252)

table.AnnualizedReturns(portfolioReturn)
table.CalendarReturns(portfolioReturn)

################################# Portfolio Optimisation ###################################
#install.packages("PortfolioAnalytics")
library(PortfolioAnalytics)
library(ROI)
library(ROI.plugin.glpk)
library(ROI.plugin.quadprog)
library(ROI.plugin.symphony)
portfolioReturns <- na.omit(ROC(portfolioPrices))
 portf <-portfolio.spec(colnames(portfolioReturns))
 
portf <-add.constraint(portf,type='weight_sum',min_sum=1,max_sum=1)
#portf <-add.constraint(portf,type="transaction_cost",ptc=0.001)
#portf <-add.constraint(portf,type='box',min=0,max=0.5)
portf <- add.objective(portf,type= "return",name='mean')
portf <- add.objective(portf, type ="risk",name='StdDev')


library(corpcor)
optPort <- optimize.portfolio(portfolioReturns, portf, optimize_method = "ROI", trace=TRUE)

optPort$R <- portfolioReturns

chart.Weights(optPort)

ef <-extractEfficientFrontier(optPort,match.col="StdDev",n.portfolios = 25, risk_aversion=NULL)


chart.EfficientFrontier(ef,
                        match.col = "StdDev", n.portfolios = 25, xlim = NULL, ylim = NULL,
                        cex.axis = 0.8, element.color = "darkgray", main = "Efficient Frontier",
                        RAR.text = "SR", rf = 0, tangent.line = TRUE, cex.legend = 0.8,
                        chart.assets = TRUE, labels.assets = TRUE, pch.assets = 21,
                        cex.assets = 0.8)


############################### Portfolio optimisation backtesting ###########################




equal_weight <- rep(1 / ncol(portfolioReturns), ncol(portfolioReturns))
benchmark <- Return.portfolio(portfolioReturns, weights = equal_weight)
colnames(benchmark) <- "Benchmark Portfolio"

sp500prices <- getSymbols.yahoo("SPY", from='2016-01-01', periodicity = 'daily', auto.assign=FALSE)[,4]
sp500Rets <- na.omit(ROC(sp500prices))
sp500Rets <- as.xts(sp500Rets)


opt_weights <-extractWeights(optPort)
opt_returns <- Return.portfolio(portfolioReturns, weights=rebal_weights)

rets_df <- cbind(opt_returns, benchmark, sp500Rets)


table.AnnualizedReturns(opt_returns)
charts.PerformanceSummary(rets_df, main="Performance level Over Time")





