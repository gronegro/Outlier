library(plyr)
library(dplyr)
library(rjson)
library(jsonlite)
library(pracma)
library(scales)
library(mongolite)


setwd("/home/juang/Documents/outlier-backend")
# =======
# setwd("/home/juandaserni/Documents/OutLierHunter/outlier-backend")
# >>>>>>> 4c84f17d3f4d05e0677e4400966f504099980a45
source("Relevance.R")
source("WBIFS.R")
source("SuperStocks.R")
source("FinancialShenanigans.R")
source("WBSP.R")
source("dfClean.R")
source("Network.R")


# Indices:  Split Quaterly-Anual /// Get Ticker index /// TTM Index
Qindex      <- function(df){which(grepl("Q", colnames(df)))}
IndexTicker <- function(df){which(colnames(df)=="Ticker")}
TTMindex    <- function(df){which(grepl("TTM", colnames(df)))}


# Markets
files <- list.files(path="Data/", pattern="*.csv", full.names=T, recursive=FALSE)
Countries <- lapply(files, function(x) {read.csv(file=x, header=TRUE, sep=",", encoding="UTF-8",na.strings=c("","nan"))})
names(Countries) <- gsub(files, pattern = paste(c(".csv","Data//"), collapse = "|"), replacement = "")
Markets          <- lapply(Countries, function(x) x[,-c(Qindex(x), TTMindex(x))])
MarketsQuaterly  <- lapply(Countries, function(x) x[,c(1, IndexTicker(x), Qindex(x))])
MarketsTTM       <- lapply(Countries, function(x) x[,c(1, IndexTicker(x), TTMindex(x))])


# Markets (clean)
CutYear <- "13"
MarketsClean         <- lapply(Markets, dfCleanAnually)
MarketsClean         <-  lapply(MarketsClean, function(x) x[,1:which(colnames(x)==CutYear)])
MarketsCleanQuaterly <- lapply(MarketsQuaterly, dfCleanQuaterly)
MarketsCleanTTM      <- lapply(MarketsTTM, dfCleanTTM)


# Quantiles (selected stocks)
qupper <- 0.95
qlower <- 1 - qupper 

# Count length of Data (Number of Years)
MarketRelevance <- lapply(MarketsClean, function(x) Relevance(x,unique(x$Ticker)))

# Warren Buffet and the interpretation of financial statements
MarketWBIFS            <- lapply(MarketsClean, function(x) WBIFS(x,unique(x$Ticker)))
MarketWBIFS            <- mapply(function(x, y) left_join(x, y, by = "Ticker", all = T), x = MarketWBIFS, y = MarketRelevance, SIMPLIFY = F)
MarketWBIFS            <- lapply(MarketWBIFS, RankbyRelevance)
QuantileUpperWBIFS     <- lapply(MarketWBIFS, function(x) x[which(x$RankbyRelevance > quantile(x$RankbyRelevance, probs=qupper)),c(1,length(x))])
QuantileLowerWBIFS     <- lapply(MarketWBIFS, function(x) x[which(x$RankbyRelevance < quantile(x$RankbyRelevance, probs=qlower)),c(1,length(x))])
QuantileWBIFS          <- mapply(rbind, QuantileUpperWBIFS, QuantileLowerWBIFS, SIMPLIFY=FALSE)

# Warren Buffet Stock Portfolio - Equity Bond
MarketWBSP             <- lapply(MarketsClean, function(x) WBSP(x,unique(x$Ticker)))
MarketWBSP             <- mapply(function(x, y) left_join(x, y, by = "Ticker", all = T), x = MarketWBSP, y = MarketRelevance, SIMPLIFY = F)
MarketWBSP             <- lapply(MarketWBSP, RankbyRelevance)
QuantileUpperWBSP      <- lapply(MarketWBSP, function(x) x[which(x$RankbyRelevance >= quantile(x$RankbyRelevance, probs=qupper)),c(1,length(x))])
QuantileLowerWBSP      <- lapply(MarketWBSP, function(x) x[which(x$RankbyRelevance <= quantile(x$RankbyRelevance, probs=qlower)),c(1,length(x))])
QuantileWBSP           <- mapply(rbind, QuantileUpperWBSP, QuantileLowerWBSP, SIMPLIFY=FALSE)


# Super Stocks
MarketSuperStocks            <- mapply(function(x, y) SuperStocks(x, y, unique(x$Ticker)), x = MarketsClean, y = MarketsCleanQuaterly, SIMPLIFY = F)
MarketSuperStocks            <- mapply(function(x, y) left_join(x, y, by = "Ticker", all = T), x = MarketSuperStocks, y = MarketRelevance, SIMPLIFY = F)
MarketSuperStocks            <- lapply(MarketSuperStocks, RankbyRelevance)
QuantileUpperSuperStocks     <- lapply(MarketSuperStocks, function(x) x[which(x$RankbyRelevance >= quantile(x$RankbyRelevance, probs=qupper)),c(1,length(x))])
QuantileLowerSuperStocks     <- lapply(MarketSuperStocks, function(x) x[which(x$RankbyRelevance <= quantile(x$RankbyRelevance, probs=qlower)),c(1,length(x))])
QuantileSuperStocks          <- mapply(rbind, QuantileUpperSuperStocks, QuantileLowerSuperStocks, SIMPLIFY=FALSE)

# Financial Shenanigans
# <<<<<<< HEAD
MarketFinancialShenanigans            <- lapply(MarketsClean,function(x) FinancialShenanigans(x,unique(x$Ticker)))
QuantileUpperFinancialShenanigans     <- lapply(MarketFinancialShenanigans, function(x) x[which(x$RankFinancialShenanigans >= floor(quantile(x$RankFinancialShenanigans, probs=qupper))),c(1,length(x))])
QuantileLowerFinancialShenanigans     <- lapply(MarketFinancialShenanigans, function(x) x[which(x$RankFinancialShenanigans <= ceiling(quantile(x$RankFinancialShenanigans, probs=qlower))),c(1,length(x))])
QuantileFinancialShenanigans          <- mapply(rbind, QuantileUpperFinancialShenanigans, QuantileLowerFinancialShenanigans, SIMPLIFY=FALSE)

MarketFinancialShenanigans            <- mapply(function(x, y) FinancialShenanigans(x, y, unique(x$Ticker)), x = MarketsClean, y = MarketsCleanQuaterly, SIMPLIFY = F)
MarketFinancialShenanigans            <- mapply(function(x, y) left_join(x, y, by = "Ticker", all = T), x = MarketFinancialShenanigans, y = MarketRelevance, SIMPLIFY = F)
MarketFinancialShenanigans            <- lapply(MarketFinancialShenanigans, RankbyRelevance)
QuantileUpperFinancialShenanigans     <- lapply(MarketFinancialShenanigans, function(x) x[which(x$RankFinancialShenanigans >= floor(quantile(x$RankFinancialShenanigans, probs=qupper))),c(1,length(x))])
QuantileLowerFinancialShenanigans     <- lapply(MarketFinancialShenanigans, function(x) x[which(x$RankFinancialShenanigans <= ceiling(quantile(x$RankFinancialShenanigans, probs=qlower))),c(1,length(x))])
QuantileFinancialShenanigans          <- mapply(rbind, QuantileUpperFinancialShenanigans, QuantileLowerFinancialShenanigans, SIMPLIFY=FALSE)
# >>>>>>> 4c84f17d3f4d05e0677e4400966f504099980a45

# Market
Books <- c("WBIFS","SuperStocks")
MarketNames <- c("CANADA")
QuantileMarket  <- Map(c, QuantileWBIFS, QuantileSuperStocks)
NetworkMarket   <- lapply(QuantileMarket, function(x) Network(x, Books))

# Market2JSON
Marketjson <- lapply(NetworkMarket, function(x) rjson::toJSON(list(nodes=x$nodes,links=x$links)))
sapply(names(Marketjson), function(x) write(Marketjson[[x]], file=paste("Javascript/",paste(x,".json",sep=""),sep="")))

# MongoDB
MarketDataMongo <- lapply(Marketjson, function(x) jsonlite::fromJSON(x))
mapply(function(x,y) {m <- mongo(collection = paste(y,"nodes",sep=""),  db = "outlier", url = "mongodb://outlier:outlier@ds027385.mongolab.com:27385/outlier")
                                     m$insert(x$nodes)}, x = MarketDataMongo, y = names(Countries), SIMPLIFY = F)
mapply(function(x,y) {m <- mongo(collection = paste(y,"links",sep=""),  db = "outlier", url = "mongodb://outlier:outlier@ds027385.mongolab.com:27385/outlier")
                                      m$insert(x$links)}, x = MarketDataMongo, y = names(Countries), SIMPLIFY = F)

