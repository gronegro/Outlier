# Super Stocks

SuperStocks <- function(df, dfQ, Ticker){
  
  Escalar <- function(x){
    x[which(is.infinite(x) & x<0)] <- min(x[which(!is.infinite(x))]) 
    x[which(is.infinite(x) & x>0)] <- max(x[which(!is.infinite(x))]) 
    as.numeric(sigmoid(17*scale(x), a = 1, b = 0))
  }
  
  CAGR <- function(x){
    x <- na.omit(x)
    FV=x[length(x)] 
    IV=x[1]
    n=length(x)-1
    cagr <- (FV/IV)^(1/n)-1
    return(cagr)
  }
  
  CAGRMean <- function(x){
    x <- na.omit(x)
    n <- length(x);
    returns <- log(x[-1]/x[-n])
    cagrmean <- mean(returns)
  }
  
  m     <- matrix(0, ncol = 1, nrow = length(Ticker))
  SuperStocks <- data.frame(m)
  colnames(SuperStocks) <- c('TickerSuperStocks')
  SuperStocks$TickerSuperStocks <- as.character(Ticker)
  
  PS <- apply(df[which(df[,1]=="PS Ratio"),-c(1,2)],1,function(x) mean(tail(na.omit(x),2)))
  PS[is.nan(PS)] <- 4
  PSAhead  <- as.numeric(0 < PS & PS < 0.75)*(1-Escalar(PS))
  PSAlert  <- -as.numeric(PS > 1.5)*Escalar(PS)
  PSAlert2 <- -as.numeric(PS > 3)*Escalar(PS)
  TickerSuperStocks <-  as.character(df[which(df$Metric=="PS Ratio"), 2])
  PS <- as.data.frame(cbind(TickerSuperStocks, PSAhead, PSAlert,PSAlert2))
  SuperStocks <- left_join(SuperStocks, PS, by = "TickerSuperStocks")
  SuperStocks$PSAhead[is.na(SuperStocks$PSAhead)] <- 0
  SuperStocks$PSAlert[is.na(SuperStocks$PSAlert)] <- -1
  SuperStocks$PSAlert2[is.na(SuperStocks$PSAlert2)] <- -1
  
  Revenue1 <- as.numeric(apply(df[which(df[,1]=="Revenue"),-c(1,2)],1,function(x) CAGR(tail(na.omit(x),2))))
  Revenue2 <- as.numeric(apply(df[which(df[,1]=="Revenue"),-c(1,2)],1,function(x) CAGR(tail(na.omit(x),3))))
  Revenue3 <- as.numeric(apply(df[which(df[,1]=="Revenue"),-c(1,2)],1,function(x) CAGR(tail(na.omit(x),4))))
  RevenueGro <- as.numeric(apply(df[which(df[,1]=="Revenue"),-c(1,2)],1,function(x) CAGRMean(head(na.omit(x),length(na.omit(x))-2))))
  DownRevAhead <- as.numeric(Revenue1<0 | Revenue2<0 | Revenue3<0)*Escalar(RevenueGro)
  DownRevAlert <- -as.numeric(RevenueGro<0)*(1-Escalar(RevenueGro))
  TickerSuperStocks <-  as.character(df[which(df$Metric=="Revenue"), 2])
  DownRev <- as.data.frame(cbind(TickerSuperStocks, DownRevAhead, DownRevAlert))
  SuperStocks <- left_join(SuperStocks, DownRev, by = "TickerSuperStocks")
  SuperStocks$DownRevAhead[is.na(SuperStocks$DownRevAhead)] <- 0
  SuperStocks$DownRevAlert[is.na(SuperStocks$DownRevAlert)] <- -1
  
  RevenueQ <- as.numeric(apply(dfQ[which(dfQ[,1]=="Revenue"),-c(1,2)],1,function(x) CAGR(na.omit(x))))
  RevenueQAhead <- as.numeric(RevenueQ>0)*Escalar(RevenueQ)
  RevenueQAlert <- -as.numeric(RevenueQ<0)*(1-Escalar(RevenueQ))
  TickerSuperStocks <-  as.character(df[which(df$Metric=="Revenue"), 2])
  RevenueQ <- as.data.frame(cbind(TickerSuperStocks, RevenueQAhead, RevenueQAlert))
  SuperStocks <- left_join(SuperStocks, RevenueQ, by = "TickerSuperStocks")
  SuperStocks$RevenueQAhead[is.na(SuperStocks$RevenueQAhead)] <- 0
  SuperStocks$RevenueQAlert[is.na(SuperStocks$RevenueQAlert)] <- -1
  
  OpIncClose0   <- as.numeric(apply(dfQ[which(dfQ[,1]=="Operating Income"),-c(1,2)],1,function(x) abs(tail(na.omit(x),1))/mean(na.omit(x))))
  OpIncClose0Ahead <- as.numeric(OpIncClose0>0)*(Escalar(-2*abs(OpIncClose0)))
  OpIncClose0Alert <- -as.numeric(OpIncClose0<0)*(1-Escalar(-2*abs(OpIncClose0)))
  TickerSuperStocks <-  as.character(df[which(df$Metric=="Operating Income"), 2])
  OpIncClose0 <- as.data.frame(cbind(TickerSuperStocks, OpIncClose0Ahead, OpIncClose0Alert))
  SuperStocks <- left_join(SuperStocks, OpIncClose0, by = "TickerSuperStocks")
  SuperStocks$OpIncClose0Ahead[is.na(SuperStocks$OpIncClose0Ahead)] <- 0
  SuperStocks$OpIncClose0Alert[is.na(SuperStocks$OpIncClose0Alert)] <- -1
  
  OpIncQ <- as.numeric(apply(dfQ[which(dfQ[,1]=="Operating Income"),-c(1,2)],1,function(x) CAGR(na.omit(x))))
  OpIncQAhead <- as.numeric(OpIncQ>0.25)*Escalar(OpIncQ)
  OpIncQAlert <- -as.numeric(OpIncQ<0)*(1-Escalar(OpIncQ))
  TickerSuperStocks <-  as.character(df[which(df$Metric=="Operating Income"), 2])
  OpIncQ <- as.data.frame(cbind(TickerSuperStocks, OpIncQAhead, OpIncQAlert))
  SuperStocks <- left_join(SuperStocks, OpIncQ, by = "TickerSuperStocks")
  SuperStocks$OpIncQAhead[is.na(SuperStocks$OpIncQAhead)] <- 0
  SuperStocks$OpIncQAlert[is.na(SuperStocks$OpIncQAlert)] <- -1
  
  # For Technology companies
  MarketCap <- df[which(df$Metric=="Market Cap"), -c(1,2)]
  rownames(MarketCap) <- df[which(df$Metric=="Market Cap"), 2]
  RD   <- df[which(df$Metric=="Research &Development"), -c(1,2)]
  rownames(RD)   <- df[which(df$Metric=="Research &Development"), 2]
  RD2MarketCapAux            <- cbind(TickerSuperStocks=c(rownames(MarketCap), rownames(RD)), rbind.fill(list(MarketCap, RD)))
  TickerSuperStocks   <- as.character(sort(unique(RD2MarketCapAux$TickerSuperStocks)))
  RD2MarketCapAux                           <- ddply(RD2MarketCapAux, .(TickerSuperStocks), function(x) x[1,]/x[2,])
  RD2MarketCapAux[RD2MarketCapAux<0]          <- Inf
  RD2MarketCapAux <- as.numeric(apply(RD2MarketCapAux,1,function(x) tail(na.omit(x),1)))
  RD2MarketCapAhead          <- as.numeric(RD2MarketCapAux > 5 & RD2MarketCapAux < 10)
  RD2MarketCapAlert          <- -as.numeric(RD2MarketCapAux > 15)*Escalar(RD2MarketCapAux)
  RD2MarketCap <- as.data.frame(cbind(TickerSuperStocks, RD2MarketCapAhead, RD2MarketCapAlert))
  SuperStocks <- left_join(SuperStocks, RD2MarketCap, by = "TickerSuperStocks")
  SuperStocks$RD2MarketCapAhead[is.na(SuperStocks$RD2MarketCapAhead)] <- 0
  SuperStocks$RD2MarketCapAlert[is.na(SuperStocks$RD2MarketCapAlert)] <- -1
  
  indxAhead   <- grep("Ahead",colnames(SuperStocks))
  indxAlert   <- grep("Alert",colnames(SuperStocks))
  SuperStocks[-c(1)] <- lapply(SuperStocks[-c(1)], function(x) as.numeric(as.character(x)))
  SuperStocks <- transform(SuperStocks, Rank=rowSums(SuperStocks[,-c(1)]))
  mask <- apply(SuperStocks[indxAhead], 2, is.nan)
  SuperStocks[indxAhead][mask] <- 0
  mask <- apply(SuperStocks[indxAlert], 2, is.nan)
  SuperStocks[indxAlert][mask] <- 0
  mask <- apply(SuperStocks[indxAhead], 2, is.na)
  SuperStocks[indxAhead][mask] <- 0
  mask <- apply(SuperStocks[indxAlert], 2, is.na)
  SuperStocks[indxAlert][mask] <- 0
  SuperStocks <- transform(SuperStocks, Rank=rowSums(SuperStocks[,-c(1)], na.rm = TRUE))
  colnames(SuperStocks)[1] <-"Ticker"
  
  SuperStocks
  
}


# Be careful with PS ratio of different sectors (technology, retailing)
# Be careful when assigning -1 to nans in alert.
# Think about CAGR. WHat happens with intraperiods?
# Ahead: PS < 0.4 Alert: PS > 0.8 (For traditional industrial companies)