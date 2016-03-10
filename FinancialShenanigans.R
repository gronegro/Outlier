FinancialShenanigans <- function(df, dfQ, Ticker){
  
  m     <- matrix(0, ncol = 1, nrow = length(Ticker))
  FinancialShenanigans <- data.frame(m)
  colnames(FinancialShenanigans) <- c('TickerFinancialShenanigans')
  FinancialShenanigans$TickerFinancialShenanigans <- as.character(Ticker)
  
  Escalar <- function(x){
    x[which(is.infinite(x) & x<0)] <- min(x[which(!is.infinite(x))]) 
    x[which(is.infinite(x) & x>0)] <- max(x[which(!is.infinite(x))]) 
    as.numeric(sigmoid(17*scale(x), a = 1, b = 0))
  }
  
  
  CAGRLast <- function(x){
    x <- na.omit(x)
    FV=x[length(x)] 
    IV=x[length(x)-1]
    n=1
    cagr <- (FV/IV)^(1/n)-1
    return(cagr)
  }
  
  # Rapid Increment in sales (unexplained manner)
  RevenueCAGRLast <- apply(df[which(df[,1]=="Revenue"),-c(1,2,length(df))], 1, function(x) CAGRLast(unlist(unname(x))))
  RevenueCAGRLastAhead <- Escalar(as.numeric(RevenueCAGRLast))
  RevenueCAGRLastAlert <- -Escalar(as.numeric(RevenueCAGRLast))
  TickerFinancialShenanigans <-  as.character(df[which(df$Metric=="Revenue"), 2])
  RevenueCAGR <- as.data.frame(cbind(TickerFinancialShenanigans, RevenueCAGRLastAhead, RevenueCAGRLastAlert))
  RevenueCAGR$RevenueCAGRLastAhead <- as.numeric(as.character(RevenueCAGR$RevenueCAGRLastAhead))
  RevenueCAGR$RevenueCAGRLastAlert <- as.numeric(as.character(RevenueCAGR$RevenueCAGRLastAlert))  
  FinancialShenanigans <- left_join(FinancialShenanigans, RevenueCAGR, by = "TickerFinancialShenanigans")
  FinancialShenanigans$RevenueCAGRLastAhead[is.na(RevenueCAGR$RevenueCAGRLastAhead)] <- 0
  FinancialShenanigans$RevenueCAGRLastAlert[is.na(RevenueCAGR$RevenueCAGRLastAlert)] <- -1

  # Slow Increment in earnings 
  EarningsCAGRLast <- apply(df[which(df[,1]=="Net Income (Continuing Operations)"),-c(1,2,length(df))], 1, function(x) CAGRLast(unlist(unname(x))))
  EarningsCAGRLastAhead <- (EarningsCAGRLast>0)*(1-Escalar(as.numeric(EarningsCAGRLast)))
  EarningsCAGRLastAlert <- -(EarningsCAGRLast>0)*(Escalar(as.numeric(EarningsCAGRLast)))
  TickerFinancialShenanigans <-  as.character(df[which(df$Metric=="Net Income (Continuing Operations)"), 2])
  EarningsCAGR <- as.data.frame(cbind(TickerFinancialShenanigans, EarningsCAGRLastAhead, EarningsCAGRLastAlert))
  EarningsCAGR$EarningsCAGRLastAhead <- as.numeric(as.character(EarningsCAGR$EarningsCAGRLastAhead))
  EarningsCAGR$EarningsCAGRLastAlert <- as.numeric(as.character(EarningsCAGR$EarningsCAGRLastAlert))  
  FinancialShenanigans <- left_join(FinancialShenanigans, EarningsCAGR, by = "TickerFinancialShenanigans")
  FinancialShenanigans$EarningsCAGRLastAhead[is.na(EarningsCAGR$EarningsCAGRLastAhead)] <- 0
  FinancialShenanigans$EarningsCAGRLastAlert[is.na(EarningsCAGR$EarningsCAGRLastAlert)] <- -1
  
  # Wide spread between sales and earnings growth
  FinancialShenanigans$SpreadSales2EarnLastAhead <- Escalar(FinancialShenanigans$RevenueCAGRLastAhead - FinancialShenanigans$EarningsCAGRLastAhead)
  FinancialShenanigans$SpreadSales2EarnLastAlert <- -(1-Escalar(FinancialShenanigans$RevenueCAGRLastAlert - FinancialShenanigans$EarningsCAGRLastAlert))
  
  # Cash Flow from Operations vs Net Income
  CFFO   <- df[which(df$Metric=="Cash Flow from Operations"), -c(1,2)]
  rownames(CFFO)   <- df[which(df$Metric=="Cash Flow from Operations"), 2]
  NI <- df[which(df$Metric=="Net Income"), -c(1,2)]
  rownames(NI) <- df[which(df$Metric=="Net Income"), 2]
  CFFOvsNIAux  <- cbind(TickerFinancialShenanigans=c(rownames(CFFO), rownames(NI)), rbind.fill(list(CFFO, NI)))
  TickerFinancialShenanigans  <- as.character(sort(unique(CFFOvsNIAux$TickerFinancialShenanigans)))
  CFFOvsNIAux                 <- ddply(CFFOvsNIAux, .(TickerFinancialShenanigans), function(x) (x[1,]-x[2,])/x[1,])
  CFFOvsNIAux                 <- do.call(data.frame,lapply(CFFOvsNIAux, function(x) replace(x, is.infinite(x),NA)))
  CFFOvsNIstdAux            <- apply(CFFOvsNIAux,1,sd,na.rm=TRUE)
  CFFOvsNIstdAhead          <- Escalar(CFFOvsNIstdAux)
  CFFOvsNIstdAlert          <- -(1-Escalar(CFFOvsNIstdAux))
  CFFOvsNIstd <- as.data.frame(cbind(TickerFinancialShenanigans, CFFOvsNIstdAhead, CFFOvsNIstdAlert))
  FinancialShenanigans <- left_join(FinancialShenanigans, CFFOvsNIstd, by = "TickerFinancialShenanigans")
  
  # Cash Flow from Operations vs Net Income last two years
  CFFOvsNITailAux   <- apply(CFFOvsNIAux,1,function(x) sd(tail(na.omit(x),2)))
  CFFOvsNIstdTailAhead          <- Escalar(as.numeric(CFFOvsNITailAux))
  CFFOvsNIstdTailAlert          <- -(1-Escalar(as.numeric(CFFOvsNITailAux)))
  CFFOvsNIstdTail <- as.data.frame(cbind(TickerFinancialShenanigans, CFFOvsNIstdTailAhead, CFFOvsNIstdTailAlert))
  FinancialShenanigans <- left_join(FinancialShenanigans, CFFOvsNIstdTail, by = "TickerFinancialShenanigans")
  
  # Cash Flow from Operations vs Net Income last year
  CFFOvsNILastAux   <- apply(CFFOvsNIAux,1,function(x) tail(na.omit(x),1))
  CFFOvsNIstdLastAhead          <- Escalar(as.numeric(CFFOvsNILastAux))
  CFFOvsNIstdLastAlert          <- -(1-Escalar(as.numeric(CFFOvsNILastAux)))
  CFFOvsNIstdLast <- as.data.frame(cbind(TickerFinancialShenanigans, CFFOvsNIstdLastAhead, CFFOvsNIstdLastAlert))
  FinancialShenanigans <- left_join(FinancialShenanigans, CFFOvsNIstdLast, by = "TickerFinancialShenanigans")
  
  # Cash Flow from Operations vs Net Income last Quaterlies
  CFFOQ   <- dfQ[which(dfQ$Metric=="Cash Flow from Operations"), -c(1,2)]
  rownames(CFFOQ)   <- dfQ[which(dfQ$Metric=="Cash Flow from Operations"), 2]
  NIQ <- dfQ[which(dfQ$Metric=="Net Income"), -c(1,2)]
  rownames(NIQ) <- dfQ[which(dfQ$Metric=="Net Income"), 2]
  CFFOvsNIQAux  <- cbind(TickerFinancialShenanigans=c(rownames(CFFOQ), rownames(NIQ)), rbind.fill(list(CFFOQ, NIQ)))
  TickerFinancialShenanigans  <- as.character(sort(unique(CFFOvsNIQAux$TickerFinancialShenanigans)))
  CFFOvsNIQAux                 <- ddply(CFFOvsNIQAux, .(TickerFinancialShenanigans), function(x) (x[1,]-x[2,])/x[1,])
  CFFOvsNIQAux                 <- do.call(data.frame,lapply(CFFOvsNIQAux, function(x) replace(x, is.infinite(x),NA)))
  CFFOvsNIQAux                 <- apply(CFFOvsNIQAux,1,function(x) sd(c(x[2],x[6])))
  CFFOvsNIQstdAhead          <- Escalar(CFFOvsNIQAux)
  CFFOvsNIQstdAlert          <- -(1-Escalar(CFFOvsNIQAux))
  CFFOvsNIQstd <- as.data.frame(cbind(TickerFinancialShenanigans, CFFOvsNIQstdAhead, CFFOvsNIQstdAlert))
  FinancialShenanigans <- left_join(FinancialShenanigans, CFFOvsNIQstd, by = "TickerFinancialShenanigans")
  
  FinancialShenanigans
  
}