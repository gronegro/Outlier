WBIFS <- function(df, Ticker){
  
  Escalar <- function(x){
    x[which(is.infinite(x) & x<0)] <- min(x[which(!is.infinite(x))]) 
    x[which(is.infinite(x) & x>0)] <- max(x[which(!is.infinite(x))]) 
    as.numeric(sigmoid(17*scale(x), a = 1, b = 0))
  }
  
  m     <- matrix(0, ncol = 1, nrow = length(Ticker))
  WBIFS <- data.frame(m)
  colnames(WBIFS) <- c('TickerWBIFS')
  WBIFS$TickerWBIFS <- as.character(Ticker)
  
  GrossMarginAvg   <- rowMeans(df[which(df$Metric=="Gross Margin %"), -c(1,2)], na.rm = TRUE, dims = 1)
  GrossMarginAhead <- as.numeric(GrossMarginAvg>=40)*Escalar(GrossMarginAvg)
  GrossMarginAlert <- -as.numeric(GrossMarginAvg<=20)*(1-Escalar(GrossMarginAvg))
  TickerWBIFS <-  as.character(df[which(df$Metric=="Gross Margin %"), 2])
  GrossMargin <- as.data.frame(cbind(TickerWBIFS, GrossMarginAhead, GrossMarginAlert))
  WBIFS <- left_join(WBIFS, GrossMargin, by = "TickerWBIFS")

  
  
  SGA   <- df[which(df$Metric=="Selling, General, &Admin. Expense"), -c(1,2)]
  rownames(SGA)   <- df[which(df$Metric=="Selling, General, &Admin. Expense"), 2]
  Gross <- df[which(df$Metric=="Gross Profit"), -c(1,2)]
  rownames(Gross) <- df[which(df$Metric=="Gross Profit"), 2]
  SGA2GrossAux            <- cbind(TickerWBIFS=c(rownames(SGA), rownames(Gross)), rbind.fill(list(SGA, Gross)))
  TickerWBIFS   <- as.character(sort(unique(SGA2GrossAux$TickerWBIFS)))
  SGA2GrossAux                          <- ddply(SGA2GrossAux, .(TickerWBIFS), function(x) x[1,]/x[2,])
  SGA2GrossAux[SGA2GrossAux<0]          <- Inf
  SGA2GrossAux            <- rowMeans(SGA2GrossAux, na.rm = TRUE, dims = 1) 
  SGA2GrossAhead          <- as.numeric(SGA2GrossAux <= 0.3)*(1-Escalar(SGA2GrossAux))
  SGA2GrossAlert          <- -as.numeric(SGA2GrossAux >= 0.8)*Escalar(SGA2GrossAux)
  SGA2Gross <- as.data.frame(cbind(TickerWBIFS, SGA2GrossAhead, SGA2GrossAlert), stringsAsFactors=FALSE)
  WBIFS <- left_join(WBIFS, SGA2Gross, by = "TickerWBIFS")

  
  
  RD   <- df[which(df$Metric=="Research &Development"), -c(1,2)]
  rownames(RD)   <- df[which(df$Metric=="Research &Development"), 2]
  RD2GrossAux            <- cbind(TickerWBIFS=c(rownames(RD), rownames(Gross)), rbind.fill(list(RD, Gross)))
  TickerWBIFS   <- as.character(sort(unique(RD2GrossAux$TickerWBIFS)))
  RD2GrossAux                          <- ddply(RD2GrossAux, .(TickerWBIFS), function(x) x[1,]/x[2,])
  RD2GrossAux[RD2GrossAux<0]          <- Inf
  RD2GrossAux            <- rowMeans(RD2GrossAux, na.rm = TRUE, dims = 1) 
  RD2GrossAhead          <- as.numeric(RD2GrossAux == 0)
  RD2GrossAlert          <- -as.numeric(RD2GrossAux >= 0.2)*Escalar(RD2GrossAux)
  RD2Gross <- as.data.frame(cbind(TickerWBIFS, RD2GrossAhead, RD2GrossAlert))
  WBIFS <- left_join(WBIFS, RD2Gross, by = "TickerWBIFS")

  
  
  Dep   <- df[which(df$Metric=="Depreciation, Depletion and Amortization"), -c(1,2)]
  rownames(Dep)   <- df[which(df$Metric=="Depreciation, Depletion and Amortization"), 2]
  Dep2GrossAux            <- cbind(TickerWBIFS=c(rownames(Dep), rownames(Gross)), rbind.fill(list(Dep, Gross)))
  TickerWBIFS   <- as.character(sort(unique(Dep2GrossAux$TickerWBIFS)))
  Dep2GrossAux                          <- ddply(Dep2GrossAux, .(TickerWBIFS), function(x) x[1,]/x[2,])
  Dep2GrossAux[Dep2GrossAux<0]           <- Inf
  Dep2GrossAux            <- rowMeans(Dep2GrossAux, na.rm = TRUE, dims = 1) 
  Dep2GrossAhead          <- as.numeric(Dep2GrossAux <= 0.1)*(1-Escalar(Dep2GrossAux))
  Dep2GrossAlert          <- -as.numeric(Dep2GrossAux >= 0.2)*Escalar(Dep2GrossAux)
  Dep2Gross <- as.data.frame(cbind(TickerWBIFS, Dep2GrossAhead, Dep2GrossAlert))
  WBIFS <- left_join(WBIFS, Dep2Gross, by = "TickerWBIFS")

  
  
  Int   <- df[which(df$Metric=="Interest Expense"), -c(1,2)]
  rownames(Int)   <- df[which(df$Metric=="Interest Expense"), 2]
  OpInc <- df[which(df$Metric=="Operating Income"), -c(1,2)]
  rownames(OpInc) <- df[which(df$Metric=="Operating Income"), 2]
  Int2OpIncAux            <- cbind(TickerWBIFS=c(rownames(Int), rownames(OpInc)), rbind.fill(list(Int, OpInc)))
  TickerWBIFS   <- as.character(sort(unique(Int2OpIncAux$TickerWBIFS)))
  Int2OpIncAux                          <- ddply(Int2OpIncAux, .(TickerWBIFS), function(x) x[1,]/x[2,])
  Int2OpIncAux[Int2OpIncAux<0]          <- Inf
  Int2OpIncAux            <- rowMeans(Int2OpIncAux, na.rm = TRUE, dims = 1) 
  Int2OpIncAhead          <- as.numeric(Int2OpIncAux <= 0.3)*(1-Escalar(Int2OpIncAux))
  Int2OpIncAlert          <- -as.numeric(Int2OpIncAux >= 0.7)*Escalar(Int2OpIncAux)
  Int2OpInc <- as.data.frame(cbind(TickerWBIFS, Int2OpIncAhead, Int2OpIncAlert))
  WBIFS <- left_join(WBIFS, Int2OpInc, by = "TickerWBIFS")

  
  # Each market is different (TaxRate)
  TaxRate <- 35
  TaxAhead <- as.numeric(rowMeans(abs(df[which(df$Metric=="Tax Rate %"), -c(1,2)]-TaxRate), na.rm = TRUE, dims = 1)<=2)
  TaxAhead <- TaxAhead*(1-Escalar(TaxAhead))
  TaxAlert <- -as.numeric(rowMeans(abs(df[which(df$Metric=="Tax Rate %"), -c(1,2)]-TaxRate), na.rm = TRUE, dims = 1)>=5)
  TaxAlert <- TaxAlert*Escalar(TaxAlert)
  TickerWBIFS <-  as.character(df[which(df$Metric=="Tax Rate %"), 2])
  Tax <- as.data.frame(cbind(TickerWBIFS, TaxAhead, TaxAlert))
  WBIFS <- left_join(WBIFS, Tax, by = "TickerWBIFS")

  
  # How to consider Gain (or Loss) on sale of assets and other chapter?
  
  
  NetMarginAhead <- as.numeric(rowMeans(df[which(df$Metric=="Net Margin %"), -c(1,2)], na.rm = TRUE, dims = 1)>=20)
  NetMarginAhead <- NetMarginAhead*Escalar(NetMarginAhead)
  NetMarginAlert <- -as.numeric(rowMeans(df[which(df$Metric=="Net Margin %"), -c(1,2)], na.rm = TRUE, dims = 1)<=10)
  NetMarginAlert <- NetMarginAlert*(1-Escalar(NetMarginAhead))
  TickerWBIFS <-  as.character(df[which(df$Metric=="Net Margin %"), 2])
  NetMargin <- as.data.frame(cbind(TickerWBIFS, NetMarginAhead, NetMarginAlert))
  WBIFS <- left_join(WBIFS, NetMargin, by = "TickerWBIFS")

  
  # Replacing the concept of smooth up trending eps   
  # It doesn't count, but it is important to take it into account -> Current ratio
  
  
  CAssets   <- df[which(df$Metric=="Total Current Assets"), -c(1,2)]
  rownames(CAssets)   <- df[which(df$Metric=="Total Current Assets"), 2]
  CLiabilities <- df[which(df$Metric=="Total Current Liabilities"), -c(1,2)]
  rownames(CLiabilities) <- df[which(df$Metric=="Total Current Liabilities"), 2]
  CurrentAux            <- cbind(TickerWBIFS=c(rownames(CAssets), rownames(CLiabilities)), rbind.fill(list(CAssets, CLiabilities)))
  TickerWBIFS   <- as.character(sort(unique(CurrentAux$TickerWBIFS)))
  CurrentAux                          <- ddply(CurrentAux, .(TickerWBIFS), function(x) x[1,]/x[2,])
  CurrentAux[CurrentAux<0]          <- -Inf
  CurrentAux            <- rowMeans(CurrentAux, na.rm = TRUE, dims = 1) 
  CurrentAhead          <- as.numeric(CurrentAux >= 1)*Escalar(CurrentAux)
  CurrentAlert          <- -as.numeric(CurrentAux <= 1)*(1-Escalar(CurrentAux))
  Current <- as.data.frame(cbind(TickerWBIFS, CurrentAhead, CurrentAlert))
  WBIFS <- left_join(WBIFS, Current, by = "TickerWBIFS")

  
  ShortDebt   <- df[which(df$Metric=="Current Portion of Long-Term Debt"), -c(1,2)]
  rownames(ShortDebt)   <- df[which(df$Metric=="Current Portion of Long-Term Debt"), 2]
  LongDebt <- df[which(df$Metric=="Long-Term Debt"), -c(1,2)]
  rownames(LongDebt) <- df[which(df$Metric=="Long-Term Debt"), 2]
  ShortvsLongAux            <- cbind(TickerWBIFS=c(rownames(ShortDebt), rownames(LongDebt)), rbind.fill(list(ShortDebt, LongDebt)))
  TickerWBIFS   <- as.character(sort(unique(ShortvsLongAux$TickerWBIFS)))
  ShortvsLongAux                          <- ddply(ShortvsLongAux, .(TickerWBIFS), function(x) x[1,]/x[2,])
  ShortvsLongAux[ShortvsLongAux<0]          <- Inf
  ShortvsLongAux            <- rowMeans(ShortvsLongAux, na.rm = TRUE, dims = 1) 
  ShortvsLongAhead          <- as.numeric(ShortvsLongAux <= 0.6)*(1-Escalar(ShortvsLongAux))
  ShortvsLongAlert          <- -as.numeric(ShortvsLongAux >= 2)*Escalar(ShortvsLongAux)
  ShortvsLong <- as.data.frame(cbind(TickerWBIFS, ShortvsLongAhead, ShortvsLongAlert))
  WBIFS <- left_join(WBIFS, ShortvsLong, by = "TickerWBIFS")

  
  NIContOps   <- df[which(df$Metric=="Net Income (Continuing Operations)"), -c(1,2)]
  rownames(NIContOps)   <- df[which(df$Metric=="Net Income (Continuing Operations)"), 2]
  LTDPaymentAux            <- cbind(TickerWBIFS=c(rownames(NIContOps), rownames(LongDebt)), rbind.fill(list(NIContOps, LongDebt)))
  TickerWBIFS   <- as.character(sort(unique(LTDPaymentAux$TickerWBIFS)))
  LTDPaymentAux                          <- ddply(LTDPaymentAux, .(TickerWBIFS), function(x) x[1,]/x[2,])
  LTDPaymentAux[LTDPaymentAux<0]          <- Inf
  LTDPaymentAux            <- rowMeans(LTDPaymentAux, na.rm = TRUE, dims = 1) 
  LTDPaymentAhead          <- as.numeric(LTDPaymentAux <= 4)*(1-Escalar(LTDPaymentAux))
  LTDPaymentAlert          <- -as.numeric(LTDPaymentAux >= 10)*Escalar(LTDPaymentAux)
  LTDPayment <- as.data.frame(cbind(TickerWBIFS, LTDPaymentAhead, LTDPaymentAlert))
  WBIFS <- left_join(WBIFS, LTDPayment, by = "TickerWBIFS")

  
  Equity   <- df[which(df$Metric=="Total Equity"), -c(1,2)]
  rownames(Equity)   <- df[which(df$Metric=="Total Equity"), 2]
  Treasury <- df[which(df$Metric=="Treasury Stock"), -c(1,2)]
  rownames(Treasury) <- df[which(df$Metric=="Treasury Stock"), 2]
  Debt2EquityAux            <- cbind(TickerWBIFS=c(rownames(ShortDebt),rownames(LongDebt),rownames(Equity), rownames(Treasury)), rbind.fill(list(ShortDebt, LongDebt, Equity, Treasury)))
  TickerWBIFS   <- as.character(sort(unique(Debt2EquityAux$TickerWBIFS)))
  Debt2EquityAux                          <- ddply(Debt2EquityAux, .(TickerWBIFS), function(x) (x[1,]+x[2,])/(x[3,]+x[4,]))
  Debt2EquityAux[Debt2EquityAux<0]          <- Inf
  Debt2EquityAux            <- rowMeans(Debt2EquityAux, na.rm = TRUE, dims = 1) 
  Debt2EquityAhead          <- as.numeric(Debt2EquityAux <= 0.8)*(1-Escalar(Debt2EquityAux))
  Debt2EquityAlert          <- -as.numeric(Debt2EquityAux >= 4)*Escalar(Debt2EquityAux)
  Debt2Equity <- as.data.frame(cbind(TickerWBIFS, Debt2EquityAhead, Debt2EquityAlert))
  WBIFS <- left_join(WBIFS, Debt2Equity, by = "TickerWBIFS")

  
  NonePrefAhead <- as.numeric(rowSums(df[which(df$Metric=="Preferred Stock"), -c(1,2)], na.rm = TRUE)==0)
  NonePrefAlert <- -as.numeric(rowSums(df[which(df$Metric=="Preferred Stock"), -c(1,2)], na.rm = TRUE)>0)
  NonePrefAlert <- NonePrefAlert*Escalar(NonePrefAlert)
  TickerWBIFS <-  as.character(df[which(df$Metric=="Preferred Stock"), 2])
  NonePref <- as.data.frame(cbind(TickerWBIFS, NonePrefAhead, NonePrefAlert))
  WBIFS <- left_join(WBIFS, NonePref, by = "TickerWBIFS")

  
  CAGR <- function(x){
      x <- na.omit(x)
      FV=x[length(x)] 
      IV=x[1]
      n=length(x)-1
      cagr <- (FV/IV)^(1/n)-1
      return(cagr)
  }
  
  RetainCAGR <- apply(df[which(df[,1]=="Retained Earnings"),-c(1,2)], 1, function(x) CAGR(unlist(unname(x))))
  RetainCAGRAhead <- as.numeric(RetainCAGR > 0.06)*Escalar(as.numeric(RetainCAGR))
  RetainCAGRAlert <- -as.numeric(RetainCAGR < 0)*(1-Escalar(as.numeric(RetainCAGR)))
  TickerWBIFS <-  as.character(df[which(df$Metric=="Retained Earnings"), 2])
  RetainCAGR <- as.data.frame(cbind(TickerWBIFS, RetainCAGRAhead, RetainCAGRAlert))
  WBIFS <- left_join(WBIFS, RetainCAGR, by = "TickerWBIFS")


  ROEAux            <- cbind(TickerWBIFS=c(rownames(NIContOps),rownames(Equity), rownames(Treasury)), rbind.fill(list(OpInc, Equity, Treasury)))
  TickerWBIFS   <- as.character(sort(unique(ROEAux$TickerWBIFS)))
  ROEAux                          <- ddply(ROEAux, .(TickerWBIFS), function(x) (x[1,])/(x[2,]+x[3,]))
  ROEAux[ROEAux<0]          <- -Inf
  ROEAux                    <- rowMeans(ROEAux, na.rm = TRUE, dims = 1) 
  ROEAhead                  <- as.numeric(ROEAux >= 0.15)*Escalar(ROEAux)
  ROEAlert                  <- -as.numeric(ROEAux <= 0.05)*(1-Escalar(ROEAux))
  ROE <- as.data.frame(cbind(TickerWBIFS, ROEAhead, ROEAlert))
  WBIFS <- left_join(WBIFS, ROE, by = "TickerWBIFS")
  
  Capex   <- df[which(df$Metric=="Capital Expenditure"), -c(1,2)]
  rownames(Capex)   <- df[which(df$Metric=="Capital Expenditure"), 2]
  Capex2OpIncAux            <- cbind(TickerWBIFS=c(rownames(Capex), rownames(OpInc)), rbind.fill(list(Capex, OpInc)))
  TickerWBIFS   <- as.character(sort(unique(Capex2OpIncAux$TickerWBIFS)))
  Capex2OpIncAux                          <- ddply(Capex2OpIncAux, .(TickerWBIFS), function(x) x[1,]/x[2,])
  Capex2OpIncAux[Capex2OpIncAux<0]          <- Inf
  Capex2OpIncAux            <- rowMeans(Capex2OpIncAux, na.rm = TRUE, dims = 1) 
  Capex2OpIncAhead          <- as.numeric(Capex2OpIncAux <= 0.5)*(1-Escalar(Capex2OpIncAux))
  Capex2OpIncAlert          <- -as.numeric(Capex2OpIncAux >= 3)*Escalar(Capex2OpIncAux)
  Capex2OpInc <- as.data.frame(cbind(TickerWBIFS, Capex2OpIncAhead, Capex2OpIncAlert))
  WBIFS <- left_join(WBIFS, Capex2OpInc, by = "TickerWBIFS")

  
  Capex2OpIncAhead2          <- as.numeric(Capex2OpIncAux <= 0.25)*(1-Escalar(Capex2OpIncAux))
  Capex2OpIncAlert2          <- -as.numeric(Capex2OpIncAux >= 4)*Escalar(Capex2OpIncAux)
  Capex2OpInc2 <- as.data.frame(cbind(TickerWBIFS, Capex2OpIncAhead2, Capex2OpIncAlert2))
  WBIFS <- left_join(WBIFS, Capex2OpInc2, by = "TickerWBIFS")

  
  
  # Fix NaN and NA
  indxAhead   <- grep("Ahead",colnames(WBIFS))
  indxAlert   <- grep("Alert",colnames(WBIFS))
  WBIFS[-c(1)] <- lapply(WBIFS[-c(1)], function(x) as.numeric(as.character(x)))
  mask <- apply(WBIFS[indxAhead], 2, is.nan)
  WBIFS[indxAhead][mask] <- 0
  mask <- apply(WBIFS[indxAlert], 2, is.nan)
  WBIFS[indxAlert][mask] <- 0
  mask <- apply(WBIFS[indxAhead], 2, is.na)
  WBIFS[indxAhead][mask] <- 0
  mask <- apply(WBIFS[indxAlert], 2, is.na)
  WBIFS[indxAlert][mask] <- 0
  WBIFS <- transform(WBIFS, Rank=rowSums(WBIFS[,-c(1)]))
  colnames(WBIFS)[1] <-"Ticker"
  
  WBIFS

}


#WBIFS[is.na(WBIFS)] <- 0 # depends if is ahead or alert