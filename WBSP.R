WBSP <- function(df, Ticker){
  
  Escalar <- function(x){
    x[which(is.infinite(x) & x<0)] <- min(x[which(!is.infinite(x))]) 
    x[which(is.infinite(x) & x>0)] <- max(x[which(!is.infinite(x))]) 
    as.numeric(sigmoid(17*scale(x), a = 1, b = 0))
  }
  
  m     <- matrix(0, ncol = 1, nrow = length(Ticker))
  WBSP <- data.frame(m)
  colnames(WBSP) <- c('TickerWBSP')
  WBSP$TickerWBSP <- as.character(Ticker)
  
  EPS <- df[which(df$Metric=="Earnings per Share (diluted) (C$)"), -c(1,2)]
  
  TickerWBSP <-  as.character(df[which(df$Metric=="Earnings per Share (diluted) (C$)"), 2])
  LenEPS <- apply(EPS,1, function(x) length(x[!is.na(x)]))
  PositiveRets <- apply(EPS,1, function(x) diff(as.numeric(x[!is.na(x)])) * 
                          as.numeric(x[!is.na(x)][1:length(x[!is.na(x)])-1]>0))
  ConsistentGrowthAux <- mapply(function(x,y) sum(x>0)/(y-1), x=PositiveRets, y= LenEPS)
  ConsistentGrowthAux[is.na(ConsistentGrowthAux)]=0
  ConsistentGrowthAhead <- as.numeric(ConsistentGrowthAux>0.7)*ConsistentGrowthAux
  ConsistentGrowthAlert <- as.numeric(ConsistentGrowthAux<=0.4)*(1-ConsistentGrowthAux)
  ConsistentGrowth <- as.data.frame(cbind(TickerWBSP, ConsistentGrowthAhead, ConsistentGrowthAlert))
  WBSP <- left_join(WBSP, ConsistentGrowth, by = "TickerWBSP")
  
  TotalGrowth <- apply(EPS,1, function(x) (x[!is.na(x)][size(x[!is.na(x)],2)]-x[!is.na(x)][1])/x[!is.na(x)][1])
  AnualGrowth <- mapply(function(x,y) (x+1)^(1/(y-1))-1,x=TotalGrowth, y=LenEPS)
  fwdEPS <- mapply(function(x,y) x*(1+y)^10, x=EPS[,length(EPS)], y= AnualGrowth)
  rownames(EPS) <- df[which(df$Metric=="Earnings per Share (diluted) (C$)"), 2]
  Price <- apply(df[which(df$Metric=="Month End Stock Price (C$)"), -c(1,2)], 1, function(x) tail(x, n=1))
  EqBondReturnAux <- mapply(function(x,y) x/y, x=fwdEPS, y= Price)
  EqBondReturnAux[is.infinite(EqBondReturnAux) | is.na(EqBondReturnAux) | EqBondReturnAux<0]=0
  EqBondReturnAhead <- as.numeric(EqBondReturnAux>0.06)*Escalar(EqBondReturnAux)
  EqBondReturnAlert <- as.numeric(EqBondReturnAux<0.03)*(1-Escalar(EqBondReturnAux))
  EqBondReturn <- as.data.frame(cbind(TickerWBSP, EqBondReturnAhead, EqBondReturnAlert))
  WBSP <- left_join(WBSP, EqBondReturn, by = "TickerWBSP")
  
#   # Fix NaN and NA
  indxAhead   <- grep("Ahead",colnames(WBSP))
  indxAlert   <- grep("Alert",colnames(WBSP))
  WBSP[-c(1)] <- lapply(WBSP[-c(1)], function(x) as.numeric(as.character(x)))
  mask <- apply(WBSP[indxAhead], 2, is.nan)
  WBSP[indxAhead][mask] <- 0
  mask <- apply(WBSP[indxAlert], 2, is.nan)
  WBSP[indxAlert][mask] <- 0
  mask <- apply(WBSP[indxAhead], 2, is.na)
  WBSP[indxAhead][mask] <- 0
  mask <- apply(WBSP[indxAlert], 2, is.na)
  WBSP[indxAlert][mask] <- 0
  WBSP <- transform(WBSP, Rank=rowSums(WBSP[,-c(1)]))
  colnames(WBSP)[1] <-"Ticker"
  
  WBSP
}
