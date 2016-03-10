Relevance <- function(df, Ticker){
  
  Ncol       <- length(df)-2
  Multiplier <- as.numeric(sigmoid(scale(1:Ncol), a = 1, b = 0))
  mask <- apply(df, 2, is.na)
  mask <- abs(mask*1-1)
  mask <- sweep(mask[,3:length(mask[1,])],MARGIN=2,Multiplier,`*`)
  df$Relevance <- rowSums(mask)
  Relevance <- ddply(df, .(Ticker), summarise, Relevance = mean(Relevance))
  Relevance$Relevance <- as.numeric(sigmoid(Relevance$Relevance, a = 1, b = 0))
  
  Relevance

}

RankbyRelevance <- function(df) {
  
  df$RankbyRelevance <- df$Rank*df$Relevance
  
  df
}