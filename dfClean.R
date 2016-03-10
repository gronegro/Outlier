dfCleanAnually <- function(df){
  
    # Limpieza Data
    colnames(df) <- gsub("[^0-9]", "",colnames(df))
    colnames(df)[1:2]  <- c("Metric","Ticker")
    df$Metric <- as.factor(df$Metric)
    df$Ticker <- as.factor(df$Ticker)
    
    for(i in 4:length(colnames(df))){
      df <- df[which(!grepl("[A-Z]", df[,i])),] 
    }
    
    df[-c(1,2)] <- lapply(df[-c(1,2)], function(x) as.numeric(as.character(x)))
    df <- df[!duplicated(df[,c(1,2)]),]
    colnames(df)[which(colnames(df)=="99")] <- "-01"
    colnames(df)[which(colnames(df)=="98")] <- "-02"
    colnames(df)[which(colnames(df)=="97")] <- "-03"
    colnames(df)[which(colnames(df)=="96")] <- "-04"
    colnames(df)[which(colnames(df)=="95")] <- "-05"
    colnames(df)[!grepl("[A-Z]", colnames(df))] <- as.character(as.numeric(colnames(df)[!grepl("[A-Z]", colnames(df))]))
    df <- df[c("Metric", "Ticker", as.character(sort(as.numeric(colnames(df)[!grepl("[A-Z]", colnames(df))]))))]

}



dfCleanQuaterly <- function(df){
  
  # Limpieza Data
  
  colnames(df)[1:2]  <- c("Metric", "Ticker")
  df$Metric <- as.factor(df$Metric)
  df$Ticker <- as.factor(df$Ticker)
  
  for(i in 3:length(colnames(df))){
    df <- df[which(!grepl("[A-Z]", df[,i])),] 
  }
  
  df[-c(1,2)] <- lapply(df[-c(1,2)], function(x) as.numeric(as.character(x)))
  df <- df[!duplicated(df[,c(1,2)]),]  
  
}



dfCleanTTM <- function(df){
  
  # Limpieza Data
  
  colnames(df)[1:2]  <- c("Metric", "Ticker")
  df$Metric <- as.factor(df$Metric)
  df$Ticker <- as.factor(df$Ticker)
  
  for(i in 3:length(colnames(df))){
    df <- df[which(!grepl("[A-Z]", df[,i])),] 
  }
  
  df[-c(1,2)] <- lapply(df[-c(1,2)], function(x) as.numeric(as.character(x)))
  df <- df[!duplicated(df[,c(1,2)]),]  
  
}