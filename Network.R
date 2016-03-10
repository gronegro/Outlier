Network <- function(Quantile, Books){
  
  # Force Network Market
  Source     <- matrix(0, ncol = 1, nrow = sum(unname(sapply(Quantile, length)))/2)
  MisLinks <- data.frame(Source)
  signo <- lapply(Quantile[grep("Rank", names(Quantile))],sign)
  MisLinks$Source <-   unlist(lapply(seq_along(signo), function(y, n, i) { paste(n[[i]], y[[i]]) }, y=signo, n=Books))
  MisLinks$target <- unlist(Quantile[grep("Ticker", names(Quantile))])

  names <- sort(unique(as.factor(c(MisLinks$Source,MisLinks$target))))
  name  <- matrix(0, ncol = 1, nrow = length(names))
  MisNodes <- data.frame(name)
  MisNodes$name  <- names
  MisNodes$group <- as.integer(1)
  MisNodes$size  <- as.integer(10)
  MisLinks$Source <- mapvalues(MisLinks$Source, from=names, to=as.numeric(names)-1)
  MisLinks$Source <- as.integer(MisLinks$Source)
  MisLinks$target <- mapvalues(MisLinks$target, from=names, to=as.numeric(names)-1)
  MisLinks$target <- as.integer(MisLinks$target)
  MisLinks$value <- as.integer(10)
  colnames(MisLinks)[1] <- "source"
  
  Network <- list(nodes=unname(split(MisNodes, 1:nrow(MisNodes))), links=unname(split(MisLinks, 1:nrow(MisLinks))))
  
  Network
  
}