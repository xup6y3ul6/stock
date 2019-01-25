# load packages
{
  library(ggplot2)
  library(plotly)
  library(tidyr)
  library(dplyr)
  library(factoextra)
  library(mclust)
  library(ClusterR)
}

# load data
{
  fileNames <- dir("data")
  fileNames <- paste0("data/", fileNames)
  data <- lapply(fileNames, read.csv, header = TRUE)
  numOfData <- length(data)  # == 160 subjects
  numOfPair <- numOfData / 2 # == 80 pairs
  
  for(i in 1:numOfPair){
    stopifnot(data[[2*i-1]]$StockPrice == data[[2*i-1]]$StockPrice)
    
    lastRow = length(data[[2*i-1]]$p1Cash[!is.na(data[[2*i-1]]$p1Cash)])
    if(data[[2*i-1]][lastRow, "p1Cash"] > data[[2*i]][lastRow, "p2Cash"]){
      data[[2*i-1]]$Outcome = "win"
      data[[2*i]]$Outcome = "loss"
    }else{
      data[[2*i-1]]$Outcome = "loss"
      data[[2*i]]$Outcome = "win"
    }
  }
}

# costomized function
{
  pcCategorize <- function(pc){
    n = length(pc[!is.na(pc)])
    output = vector(mode = "character", length = n)
    output[1] = "STALL" 
    for(i in 2:n){
      if((pc[i] - pc[i-1]) > 0) output[i] = "RISE"
      else if((pc[i] - pc[i-1]) < 0) output[i] = "FALL"
      else output[i] = "STALL"
    }
    output <- c(output, rep(NA, (length(pc)-n)))
    factor(output)
  }
}

# stockList
{
  dataList<- list()
  for(i in 1:numOfPair){
    dataList[[i]] <- cbind(data[[2*i-1]][c("Trials", "StockPrice", 
                                           "p1Stock", "p1Cash", "p1TotalAsset", "p1Decision")], 
                           data[[2*i]][c("p2Stock", "p2Cash", "p2TotalAsset", "p2Decision")])
    n = nrow(dataList[[i]])
    dataList[[i]]$PriceChange = c(0, (dataList[[i]]$StockPrice[2:n] - dataList[[i]]$StockPrice[1:(n-1)]))
    dataList[[i]]$IPriceChange = pcCategorize(dataList[[i]]$StockPrice)
  }
}

# decision
{
  decisionList <- list()
  decisionMat <- matrix(NA, nrow = numOfPair, ncol = 9)
  for(i in 1:numOfPair){
    decisionList[[i]] <- cbind(data[[2*i-1]][1:100, "p1Decision", drop = FALSE], 
                               data[[2*i]][1:100, "p2Decision", drop = FALSE])
    . <- table(decisionList[[i]])
    if(i == 72) next # because that the dicisions of subject(no.72, p2) only had buy & sell
    decisionMat[i,] <- matrix(t(.[2:4, 2:4]), nrow = 1)
  }
  decisionMat[72,] <- c(11, 2, 0, 28, 36, 0, 13, 10, 0)
  
  decisionDF <- data.frame(decisionMat)
  names(decisionDF) <- c("buy_buy", "buy_noTrade", "buy_sell",
                         "noTrade_buy", "noTrade_noTrade", "noTrade_sell",
                         "sell_buy", "sell_noTrade", "sell_sell")
  decisionDF$Player <- 1:numOfPair
}

# price change, player decision, check history, outcome
{
  dList <- list()
  dDF <- data.frame()
  for(j in 1:numOfData){
    temp <- data.frame(Trials = data[[j]]$Trials,
                       PriceChange = pcCategorize(data[[j]]$StockPrice),
                       Decision = factor(data[[j]][,7], levels = c("buy", "no trade", "sell")),
                       CheckHistory = factor(data[[j]][,8], levels = c("yes", "no")),
                       Outcome = factor(data[[j]][,9], levels = c("win", "loss")), 
                       Player = j)
    dList[[j]] <- temp
    dDF <- rbind(dDF, dList[[j]])
  }
  
}

# 昊閎的，對手前期決定與自己當期決定的kmean
{
  dcNames <- dir("bsn/", pattern = "\\.csv")
  dcNames2 <- paste0("bsn/", dcNames)
  dcData <- lapply(dcNames2, read.csv)
  names(dcData) <- dcNames
}
