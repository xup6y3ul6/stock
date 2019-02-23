# load packages
{
  library(ggplot2)
  library(plotly)
  library(tidyr)
  library(dplyr)
  library(factoextra)
  library(mclust)
  library(ClusterR)
  library(ggpubr)
}

# load all data (behavior+hyperscanning)
{
  .fileNames <- dir("data")
  fileNames <- paste0("data/", .fileNames)
  data <- lapply(fileNames, read.csv, header = TRUE)
  numOfData <- length(data)  # == 160 subjects
  numOfPair <- numOfData / 2 # == 80 pairs
  
  for(i in 1:numOfPair){
    # 先確認是不是同一組(同組的股票價格應該都會是一樣的)
    stopifnot(data[[2*i-1]]$StockPrice == data[[2*i-1]]$StockPrice) 
    # 增加一欄：玩家該場的勝負輸贏
    .lastRow = length(data[[2*i-1]]$p1Cash[!is.na(data[[2*i-1]]$p1Cash)])
    if(data[[2*i-1]][.lastRow, "p1Cash"] > data[[2*i]][.lastRow, "p2Cash"]){
      data[[2*i-1]]$Outcome = "win"
      data[[2*i]]$Outcome = "loss"
    }else{
      data[[2*i-1]]$Outcome = "loss"
      data[[2*i]]$Outcome = "win"
    }
    # 增加一欄該玩家的ID，也就是該檔名
    data[[2*i-1]]$ID <- sub(".csv", "", basename(fileNames[2*i-1]))
    data[[2*i]]$ID <- sub(".csv", "", basename(fileNames[2*i]))
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
  pairData<- list()
  for(i in 1:numOfPair){
    pairData[[i]] <- cbind(data[[2*i-1]][c("Trials", "StockPrice", 
                                           "p1Stock", "p1Cash", "p1TotalAsset", "p1Decision")], 
                           data[[2*i]][c("p2Stock", "p2Cash", "p2TotalAsset", "p2Decision")])
    n = nrow(pairData[[i]])
    pairData[[i]]$PriceChange = c(0, (pairData[[i]]$StockPrice[2:n] - pairData[[i]]$StockPrice[1:(n-1)]))
    pairData[[i]]$IPriceChange = pcCategorize(pairData[[i]]$StockPrice)
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

# deltaAsset-Action
# deltaAseet是自己剪去對手的Asset
{
  .list <- list()
  for(i in 1:length(data)){
    if((i %% 2) == 1){
      .df <- data.frame(deltaTA = data[[i]]$p1TotalAsset[1:100] - data[[i+1]]$p2TotalAsset[1:100],
                        decision = factor(data[[i]]$p1Decision[1:100], levels = c("buy", "no trade", "sell")))
    }else{
      .df <- data.frame(deltaTA = data[[i]]$p2TotalAsset[1:100] - data[[i-1]]$p1TotalAsset[1:100],
                        decision = factor(data[[i]]$p2Decision[1:100], levels = c("buy", "no trade", "sell")))
    }
    
    .x <- vector(mode = "character", length = nrow(.df))
    for (j in 1:nrow(.df)) {
      if (.df$deltaTA[j] > 0) {
        .x[j] <- "LEADING"
      }else if (.df$deltaTA[j] < 0) {
        .x[j] <- "LAGGING"
      }else {
        .x[j] <- "COINCIDENT"
      }
    }
    
    .df$dtaCategorise <- factor(.x, levels = c("LEADING", "COINCIDENT", "LAGGING"))
    
    . <- table(.df[c("dtaCategorise", "decision")]) / 100
    .list[[i]] <- matrix(t(.), nrow = 1, byrow = FALSE)
  }
  dAsA_data <- as.data.frame(do.call("rbind", .list))
  names(dAsA_data) <- c("LEADING-buy", "LEADING-no trade", "LEADING-sell",
                   "COINCIDENT-buy", "COINCIDENT-no trade", "COINCIDENT-sell",
                   "LAGGING-buy", "LAGGING-no trade", "LAGGING-sell")
}

###
# .d <- dAsA_data %>% 
#   mutate(cluster = test$cluster) %>% 
#   gather(key = "dimansions", value = "ratio", -cluster) %>% 
#   mutate(deltaAsset = sapply(strsplit(dimansions, "-"), "[", 1), 
#          action = sapply(strsplit(dimansions, "-"), "[", 2))
# 
# p <- .d %>% 
#   ggplot(aes(x = action, y = ratio, color = action)) +
#   geom_boxplot() + 
#   geom_jitter(position = position_jitter(0.2), alpha = I(0.25)) +
#   facet_grid(. ~ deltaAsset) +
#   theme_bw()
# p


