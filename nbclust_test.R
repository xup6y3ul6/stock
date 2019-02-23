. <- dDF %>% # dDF為原始資料 
  filter(Trials <= 100)
.table <- table(.$Player, .$Decision, .$PriceChange)
nPlayer <- dim(.table)[1] #table(.$Player,,)
dMat <- matrix(.table, nrow = nPlayer)
dMat <- dMat / rowSums(dMat) 
colnames(dMat) <- c("FALL-buy","FALL-no trade","FALL-sell",
                    "RISE-buy","RISE-no trade","RISE-sell",
                    "STALL-buy","STALL-no trade","STALL-sell")
dMat # dMat為整理後成matrix的資料

library(NbClust)
.result <- NbClust(dMat, method = "kmeans", max.nc = 6, 
                   index = c())

.methods <- c("kl", "ch", "hartigan", "ccc", "scott", "marriot", "trcovw", "tracew",
              "friedman", "rubin", "cindex", "db", "silhouette", "duda", "pseudot2", 
              "beale", "ratkowsky", "ball", "ptbiserial", "gap", "frey", "mcclain", 
              "gamma", "gplus", "tau", "dunn", "hubert", "sdindex", "dindex", "sdbw")

.result <- list()

for(i in 1:length(.methods)){
  tryCatch(
    .result[[i]] <- NbClust(dMat, method = "kmeans", index = .methods[i]), 
    error = function(err){
      print(.methods[i])
    }
  )
}

bestN <- lapply(.result, function(.){try(.$Best.nc, silent = TRUE)})
names(bestN) <- .methods
bestN

