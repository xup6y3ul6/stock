. <- dDF %>% 
  filter(Trials >= 1, Trials <= 100)
.table <- table(.$Player, .$Decision, .$PriceChange)


.nPlayer <- dim(.table)[1] #table(.$Player,,)
.dMat <- matrix(.table, nrow = .nPlayer)
.fallSum <- rowSums(.dMat[, 1:3])
.riseSum <- rowSums(.dMat[, 4:6])
.stallSum <- rowSums(.dMat[, 7:9])
.dMat2 <- cbind(
  .dMat[, 1:3] / .fallSum, 
  .dMat[, 4:6] / .riseSum, 
  .dMat[, 7:9] / .stallSum
) 
colnames(.dMat2) <- c("FALL-buy","FALL-no trade","FALL-sell",
                      "RISE-buy","RISE-no trade","RISE-sell",
                      "STALL-buy","STALL-no trade","STALL-sell")
rownames(.dMat2) <- 1:.nPlayer

.clusterTable <- as.data.frame(round(.dMat2, 3))
.clusterTable$player <- rownames(.clusterTable)
.clusterTable$cluster <-hkmeans(.dMat2,4)$cluster
.ct1 <- mutate(.clusterTable, group = 1)
.ct2 <- mutate(.clusterTable, group = 2)
.ct <- .clusterTable






.data <- .ct %>% 
  filter(cluster %in% c(1, 2)) %>% 
  gather(key = "dimansions", value = "ratio", -cluster, -player) %>% 
  select(-player) %>% 
  group_by(cluster, dimansions) %>% 
  summarise(n = length(ratio), 
            ratio.total.mean = mean(ratio), 
            ratio.total.sd = sd(ratio)) %>% 
  mutate(priceChange = sapply(strsplit(dimansions, "-"), "[", 1), 
         action = sapply(strsplit(dimansions, "-"), "[", 2)) %>% 
  filter((cluster == 1 & priceChange == "FALL") | (cluster == 2 & priceChange == "FALL"))
g <- ggplot(.data, aes(x = priceChange, y = ratio.total.mean, fill = action)) +
  geom_histogram(stat = "identity", position = "dodge") + 
  facet_wrap(~ cluster) +
  theme_bw()
ggplotly(g)

