library(shiny)
source("data.R")

function(input, output, session){
  # "all data table"
  {
    output$allData <- DT::renderDataTable(
      DT::datatable({
        if(input$player == "player 1") pairIndex = as.integer(input$pairNo)*2-1
        if(input$player == "player 2") pairIndex = as.integer(input$pairNo)*2
        
        trialIndex <-  1:101
        switch(input$mrkCnd, 
               "Balence (1~20)" = {trialIndex = 1:20}, 
               "Bubble (21~60)" = {trialIndex = 21:60}, 
               "Burst (61~100)" = {trialIndex = 61:101})
        
        data[[pairIndex]][trialIndex,]
        },
        options = list(paging = FALSE),
        rownames = FALSE
      )
    )
  }

  # "stock plot"
  {
    spData<- reactive({
      pairData[[as.integer(input$pairNumber)]][c("Trials", "StockPrice")] %>% 
        filter(Trials >= input$trialRange[1] & 
               Trials <= input$trialRange[2])
    })
    
    selectedVar <- reactiveValues(col = c(1, 3, 7), name = "Stock number")
    observeEvent(input$stock, {selectedVar$col <- c(1, 3, 7)
                               selectedVar$name <- "Stock number"})
    observeEvent(input$cash, {selectedVar$col <- c(1, 4, 8)
                              selectedVar$name <- "Cash"})
    observeEvent(input$totalAsset, {selectedVar$col <- c(1, 5, 9)
                                    selectedVar$name <- "Total asset"})
    observeEvent(input$deltaAsset, {selectedVar$col <- c(1, 5, 9)
                                    selectedVar$name <- "Delta asset"})
    observeEvent(input$decision, {selectedVar$col <- c(1, 6, 10)
                                  selectedVar$name <- "Decision"})
    
    playerData <- reactive({
      .dL <- pairData[[as.integer(input$pairNumber)]][selectedVar$col] %>% 
        filter(Trials >= input$trialRange[1] & 
               Trials <= input$trialRange[2]) 
      if(selectedVar$name == "Delta asset"){
        .dL <- .dL %>% 
          mutate(deltaPrice_p1 = p1TotalAsset - p2TotalAsset,
                 deltaPrice_p2 = p2TotalAsset - p1TotalAsset) %>% 
          select(Trials, deltaPrice_p1, deltaPrice_p2)
      }
      .dL %>% gather(key = "player", value = "value", 2, 3)
    })
    
    output$sp_StockPricePlot <- renderPlotly({
      g <- ggplot(spData(), aes(x = Trials, y = StockPrice)) +
        geom_line() +
        theme_bw() +
        labs(title = paste0("Pair No. ", input$pairNumber, "_", "Stock price by trials"),
             x = "trials",
             y = selectedVar$name)
      ggplotly(g)
    })
    
    output$sp_SelectedVarPlot <- renderPlotly({
      g <- ggplot(playerData(), aes(x = Trials, y = value, group = player)) +
        geom_line(aes(linetype = player, color = player)) +
        theme_bw() +
        labs(title = paste0("Pair No. ", input$pairNumber, "_", selectedVar$name, " by trials"),
             x = "trials",
             y = "stock number") + 
        theme(legend.position = "bottom")
      
      ggplotly(g)%>%
        layout(legend = list(orientation = "h", x = 0.35, y = -0.2))
    })      
  }
  
  # "Action(p1,t) vs. Action(p2,t): Kmeans"
  {
    selectedDF <- reactive({
      decisionDF %>% 
        filter(Player %in% input$ipa_no.pair) %>% 
        select(-Player)
    })
    clusters <- reactive({
      kmeans(selectedDF(), input$ipa_k)
    })
    
    output$ipa_kmeansPlot <- renderPlot({
      g <- ggplot(selectedDF(), aes_string(input$ipa_xcol, input$ipa_ycol)) +
        geom_point(aes(color = factor(clusters()$cluster))) +
        geom_point(data = as.data.frame(clusters()$center), 
                   aes_string(input$ipa_xcol, input$ipa_ycol), size = 5, shape = 4) +
        theme_bw()
      g
    })
    
    output$ipa_fvizPlot <- renderPlot({
      g <- fviz_cluster(clusters(),               # 分群結果
                        data = selectedDF(),      # 資料
                        geom = c("point","text"), # 點和標籤(point & label)
                        frame.type = "norm") +    # 框架型態
        theme_bw()
      g
    })
    
    output$ipa_elbowPlot <- renderPlot({
      g <- fviz_nbclust(selectedDF(), 
                        FUNcluster = kmeans,# K-Means
                        method = "wss",     # total within sum of square
                        k.max = 12) +       # max number of clusters to consider
        labs(title="Elbow Method for K-Means") +
        theme_bw()
      g
    })
    
    output$ipa_silhouettePlot <- renderPlot({
      g <- fviz_nbclust(selectedDF(), 
                        FUNcluster = kmeans,# K-Means
                        method =  "silhouette",     # total within sum of square
                        k.max = 12) +       # max number of clusters to consider
        labs(title="Avg.Silhouette Method for K-Means") +
        theme_bw()
      g
    })
    
    output$ipa_dendPlot <- renderPlot({
      g <- fviz_dend(hkmeans(selectedDF(), input$ipa_k), cex = 0.6)
      g
    })
  }
  
  # page: 昊閎的K-mean
  {
    dcSelectDF <- reactive({
      switch(input$time, 
             "All" = {index1 = c(1, 5)},
             "Behavior" = {index1 = 1},
             "Neural" = {index1 = 5})
      switch(input$period,
             "All" = {index2 = 0},
             "Balance" = {index2 = 1},
             "Bubble" = {index2 = 2},
             "Burst" = {index2 = 3})
      index = index1 + index2
      . <- data.frame()
      for(i in index){
        . <- rbind(., dcData[[i]])
      }
      .[,-10]
    })
    
    dcClusters <- reactive({
      kmeans(dcSelectDF(), input$k_means)
    })
    
    output$dcFvizPlot <- renderPlot({
      g <- fviz_cluster(dcClusters(),                  # 分群結果
                        data = dcSelectDF(),        # 資料
                        geom = c("point","text"), # 點和標籤(point & label)
                        frame.type = "norm") +    # 框架型態
        theme_bw()
      g
    })
    
    output$dcElbowPlot <- renderPlot({
      g <- fviz_nbclust(dcSelectDF(), 
                        FUNcluster = kmeans,# K-Means
                        method = "wss",     # total within sum of square
                        k.max = 12) +       # max number of clusters to consider
        labs(title="Elbow Method for K-Means") +
        theme_bw()
      g
    })
    
    output$dcDendPlot <- renderPlot({
      g <- fviz_dend(hkmeans(dcSelectDF(), input$k_means), cex = 0.6)
      g
    })
  }

  # dAsA
  {
    # 更新可選擇cluster的總數量
    observe({
      maxCluster <- as.integer(input$dAsA_k)
      updateSelectInput(session, "dAsA_selectCluster", choices = 1:maxCluster)
    })
    
    # 轉換原始資料
    dAsA_clusters <- reactive({
      kmeans(dAsA_data, input$dAsA_k, nstart = 10, iter.max = 20)
    })
    
    dAsA_selectData <- reactive({
      dAsA_data %>% 
        mutate(cluster = dAsA_clusters()$cluster) %>% 
        filter(cluster == as.integer(input$dAsA_selectCluster)) %>% 
        select(-cluster) %>% 
        gather(key = "dimansions", value = "total.ratio") %>%
        mutate(deltaAsset = sapply(strsplit(dimansions, "-"), "[", 1), 
               action = sapply(strsplit(dimansions, "-"), "[", 2))
    })
    
    # plot
    output$dAsA_clusterPlot <- renderPlot({
      g <- dAsA_selectData() %>% 
        group_by(dimansions, deltaAsset, action) %>% 
        summarise(n = length(total.ratio), 
                  total.ratio.mean = mean(total.ratio), 
                  total.ratio.sd = sd(total.ratio)) %>% 
        ggplot(aes(x = deltaAsset, y = total.ratio.mean, fill = action)) +
        geom_bar(stat = "identity", position = "dodge") + 
        geom_errorbar(aes(ymin = ifelse((total.ratio.mean - total.ratio.sd) > 0, total.ratio.mean - total.ratio.sd, 0),
                          ymax = total.ratio.mean + total.ratio.sd),
                      position = position_dodge(0.9), width = 0.1) +
        coord_cartesian(ylim = c(input$dAsA_ylim[1], input$dAsA_ylim[2])) +
        theme_bw()
      g
    })
    
    output$dAsA_tTestPlot <- renderPlot({
      g <- dAsA_selectData() %>% 
        ggplot(aes(x = action, y = total.ratio, color = action)) +
        geom_boxplot() + 
        geom_jitter(position = position_jitter(0.2), alpha = I(0.25)) +
        facet_grid(. ~ deltaAsset) +
        theme_bw()
      g + stat_compare_means(method = "t.test", paired = TRUE, 
                             comparisons = list(c("buy", "no trade"), c("no trade", "sell"), c("buy", "sell")),
                             label = "p.signif",
                             label.y = c(0.6, 0.65, 0.7))
    })
    
    output$dAsA_fvizPlot <- renderPlot({
      g <- fviz_cluster(dAsA_clusters(),          # 分群結果
                        data = dAsA_data,         # 資料
                        geom = c("point","text"), # 點和標籤(point & label)
                        frame.type = "norm") +    # 框架型態
        theme_bw()
      g
    })
    
    output$dAsA_elbowPlot <- renderPlot({
      g <- fviz_nbclust(dAsA_data, 
                        FUNcluster = kmeans,# K-Means
                        method = "wss",     # total within sum of square
                        k.max = 12) +       # max number of clusters to consider
        labs(title="Elbow Method for K-Means") +
        theme_bw()
      g
    })
    
    output$dAsA_dendPlot <- renderPlot({
      g <- fviz_dend(hkmeans(dAsA_data, input$dAsA_k), cex = 0.6)
      g
    })
  }
  
  # page: "decision"
  {
    DPCOC <- reactive({
      dDF %>% 
        filter(Trials >= input$d_TrialRange[1], Trials <= input$d_TrialRange[2], 
               CheckHistory %in% input$CH, Outcome %in% input$OC) %>% 
        select(PriceChange, Decision) %>% 
        group_by(PriceChange, Decision) %>% 
        summarise(Count = length(Decision))
    })
    
    output$d_plot <- renderPlot({
      g <- ggplot(DPCOC(), aes(x = PriceChange, y = Count, fill = Decision)) +
        geom_bar(stat = "identity", position = "dodge") + 
        theme_bw()
      g
    })
    
    dMat <- reactive({
      . <- dDF %>% 
        filter(Trials >= input$d_TrialRange[1], Trials <= input$d_TrialRange[2],
               CheckHistory %in% input$CH, Outcome %in% input$OC,
               Player %in% input$no.player)
      .table <- table(.$Player, .$Decision, .$PriceChange)
      nPlayer <- dim(.table)[1] #table(.$Player,,)
      dMat <- matrix(.table, nrow = nPlayer)
      dMat <- dMat / rowSums(dMat) 
      colnames(dMat) <- c("FALL-buy","FALL-no trade","FALL-sell",
                          "RISE-buy","RISE-no trade","RISE-sell",
                          "STALL-buy","STALL-no trade","STALL-sell")
      dMat
    })
    
    output$d_kmeanPlot <- renderPlot({
      g <- fviz_nbclust(dMat(), 
                       FUNcluster = kmeans,# K-Means
                       method = "wss",     # total within sum of square
                       k.max = 12) +       # max number of clusters to consider
        labs(title="Elbow Method for K-Means") +
        theme_bw()
      g
    })
    
    output$d_dendPlot <- renderPlot({
      g <- fviz_dend(hkmeans(dMat(), input$d_Clusters), cex = 0.6)
      g
    })
    
    
  }
  # decision2
  {
    observe({
      maxCluster <- as.integer(input$d_Clusters)
      updateSelectInput(session, "d_selectCluster", choices = 1:maxCluster)
    })
    
    .dKmeans <- reactive({
      set.seed(408516)
      . <- kmeans(dMat(), input$d_Clusters, iter.max = 20, nstart = 30)
      .
    })
    
    dClusterTable <- reactive({
      dClusterTable <- as.data.frame(dMat())
      dClusterTable$player <- c(1:160)
      dClusterTable$cluster <- .dKmeans()$cluster
      dClusterTable %>% select(cluster, player, 1:9)
    })
    
    output$d_clustersTable <- DT::renderDataTable(
      DT::datatable(
        {dClusterTable() %>% 
          filter(cluster == as.integer(input$d_selectCluster))
        },
        options = list(paging = FALSE)
      )
    )
    
    dSelectTable <- reactive({
      dClusterTable() %>% 
        filter(cluster == as.integer(input$d_selectCluster)) %>% 
        gather(key = "dimansions", value = "ratio.total", -cluster, -player) %>% 
        select(-c(cluster, player)) %>% 
        group_by(dimansions) %>% 
        summarise(n = length(ratio.total), 
                  ratio.total.mean = mean(ratio.total), 
                  ratio.total.sd = sd(ratio.total)) %>% 
        mutate(priceChange = sapply(strsplit(dimansions, "-"), "[", 1), 
               action = sapply(strsplit(dimansions, "-"), "[", 2))
    })
    
    output$d_summarise <- renderTable(
      dSelectTable() %>% 
        select(priceChange, action, n, ratio.total.mean, ratio.total.sd)
    )
    output$d_plot2 <- renderPlotly({
      g <- dSelectTable() %>% 
        ggplot(aes(x = priceChange, y = ratio.total.mean, fill = action)) +
          geom_bar(stat = "identity", position = "dodge") + 
          geom_errorbar(aes(ymin = ifelse((ratio.total.mean - ratio.total.sd) > 0, ratio.total.mean - ratio.total.sd, 0),
                            ymax = ratio.total.mean + ratio.total.sd),
                        position = position_dodge(0.9), width = 0.1) +
          coord_cartesian(ylim = c(input$d_ylim[1], input$d_ylim[2])) +
          theme_bw()
      ggplotly(g)
    })
    
    output$d_testPlot <- renderPlot({
      g <- dClusterTable() %>% 
        filter(cluster == as.integer(input$d_selectCluster)) %>% 
        gather(key = "dimansions", value = "ratio.total", -cluster, -player) %>%
        mutate(priceChange = sapply(strsplit(dimansions, "-"), "[", 1), 
               action = sapply(strsplit(dimansions, "-"), "[", 2)) %>% 
        ggplot(aes(x = action, y = ratio.total, color = action)) +
        geom_boxplot() + 
        geom_jitter(position = position_jitter(0.2), alpha = I(0.25)) +
        facet_grid(. ~ priceChange) +
        theme_bw()
      g + stat_compare_means(method = input$d_testMethod, paired = TRUE, 
                             comparisons = list(c("buy", "no trade"), c("no trade", "sell"), c("buy", "sell")),
                             label = "p.signif",
                             label.y = c(0.4, 0.45, 0.5))
    })
  }
  # predict cluster
  {
    predictTable <- reactive({
      .predictList <- list()
      
      for(i in 1:numOfData){
        .select <- dDF %>% 
          filter(Player == i) %>% 
          slice(-101)
        
        .list <- list(
          .balence <- slice(.select %>% slice(1:20)),
          .bubble <- slice(.select %>% slice(21:60)),
          .bal_bab <- slice(.select %>% slice(1:60)),
          .burst <- slice(.select %>% slice(61:100)),
          .all <- .select
        )
        .ratio <- c()
        for(.index in 1:length(.list)){
          . <- .list[[.index]]
          .table <- table(.$PriceChange, .$Decision)
          .mat <- matrix(t(.table), nrow = 1)
          .mat <- .mat / rowSums(.mat) 
          colnames(.mat) <- c("FALL-buy","FALL-no trade","FALL-sell",
                              "RISE-buy","RISE-no trade","RISE-sell",
                              "STALL-buy","STALL-no trade","STALL-sell")
          .ratio <- rbind(.ratio, .mat)
        }
        .cluster <- predict_KMeans(.ratio, .dKmeans()$centers)
        class(.cluster) <- NULL
        .predictList[[i]] <- .cluster
      }
      
      predictTable <- as.data.frame(do.call("rbind", .predictList))
      names(predictTable) <- c("Balance(1-20)", "Bubble(21-60)", 
                               "Bal+Bub(1-60)", "Burst(61-100)", "All(1-100)")
      predictTable$Player <- 1:numOfData
      predictTable$Pair <- rep(1:numOfPair, each = 2)
      predictTable$Outcome <- sapply(data, function(.){.$Outcome[1]})
      return(predictTable)
    })
    
    output$d_predictTable <- DT::renderDataTable(
      DT::datatable(
        {predictTable() %>% 
            select(Player, Pair, `Balance(1-20)`:`All(1-100)`, Outcome)
        },
        options = list(paging = FALSE),
        rownames = FALSE
      )
    )
    
    output$d_downloadData <- downloadHandler(
      filename = function(){
        paste0("dP&A_Kmeans_prdictTable_k_", input$d_Clusters, "_", 
               "range_", input$d_TrialRange[1], "-", input$d_TrialRange[2],".csv")
      },
      content = function(file){
        .download <- predictTable() %>% 
          select(Player, Pair, `Balance(1-20)`:`All(1-100)`, Outcome)
        write.csv(.download, file)
      }
  
    )
    
  }
  
  
}
