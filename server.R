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
  # d_decision2
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
                  ratio.total.sd = sd(ratio.total),
                  ratio.total.se = sd(ratio.total) / sqrt(length(ratio.total))) %>% 
        mutate(priceChange = sapply(strsplit(dimansions, "-"), "[", 1), 
               action = sapply(strsplit(dimansions, "-"), "[", 2))
    })
    
    output$d_summarise <- renderTable(
      dSelectTable() %>% 
        select(priceChange, action, n, ratio.total.mean, ratio.total.sd)
    )
    output$d_plot2 <- renderPlotly({
      .err <- switch(input$d_errorBar, 
                     "se" = parse(text = "ratio.total.se"),
                     "sd" = parse(text = "ratio.total.sd"))
      
      g <- dSelectTable() %>% 
        ggplot(aes(x = priceChange, y = ratio.total.mean, fill = action)) +
          geom_bar(stat = "identity", position = "dodge") + 
          geom_errorbar(aes(ymin = ifelse((ratio.total.mean - eval(.err)) > 0, ratio.total.mean - eval(.err), 0),
                            ymax = ratio.total.mean + eval(.err)),
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
  # d_predict cluster
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
  
  
  # dc_kmeans
  {
    dc_data <- reactive({
      . <- dDF %>% 
        filter(Trials >= input$dc_trialRange[1], Trials <= input$dc_trialRange[2])
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
      
      if (as.logical(input$dc_na.rm) == TRUE) {
        if (sum(is.na(.dMat2)) > 0) {
          rmRow <- unique(which(is.na(.dMat2)) %% .nPlayer)
          .dMat2 <- .dMat2[-rmRow, ]
        } 
      }
      return(.dMat2)
    })
    
    output$dc_overAllPlot <- renderPlot({
      data <- data.frame(PriceChange = sapply(strsplit(colnames(dc_data()), "-"), "[", 1),
                         Decision = sapply(strsplit(colnames(dc_data()), "-"), "[", 2),
                         Ratio = dc_data() %>% apply(2, mean))
       
      g <- ggplot(data, aes(x = PriceChange, y = Ratio, fill = Decision)) +
        geom_bar(stat = "identity", position = "dodge") + 
        theme_bw() + 
        coord_cartesian(ylim = c(0, 0.8))
      g
    })
    
    output$dc_kmeanPlot <- renderPlot({
      g <- fviz_nbclust(dc_data(), 
                        FUNcluster = kmeans,# K-Means
                        method = "wss",     # total within sum of square
                        k.max = 12) +       # max number of clusters to consider
        labs(title="Elbow Method for K-Means") +
        theme_bw()
      g
    })
    
    output$dc_dendPlot <- renderPlot({
      g <- fviz_dend(hkmeans(dc_data(), input$dc_k), cex = 0.6)
      g
    })
    
  }
  # dc_clsters
  {
    observeEvent(input$dc_trialRange, {
      print("HI")
      trialRange <- input$dc_trialRange
      isolate(updateSliderInput(session, "dc_mleTrialRange", value = trialRange))
    })
    observeEvent(input$dc_mleTrialRange, {
      print("YO")
      trialRange <- input$dc_mleTrialRange
      isolate(updateSliderInput(session, "dc_trialRange", value = trialRange))
    })
    
    observe({
      maxCluster <- as.integer(input$dc_k)
      updateSelectInput(session, "dc_selectCluster", choices = 1:maxCluster)
      updateSelectInput(session, "dc_mleCluster", choices = 1:maxCluster)
      updateSelectInput(session, "dc_mleCluster2", choices = 1:maxCluster)
    })
    
    dc_Kmeans <- reactive({
      set.seed(408516)
      if (input$dc_clusterMethod == "kmeans") {
        . <- kmeans(dc_data(), input$dc_k, iter.max = 20, nstart = 30)
      } else {
        . <- hkmeans(dc_data(), input$dc_k)
      }
      .
    })
    
    dc_clusterTable <- reactive({
      .clusterTable <- as.data.frame(round(dc_data(), 3))
      .clusterTable$player <- rownames(.clusterTable)
      .clusterTable$cluster <- dc_Kmeans()$cluster
      return(.clusterTable)
    })
    
    output$dc_clustersTable <- DT::renderDataTable(
      DT::datatable(
        {dc_clusterTable() %>% 
           filter(cluster == as.integer(input$dc_selectCluster)) %>% 
           select(cluster, player, 1:9)
        },
        options = list(paging = FALSE)
      )
    )
    
    dc_selectTable <- reactive({
      dc_clusterTable() %>% 
        filter(cluster == as.integer(input$dc_selectCluster)) %>% 
        gather(key = "dimansions", value = "ratio", -cluster, -player) %>% 
        select(-c(cluster, player)) %>% 
        group_by(dimansions) %>% 
        summarise(n = length(ratio), 
                  ratio.total.mean = mean(ratio), 
                  ratio.total.sd = sd(ratio),
                  ratio.total.se = sd(ratio)/sqrt(length(ratio))) %>% 
        mutate(priceChange = sapply(strsplit(dimansions, "-"), "[", 1), 
               action = sapply(strsplit(dimansions, "-"), "[", 2))
    })
    
    output$dc_clusterPlot <- renderPlotly({
      .err <- switch(input$dc_errorBar,
                     "sd" = parse(text = "ratio.total.sd"),
                     "se" = parse(text = "ratio.total.se"))
      
      g <- dc_selectTable() %>% 
        ggplot(aes(x = priceChange, y = ratio.total.mean, fill = action)) +
        geom_bar(stat = "identity", position = "dodge") + 
        geom_errorbar(aes(ymin = ifelse((ratio.total.mean - eval(.err)) > 0, ratio.total.mean - eval(.err), 0),
                          ymax = ratio.total.mean + eval(.err)),
                      position = position_dodge(0.9), width = 0.1) +
        coord_cartesian(ylim = c(input$dc_ylim[1], input$dc_ylim[2])) +
        theme_bw()
      ggplotly(g)
    })
    
    output$dc_summarise <- renderTable({
      dc_selectTable() %>% 
        select(priceChange, action, n, ratio.total.mean, ratio.total.sd)
    })
    
  }
  # dc_MLE & LR test
  {
    # UI
    output$dc_uiCondition <- renderUI({
      switch(input$dc_condition,
             "intra-condition" = {tagList(
               sliderInput("dc_mleTrialRange", "Select: trial range 1",
                           min = 1, max = 100, value = c(1, 100)),
               selectInput("dc_mleCluster", "Select: cluster", 
                           choices = 1:4, selected = 1),
               selectInput("dc_mleDPCondi", "Select: delta Price condition",
                           choices = c("FALL", "RISE", "STALL")),
               hr(),
               h4("General model"),
               selectInput("dc_mleGenModel", "Select: general model",
                           choices = c("df=2 (p,q,r unknown)",
                                       "df=2n (p_i,q_i,r_i unknown)",
                                       "df=1 (p = q unknown)",
                                       "df=1 (p = r unknown)",
                                       "df=1 (q = r unknown)")),
               
               hr(),
               h4("Restrict model"),
               selectInput("dc_mleResModel", "Select: restrict model",
                           choices = c("df=2 (p,q,r unknown)",
                                       "df=2 (p larger)",
                                       "df=2 (q larger)",
                                       "df=2 (r larger)",
                                       "df=1 (p = q unknown)",
                                       "df=1 (p = r unknown)",
                                       "df=1 (q = r unknown)",
                                       "df=1 (p given)",
                                       "df=1 (q given)",
                                       "df=1 (r given)",
                                       "df=0 (p,q,r given)"))
             )},
             
             "inter-condition" = {tagList(
               sliderInput("dc_mleTrialRange", "Select: trial range 1",
                           min = 1, max = 100, value = c(1, 100)),
               selectInput("dc_mleCluster", "Select: cluster 1", 
                           choices = 1:4, selected = 1),
               selectInput("dc_mleDPCondi", "Select: delta Price condition 1",
                           choices = c("FALL", "RISE", "STALL")),
               sliderInput("dc_mleTrialRange2", "Select: trial range 2",
                           min = 1, max = 100, value = c(1, 100)),
               selectInput("dc_mleCluster2", "Select: cluster 2", 
                           choices = 1:4, selected = 2),
               selectInput("dc_mleDPCondi2", "Select: delta Price condition 2",
                           choices = c("FALL", "RISE", "STALL")),
               hr(),
               h4("General model"),
               selectInput("dc_mleGenModel", "Select: general model",
                           choices = c("df=4 (p_i,q_i,r_i unknown)",
                                       "df=2 (p,q,r unknown)")),
               
               hr(),
               h4("Restrict model"),
               selectInput("dc_mleResModel", "Select: restrict model",
                           choices = c("df=2 (p,q,r unknown)"))
             )})
    })
  
    observe({
      .p <- as.numeric(input$dc_mleRes_p)
      .q <- as.numeric(input$dc_mleRes_q)
      .r <- 1-.p-.q
      updateTextInput(session, "dc_mleRes_r", value = as.character(.r))
    })
    output$dc_mleRes_pqr <- renderUI({
      switch (input$dc_mleResModel, 
              "df=0 (p,q,r given)" = tagList(textInput("dc_mleRes_p", "p =", value = 1/3),
                                             textInput("dc_mleRes_q", "q =", value = 1/3),
                                             textInput("dc_mleRes_r", "r = 1-p-q (automatic)", value = 1/3)),
              "df=1 (p given)" = textInput("dc_mleRes_1", "p =", value = "0.5"),
              "df=1 (q given)" = textInput("dc_mleRes_1", "q =", value = "0.5"), 
              "df=1 (r given)" = textInput("dc_mleRes_1", "r =", value = "0.5"),
              "df=2 (p larger)"= textInput("dc_mleRes_2", "p >=", value = "0.55"),
              "df=2 (q larger)"= textInput("dc_mleRes_2", "q >=", value = "0.55"),
              "df=2 (r larger)"= textInput("dc_mleRes_2", "r >=", value = "0.55"))
    })
    
    # new second cluster data (for inter-condition)
    dc_clusterTable2 <- reactive({
      . <- dDF %>% 
        filter(Trials >= input$dc_mleTrialRange2[1], Trials <= input$dc_mleTrialRange2[2])
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
      
      if (as.logical(input$dc_na.rm) == TRUE) {
        if (sum(is.na(.dMat2)) > 0) {
          rmRow <- unique(which(is.na(.dMat2)) %% .nPlayer)
          .dMat2 <- .dMat2[-rmRow, ]
        } 
      }
      
      set.seed(408516)
      if (input$dc_clusterMethod == "kmeans") {
        .kmeans <- kmeans(.dMat2, input$dc_k, iter.max = 20, nstart = 30)
      } else {
        .kmeans <- hkmeans(.dMat2, input$dc_k)
      }
      
      .clusterTable <- as.data.frame(round(.dMat2, 3))
      .clusterTable$player <- rownames(.clusterTable)
      .clusterTable$cluster <- .kmeans$cluster
      return(.clusterTable)
    })
    
    # select data
    dc_mleData <- reactive({
      .ct <- dc_clusterTable() %>% 
        as.data.frame() %>% 
        filter(cluster == as.integer(input$dc_mleCluster)) %>% 
        select(switch(input$dc_mleDPCondi,
          "FALL" = 1:3,   # "FAll-buy":"FALL-sell"
          "RISE" = 4:6,   # "RISE-buy":"RISE-sell"
          "STALL"= 7:9))  # "STAll-buy":"STALL-sell"
      if (input$dc_condition == "inter-condition") {
        .ct2 <- dc_clusterTable2() %>% 
          as.data.frame() %>% 
          filter(cluster == as.integer(input$dc_mleCluster2)) %>% 
          select(switch(input$dc_mleDPCondi2,
                        "FALL" = 1:3,   # "FAll-buy":"FALL-sell"
                        "RISE" = 4:6,   # "RISE-buy":"RISE-sell"
                        "STALL"= 7:9))  # "STAll-buy":"STALL-sell"
        .ct$group <- "group1"
        .ct2$group <- "group2"
        names(.ct2) <- names(.ct)
        .ct <- rbind(.ct, .ct2)
      }
      return(.ct)  
    }) 

    # plot
    output$dc_mlePlot <- renderPlotly({
      .data <- dc_clusterTable() %>% 
        filter(cluster == as.integer(input$dc_mleCluster)) %>% 
        gather(key = "dimansions", value = "ratio", -cluster, -player) %>% 
        select(-player) %>% 
        group_by(cluster, dimansions) %>% 
        summarise(n = length(ratio), 
                  ratio.total.mean = mean(ratio), 
                  ratio.total.sd = sd(ratio)) %>% 
        mutate(priceChange = sapply(strsplit(dimansions, "-"), "[", 1), 
               action = sapply(strsplit(dimansions, "-"), "[", 2),
               group = "group1") %>% 
        filter(priceChange == input$dc_mleDPCondi)
      
      if (input$dc_condition == "inter-condition") {
        .data2 <- dc_clusterTable2() %>% 
          filter(cluster == as.integer(input$dc_mleCluster2)) %>% 
          gather(key = "dimansions", value = "ratio", -cluster, -player) %>% 
          select(-player) %>% 
          group_by(cluster, dimansions) %>% 
          summarise(n = length(ratio), 
                    ratio.total.mean = mean(ratio), 
                    ratio.total.sd = sd(ratio)) %>% 
          mutate(priceChange = sapply(strsplit(dimansions, "-"), "[", 1), 
                 action = sapply(strsplit(dimansions, "-"), "[", 2),
                 group = "group2") %>% 
          filter(priceChange == input$dc_mleDPCondi2)
        
        .data <- rbind(.data, .data2)
      #   .data <- .data %>% filter(priceChange == input$dc_mleDPCondi)
      # } else {
      #   .data <- .data %>% 
      #     filter((priceChange == input$dc_mleDPCondi & cluster == as.integer(input$dc_mleCluster)) | 
      #            (priceChange == input$dc_mleDPCondi2 & cluster == as.integer(input$dc_mleCluster2)))
      }
      
      g <- ggplot(.data, aes(x = priceChange, y = ratio.total.mean, fill = action)) +
        geom_histogram(stat = "identity", position = "dodge") + 
        facet_wrap(~ group) +
        theme_bw()
      
      ggplotly(g)
    })
    
    # nll function
    nll_condition <- function(par, x, condition, par_giv = NULL) {
      p <- vector("numeric", length = 3)
      
      switch(condition, 
             "df4"         = {p[1] <- par[1];                  p[2] <- par[2];                  p[3] <- 1-par[1]-par[2]},
             "df2n"        = {p[1] <- par[1];                  p[2] <- par[2];                  p[3] <- 1-par[1]-par[2]},
             "df2"         = {p[1] <- par[1];                  p[2] <- par[2];                  p[3] <- 1-par[1]-par[2]},
             "df2:larger_p"= {p[1] <- par[1]+par_giv;          p[2] <- par[2];                  p[3] <- 1-par[1]-par[2]-par_giv},
             "df2:larger_q"= {p[1] <- 1-par[1]-par[2]-par_giv; p[2] <- par[1]+par_giv;          p[3] <- par[2]},
             "df2:larger_r"= {p[1] <- par[2];                  p[2] <- 1-par[1]-par[2]-par_giv; p[3] <- par[1]+par_giv},
             "df1:same_pq" = {p[1] <- par;                     p[2] <- par;                     p[3] <- 1-2*par},
             "df1:same_pr" = {p[1] <- par;                     p[2] <- 1-2*par;                 p[3] <- par},
             "df1:same_qr" = {p[1] <- 1-2*par;                 p[2] <- par;                     p[3] <- par},
             "df1:given_p" = {p[1] <- par_giv;                 p[2] <- par;                     p[3] <- 1-par-par_giv},
             "df1:given_q" = {p[1] <- 1-par-par_giv;           p[2] <- par_giv;                 p[3] <- par},
             "df1:given_r" = {p[1] <- par;                     p[2] <- 1-par-par_giv;           p[3] <- par_giv},
             "df0"         = {p[1] <- par_giv[1];              p[2] <- par_giv[2];              p[3] <- par_giv[3]}
      )
      
      logLike <- sum(x * log(p)) # omit constant
      return(-logLike)
    }
    nll <- function(parameters_est, data, condition, parameters_giv = NULL) {
      negLogLike <- 0
      
      if (is.null(parameters_giv)) {
        parLogis <- plogis(parameters_est) # restrict p in 0 ~ 1
      } else {
        parameters_giv <- plogis(parameters_giv)
        parLogis <- plogis(parameters_est)*(1-parameters_giv)
      }
      
      n <- nrow(data)
      if (condition == "df2n") {
        for (i in 1:n) {
          .x <- data[i, 1:3]
          .parLogis <- parLogis[(2*i-1):(2*i)]
          negLogLike <- negLogLike + nll_condition(.parLogis, .x, condition, parameters_giv)
        }
      } else if (condition == "df4") {
        .nGroup1 <- data %>% filter(group == "group1") %>% nrow()
        for (i in 1:n) {
          .x <- data[i, 1:3]
          if (i <= .nGroup1) {
            .parLogis <- parLogis[1:2]
          } else{
            .parLogis <- parLogis[3:4]
          } 
          negLogLike <- negLogLike + nll_condition(.parLogis, .x, condition, parameters_giv)
        }
      } else {
        for (i in 1:n) {
          .x <- data[i, 1:3]
          negLogLike <- negLogLike + nll_condition(parLogis, .x, condition, parameters_giv)
        }
      }
      
      return(negLogLike)
    }
    
    # general model
    dc_generalModel <- eventReactive(input$dc_update, {
      
      switch(input$dc_mleGenModel,
             "df=4 (p_i,q_i,r_i unknown)" = {.condition <- "df4"
                                             .parameter_general <- qlogis(rep(1/5, 4))},
             "df=2n (p_i,q_i,r_i unknown)" = {.condition <- "df2n"
                                              .initiVal<- dc_mleData()[c(1, 2)] %>% as.matrix() %>% t() %>% matrix(ncol = 1) 
                                              .initiVal <- .initiVal-0.0001 # translate
                                              .initiVal[which(.initiVal <= 0)] <- .initiVal[which(.initiVal <= 0)]+0.00011 # translate to prevent log(0)
                                              .parameter_general <- qlogis(.initiVal)},
             "df=2 (p,q,r unknown)" = {.condition <- "df2"
                                       .parameter_general <- qlogis(rep(1/3, 2))},
             "df=1 (p = q unknown)" = {.condition <- "df1:same_pq"
                                       .parameter_general <- qlogis(c(0.1))},
             "df=1 (p = r unknown)" = {.condition <- "df1:same_pr"
                                       .parameter_general <- qlogis(c(0.1))},
             "df=1 (q = r unknown)" = {.condition <- "df1:same_qr"
                                       .parameter_general <- qlogis(c(0.1))}
      )
      
      .result <- nlm(nll, .parameter_general, dc_mleData(), condition = .condition, hessian = TRUE)
      return(.result)
    })
    
    # restrict model
    dc_restrictModel <- eventReactive(input$dc_update, { 
      .parameter_given <- NULL
      switch(input$dc_mleResModel,
        "df=2 (p,q,r unknown)" = {.condition <- "df2"
                                  .parameter_restrict <- qlogis(rep(1/3, 2))},
        "df=2 (p larger)" = {.condition <- "df2:larger_p"
                             .parameter_given <- qlogis(as.numeric(input$dc_mleRes_2))
                             .parameter_restrict <- qlogis(rep(1/3, 2))},
        "df=2 (q larger)" = {.condition <- "df2:larger_q"
                             .parameter_given <- qlogis(as.numeric(input$dc_mleRes_2))
                             .parameter_restrict <- qlogis(rep(1/3, 2))},
        "df=2 (r larger)" = {.condition <- "df2:larger_r"
                             .parameter_given <- qlogis(as.numeric(input$dc_mleRes_2))
                             .parameter_restrict <- qlogis(rep(1/3, 2))},
        "df=1 (p = q unknown)" = {.condition <- "df1:same_pq"
                                  .parameter_restrict <- qlogis(c(0.1))},
        "df=1 (p = r unknown)" = {.condition <- "df1:same_pr"
                                  .parameter_restrict <- qlogis(c(0.1))},
        "df=1 (q = r unknown)" = {.condition <- "df1:same_qr"
                                  .parameter_restrict <- qlogis(c(0.1))},
        "df=1 (p given)" = {.condition <- "df1:given_p"
                            .parameter_given <- qlogis(as.numeric(input$dc_mleRes_1))
                            .parameter_restrict <- qlogis(c(0.1))},
        "df=1 (q given)" = {.condition <- "df1:given_q"
                            .parameter_restrict <- qlogis(c(0.1))
                            .parameter_given <- qlogis(as.numeric(input$dc_mleRes_1))},
        "df=1 (r given)" = {.condition <- "df1:given_r"
                            .parameter_restrict <- qlogis(c(0.1))
                            .parameter_given <- qlogis(as.numeric(input$dc_mleRes_1))},
        "df=0 (p,q,r given)" = {.condition <- "df0"
                                .parameter_given <- qlogis(as.numeric(c(input$dc_mleRes_p, input$dc_mleRes_q, input$dc_mleRes_r)))
                                .parameter_restrict <- -99} #我們沒有要估計的參數，任意設的
      )
      .result <- nlm(nll, .parameter_restrict, dc_mleData(), condition = .condition, parameters_giv = .parameter_given, hessian = TRUE)
      return(.result)
      
    }) 
    
    # output result: MLE
    dc_mleParameter <- reactive({
      input$dc_update
      isolate({
        .parName <- c("p_buy", "q_noTrade", "r_sell")
        if (input$dc_mleResModel != "df=0 (p,q,r given)") {.h_res <- sqrt(diag(solve(dc_restrictModel()$hessian)))}
        .h_gen <- sqrt(diag(solve(dc_generalModel()$hessian)))
        .hessian_res <- NA
        .hessian_gen <- NA
        
        .parEstimate_gen <- vector("numeric", length = 3)
        switch(input$dc_mleGenModel,
               "df=4 (p_i,q_i,r_i unknown)" = {.n <- length(dc_generalModel()$estimate)/2
                                               .pl_est <- round(plogis(dc_generalModel()$estimate), digits = 3)
                                               .parEstimate_gen[1] <- paste(.pl_est[seq(1, 2*.n, 2)], collapse = ", ")
                                               .parEstimate_gen[2] <- paste(.pl_est[seq(2, 2*.n, 2)], collapse = ", ")
                                               .parEstimate_gen[3] <- paste(round(1-(.pl_est[seq(1, 2*.n, 2)]+.pl_est[seq(2, 2*.n, 2)]), digits = 3), collapse = ", ")
                                               .hessian_gen <- c(.h_gen[1:2], 0, .h_gen[3:4], 0)},
               "df=2n (p_i,q_i,r_i unknown)" = {.n <- length(dc_generalModel()$estimate)/2
                                                .pl_est <- round(plogis(dc_generalModel()$estimate), digits = 3)
                                                .parEstimate_gen[1] <- paste(.pl_est[seq(1, 2*.n, 2)], collapse = ", ")
                                                .parEstimate_gen[2] <- paste(.pl_est[seq(2, 2*.n, 2)], collapse = ", ")
                                                .parEstimate_gen[3] <- paste(round(1-(.pl_est[seq(1, 2*.n, 2)]+.pl_est[seq(2, 2*.n, 2)]), digits = 3), collapse = ", ")},
               "df=2 (p,q,r unknown)" = {.parEstimate_gen <- plogis(dc_generalModel()$estimate)
                                         .parEstimate_gen[3] <- 1 - sum(.parEstimate_gen)
                                         .hessian_gen <- c(.h_gen, 0)},
               "df=1 (p = q unknown)" = {.parEstimate_gen[c(1, 2)] <- plogis(dc_generalModel()$estimate)
                                         .parEstimate_gen[3] <- 1 - sum(.parEstimate_gen)
                                         .hessian_gen <- c(0, 0, .h_gen)},
               "df=1 (p = r unknown)" = {.parEstimate_gen[c(1, 3)] <- plogis(dc_generalModel()$estimate)
                                         .parEstimate_gen[2] <- 1 - sum(.parEstimate_gen)
                                         .hessian_gen <- c(0, .h_gen, 0)},
               "df=1 (q = r unknown)" = {.parEstimate_gen[c(2, 3)] <- plogis(dc_generalModel()$estimate)
                                         .parEstimate_gen[1] <- 1 - sum(.parEstimate_gen)
                                         .hessian_gen <- c(.h_gen, 0, 0)}
        )
        
        .parEstimate_res <- vector("numeric", length = 3)
        switch(input$dc_mleResModel,
               "df=2 (p,q,r unknown)" = {.parEstimate_res <- plogis(dc_restrictModel()$estimate)
                                         .parEstimate_res[3] <- 1 - sum(.parEstimate_res)
                                         .hessian_res <- c(.h_res, 0)},
               "df=2 (p larger)" = {.parEstimate_res[1] <- plogis(dc_restrictModel()$estimate)[1]*(1-as.numeric(input$dc_mleRes_2))+as.numeric(input$dc_mleRes_2)
                                    .parEstimate_res[2] <- plogis(dc_restrictModel()$estimate)[2]*(1-as.numeric(input$dc_mleRes_2))
                                    .parEstimate_res[3] <- 1-sum(.parEstimate_res)},
               "df=2 (q larger)" = {.parEstimate_res[2] <- plogis(dc_restrictModel()$estimate)[1]*(1-as.numeric(input$dc_mleRes_2))+as.numeric(input$dc_mleRes_2)
                                    .parEstimate_res[3] <- plogis(dc_restrictModel()$estimate)[2]*(1-as.numeric(input$dc_mleRes_2))
                                    .parEstimate_res[1] <- 1-sum(.parEstimate_res)},
               "df=2 (r larger)" = {.parEstimate_res[3] <- plogis(dc_restrictModel()$estimate)[1]*(1-as.numeric(input$dc_mleRes_2))+as.numeric(input$dc_mleRes_2)
                                    .parEstimate_res[1] <- plogis(dc_restrictModel()$estimate)[2]*(1-as.numeric(input$dc_mleRes_2))
                                    .parEstimate_res[2] <- 1-sum(.parEstimate_res)},
               "df=1 (p given)" = {.parEstimate_res[1] <- as.numeric(input$dc_mleRes_1)
                                   .parEstimate_res[2] <- plogis(dc_restrictModel()$estimate)*(1-.parEstimate_res[1])
                                   .parEstimate_res[3] <- 1-sum(.parEstimate_res)},
               "df=1 (q given)" = {.parEstimate_res[2] <- as.numeric(input$dc_mleRes_1)
                                   .parEstimate_res[3] <- plogis(dc_restrictModel()$estimate)*(1-.parEstimate_res[2])
                                   .parEstimate_res[1] <- 1 - sum(.parEstimate_res)},
               "df=1 (r given)" = {.parEstimate_res[3] <- as.numeric(input$dc_mleRes_1)
                                   .parEstimate_res[1] <- plogis(dc_restrictModel()$estimate)*(1-.parEstimate_res[3])
                                   .parEstimate_res[2] <- 1 - sum(.parEstimate_res)},
               "df=1 (p = q unknown)" = {.parEstimate_res[c(1, 2)] <- plogis(dc_restrictModel()$estimate)
                                         .parEstimate_res[3] <- 1 - sum(.parEstimate_res)
                                         .hessian_res <- c(0, 0, .h_res)},
               "df=1 (p = r unknown)" = {.parEstimate_res[c(1, 3)] <- plogis(dc_restrictModel()$estimate)
                                         .parEstimate_res[2] <- 1 - sum(.parEstimate_res)
                                         .hessian_res <- c(0, .h_res, 0)},
               "df=1 (q = r unknown)" = {.parEstimate_res[c(2, 3)] <- plogis(dc_restrictModel()$estimate)
                                         .parEstimate_res[1] <- 1 - sum(.parEstimate_res)
                                         .hessian_res <- c(.h_res, 0, 0)},
               "df=0 (p,q,r given)" = {.parEstimate_res <- as.numeric(c(input$dc_mleRes_p, input$dc_mleRes_q, input$dc_mleRes_r))})
        
        data.frame(parameter_name = .parName, 
                   restrict_model = .parEstimate_res, 
                   restrict_hessian = .hessian_res,
                   general_model = .parEstimate_gen,
                   general_hessian = .hessian_gen)
      })
    })
    output$dc_mleParameterTable <- renderTable({
      return(dc_mleParameter())
    })
    output$dc_mleParameterPlot <- renderPlotly({
      .est_res <- dc_mleParameter() %>% select(parameter_name, restrict_model, restrict_hessian)
      names(.est_res) <- c("parameter_name", "estimate", "hessian")
      .est_gen <- dc_mleParameter() %>% select(parameter_name, general_model, general_hessian)
      names(.est_gen) <- c("parameter_name", "estimate", "hessian")
      .est <- rbind(.est_res, .est_gen) %>% 
        mutate(ymin = estimate - hessian, ymax = estimate + hessian, 
               model = c(rep("restrict", 3), rep("general", 3)))
      
      print(.est)
      g <- ggplot(.est, aes(x = parameter_name, y = estimate, fill = model)) +
        geom_histogram(stat = "identity", position = "dodge") +
        geom_errorbar(aes(ymin = ymin, ymax = ymax),
                      position = position_dodge(0.9), width = 0.1) +
        theme_bw()
      
      ggplotly(g)
    })
    
    # output result: LR test
    output$dc_LRtest <- renderTable({
      nll_general <- dc_generalModel()$minimum
      df_general <- length(dc_generalModel()$estimate)
      aic_general <- 2*df_general + 2*nll_general
      
      nll_restrict <- dc_restrictModel()$minimum
      df_restrict <- ifelse(sum(dc_restrictModel()$estimate == -99), 0L, length(dc_restrictModel()$estimate))
      aic_restrict <- 2*df_restrict + 2*nll_restrict
      
      G2 <- 2 * (nll_restrict - nll_general)
      chisqCriteria <- qchisq(0.95, df = df_general - df_restrict)
      pvalue = 1 - pchisq(G2, df = df_general - df_restrict)
      if (pvalue <= 0.001) {significance <- "***"
      } else if (pvalue <= 0.01) {significance <- "**"
      } else if (pvalue <= 0.05) {significance <- "*"
      } else {significance <- "ns"
      }
      
      data.frame(df_restrict, nll_restrict, aic_restrict,
                 df_general, nll_general, aic_general, 
                 G2, chisqCriteria, pvalue, significance)

    })
  }
}
