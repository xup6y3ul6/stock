library(shiny)
source("data.R")

navbarPage(
  title = "EBG behavior data",
  # "all data table"
  {tabPanel("All data table",
    titlePanel(
      "All data table (behavior & hyperscanning)"
    ),
    
    fluidRow(
      column(4,
             selectInput("pairNo", "No. pair (1~80)", 1:numOfPair, selected = 1)),
      column(4,
             selectInput("player", "Selcet player", 
                         c("player 1", "player 2"))),
      column(4, 
             selectInput("mrkCnd", "Select market condition",
                         c("All", "Balence (1~20)", "Bubble (21~60)", "Burst (61~100)")))
    ),
    
    DT::dataTableOutput("allData")
  )},
  
  # [sp] "stock plot" 
  {tabPanel("Stock price",
     headerPanel(
       "Stock price & Player information (by Pair)"
     ),
     
     sidebarPanel(
       selectInput("pairNumber", "No. pair", 1:numOfPair, selected = 1),
       sliderInput("trialRange", "trial range",
                   min = 1, max = 101, value = c(1, 101)),
       
       tags$hr(style = "border-color: black"),
       tags$p("2nd plot selection"),
       
       actionButton("stock", "Stock hold"),
       actionButton("cash", "Cash"),
       actionButton("totalAsset", "Total asset"),
       actionButton("deltaAsset", "Delta asset"),
       actionButton("decision", "Decision")
     ),
     
     mainPanel(
       plotlyOutput("sp_StockPricePlot"),
       plotlyOutput("sp_SelectedVarPlot")
     )
  )},
  
  # [ipa](interplayer action) 
  # "Action(i,t)~Action(j,t): K-means"
  {tabPanel(withMathJax(helpText("\\(A_i^t\\,vs.\\,A_j^t\\)")),
    headerPanel(
      "當期票價下 對手與自己的決策組合，做Kmeans clustering"
    ),
    sidebarPanel(
     selectInput('ipa_xcol', 'X Variable', names(decisionDF)),
     selectInput('ipa_ycol', 'Y Variable', names(decisionDF),
                 selected = names(decisionDF)[[9]]),
     numericInput('ipa_k', 'Cluster count', 4,
                  min = 1, max = 9),
     checkboxGroupInput("ipa_no.pair", "Select: the pair of players",
                        choices = unique(decisionDF$Player),
                        selected = unique(decisionDF$Player), 
                        inline = TRUE)
    ),
    mainPanel(
      plotOutput('ipa_kmeansPlot'),
      plotOutput('ipa_fvizPlot'),
      plotOutput('ipa_elbowPlot'),
      plotOutput('ipa_silhouettePlot'),
      plotOutput('ipa_dendPlot')
    )
  )},
  
  # [ipal1](interplayer action lag 1) 
  # "Action(i,t)~Action(j, t-1): K-means(昊閎之前做的）"
  {
    tabPanel(withMathJax(helpText("\\(A_i^t\\,vs.\\,A_j^{t-1}\\)")),
             headerPanel(
               "前一期對手的決定 與 本期自己的決定，做Kmeans clustering"
             ),
             
             sidebarPanel(
               selectInput("time", "Select: experimental time",
                           choices = c("All", "Behavior", "Neural")),
               selectInput("period", "Select: game period",
                           choices = c("All", "Balance", "Bubble", "Burst")),
               hr(),
               numericInput("k_means", 'Cluster count', 4,
                            min = 1, max = 9)
             ),
             
             mainPanel(
               plotOutput("dcFvizPlot"),
               plotOutput("dcElbowPlot"),
               plotOutput("dcDendPlot")
             )
    )
  },
  
  # [dAsA](delta asset & action)
  # "deltaAsset((i-j),t)-Action(i,t): K-means"
  {
    tabPanel(withMathJax(helpText("\\(A_i^t\\,vs.\\,\\Delta Asset_{i-j}^t\\)")),
      headerPanel(
        "當期雙方的總資產差異與自己的決策，做K-means clustering"
      ),
      
      sidebarPanel(
        numericInput("dAsA_k", "Number of Clusters", 6,
                     min = 1, max = 9),
        selectInput("dAsA_selectCluster", "Select: cluster",
                    choices = 1:6, selected = 1),
        sliderInput("dAsA_ylim", "Select: y axis range", 
                    min = 0, max = 1, value = c(0, 0.5), step = 0.05)
      ),
      
      mainPanel(
        plotOutput("dAsA_clusterPlot"),
        plotOutput("dAsA_tTestPlot"),
        plotOutput("dAsA_fvizPlot"),
        plotOutput("dAsA_elbowPlot"),
        plotOutput("dAsA_dendPlot")
      )
    )
  },
  
  # [d]
  # PriceChange(t) vs. Action(t): K-means
  {tabPanel(withMathJax(helpText("\\(A_i^t\\,vs.\\,\\Delta P^t \\)")),
    headerPanel(
      "股票變化(漲/持平/跌)與決策配對(買/不買不賣/賣)"
    ), 
    
    tabsetPanel(
      tabPanel("K-means",
        sidebarPanel(
          sliderInput("d_TrialRange", "trial range",
                      min = 1, max = 100, value = c(1, 100)),
          checkboxGroupInput("OC", "Select: win or loss player",
                             choices = levels(dDF$Outcome), selected = levels(dDF$Outcome)),
          checkboxGroupInput("CH", "Select: if player checked history in trials",
                             choices = levels(dDF$CheckHistory), selected = levels(dDF$CheckHistory)),
          checkboxGroupInput("no.player", "Select: the players",
                             choices = unique(dDF$Player),
                             selected = unique(dDF$Player), 
                             inline = TRUE)
        ),
        mainPanel(
          plotOutput("d_plot"),
          plotOutput("d_kmeanPlot"),
          plotOutput("d_dendPlot")
          
        )
      ),
      
      tabPanel("by Cluster",
        sidebarPanel(
        numericInput('d_Clusters', 'Number of Clusters', 4,
                    min = 1, max = 12),
        selectInput("d_selectCluster", "Select: cluster",
                   choices = 1:4, selected = 1),
        sliderInput("d_ylim", "Select: y axis range", 
                    min = 0, max = 1, value = c(0, 0.35), step = 0.05),
        hr(),
        selectInput("d_testMethod", "Select: test",
                    choices = c("wilcox.test", "t.test"))
        ),
        mainPanel(
        plotlyOutput("d_plot2"),
        plotOutput("d_testPlot"),
        tableOutput("d_summarise"),
        DT::dataTableOutput("d_clustersTable")
        )
      ),
      
      tabPanel("by Phase",
        
        mainPanel(
          downloadButton('d_downloadData', label = '點此下載下表'),
          DT::dataTableOutput("d_predictTable")
        )         
      )
    )
  )}

)
