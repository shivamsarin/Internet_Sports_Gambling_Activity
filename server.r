options(shiny.maxRequestSize=35*1024^2)
server <- function(input, output,session) {
  
  ###### Reading CSV Files ########
  filedata <- reactive({
    
    inFile <- input$file1
    
    if (is.null(inFile))
      return(NULL)
    
   
      f=read.csv(unz(inFile$datapath,"basetable.csv"),stringsAsFactors=FALSE) ###2####
    
    
    ###3###
    return(f)
    
    
  })
  
  
  ###### Validating Filter Data ###############
  filterValidaterTable <- function(df = NULL){
    
    if(!is.null(input$gender)){
      df <- subset(df,df$Gender %in% input$gender)
    }
    if(!is.null(input$country)){
      df <- subset(df,df$Country %in% input$country)
    }
    if(!is.null(input$product)){
      df <- subset(df,df$ProductID %in% input$product)
    }
    if(!is.null(input$app)){
      df <- subset(df,df$ApplicationID %in% input$app)
    }
    if(!is.null(input$lang)){
      df <- subset(df,df$Language %in% input$lang)
    }
    return(df)
  }
  ########### Output Data Table ##########
  output$table <- renderDataTable({
    
    if(is.null(filedata()))
    {return ()}
    else
    {
        data<-filedata()
        filterValidaterTable(data)
    }
    
    },options = list(searching = FALSE),rownames=FALSE)
 
  
  ############## Select Gender ########## 
  output$select_gender <- renderUI({ 
    
    pickerInput('gender', "Select Gender", na.omit(unique(sort(filedata()$Gender))),
                options = list(`actions-box` = TRUE),multiple = T,
                selected = na.omit(unique(sort(filedata()$Gender))))
    
  })
  
  ############## Select ProductID ########## 
  output$select_product <- renderUI({ 
    
    pickerInput('product', "Select Product ID", na.omit(unique(sort(filedata()$ProductID))),
                options = list(`actions-box` = TRUE),multiple = T,
                selected = na.omit(unique(sort(filedata()$ProductID))))
    
  })
  

  
  ############## Select Country ########## 
  output$select_country <- renderUI({ 
    pickerInput('country', "Select Country", na.omit(unique(sort(filedata()$Country))),
                options = list(`actions-box` = TRUE),multiple = T,
                selected = na.omit(unique(sort(filedata()$Country))))
    
  })
  
  
  ############## Select Language ########## 
  output$select_Language <- renderUI({ 
    
    pickerInput('lang', "Select Language", na.omit(unique(sort(filedata()$Language))),
                options = list(`actions-box` = TRUE),multiple = T,
                selected = na.omit(unique(sort(filedata()$Language))))
    
  })
  
  ############## Select Application ########## 
  output$select_application <- renderUI({ 
    
    df <- filedata() 
    pickerInput('app', "Select Application ID", na.omit(unique(sort(filedata()$ApplicationID))),
                options = list(`actions-box` = TRUE),multiple = T,
                selected = na.omit(unique(sort(filedata()$ApplicationID))))
    
  })
  
  ################### Country ################### 
  observeEvent(
    input$country,
    updateSelectInput(session,"app", "Select Application ID",choices = na.omit(unique(sort(filedata()$ApplicationID))),
                      selected = na.omit(unique(sort(filedata()$ApplicationID[filedata()$Country %in% input$country 
                                                                              & filedata()$ProductID %in% input$product
                                                                              & filedata()$Language %in% input$lang
                                                                              & filedata()$ApplicationID %in% input$app
                                                                              & filedata()$Gender %in% input$gender]))))
  )
  observeEvent(
    input$country,
    updateSelectInput(session,"lang", "Select Language",choices = na.omit(unique(sort(filedata()$Language))),
                      selected = na.omit(unique(sort(filedata()$Language[filedata()$Country %in% input$country 
                                                                         & filedata()$ProductID %in% input$product
                                                                         & filedata()$Language %in% input$lang
                                                                         & filedata()$ApplicationID %in% input$app
                                                                         & filedata()$Gender %in% input$gender]))))
  )
  observeEvent(
    input$country,
    updateSelectInput(session,"product", "Select Product ID",choices = na.omit(unique(sort(filedata()$ProductID))),
                      selected = na.omit(unique(sort(filedata()$ProductID[filedata()$Country %in% input$country 
                                                                          & filedata()$ProductID %in% input$product
                                                                          & filedata()$Language %in% input$lang
                                                                          & filedata()$ApplicationID %in% input$app
                                                                          & filedata()$Gender %in% input$gender]))))
  )
  
  observeEvent(
    input$country,
    updateSelectInput(session,"gender", "Select Gender",choices = na.omit(unique(sort(filedata()$Gender))),
                      selected = na.omit(unique(sort(filedata()$Gender[filedata()$Country %in% input$country 
                                                                          & filedata()$ProductID %in% input$product
                                                                          & filedata()$Language %in% input$lang
                                                                          & filedata()$ApplicationID %in% input$app
                                                                          & filedata()$Gender %in% input$gender]))))
  )
  
  ################### Product ################### 
  observeEvent(
    input$product,
    updateSelectInput(session,"app", "Select Application ID",choices = na.omit(unique(sort(filedata()$ApplicationID))),
                      selected = na.omit(unique(sort(filedata()$ApplicationID[filedata()$Country %in% input$country 
                                                                              & filedata()$ProductID %in% input$product
                                                                              & filedata()$Language %in% input$lang
                                                                              & filedata()$ApplicationID %in% input$app
                                                                              & filedata()$Gender %in% input$gender]))))
  )
  observeEvent(
    input$product,
    updateSelectInput(session,"lang", "Select Language",choices = na.omit(unique(sort(filedata()$Language))),
                      selected = na.omit(unique(sort(filedata()$Language[filedata()$Country %in% input$country 
                                                                         & filedata()$ProductID %in% input$product
                                                                         & filedata()$Language %in% input$lang
                                                                         & filedata()$ApplicationID %in% input$app
                                                                         & filedata()$Gender %in% input$gender]))))
  )
  observeEvent(
    input$product,
    updateSelectInput(session,"country", "Select Country",choices = na.omit(unique(sort(filedata()$Country))),
                      selected = na.omit(unique(sort(filedata()$Country[filedata()$Country %in% input$country 
                                                                        & filedata()$ProductID %in% input$product
                                                                        & filedata()$Language %in% input$lang
                                                                        & filedata()$ApplicationID %in% input$app
                                                                        & filedata()$Gender %in% input$gender]))))
  )
  
  observeEvent(
    input$product,
    updateSelectInput(session,"gender", "Select Gender",choices = na.omit(unique(sort(filedata()$Gender))),
                      selected = na.omit(unique(sort(filedata()$Gender[filedata()$Country %in% input$country 
                                                                        & filedata()$ProductID %in% input$product
                                                                        & filedata()$Language %in% input$lang
                                                                        & filedata()$ApplicationID %in% input$app
                                                                        & filedata()$Gender %in% input$gender]))))
  )
  
  ################### Application ID ################### 
  observeEvent(
    input$app,
    updateSelectInput(session,"product", "Select Product ID",choices = na.omit(unique(sort(filedata()$ProductID))),
                      selected = na.omit(unique(sort(filedata()$ProductID[filedata()$Country %in% input$country 
                                                                              & filedata()$ProductID %in% input$product
                                                                              & filedata()$Language %in% input$lang
                                                                              & filedata()$ApplicationID %in% input$app
                                                                              & filedata()$Gender %in% input$gender]))))
  )
  observeEvent(
    input$app,
    updateSelectInput(session,"lang", "Select Language",choices = na.omit(unique(sort(filedata()$Language))),
                      selected = na.omit(unique(sort(filedata()$Language[filedata()$Country %in% input$country 
                                                                         & filedata()$ProductID %in% input$product
                                                                         & filedata()$Language %in% input$lang
                                                                         & filedata()$ApplicationID %in% input$app
                                                                         & filedata()$Gender %in% input$gender]))))
  )
  observeEvent(
    input$app,
    updateSelectInput(session,"country", "Select Country",choices = na.omit(unique(sort(filedata()$Country))),
                      selected = na.omit(unique(sort(filedata()$Country[filedata()$Country %in% input$country 
                                                                        & filedata()$ProductID %in% input$product
                                                                        & filedata()$Language %in% input$lang
                                                                        & filedata()$ApplicationID %in% input$app
                                                                        & filedata()$Gender %in% input$gender]))))
  )
  
  observeEvent(
    input$app,
    updateSelectInput(session,"gender", "Select Gender",choices = na.omit(unique(sort(filedata()$Gender))),
                      selected = na.omit(unique(sort(filedata()$Gender[filedata()$Country %in% input$country 
                                                                        & filedata()$ProductID %in% input$product
                                                                        & filedata()$Language %in% input$lang
                                                                        & filedata()$ApplicationID %in% input$app
                                                                        & filedata()$Gender %in% input$gender]))))
  )
  
  ################### Language ################### 
  observeEvent(
    input$app,
    updateSelectInput(session,"product", "Select Product ID",choices = na.omit(unique(sort(filedata()$ProductID))),
                      selected = na.omit(unique(sort(filedata()$ProductID[filedata()$Country %in% input$country 
                                                                          & filedata()$ProductID %in% input$product
                                                                          & filedata()$Language %in% input$lang
                                                                          & filedata()$ApplicationID %in% input$app
                                                                          & filedata()$Gender %in% input$gender]))))
  )
  observeEvent(
    input$lang,
    updateSelectInput(session,"app", "Select Application ID",choices = na.omit(unique(sort(filedata()$ApplicationID))),
                      selected = na.omit(unique(sort(filedata()$ApplicationID[filedata()$Country %in% input$country 
                                                                         & filedata()$ProductID %in% input$product
                                                                         & filedata()$Language %in% input$lang
                                                                         & filedata()$ApplicationID %in% input$app
                                                                         & filedata()$Gender %in% input$gender]))))
  )
  observeEvent(
    input$app,
    updateSelectInput(session,"country", "Select Country",choices = na.omit(unique(sort(filedata()$Country))),
                      selected = na.omit(unique(sort(filedata()$Country[filedata()$Country %in% input$country 
                                                                        & filedata()$ProductID %in% input$product
                                                                        & filedata()$Language %in% input$lang
                                                                        & filedata()$ApplicationID %in% input$app
                                                                        & filedata()$Gender %in% input$gender]))))
  )
  
  observeEvent(
    input$app,
    updateSelectInput(session,"gender", "Select Gender",choices = na.omit(unique(sort(filedata()$Gender))),
                      selected = na.omit(unique(sort(filedata()$Gender[filedata()$Country %in% input$country 
                                                                        & filedata()$ProductID %in% input$product
                                                                        & filedata()$Language %in% input$lang
                                                                        & filedata()$ApplicationID %in% input$app
                                                                        & filedata()$Gender %in% input$gender]))))
  )
  
  ################### Gender ################### 
  observeEvent(
    input$gender,
    updateSelectInput(session,"product", "Select Product ID",choices = na.omit(unique(sort(filedata()$ProductID))),
                      selected = na.omit(unique(sort(filedata()$ProductID[filedata()$Country %in% input$country 
                                                                          & filedata()$ProductID %in% input$product
                                                                          & filedata()$Language %in% input$lang
                                                                          & filedata()$ApplicationID %in% input$app
                                                                          & filedata()$Gender %in% input$gender]))))
  )
  observeEvent(
    input$gender,
    updateSelectInput(session,"app", "Select Application ID",choices = na.omit(unique(sort(filedata()$ApplicationID))),
                      selected = na.omit(unique(sort(filedata()$ApplicationID[filedata()$Country %in% input$country 
                                                                              & filedata()$ProductID %in% input$product
                                                                              & filedata()$Language %in% input$lang
                                                                              & filedata()$ApplicationID %in% input$app
                                                                              & filedata()$Gender %in% input$gender]))))
  )
  observeEvent(
    input$gender,
    updateSelectInput(session,"country", "Select Country",choices = na.omit(unique(sort(filedata()$Country))),
                      selected = na.omit(unique(sort(filedata()$Country[filedata()$Country %in% input$country 
                                                                        & filedata()$ProductID %in% input$product
                                                                        & filedata()$Language %in% input$lang
                                                                        & filedata()$ApplicationID %in% input$app
                                                                        & filedata()$Gender %in% input$gender]))))
  )
  
  observeEvent(
    input$gender,
    updateSelectInput(session,"lang", "Select Language",choices = na.omit(unique(sort(filedata()$Language))),
                      selected = na.omit(unique(sort(filedata()$Language[filedata()$Country %in% input$country 
                                                                       & filedata()$ProductID %in% input$product
                                                                       & filedata()$Language %in% input$lang
                                                                       & filedata()$ApplicationID %in% input$app
                                                                       & filedata()$Gender %in% input$gender]))))
  )
  
  ###########Vis 1############
  output$graph1<-renderPlotly({
    new<-getplayersperproduct()
    new
  })
  
  ###########Vis 2############
  output$graph2<-renderPlotly({
    new<-getfreqpokermonths()
    new
  })
  
  ###########Vis 3############
  output$graph3<-renderPlotly({
    new<-getfrequsermonth()
    new
  })
  
  ###########Vis 4############
  output$graph4<-renderPlotly({
    new<-getfreqapprmonth()
    new
  })
  
  ###########Vis 5############
  output$graph5<-renderPlotly({
    new<-getfreqage()
    new
  })
  
  ###########Vis 6############
  output$graph6<-renderPlotly({
    new<-getpielang()
    new
  })
  
  ###########Vis 7############
  output$graph7<-renderPlotly({
    new<-getpiechurn()
    new
  })
  
  ###########Vis 8############
  output$graph8<-renderPlotly({
    new<-getmapcountry()
    new
  })
  
  ###########Vis 9############
  output$graph9<-renderPlotly({
    new<-getfreqlor()
    new
  })
  
  ###########Vis 10############
  output$graph10<-renderPlotly({
    new<-getpiegender()
    new
  })
  
  ###########Vis 11############
  output$graph11<-renderPlotly({
    new<-getfreqpokeravgdays()
    new
  })
  
  ###########Vis 12############
  output$graph12<-renderPlotly({
    new<-getfrequseraggavgdays()
    new
  })
  
  ###########Vis 13############
  output$graph13<-renderPlotly({
    new<-getprofitloss()
    new[[1]]
  })
  
  ###########Vis 14############
  output$graph14<-renderPlotly({
    new<-getwinprofstakes()
    new
  })

  ###########Vis 15############
  output$graph15<-renderPlotly({
    new<-getfreqprofit()
    new
  })
  
  ###########Vis 15############
  output$graph16<-renderPlotly({
    new<-getprofitloss()
    new[[2]]
  })
  
  output$graph17<-renderPlotly({
    new<-getprofstakes()
    new
  })
  
  
  
  filterValidater <- function(df = NULL){
    
    if(!is.null(input$gender)){
      df <- subset(df,df$Gender %in% input$gender)
    }
    if(!is.null(input$country)){
      df <- subset(df,df$Country %in% input$country)
    }
    if(!is.null(input$product)){
      df <- subset(df,df$ProductID %in% input$product)
    }
    if(!is.null(input$app)){
      df <- subset(df,df$ApplicationID %in% input$app)
    }
    if(!is.null(input$lang)){
      df <- subset(df,df$Language %in% input$lang)
    }
    if(identical(df,filedata())){
      df <- NULL
    }
    return(df)
  }
  
  ########### Generating Players / Products ##############
  getplayersperproduct<-reactive({
    
    df=filedata()
    
    df = filterValidater(df)
    
    validate(need(
             nrow(df)!=0,"Data Not Found, Change Selection!!"))
    
  
    product_wise_players <- df %>%
    group_by(ProductID) %>%
    summarise(unique_players = n_distinct(USERID))

    product_wise_players <- product_wise_players[complete.cases(product_wise_players),]

    product_wise_players_PC <- unique(df[,c("USERID","PC_Recency")])
    if(!is.na(product_wise_players_PC))
    {
    product_wise_players_PC <- sum(!is.na(product_wise_players_PC$PC_Recency))
    product_wise_players <- rbind(product_wise_players,c("Poker_Chips",product_wise_players_PC))
    product_wise_players$unique_players <- as.numeric(product_wise_players$unique_players)
    }
      product_wise_players %>%
      group_by(ProductID) %>%
      summarize(unique_players = unique_players) %>%

      plot_ly(labels = ~ProductID, values = ~unique_players) %>%
      add_pie(hole = 0.6) %>%
      layout(title = "Pie Chart of Products",  showlegend = F,
             xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
             yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
    
      
  })
   
   
  
  
  
  
  ###########two tab 1 ##############
  getfreqpokermonths<-reactive({
    df=filedata()
    df = filterValidater(df)
    validate(need(
      nrow(df)!=0,"Data Not Found, Change Selection!!"))
    
    freq_by_months <- unique(df[,c("USERID","PC_Freq_sessions_first3Months",
                                          "PC_Freq_sessions_mid3Months",
                                          "PC_Freq_sessions_last2Months")])
    names(freq_by_months) <- c("USERID","First 3 Months",
                               "Mid 3 Months",
                               "Last 2 Months")
    freq_by_months <- gather(freq_by_months,Freq,value,-USERID)
    freq_by_months <- freq_by_months[complete.cases(freq_by_months),]
    freq_by_months$value <- as.character(freq_by_months$value)
    
    x = factor(freq_by_months$Freq,c("USERID","First 3 Months",
                                     "Mid 3 Months",
                                     "Last 2 Months"))
    y = freq_by_months$value
    
    plot_ly(y=y, x=x, histfunc='sum', type = "histogram") %>%
      layout(title = "Frequency Plot for Poker Chips by months",
             yaxis=list(type='linear'),
             xaxis=list(tickangle = 315))
    


  })
  
  ###########three tab 1 ##############
  getfrequsermonth<-reactive({
    df=filedata()
    df = filterValidater(df)
    validate(need(
      nrow(df)!=0,"Data Not Found, Change Selection!!"))
    
    freq_by_months <- unique(df[,c("USERID","UA_Freq_bets_first3Months",
                                          "UA_Freq_bets_mid3Months",
                                          "UA_Freq_bets_last2Months")])
    names(freq_by_months) <- c("USERID","First 3 Months",
                               "Mid 3 Months",
                               "Last 2 Months")
    freq_by_months <- gather(freq_by_months,Freq,value,-USERID)
    freq_by_months <- freq_by_months[complete.cases(freq_by_months),]
    freq_by_months$value <- as.character(freq_by_months$value)
    
    x = factor(freq_by_months$Freq,c("USERID","First 3 Months",
                                     "Mid 3 Months",
                                     "Last 2 Months"))
    y = freq_by_months$value
    
    plot_ly(y=y, x=x, histfunc='sum', type = "histogram") %>%
      layout(title = "Frequency Plot for User Aggregation by months",
             yaxis=list(type='linear'),
             xaxis=list(tickangle = 315))
    
    
  })
  
  
  
  
  
  
  ###########one tab 2 ##############
  getfreqapprmonth<-reactive({
    df=filedata()
    df = filterValidater(df)
    validate(need(
      nrow(df)!=0,"Data Not Found, Change Selection!!"))
    
    freq_by_application <- unique(df[,c("USERID","ApplicationID")])
    freq_by_application <- freq_by_application[complete.cases(freq_by_application),]
    
    freq_for_ordered_names <- freq_by_application %>%
      group_by(ApplicationID) %>%
      summarise(freq = n()) %>%
      arrange(desc(freq))
    
    freq_by_application$value <- as.character(1)
    
    x = factor(freq_by_application$ApplicationID, levels = freq_for_ordered_names$ApplicationID)
    y = freq_by_application$value
    
    plot_ly(y=y, x=x, histfunc='sum', type = "histogram") %>%
      layout(title = "Frequency Plot - Application used",
             yaxis=list(type='linear'),
             xaxis=list(tickangle = 315))
    
    
  })
  
  
  ###########two tab 2 ##############
  getfreqage<-reactive({
    df=filedata()
    df = filterValidater(df)
    validate(need(
      nrow(df)!=0,"Data Not Found, Change Selection!!"))
    
    freq_by_age <- unique(df[,c("USERID","AGE")])
    freq_by_age <- freq_by_age[complete.cases(freq_by_age),]
    
    x = freq_by_age$AGE
    
    plot_ly(x=x, type = "histogram") %>%
      layout(title = "Frequency Plot - Age")
    
    
    
  })
  
  
  ###########three tab 2 ##############
  getpielang<-reactive({
    df=filedata()
    df = filterValidater(df)
    validate(need(
      nrow(df)!=0,"Data Not Found, Change Selection!!"))
    
    language_wise_players <- df %>%
      group_by(Language) %>%
      summarise(unique_players = n_distinct(USERID))
    
    language_wise_players <- language_wise_players[complete.cases(language_wise_players),]
    
    language_wise_players %>%
      group_by(Language) %>%
      summarize(unique_players = unique_players) %>%
      plot_ly(labels = ~Language, values = ~unique_players) %>%
      add_pie(hole = 0.6) %>%
      layout(title = "Unique Users per Languages",  showlegend = F,
             xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
             yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
    
    
  })
  
  
  ###########one tab 3 ##############
  getpiechurn<-reactive({
    df=filedata()
    df = filterValidater(df)
    validate(need(
      nrow(df)!=0,"Data Not Found, Change Selection!!"))
    
    Churn_wise_players <- df %>%
      group_by(Churn) %>%
      summarise(players = n())
    
    Churn_wise_players <- Churn_wise_players[complete.cases(Churn_wise_players),]
    
    Churn_wise_players %>%
      group_by(Churn) %>%
      summarize(players = players) %>%
      plot_ly(labels = ~Churn, values = ~players) %>%
      add_pie(hole = 0.6) %>%
      layout(title = "Pie Chart of Churn",  showlegend = F,
             xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
             yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
    
    
  })
  
  ###########two tab 3 ##############
  getmapcountry<-reactive({
    df=filedata()
    df = filterValidater(df)
    validate(need(
      nrow(df)!=0,"Data Not Found, Change Selection!!"))
    
    freq_by_country <- unique(df[,c("USERID","Country")])
    freq_by_country <- freq_by_country[complete.cases(freq_by_country),]
    
    freq_by_country_names <- freq_by_country %>%
      group_by(Country) %>%
      summarise(freq = n()) %>%
      arrange(desc(freq))
    
    df1 <- read.csv('https://raw.githubusercontent.com/plotly/datasets/master/2014_world_gdp_with_codes.csv')
    
    freq_by_country_names <- merge(x=freq_by_country_names, y= df1,by.x = c("Country"),
                                   by.y = c("COUNTRY"), all.x = T)
    
    # light grey boundaries
    l <- list(color = toRGB("grey"), width = 0.5)
    
    # specify map projection/options
    g <- list(
      showframe = FALSE,
      showcoastlines = FALSE,
      projection = list(type = 'Mercator')
    )
    
    plot_geo(freq_by_country_names) %>%
      add_trace(
        z = ~freq, color = ~freq, colors = 'Blues',
        text = ~Country, locations = ~CODE, marker = list(line = l)
      ) %>%
      colorbar(title = 'Frequency') %>%
      layout(
        title = 'Unique Users per Country',
        geo = g
      )
    
    
  })
  
  
  ###########three tab 3 ##############
  getfreqlor<-reactive({
    df=filedata()
    df = filterValidater(df)
    validate(need(
      nrow(df)!=0,"Data Not Found, Change Selection!!"))
    
    freq_by_lor <- unique(df[,c("USERID","UA_Lor_firstPlay")])
    freq_by_lor <- freq_by_lor[complete.cases(freq_by_lor),]
    
    x = freq_by_lor$UA_Lor_firstPlay
    
    plot_ly(x=x, type = "histogram") %>%
      layout(title = "Frequency Plot - Length of relationship (Days)")
    
    
    
  })
  
  
  ###########one tab 4 ##############
  getpiegender<-reactive({
    df=filedata()
    df = filterValidater(df)
    validate(need(
      nrow(df)!=0,"Data Not Found, Change Selection!!"))
    
    Gender_wise_players <- df %>%
      group_by(Gender) %>%
      summarise(players = n_distinct(USERID))
    
    Gender_wise_players <- Gender_wise_players[complete.cases(Gender_wise_players),]
    
    Gender_wise_players %>%
      group_by(Gender) %>%
      summarize(players = players) %>%
      plot_ly(labels = ~Gender, values = ~players) %>%
      add_pie(hole = 0.6) %>%
      layout(title = "Unique Users per Gender",  showlegend = F,
             xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
             yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
    
    
    
  })
  
  ###########two tab 4 ##############
  getfreqpokeravgdays<-reactive({
    df=filedata()
    df = filterValidater(df)
    validate(need(
      nrow(df)!=0,"Data Not Found, Change Selection!!"))
    
    freq_by_IPT <- unique(df[,c("USERID","PC_IPT_mean")])
    library(pracma)
    freq_by_IPT$PC_IPT_mean <- ceil(freq_by_IPT$PC_IPT_mean)
    freq_by_IPT <- freq_by_IPT[complete.cases(freq_by_IPT),]
    
    x = freq_by_IPT$PC_IPT_mean
    
    plot_ly(x=x, type = "histogram") %>%
      layout(title = "Average number of days between betting sessions - Poker Chips")
    
    
    
  })
  
  ###########three tab 4 ##############
  getfrequseraggavgdays<-reactive({
    df=filedata()
    df = filterValidater(df)
    validate(need(
      nrow(df)!=0,"Data Not Found, Change Selection!!"))
    
    freq_by_IPT <- unique(df[,c("USERID","UA_IPT_mean")])
    freq_by_IPT$UA_IPT_mean <- ceil(freq_by_IPT$UA_IPT_mean)
    freq_by_IPT <- freq_by_IPT[complete.cases(freq_by_IPT),]
    
    x = freq_by_IPT$UA_IPT_mean
    
    plot_ly(x=x, type = "histogram") %>%
      layout(title = "Average number of days between betting sessions - User Aggregation")
    
    
    
    
  })
  
  
  ###########one tab 5 ##############
  getprofitloss<-reactive({
    df=filedata()
    df = filterValidater(df)
    validate(need(
      nrow(df)!=0,"Data Not Found, Change Selection!!"))
    
    profit_loss <- unique(df[,c("USERID","ProductID","UA_Mon_profit")])
    profit_loss$Mon <- ifelse(profit_loss$UA_Mon_profit > 0,"Profit","Loss")
    profit_loss$UA_Mon_profit <- abs(profit_loss$UA_Mon_profit)
    profit_loss <- profit_loss[complete.cases(profit_loss),]
    
    profit_loss1 <- profit_loss %>%
      group_by(ProductID) %>%
      summarise(Profit = sum(UA_Mon_profit[Mon=="Profit"],na.rm = T),
                Loss = sum(UA_Mon_profit[Mon=="Loss"],na.rm = T),
                Profit_Percentage = Profit*100/(Profit+Loss),
                Loss_Percentage = Loss*100/(Profit+Loss))
    
    profit_loss <- unique(df[,c("USERID","PC_Mon_profit")])
    profit_loss$Mon <- ifelse(profit_loss$PC_Mon_profit > 0,"Profit","Loss")
    profit_loss$PC_Mon_profit <- abs(profit_loss$PC_Mon_profit)
    
    profit_loss2 <- profit_loss %>%
      summarise(Profit = sum(PC_Mon_profit[Mon=="Profit"],na.rm = T),
                Loss = sum(PC_Mon_profit[Mon=="Loss"],na.rm = T),
                Profit_Percentage = Profit*100/(Profit+Loss),
                Loss_Percentage = Loss*100/(Profit+Loss))
    profit_loss2$ProductID <- "Poker Chips"
    profit_loss <- rbind(profit_loss1,profit_loss2)
    
    rm(profit_loss1)
    rm(profit_loss2)
    
    profit_loss <- profit_loss %>%
      group_by(ProductID) %>%
      arrange(desc(Profit))
    
    a = factor(profit_loss$ProductID, levels = profit_loss$ProductID)
    b = profit_loss$Loss
    c = profit_loss$Profit
    
    g1=plot_ly(x = ~a, y = ~b, type = 'bar', name = 'Loss') %>%
      add_trace(y = ~c, name = 'Profit') %>%
      layout(title = 'Product wise - Profit vs Loss',
             yaxis = list(title = 'Value'), barmode = 'stack',
             xaxis=list(title = 'Products',tickangle = 315))
    
    profit_loss <- profit_loss %>%
      group_by(ProductID) %>%
      arrange(desc(Profit_Percentage))
    
    x = factor(profit_loss$ProductID, levels = profit_loss$ProductID)
    
    y1 = profit_loss$Loss_Percentage
    y2 = profit_loss$Profit_Percentage
    
    g2=plot_ly(x = ~x, y = ~y1, type = 'bar', name = 'Loss %') %>%
      add_trace(y = ~y2, name = 'Profit %') %>%
      layout(title = 'Product wise - Profit% vs Loss%',
             yaxis = list(title = 'Value %'), barmode = 'stack',
             xaxis=list(title = 'Products',tickangle = 315))
    
    glist<-list(g1,g2)
     return(glist)
    
    
  })
  
  
  ###########two tab 5 ##############
  getwinprofstakes<-reactive({
    df=filedata()
    df = filterValidater(df)
    validate(need(
      nrow(df)!=0,"Data Not Found, Change Selection!!"))
    
    
    observe({
      c <- df$Country
      p <- df$ProductID
      u <- df$USERID
      a <- df$ApplicationID
      l <- df$Language
      
   
      
    })
    
    
    plot_ly(df, x =1:nrow(df), y =df$UA_rMon_profit, name = 'Profit',type = 'scatter', mode = 'lines')%>%
      add_trace(y = df$UA_rMon_wins, name = ' Wins', mode = 'lines') %>%
      add_trace(y = df$UA_rMon_Stakes, name = 'Stakes', mode = 'lines')  %>%
      add_trace(y = df$UA_rFreq_bets, name = 'Bets', mode = 'lines') %>%
      layout(title="Winnings Vs Profit Vs Stakes Vs Bets")
    
    
  })
  
  
  
  
  ###########one tab 6 ##############
  getfreqprofit<-reactive({
    df=filedata()
    df = filterValidater(df)
    validate(need(
      nrow(df)!=0,"Data Not Found, Change Selection!!"))
    
    
    observe({
      c <- df$Country
      p <- df$ProductID
      u <- df$USERID
      a <- df$ApplicationID
      l <- df$Language
 
      
    })
    
    
    ##############Bets Vs Profits/ User ID for UA ######################
    plot_ly(df, x = df$UA_rFreq_bets, y =df$UA_rMon_profit, text = df$USERID, type = 'scatter', mode = 'markers')%>%
      layout(title="Freq vs Profit for User Aggregation")
    
    
  })
  
  
  
  
  ###########two tab 6 ##############
  getprofstakes<-reactive({
    df=filedata()
    df = filterValidater(df)
    validate(need(
      nrow(df)!=0,"Data Not Found, Change Selection!!"))
    
    
    observe({
      c <- df$Country
      p <- df$ProductID
      u <- df$USERID
      a <- df$ApplicationID
      l <- df$Language
    
    })
    
    
    ############# Poker Profit vs Stakes######################3
    
    
    plot_ly(df, x =1:nrow(df), y =df$PC_rMon_profit, name = 'Profit',type = 'scatter', mode = 'lines')%>%
      add_trace(y = df$PC_rMon_Buy, name = 'Stakes', mode = 'lines')  %>%
      layout(title="Stakes Vs Profit")
    
    
  })
  
  
}




