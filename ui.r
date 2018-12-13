ui <- dashboardPage(skin = "blue",
                    dashboardHeader(title = "Descriptive Analysis"),
                    
                    dashboardSidebar(width = 350,
                                     tags$head(
                                       tags$style(HTML("
                                                       .sidebar { height: 90vh; overflow-y: auto; }
                                                       
                                                       " ))),
                                     
                                     sidebarMenu(
                                       menuItem("Input Data", tabName = "Files", icon = icon("cog",lib = 'glyphicon')),
                                       menuItem("Descriptive Analysis1", tabName = "da1", icon = icon("dashboard",lib = 'glyphicon')),
                                       menuItem("Descriptive Analysis2", tabName = "da2", icon = icon("dashboard",lib = 'glyphicon')),
                                       menuItem("Descriptive Analysis3", tabName = "da3", icon = icon("dashboard",lib = 'glyphicon')),
                                       menuItem("Descriptive Analysis4", tabName = "da4", icon = icon("dashboard",lib = 'glyphicon')),
                                       menuItem("Descriptive Analysis5", tabName = "da5", icon = icon("dashboard",lib = 'glyphicon')),
                                       menuItem("Descriptive Analysis6", tabName = "da6", icon = icon("dashboard",lib = 'glyphicon')),
                                       tags$div(
                                         
                                         
                                         
                                         htmlOutput("select_gender"),
                                         htmlOutput("select_product"),
                                         htmlOutput("select_country"),
                                         htmlOutput("select_application"),
                                         htmlOutput("select_Language")
                                        
                                       ))
                                     
                                     
                                       ),
                    dashboardBody(
                      
                      tabItems(
                        tabItem(tabName="Files",
                                
                                        
                                tabsetPanel(
                                  tabPanel("Data File",
                                           wellPanel(
                                             tags$div(fileInput('file1', 'Load Data File',
                                                                accept = c('text/csv','text/comma-separated-values',
                                                                           'text/tab-separated-values','text/plain','.csv','.tsv','.feather','.zip','fst'))))), 
                                                      tabPanel("Problem Statement", 
                                                                            wellPanel(  
                                                                            h4("Codebook for Actual Internet Sports Gambling Activity:
                                                                                     February 2005 through September 2005
                                                                                     This codebook provides information for both the raw and analytic datasets used to generate the first epidemiological report of actual
                                                                                     Internet sports betting (LaBrie et al., 2007). These datasets come from the collaborative Internet gambling research project between
                                                                                     the Division on Addictions (DOA) and bwin Interactive Entertainment, AG (bwin), an Internet betting service provider headquartered
                                                                                     in Vienna, Austria. These datasets provide evidence from the first eight months of the first prospective longitudinal, real-time, Internet
                                                                                     sports betting behavior study that took place from February 1, 2005 through September 30, 2005. The analytic dataset contains
                                                                                     information representing eight months of aggregated betting behavior data for 40,499 sequential bwin Internet sports subscribers who
                                                                                     opened an account with bwin during the period from February 1, 2005 through February 27, 2005")),
                                                               wellPanel(
                                                                 h4("Raw Dataset I - Demographics
                                                                    The raw dataset, Demographics, contains the following demographic information: User ID, Country of Residence, Language, Gender,
                                                                    Registration Date, First Pay-in Date, First Active Date, First Sports Book Active Date, First Casino Active Date, First Games Active
                                                                    Date, First Poker Active Date, and Application ID. Information about City of Residence and Date of Birth are censored to protect
                                                                    participant privacy and comply with institutional review board (IRB) requirements.")
                                                                 ),
                                                               wellPanel(
                                                                 h4("Raw Dataset II - UserDailyAggregation
                                                                    The raw dataset, UserDailyAggregation, contains the actual betting information associated with each product (please see appendix 1)
                                                                    for each participant for each calendar day with at least one transaction from February 1, 2005 through September 30, 2005. This
                                                                    dataset contains the following information: User ID, Date, Betting Product, Stakes, Winnings, and Bets. All transactions have been
                                                                    converted to Euros")
                                                                 ),
                                                               wellPanel(
                                                                 h4("Raw Dataset III - PokerChipConversions
                                                                    The raw dataset, PokerChipConversions, contains the actual poker chip transaction information from February 1, 2005 through
                                                                    September 30, 2005. The DOA obtained summaries of poker play in terms of poker chip transactions to and from the poker site for
                                                                    each poker session. The poker transaction information includes User ID, Transaction Date/Time, Transaction Type (buy or sell), and
                                                                    Transaction Amount.")
                                                                 ),
                                                               wellPanel(
                                                                 h4("Analytic Dataset - Actual Internet Sports Gambling Activity: February 2005 through
                                                                    September 2005
                                                                    This analytic dataset contains actual betting behavior data for both fixed-odds and live-action sports book betting for each participant
                                                                    aggregated over the eight-month study period from February 1, 2005 through September 30, 2005.
                                                                    Participants who had no missing values for first active date and who registered during the period of February 1, 2005 through
                                                                    February 27, 2005 were entered into the analytic dataset (i.e., in the raw dataset Demographics, variable FirstAct does not equal
                                                                    missing and variable RegDate ranges from February 1, 2005 through February 27, 2005). The DOA transferred the following
                                                                    variables for each participant from the raw dataset Demographics to the analytic dataset: UserID, Country, Language, Gender,
                                                                    RegDate, FirstPay, and computed Age (age at registration) from the raw dataset Demographics.
                                                                    The DOA excluded records in the raw dataset UserDailyAggregation that took place before the first pay-in date (i.e., variable FirstPay
                                                                    in raw dataset Demographics) in the preparation of the analytic dataset. The exclusion controlled for activities financed by
                                                                    promotional money that might not reflect the participants' behavior when personal funds are at stake. First active date of each product(not including poker) was identified from the raw dataset UserDailyAggregation, and first poker active date was identified by first date
                                                                    of poker chip purchase from raw dataset PokerChipConversions. Therefore, first active date was assigned as the earliest date of the
                                                                    first active date of all betting products. The final analytic cohort (n=40,499) includes only participants whose first active date was
                                                                    before September 1, 2005 and who betted on sports book (fixed-odds or/and live-action).
                                                                    The DOA computed the following measures for both fixed-odds and live-action sports book betting by summing the data in raw
                                                                    dataset UserDailyAggregation: TotalStakes, TotalWinnings, TotalBets, and TotalDaysActive. The DOA also identified
                                                                    FirstActiveDate and LastActiveDate for both fixed-odds and live-action sports book betting from the raw dataset
                                                                    UserDailyAggregation. Variable FirstSportsActiveDate is defined as the earlier date of fixed-odds first active date and live-action first
                                                                    active date")
                                                                 )
                                                               
                                                               
                                                               
                                                               
                                                                 ),
                                             tabPanel("Data",
                                                      wellPanel(div(style = 'overflow-x: scroll', DT::dataTableOutput('table'))
                                                      )),
                                  tabPanel("Data Definition",
                                           wellPanel(
                                             h4("Create new variables:
                                                Assumption - We have data till 2005-09-31 so we are calculating recency from 2005-10-01.",
                                                br(),
              "Recency_lastwin Recent Days on which user won.",
                                                br(),
              "Recency_lastloss Recent Days on which user lost IPT_Days Difference in the successive gaming sessions.",
              br(),
              "Month_range Creating month range for first three months = 1, mid three months = 2 and the last two months as 3",
              br(),
              tags$b("For User_Daily_Aggregation (UA)"),br(),br(),
              
              tags$b("Recency:"), br(),
              "UA_Recency - Number of days since last betting activity.", br(),
              "UA_Recency_lastwin - Number of days since last win.", br(),
              "UA_Recency_lastloss - Number of days since last loss.", br(),br(),
tags$b("Interpurchase time:"), br(),
              "UA_IPT_std - Standard Deviation of the number of days between betting sessions", br(),
              "UA_IPT_min - Minimun number of days between betting sessions", br(),
              "UA_IPT_max - Maximum number of days between betting sessions", br(),
              "UA_IPT_mean - Average number of days between betting sessions", br(),
              "UA_IPT_CV - Coefficient of variation of interpurchase time (ratio of UA_IPT_std to UA_IPT_mean)", br(),br(),
tags$b("Length of relationship:"), br(),
              "UA_Lor_firstPlay - Number of days since first betting", br(),br(),
tags$b("Frequency:"), br(),
              "UA_Freq_bets - Total number of bets", br(),
              "UA_Freq_bets_first3Months - Total number of bets from Feb to April", br(),
              "UA_Freq_bets_mid3Months - Total number of bets from May to July", br(),
              "UA_Freq_bets_last2Months - Total number of bets from August to September", br(),
  "UA_Freq_sessions_won - Total number of bets where winning is greater than 0", br(),
  "UA_Freq_sessions_lost - Total number of bets where winning is equal to 0", br(),
  "UA_rFreq_bets - Total number of bets relative to the length of relationship (UA_Lor_firstPlay)", br(),br(),
tags$b("Monetary:"), br(),
  "UA_Mon_Stakes - Total monetary amount of stakes.", br(),
  "UA_Mon_wins - Total monetary amount of wins.", br(),
  "UA_Mon_profit - Total monetary amount of profit (wins - stakes).", br(),
  "UA_rMon_Stakes - Total monetary amount of stakes relative to the length of relationship (UA_Lor_firstPlay)", br(),
  "UA_rMon_wins - Total monetary amount of wins relative to the length of relationship (UA_Lor_firstPlay)", br(),
  "UA_rMon_profit - Total monetary amount of profit relative to the length of relationship (UA_Lor_firstPlay)", br(),br(),
              
tags$b("For Poker_Chips (PC)"),br(),br(),

tags$b("Recency:"),br(),
  "PC_Recency - Number of days since last betting activity",br(),br(),
tags$b("Interpurchase time:"),br(),
  "PC_IPT_std - Standard Deviation of the number of days between betting sessions",br(),
"PC_IPT_min - Minimun number of days between betting sessions",br(),
"PC_IPT_max - Maximum number of days between betting sessions",br(),
"PC_IPT_mean - Average number of days between betting sessions",br(),
"PC_IPT_CV - Coefficient of variation of interpurchase time (ratio of UA_IPT_std to UA_IPT_mean),sessions",br(),br(),
tags$b("Length of relationship:"),br(),
  "PC_Lor_firstPlay - Number of days since first betting",br(),br(),
tags$b("Frequency:"),br(),
  "PC_Freq_sessions - Total number of sessions played",br(),
"PC_Freq_sessions_first3Months - Total number of sessions from Feb to April",br(),
"PC_Freq_sessions_mid3Months - Total number of sessions from May to July",br(),
"PC_Freq_sessions_last2Months - Total number of sessions from August to September",br(),
"PC_rFreq_sessions - Total number of sessions relative to the length of relationship (PC_Lor_firstPlay)",br(),br(),
tags$b("Monetary:"),br(),
"PC_Mon_Buy - Total monetary amount of Buys",br(),
"PC_Mon_Sell - Total monetary amount of Sells",br(),
"PC_Mon_profit - Total monetary amount of profit (Sell - Buy)",br(),
"PC_rMon_Buy - Total monetary amount of Buy relative to the length of relationship (PC_Lor_firstPlay)",br(),
"PC_rMon_Sell - Total monetary amount of sell relative to the length of relationship (PC_Lor_firstPlay)",br(),
"PC_rMon_profit - Total monetary amount of profit relative to the length of relationship (PC_Lor_firstPlay)",br(),br(),

  tags$b("Create a new variable CHURN based on following rulset-"),br(),br(),
  "1) If the UA_IPT_CV is NA it means the customer has gambled at most twice in the entire data which means he is a tourist.",br(),
  "2) If the UA_IPT_CV is greater than 3 it means the customer has higher variability so he is a churner",br(),
  "3) If the UA_IPT_CV is less than 3 then the customer has low variability so he is a non-churner",br()
              









)
                                             )
                                             
                                                                 )
                                  )),
                                                                 
                        # FirST tab content
                        
                        tabItem(tabName = "da1",
                                # FirST tab content
                                         tabPanel("Product/User",
                                                  withSpinner(wellPanel(fluidRow(plotlyOutput("graph1")))
                                                  ,type = 8)
                                                  ),
                                          tabPanel("Freqplot",
                                                   withSpinner(wellPanel(fluidRow(plotlyOutput("graph2")))
                                                  ,type = 8)
                                                  ),
                                          tabPanel("Freqplot 2",
                                                   withSpinner(wellPanel(fluidRow(plotlyOutput("graph3")))
                                                               ,type = 8)
                                          )
                        ),
                        tabItem(tabName = "da2",
                                tabPanel("one",
                                         withSpinner(wellPanel(fluidRow(plotlyOutput("graph4"))),type = 8)
                                         ),
                                tabPanel("two",
                                         withSpinner(wellPanel(fluidRow(plotlyOutput("graph5"))),type = 8)
                                ),
                                tabPanel("three",
                                         withSpinner(wellPanel(fluidRow(plotlyOutput("graph6"))),type = 8)
                                )
                        ),
                        tabItem(tabName = "da3",
                                tabPanel("one",
                                         withSpinner(wellPanel(fluidRow(plotlyOutput("graph7"))),type = 8)
                                ),
                                tabPanel("two",
                                         withSpinner(wellPanel(fluidRow(plotlyOutput("graph8"))),type = 8)
                                ),
                                tabPanel("three",
                                         withSpinner(wellPanel(fluidRow(plotlyOutput("graph9"))),type = 8)
                                )
                               
                        ),
                        tabItem(tabName = "da4",
                                # FirST tab content
                                
                                tabPanel("one",
                                         withSpinner(wellPanel(fluidRow(plotlyOutput("graph10"))),type = 8)
                                ),
                                tabPanel("two",
                                         withSpinner(wellPanel(fluidRow(plotlyOutput("graph11"))),type = 8)
                                ),
                                tabPanel("three",
                                         withSpinner(wellPanel(fluidRow(plotlyOutput("graph12"))),type = 8)
                                )
                        ),
                        tabItem(tabName = "da5",
                                
                                tabPanel("one",
                                         withSpinner(wellPanel(fluidRow(plotlyOutput("graph13"))),type = 8)
                                ),
                                tabPanel("two",
                                         withSpinner(wellPanel(fluidRow(plotlyOutput("graph16"))),type = 8)
                                ),
                                tabPanel("three",
                                         withSpinner(wellPanel(fluidRow(plotlyOutput("graph15"))),type = 8)
                                )
                        ),
                        tabItem(tabName = "da6",
                                
                                tabPanel("one",
                                         withSpinner(wellPanel(fluidRow(plotlyOutput("graph14"))),type = 8)
                                ),
                                tabPanel("two",
                                         withSpinner(wellPanel(fluidRow(plotlyOutput("graph17"))),type = 8)
                                )
                        )
                        
                        
                        )
                      
            ))


