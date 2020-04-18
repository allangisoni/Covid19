library(shiny)
library(shinydashboard)
library(shinyjs)
library(shinybusy)

dashboardPage(
  dashboardHeader(title = "COVID 19 DASHBOARD"),
  dashboardSidebar(
    sliderInput("rateThreshold", "Warn when rate exceeds",
                min = 0, max = 50, value = 3, step = 0.1
    ),
    sidebarMenu(
      menuItem("Main", tabName = "dashboard"),
      menuItem("Raw data", tabName = "rawdata")
    )
  ),
  dashboardBody(style="background-color:#203644", 
    tabItems(
      tabItem("dashboard",
              fluidRow(column(width=3,
                valueBoxOutput("confirmed", width = NULL),
                valueBoxOutput("deaths", width=NULL),
                valueBoxOutput("recoveries" , width=NULL)
                ),   column(9, 
                            div(withSpinner(plotlyOutput("plot1", height = 350) )))
             ,style="background-color:#203644" ),
             
             fluidRow(column(width=6,
                             div(withSpinner(plotlyOutput("plot2", height = 350) ))),
                      column(width=6,
                             div(withSpinner(plotlyOutput("plot3", height = 350) )))),
             fluidRow( style="margin-top:16px",
               column(12,
                      box(
                        title = 'COVID Summary', width = NULL,status = "info",
                        div(style = 'overflow-x: scroll', withSpinner(
                           DT::dataTableOutput('covidtbl'))))
               ))
             
             
             
      ),
      tabItem("rawdata",
              numericInput("maxrows", "Rows to show", 25),
              verbatimTextOutput("rawtable"),
              downloadButton("downloadCsv", "Download as CSV")
      )
    )
  )
)

