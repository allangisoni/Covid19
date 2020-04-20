library(shiny)
library(shinydashboard)
library(shinydashboardPlus)
library(shinyjs)
library(shinybusy)

dashboardPage(
  dashboardHeader(title = "COVID 19 DASHBOARD"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Dashboard", tabName = "dashboard", icon = icon("fas fa-tachometer-alt", lib = "font-awesome")),
      menuItem("Charts", tabName = "chart", icon = icon("fas fa-chart-pie", lib = "font-awesome"),
               menuSubItem("Kenya", tabName = "kenya"),
               menuSubItem("World", tabName = "world")),
      menuItem("Map", tabName = "map",icon = icon("fas fa-globe-africa", lib = "font-awesome"))
      

    )
  ),
  dashboardBody( style="background-color:#203644",
    tabItems(
      tabItem("dashboard", 
              fluidRow(column(width=3,
                valueBoxOutput("confirmed", width = NULL),
                valueBoxOutput("deaths", width=NULL),
                valueBoxOutput("recoveries" , width=NULL)
                ),   column(9,
                         gradientBox(
                           title = "Confirmed Cases",width =12, gradientColor = "teal", closable = FALSE,
                           boxToolSize="sm", footer = withSpinner(plotlyOutput("plot1", height = 350)),
                           "Since January"))
             ,style="background-color:#203644" ),
             
             fluidRow(column(width=6,
                             gradientBox(title = "Confirmed Deaths", width =12, gradientColor = "teal", closable = FALSE, boxToolSize="sm", footer = withSpinner(plotlyOutput("plot2", height = 350)), "Top 5 countries" )),
                      column(width=6,
                             gradientBox(title = "Confirmed Recoveries", width =12, gradientColor = "teal", closable = FALSE, boxToolSize="sm", footer = withSpinner(plotlyOutput("plot3", height = 350)), "Top 5 countries" ))),
             fluidRow( style="margin-top:16px",
               column(12,
                      box(
                        title = 'COVID Summary', width = NULL,status = "info",
                        div(style = 'overflow-x: scroll', withSpinner(
                           DT::dataTableOutput('covidtbl'))))
               ))
             
             
             
      ),
      tabItem("map",
              numericInput("maxrows", "Rows to show", 25),
              verbatimTextOutput("rawtable"),
              downloadButton("downloadCsv", "Download as CSV")
      ),
      tabItem("kenya",
              fluidRow(
              column(6,
                     gradientBox(
                       title = "General",width =12, gradientColor = "teal", closable = FALSE,
                       boxToolSize="sm", footer = withSpinner(plotOutput("plot5", height = 350)),
                       "Kenya COVID-19 stats")),
              
              
              column(6,
                     gradientBox(
                       title = "Recoveries",width =12, gradientColor = "teal", closable = FALSE,
                       boxToolSize="sm", footer = withSpinner(plotlyOutput("plot4", height = 350)),
                       "Since January"))
              ),
              fluidRow(style="margin-left:4px;margin-right:4px",
                column(width=3,
                              valueBoxOutput("kenya_confirmed", width = NULL)),
                       column(width=3, valueBoxOutput("kenya_deaths", width=NULL)),
                       column(width=3, valueBoxOutput("kenya_recoveries" , width=NULL)),
                       column(width=3, valueBoxOutput("kenya_active" , width=NULL))
                              
              ),
              fluidRow(style="margin-left:4px;margin-right:4px",
                gradientBox(
                  title = "Deaths",width =12, gradientColor = "teal", closable = FALSE,
                  boxToolSize="sm", footer = withSpinner(plotlyOutput("plot6", height = 400)),
                  "Since January")
              )
              ),
      
      tabItem("world",
              fluidRow(
                column(12,
                       gradientBox(
                         title = "Global",width =12, gradientColor = "teal", closable = FALSE,
                         boxToolSize="sm", footer = withSpinner(plotOutput("plot7", height = 450)),
                         "Confirmed cases by country"))),
              fluidRow(
                column(12,
                       gradientBox(
                         title = "Recoveries",width =12, gradientColor = "teal", closable = FALSE,
                         boxToolSize="sm", footer = withSpinner(plotlyOutput("plot8", height = 450)),
                         "Recoveries stats by country"))),
              fluidRow(
                column(12,
                       gradientBox(
                         title = "Deaths",width =12, gradientColor = "teal", closable = FALSE,
                         boxToolSize="sm", footer = withSpinner(plotlyOutput("plot9", height = 450)),
                         "Death stats by country")))
      )
  
    )
    )
  )


