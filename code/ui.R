library(shiny)
library(shinydashboard)
library(shinydashboardPlus)
library(shinyjs)
library(shinybusy)

dashboardPage(
  dashboardHeader(title = "COVID 19 DASHBOARD"),
  dashboardSidebar(
    sliderInput("rateThreshold", "Warn when rate exceeds",
                min = 0, max = 50, value = 3, step = 0.1
    ),
    sidebarMenu(
      menuItem("Dashboard", tabName = "dashboard", icon = icon("fas fa-tachometer-alt", lib = "font-awesome")),
      menuItem("Chart", tabName = "chart", icon = icon("fas fa-chart-pie", lib = "font-awesome")),
      menuItem("Map", tabName = "map",icon = icon("fas fa-globe-africa", lib = "font-awesome"))

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
                         gradientBox(
                           title = "Confirmed Cases",width =12, gradientColor = "teal", closable = FALSE, boxToolSize="sm", footer = withSpinner(plotlyOutput("plot1", height = 350)),"Since January"))
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
      )
    )
  )
)

