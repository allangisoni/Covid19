library(shiny)
library(shinyjs)
library(shinydashboard)
library(shinydashboardPlus)
library(shinybusy)
library(shinyWidgets)
library(xlsx)
library(shinycssloaders)
library(sodium)
library(scales)
library(lubridate)
library(plotly)
library(tidyr)
library(dplyr)
library(reshape2)
library(grid)
library(gridExtra)
library(highcharter)
#library(tmap)
#library(leaflet)


dataUrl <- a(href="https://data.humdata.org/dataset/novel-coronavirus-2019-ncov-cases",  
                    "Johns Hopkins University Center for Systems Science and Engineering (JHU CCSE)." )
gitHubUrl <- a(href="https://github.com/dihesus/Covid19", "Github")
dataCode <- ("Code and data used to generate this site mapping tool are available on")
dtbr <- tags$br()
linkedInIcon <- icon("fab fa-linkedin", lib="font-awesome")
linkedInUrl <- a(href="https://www.linkedin.com/in/allan-gisoni/", "LinkedIn")
profileimage <- img("hesus.jpg")

dashboardPagePlus(
  dashboardHeaderPlus(title =tagList(span(class="logo-lg","COVID 19"),img(src="")) , enable_rightsidebar = FALSE), 
  enable_preloader = TRUE, loading_duration = 3, title = "COVID 19 DASHBOARD",
  dashboardSidebar(
    sidebarMenu(id="sidebar",
      menuItem("Dashboard", tabName = "dashboard", icon = icon("fas fa-tachometer-alt", lib = "font-awesome")),
      menuItem("Charts", tabName = "chart", icon = icon("fas fa-chart-pie", lib = "font-awesome"),
               menuSubItem("Kenya", tabName = "kenya"),
               menuSubItem("World", tabName = "world")),
      menuItem("World Map", tabName = "map",icon = icon("fas fa-globe-africa", lib = "font-awesome")),
      menuItem("About", tabName = "about",icon = icon("fas fa-info-circle", lib = "font-awesome"))
      

    )
  
  ),
  dashboardBody(useShinyjs(),style="background-color:#203644", 
    tabItems(
      tabItem("dashboard", 
              fluidRow(column(width=3,
                valueBoxOutput("confirmed", width = NULL),
                valueBoxOutput("deaths", width=NULL),
                valueBoxOutput("recoveries" , width=NULL),
                valueBoxOutput("active" , width=NULL)
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
             fluidRow( style="margin-top:16px;margin-bottom:8px",
               column(12,
                      box(
                        title = 'COVID Summary', width = NULL,status = "info",
                        div(style = 'overflow-x: scroll', withSpinner(
                           DT::dataTableOutput('covidtbl'))))
               ))
             
             
             
      ),
      tabItem("about",
              fluidRow(column(
                width=12, style="height:100px"
              )),
             fluidRow(
               column(width=8, offset = 2,
               widgetUserBox(
                 width = 12,
                 title = "Allan Gisoni",
                 subtitle = tagList("Software Developer" ) ,
                 type = NULL,
                 src ="https://image.flaticon.com/icons/svg/2230/2230951.svg",
                 #src="hesus.jpg",
                 background = TRUE,
                 backgroundUrl = "https://www.pexels.com/photo/apple-devices-books-business-coffee-572056/",
                 closable = FALSE,
                 "General Info",
                 footer = tagList( "This site is updated daily. All data resources used have been taken from "
                                  ,dataUrl,dtbr,dtbr,dataCode,gitHubUrl,".", dtbr, dtbr, "Get in touch:",  socialButton(
                                    url = "https://www.linkedin.com/in/allan-gisoni/",
                                    type = "linkedin"
                                  )," ",socialButton(
                                    url ="https://github.com/dihesus",
                                    type= "github"
                                  ),dtbr, "Email: Allangisoni@gmail.com" )
               )
               )
               
      ),
      fluidRow(column(
        width=12, style="height:120px"
      ))),
      tabItem("kenya",
              fluidRow(
              column(6,
                     gradientBox(
                       title = "General",width =12, gradientColor = "teal", closable = FALSE,
                       boxToolSize="sm", footer = withSpinner(plotOutput("plot5", height = 350)),
                       "Kenya COVID-19 stats")),
              
              
              column(6,
                     gradientBox(
                       title = "Confirmed Cases",width =12, gradientColor = "teal", closable = FALSE,
                       boxToolSize="sm", footer = withSpinner(plotlyOutput("plot4", height = 350)),
                       "Since March"))
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
                  "Since March")
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
                boxPlus(
                  width = 12,
                  title = "Recovery stats by country", 
                  closable = FALSE, 
                  status = "info", 
                  solidHeader = FALSE, 
                  collapsible = TRUE,
                  enable_sidebar = TRUE,
                  sidebar_width = 25,
                  sidebar_start_open = TRUE,
                  sidebar_content = tagList(
                    sliderInput(
                      "slider_rcountry", 
                      "Number of countries to display:",
                      min = 2, 
                      max = 200, 
                      value = 5
                    )
                  ),
                  plotlyOutput("plot8", height = 450)
                )
                ),
              fluidRow(
                boxPlus(
                  width = 12,
                  title = "Death stats by country", 
                  closable = FALSE, 
                  status = "info", 
                  solidHeader = FALSE, 
                  collapsible = TRUE,
                  enable_sidebar = TRUE,
                  sidebar_width = 25,
                  sidebar_start_open = TRUE,
                  sidebar_content = tagList(
                    sliderInput(
                      "slider_country", 
                      "Number of countries to display:",
                      min = 2, 
                      max = 200, 
                      value = 5
                    )
                  ),
                  plotlyOutput("plot9", height = 450)
                )
                
                )
      ),
      
      tabItem("map",
              fluidRow(
              withSpinner(  highchartOutput("world_tmap", height = 700))
              ))
  
    )
    ), dashboardFooter(right_text = "Last updated on 19/05/2020 07.47AM")
  )


