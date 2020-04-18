rm(list = ls())
library(scales)
library(lubridate)
library(plotly)
library(tidyr)
library(dplyr)
library(DT)


#library(ggplot2)
#library(tidyverse)

library(reshape2)
library(grid)
library(gridExtra)

options(scipen = 999)

#setwd("C:\\Users\\Allan\\OneDrive\\Documents\\Covid19\\")
getwd()
setwd("C:\\Users\\85036758\\Documents\\Covid19\\")
df <- read.csv("data\\deaths.csv", header = TRUE, stringsAsFactors = FALSE)
confirmed_cases <- read.csv("data\\covid19_confirmed.csv", header = TRUE, stringsAsFactors = FALSE)
recovery_cases <- read.csv("data\\covid19_recovered.csv", header = TRUE, stringsAsFactors = FALSE)
print(df)
print(confirmed_cases)

server <- function(input, output) { 
  
  
  formatted_df <- df %>%
    gather(date, numofdeaths, 'X43852':'X43935', convert = TRUE) %>%
    mutate(date = sub("X", " ", date))  %>%
    rename(country = Country.Region)  %>%
    mutate(date = as.numeric(date)) %>%
    mutate(date = as.Date(date, origin="1899-12-30")) %>%
    select(country, date, numofdeaths) %>%
    group_by(country, date) %>%
    summarize(numofdeaths = sum(numofdeaths))
  
  formatted_df 
  summary(formatted_df)
  
  fom_confirmed_cases<- confirmed_cases %>%
    gather(date, numofcases, 'X43852':'X43935', convert = TRUE) %>%
    mutate(date = sub("X", " ", date))  %>%
    rename(country = Country.Region)  %>%
    mutate(date = as.numeric(date)) %>%
    mutate(date = as.Date(date, origin="1899-12-30")) %>%
    select(country, date, numofcases) %>%
    group_by(country, date) %>%
    summarize(numofcases = sum(numofcases))
  
  
  fom_recovery_cases<- recovery_cases %>%
    gather(date, numofrecoveries, 'X43852':'X43935', convert = TRUE) %>%
    mutate(date = sub("X", " ", date))  %>%
    rename(country = Country.Region)  %>%
    mutate(date = as.numeric(date)) %>%
    mutate(date = as.Date(date, origin="1899-12-30")) %>%
    select(country, date, numofrecoveries) %>%
    group_by(country, date) %>%
    summarize(numofrecoveries = sum(numofrecoveries))
  
  
  
  
  plotdata <- formatted_df%>%
    filter(numofdeaths >500  & date == "2020-04-14") %>%
    as_tibble()
  
  
  summary(plotdata)
  
  # plotdata$country <- as.character(plotdata$country)
  
  pt <- ggplot(data=plotdata, aes(x=country, y=numofdeaths, fill=country))+
    geom_bar(stat = "identity", width = 0.3) +
    # geom_text(aes(x = country, y = numofdeaths, label = numofdeaths)) +
    guides(fill=FALSE)+
    labs(title="Deaths per Country", 
         x= "Country",
         y= "Num of Deaths",
         #subtitle="", 
         caption="source:  Johns Hopkins University Center for Systems Science and Engineering (JHU CCSE)") +
    theme(axis.text.x = element_text(angle=65, vjust = 0.4), legend.position="none") 
  pt
  ggplotly(pt, tooltip = "text")
  
 
  
  totalofdeaths  <- formatted_df %>%
    filter(date == "2020-04-14") %>%
    as_tibble()
  
  totaldeaths = sum(totalofdeaths$numofdeaths)
  totaldeaths
  
  #fig2 <- ggplotly(pt2, tooltip = "text")
  #fig2
  
  
  
  # CONFIRMED CASES
  
  confirmedplotdata <- fom_confirmed_cases %>% 
    filter(date == "2020-04-14") %>%
    as_tibble()
  
  confirmedplotdata  
  totalconfirmedcases <- sum(confirmedplotdata$numofcases)
  totalconfirmedcases
  
  
  # RECOVERY CASES
  
  recoveryplotdata <- fom_recovery_cases %>%
    filter(date == "2020-04-14") %>%
    as_tibble()
  recoveryplotdata
  
  totalrecoveries <- sum(recoveryplotdata$numofrecoveries)
  totalrecoveries
  
  
  uniquecountries <-formatted_df%>%
    filter( date == "2020-04-14") %>%
    select(country, numofdeaths)
  
  covid_df <- cbind.data.frame(country=uniquecountries$country,  confirmed_cases=confirmedplotdata$numofcases,
                               deaths=uniquecountries$numofdeaths, recoveries=recoveryplotdata$numofrecoveries )
  covid_df <- covid_df %>% 
              rename(Country = country, Confirmed_Cases = confirmed_cases, Deaths=deaths,
                     Recoveries=recoveries) %>% 
              mutate(Active =Confirmed_Cases-Deaths-Recoveries)
  
  
  
  output$confirmed <- renderValueBox({
    valueBox(
      value = totalconfirmedcases,
      subtitle = "Total confirmed cases",
      icon = icon("area-chart"),
      color = "yellow"
    )
  })
  
  
  output$deaths <- renderValueBox({
    valueBox(
      value = totaldeaths,
      subtitle = "Total deaths ",
      icon = icon("area-chart"),
      color = "red"
    )
  })
  
  output$recoveries <- renderValueBox({
    valueBox(
      value = totalrecoveries,
      subtitle = "Total Recoveries ",
      icon = icon("area-chart"),
      color = "green"
    )
  })
  
  output$plot1 <- renderPlotly({
    
         filcountries <- confirmedplotdata %>% 
                          top_n(n=5, w=numofcases) 
                        
         filcountries <-  as_tibble(filcountries) 
         print(filcountries)
         
        
        plt1data <-  fom_confirmed_cases %>% 
                    filter(date >= "2020-03-14" & country == c(filcountries$country)) %>%
                    arrange(desc(numofcases)) 
        #%>% 
                   # filter(country == in (filcountries))
        print(plt1data)
    
    
      pt2 <- ggplot(plt1data, aes(x=ymd(date), numofcases, col= country, group=1, text = paste0("country:", country, "<br>","confirmed cases:", numofcases,
                                                                                                "<br>","date:", ymd(date))))+  
      geom_line()+
      geom_point() +
      scale_x_date(labels = date_format("%b-%d"), breaks = "3 days")+
      #scale_x_date(breaks = "2 days") +
      labs(title="Confirmed Cases by Date",
           x= "Date",
           y= "Cases",
           subtitle="Top 5 Countries", 
           caption="source: Johns Hopkins University Center for Systems Science and Engineering (JHU CCSE)") +
      
      theme(axis.text.x = element_text(angle=85, vjust=0.5), legend.position = "bottom", legend.title = element_blank(),
            panel.background = element_rect(fill = "#04070c", colour = "#6D9EC1",
                                            size = 2, linetype = "solid"),
            panel.grid.minor = element_blank()
            ) 
    
     ggplotly(pt2, tooltip = "text")
    
    
  })
  
  output$plot2 <- renderPlotly({ 
    
  
    pltcountry <- totalofdeaths %>% 
      top_n(n=5, w=numofdeaths) 
    
    pltcountry <- as_tibble(pltcountry)
    print(pltcountry)
    
    plotdata1 <- formatted_df%>%
               filter(date >= "2020-03-14" & country == c(pltcountry$country))
  
    print(plotdata1)
    
    plotdata1$date <- ymd(plotdata1$date )
    plotdata1$date <- as.Date(plotdata1$date, format = "%Y-%m-%d" )
    minn <- min(plotdata1$date)
    maxx <- max(plotdata1$date)
    
    
    pt2 <- ggplot(plotdata1, aes(date, numofdeaths, col= country, group=1))+  
      geom_line(size=0.6, alpha=0.6) +
      geom_point()+
      scale_x_date(breaks = "3 days", labels = date_format("%b-%d")) +
      labs(title="Deaths by Date",
           x= "Date",
           y= "Deaths",
           #subtitle="", 
           caption="source: Johns Hopkins University Center for Systems Science and Engineering (JHU CCSE)") +
      
      theme(axis.text.x = element_text(angle=85, vjust=0.5), legend.position = "bottom", legend.title = element_blank())
    
    ggplotly(pt2)%>%
      layout(legend = list(
        orientation = "h",
        x=0.5,
        y=-0.5
      )
      )
    
    })
  
  output$plot3 <- renderPlotly({
    
    rpltcountry <- recoveryplotdata %>% 
      top_n(n=5, w=numofrecoveries) 
    
    rpltcountry <- as_tibble(rpltcountry)
    print(rpltcountry)
    
    plotdata2 <- fom_recovery_cases%>%
      filter(date >= "2020-03-14" & country == c(rpltcountry$country))
    
    
    pt2 <- ggplot(plotdata2, aes(date, numofrecoveries, col= country,group=1, text= paste0("country:", country, "<br>","recoveries:", numofrecoveries,
                                                                                    "<br>","date:", ymd(date))))+  
      geom_line(size=0.6, alpha=0.6) +
      geom_point()+
      scale_x_date(breaks = "3 days", labels = date_format("%b-%d")) +
      labs(title="Recoveries by Date",
           x= "Date",
           y= "Recoveries",
           #subtitle="", 
           caption="source: Johns Hopkins University Center for Systems Science and Engineering (JHU CCSE)") +
      
      theme(axis.text.x = element_text(angle=85, vjust=0.5), legend.position = "bottom", legend.title = element_blank())
    
    ggplotly(pt2, tooltip="text")%>%
      layout(legend = list(
        orientation = "h",
        x=0.5,
        y=-0.5
      )
      )
    
  })
  
 coviddt <- datatable(
    covid_df,
    options = list(pageLength =10, dom = 'ft',  autoWidth = FALSE,
                   columnDefs = list(list(width = '160px', 
                                          targets = c(1,2,3,4,5))))
    
    
  )
  output$covidtbl <- DT::renderDataTable(coviddt,
                                         filter = c("none"),
                                           options = list(pageLength = 5, dom = 't',  autoWidth = FALSE), rownames = FALSE)
 
  
}

