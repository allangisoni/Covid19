rm(list = ls())
library(scales)
library(lubridate)
library(plotly)
library(tidyr)
library(dplyr)
library(DT)
library(reshape2)
library(grid)
library(gridExtra)
library(treemapify)
library(directlabels)
library(rsconnect)

options(scipen = 999)

#setwd("C:\\Users\\Allan\\OneDrive\\Documents\\Covid-19 S\\")
#getwd()
#setwd("C:\\Users\\85036758\\Documents\\Covid19\\")
df <- read.csv("data/deaths.csv", header = TRUE, stringsAsFactors = FALSE)
#df <- read.csv("data\\deaths.csv", header = TRUE, stringsAsFactors = FALSE)
confirmed_cases <- read.csv("data/confirmed.csv", header = TRUE, stringsAsFactors = FALSE)
#confirmed_cases <- read.csv("data\\confirmed.csv", header = TRUE, stringsAsFactors = FALSE)
recovery_cases <- read.csv("data/recovered.csv", header = TRUE, stringsAsFactors = FALSE)
#recovery_cases <- read.csv("data\\recovered.csv", header = TRUE, stringsAsFactors = FALSE)
print(df)
print(confirmed_cases)

server <- function(input, output) { 
  
  
  observeEvent(input$sidebar,{
    addClass(selector = "body", class = "sidebar-collapse")
    removeClass(selector = "body", class = "sidebar-open")
    })
 
  max_date <- "2020-04-25"
  min_date <- "2020-01-22"
  
  
  formatted_df <- df %>%
    gather(date, numofdeaths, 'X43852':'X43946', convert = TRUE) %>%
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
    gather(date, numofcases, 'X43852':'X43946', convert = TRUE) %>%
    mutate(date = sub("X", " ", date))  %>%
    rename(country = Country.Region)  %>%
    mutate(date = as.numeric(date)) %>%
    mutate(date = as.Date(date, origin="1899-12-30")) %>%
    select(country, date, numofcases) %>%
    group_by(country, date) %>%
    summarize(numofcases = sum(numofcases))
  
  
  fom_recovery_cases<- recovery_cases %>%
    gather(date, numofrecoveries, 'X43852':'X43946', convert = TRUE) %>%
    mutate(date = sub("X", " ", date))  %>%
    rename(country = Country.Region)  %>%
    mutate(date = as.numeric(date)) %>%
    mutate(date = as.Date(date, origin="1899-12-30")) %>%
    select(country, date, numofrecoveries) %>%
    group_by(country, date) %>%
    summarize(numofrecoveries = sum(numofrecoveries))
  
  
  
  
  plotdata <- formatted_df%>%
    filter(numofdeaths >2000  & date == max_date) %>%
    as_tibble()
  
  
 
  
  totalofdeaths  <- formatted_df %>%
    filter(date == max_date) %>%
    as_tibble()
  
  totaldeaths = sum(totalofdeaths$numofdeaths)
  totaldeaths
  
  #fig2 <- ggplotly(pt2, tooltip = "text")
  #fig2
  
  
  
  # CONFIRMED CASES
  
  confirmedplotdata <- fom_confirmed_cases %>% 
    filter(date == max_date) %>%
    as_tibble()
  
  confirmedplotdata  
  totalconfirmedcases <- sum(confirmedplotdata$numofcases)
  totalconfirmedcases
  
  
  # RECOVERY CASES
  
  recoveryplotdata <- fom_recovery_cases %>%
    filter(date == max_date) %>%
    as_tibble()
  recoveryplotdata
  
  totalrecoveries <- sum(recoveryplotdata$numofrecoveries)
  totalrecoveries
  
  
  uniquecountries <-formatted_df%>%
    filter( date == max_date) %>%
    select(country, numofdeaths)
  
  covid_df <- cbind.data.frame(country=uniquecountries$country,  confirmed_cases=confirmedplotdata$numofcases,
                               deaths=uniquecountries$numofdeaths, recoveries=recoveryplotdata$numofrecoveries )
  covid_df <- covid_df %>% 
              rename(Country = country, Confirmed_Cases = confirmed_cases, Deaths=deaths,
                     Recoveries=recoveries) %>% 
              mutate(Active =Confirmed_Cases-Deaths-Recoveries)
  
  
  
  output$confirmed <- renderValueBox({
    valueBox(
      value = prettyNum(totalconfirmedcases,big.mark=",",scientific=FALSE),
      subtitle = "Total confirmed cases",
      icon = icon("fas fa-lightbulb","fa-xs", lib="font-awesome"),
      color = "yellow"
    )
  })
  
  
  output$deaths <- renderValueBox({
    valueBox(
      value =  prettyNum(totaldeaths,big.mark=",",scientific=FALSE),
      subtitle = "Total deaths ",
      icon = icon("fas fa-exclamation-triangle", "fa-xs", lib="font-awesome"),
      color = "red"
    )
  })
  
  output$recoveries <- renderValueBox({
    valueBox(
      value =  prettyNum(totalrecoveries,big.mark=",",scientific=FALSE),
      subtitle = "Total Recoveries ",
      icon = icon("fas fa-heartbeat","fa-xs",lib="font-awesome"),
      color = "green"
    )
  })
  output$active <- renderValueBox({
    valueBox(
      value =  prettyNum(totalconfirmedcases-totaldeaths-totalrecoveries,big.mark=",",scientific=FALSE),
      subtitle = "Active ",
      icon = icon("fas fa-spa","fa-xs",lib="font-awesome"),
      color = "navy"
    )
  })
  
  output$plot1 <- renderPlotly({
    
         filcountries <- confirmedplotdata %>% 
                          top_n(n=5, w=numofcases) 
                        
         filcountries <-  as_tibble(filcountries) 
         print(filcountries)
         
        
        plt1data <-  fom_confirmed_cases %>% 
                    #filter(date >= "2020-03-14" & country == c(filcountries$country)) %>%
                    filter(date >= "2020-03-21" & country == c("US")) %>%
                    arrange(desc(numofcases)) 
       
        print(plt1data)
    
        pltconfirmdata <- fom_confirmed_cases %>% 
          filter(date >= min_date) %>%
          group_by(date) %>% 
          summarise(numofconfirmedcases= sum(numofcases))%>% 
          mutate(new_cases = numofconfirmedcases -lag(numofconfirmedcases, default = first(numofconfirmedcases))) 
        
    
      pt2 <- ggplot(pltconfirmdata, aes(x=ymd(date), numofconfirmedcases, col= "#e6f2ff", group=1, text = paste0( "date:", ymd(date),
                                                                                                                  "<br>","new_cases:", new_cases,
                                                                                                                 "<br>", "total cases:", numofconfirmedcases
                                                                                                        )))+  
      geom_area(fill="#e6f2ff", col= "#e6f2ff") + 
      geom_line(col= "#80bfff")+
      geom_point(col= "#001a33") +
      scale_x_date(labels = date_format("%b-%d"), breaks = "7 days")+
      scale_y_continuous(labels = comma)+  
      #scale_x_date(breaks = "2 days") +
      labs(#title="Confirmed Cases by Date",
           x= "",
           y= "Cases",
           #subtitle=" Countries", 
           caption="source: Johns Hopkins University Center for Systems Science and Engineering (JHU CCSE)") +
      
      theme(axis.text.x = element_text(angle=85, vjust=0.5), legend.position = "none", legend.title = element_blank(),
            panel.background = element_rect(fill = "#f2f2f2", colour = "#333333",
                                            size = 2, linetype = "dashed"),
            panel.grid.major.x= element_blank()
            ) 
    
     ggplotly(pt2, tooltip = "text") %>% config(displayModeBar=FALSE)
    
    
  })
  
  output$plot2 <- renderPlotly({ 
    
  
    pltcountry <-as.data.frame(totalofdeaths) %>% 
      top_n(n=5, w=numofdeaths) 
    
    pltcountry <- as_tibble(pltcountry)
    print(pltcountry)
    
    plotdata1 <- formatted_df%>%
               filter(date >= "2020-03-21" & country %in% c(pltcountry$country))
  
    print(plotdata1)
    
    plotdata1$date <- ymd(plotdata1$date )
    plotdata1$date <- as.Date(plotdata1$date, format = "%Y-%m-%d" )
    minn <- min(plotdata1$date)
    maxx <- max(plotdata1$date)
    
    
    pt2 <- ggplot(plotdata1, aes(date, numofdeaths, col= country, group=1,
                                 text= paste0("country:", country, "<br>","deaths:", numofdeaths,
                                              "<br>","date:", ymd(date))))+  
      geom_line(size=0.6, alpha=0.6) +
      geom_point()+
      scale_x_date(breaks = "3 days", labels = date_format("%b-%d")) +
      scale_y_continuous(labels = comma)+ 
      labs(#title="Deaths by Date",
           x= "",
           y= "Deaths",
           #subtitle="", 
           caption="source: Johns Hopkins University Center for Systems Science and Engineering (JHU CCSE)") +
      
      theme(axis.text.x = element_text(angle=85, vjust=0.5), legend.position = "bottom", legend.title = element_blank(),
            panel.background = element_rect(fill = "#f2f2f2", colour = "#333333",
                                            size = 2, linetype = "dashed"),
            panel.grid.major.x= element_blank())
    
    ggplotly(pt2, tooltip = "text")%>%
      layout(legend = list(
        orientation = "h",
        x=0.5,
        y=-0.5
      )
      )%>% config(displayModeBar=FALSE)
    
    })
  
  output$plot3 <- renderPlotly({
    
    rpltcountry <- as.data.frame( recoveryplotdata) %>% 
      top_n(n=5, w=numofrecoveries) 
    
    rpltcountry <- as_tibble(rpltcountry)
    print(rpltcountry)
    
    plotdata2 <- fom_recovery_cases%>%
      filter(date >= "2020-03-21" & country %in% c(rpltcountry$country))
    
    
    pt2 <- ggplot(plotdata2, aes(date, numofrecoveries, col= country,group=1, text= paste0("country:", country, "<br>","recoveries:", numofrecoveries,
                                                                                    "<br>","date:", ymd(date))))+  
      geom_line(size=0.6, alpha=0.6) +
      geom_point()+
      scale_x_date(breaks = "3 days", labels = date_format("%b-%d")) +
      scale_y_continuous(labels = comma)+ 
      labs(#title="Recoveries by Date",
           x= "",
           y= "Recoveries",
           #subtitle="", 
           caption="source: Johns Hopkins University Center for Systems Science and Engineering (JHU CCSE)") +
      
      theme(axis.text.x = element_text(angle=85, vjust=0.5), legend.position = "bottom", legend.title = element_blank(),
            panel.background = element_rect(fill = "#f2f2f2", colour = "#333333",
                                            size = 2, linetype = "dashed"),
            panel.grid.major.x= element_blank())
    
    ggplotly(pt2, tooltip="text")%>%
      layout(legend = list(
        orientation = "h",
        x=0.5,
        y=-0.5
      )
      )%>% config(displayModeBar=FALSE)
    
  })
  
  coviddt <- datatable(
    covid_df,
    options = list(pageLength =10, dom = 'Bftsp',  autoWidth = TRUE,
                   columnDefs = list(list(width = '270px', 
                                          targets = c(1,2,3,4,5))))
    
  )
  output$covidtbl <- DT::renderDataTable(coviddt,
                                         filter = c("none"),
                                           options = list(pageLength = 10, dom = 'ft',  autoWidth = TRUE), rownames = FALSE)
  output$plot4 <- renderPlotly({
    
    
    plotdata2 <- fom_confirmed_cases%>%
                 filter(date >="2020-03-12" & country == c("Kenya")) %>% 
                 arrange(date) %>% 
                 mutate(new_cases = numofcases -lag(numofcases, default = first(numofcases)))
    
    
    pt2 <- ggplot(plotdata2, aes(date, numofcases, group=1, text= paste0( "date:", ymd(date),
                                                                          "<br>","new_cases:", new_cases,
                                                                          "<br>","total cases:", numofcases)))+  
      geom_line(size=0.6, alpha=0.6, col= "#66b3ff") +
      geom_point(col= "#66b3ff")+
      scale_x_date(breaks = "7 days", labels = date_format("%b-%d")) +
      labs(#title="Recoveries by Date",
        x= "",
        y= "Case count",
        #subtitle="", 
        caption="source: Johns Hopkins University Center for Systems Science and Engineering (JHU CCSE)") +
      
      theme(axis.text.x = element_text(angle=85, vjust=0.5), legend.position = "none", legend.title = element_blank(),
            panel.background = element_rect(fill = "#f2f2f2", colour = "#333333",
                                            size = 2, linetype = "solid"),
            panel.grid.major.x= element_blank())
    
    ggplotly(pt2, tooltip="text")%>%
      layout(legend = list(
        orientation = "h",
        x=0.5,
        y=-0.5
      )
      )%>% config(displayModeBar=FALSE)
    
  })
  
  output$plot5 <- renderPlot({
    covid_df <-covid_df %>% 
      filter(Country=="Kenya")
    
    covid_df <- covid_df %>% 
      gather(category, values,Confirmed_Cases:Active , convert = TRUE)
    
    
    # Compute percentages
    covid_df$fract =100/4
    
    # Compute the cumulative percentages (top of each rectangle)
    covid_df$ymax = cumsum(covid_df$fract)
    
    # Compute the bottom of each rectangle
    covid_df$ymin = c(0, head(covid_df$ymax, n=-1))
    
    # Compute label position
    covid_df$labelPosition <- (covid_df$ymax + covid_df$ymin) / 2
    
    # Compute a good label
    covid_df$label <- paste0(covid_df$category, "\n count: ", covid_df$values)
    
    # Make the plot
    donutplot<- ggplot(covid_df, aes(ymax=ymax, ymin=ymin, xmax=4, xmin=3, fill=category)) +
      geom_rect() +
      geom_label( x=3.5, aes(y=labelPosition, label=label), size=3) +
      scale_fill_brewer(palette=4) +
      coord_polar(theta="y") +
      xlim(c(2, 4)) + theme_void()+ theme(legend.position = "none")
    donutplot 
    #ggplotly(donutplot)
  })
  
  covid_df <-covid_df %>% 
    filter(Country=="Kenya")
  output$kenya_confirmed <- renderValueBox({
    valueBox(
      value =  prettyNum(covid_df$Confirmed_Cases,big.mark=",",scientific=FALSE),
      subtitle = "Confirmed cases ",
      icon = icon("fas fa-lightbulb","fa-xs",lib="font-awesome"),
      color = "yellow"
    )
  })
  output$kenya_deaths <- renderValueBox({
    valueBox(
      value =  prettyNum(covid_df$Deaths,big.mark=",",scientific=FALSE),
      subtitle = "Deaths ",
      icon = icon("fas fa-exclamation-triangle", "fa-xs", lib="font-awesome"),
      color = "red"
    )
  })
  output$kenya_recoveries <- renderValueBox({
    valueBox(
      value =  prettyNum(covid_df$Recoveries,big.mark=",",scientific=FALSE),
      subtitle = "Recoveries ",
      icon = icon("fas fa-heartbeat","fa-xs",lib="font-awesome"),
      color = "green"
    )
  })
  output$kenya_active <- renderValueBox({
    valueBox(
      value = prettyNum(covid_df$Active ,big.mark=",",scientific=FALSE),
      subtitle = "Active cases ",
      icon = icon("fas fa-spa","fa-xs",lib="font-awesome"),
      color = "navy"
    )
  })
  
  output$plot6 <- renderPlotly({
    kenyaplotdata <- formatted_df%>%
                     filter(date >="2020-03-12" & country == "Kenya") %>% 
                     arrange(date) %>% 
                     mutate(new_deaths = numofdeaths -lag(numofdeaths, default = first(numofdeaths)))
    
    kenyapt <- ggplot(kenyaplotdata, aes(date, numofdeaths, group=1,
                                 text= paste0("date:", ymd(date),
                                              "<br>","new_deaths:", new_deaths,
                                              "<br>","total deaths:", numofdeaths)))+  
      geom_area(fill="#e6f2ff", col= "#e6f2ff") + 
      geom_line(size=0.6, alpha=0.6,col= "#66b3ff") +
      geom_point(col= "#66b3ff")+
      scale_x_date(breaks = "7 days", labels = date_format("%b-%d")) +
      labs(#title="Deaths by Date",
        x= "",
        y= "Deaths",
        #subtitle="", 
        caption="source: Johns Hopkins University Center for Systems Science and Engineering (JHU CCSE)") +
      
      theme(axis.text.x = element_text(angle=85, vjust=0.5), legend.position = "none", legend.title = element_blank(),
            panel.background = element_rect(fill = "#f2f2f2", colour = "#333333",
                                            size = 2, linetype = "dashed"),
            panel.grid.major.x= element_blank())
    
    ggplotly(kenyapt, tooltip = "text")%>%
      layout(legend = list(
        orientation = "h",
        x=0.5,
        y=-0.5
      )
      )%>% config(displayModeBar=FALSE)
  })
  
  output$plot7 <- renderPlot({
    
    pltworlddata <- fom_confirmed_cases %>% 
      filter(date >= max_date) 
    
  
     ggplot(pltworlddata, aes(area = numofcases, fill=country, label = country)) +
      geom_treemap()+
     geom_treemap_text(fontface = "italic", colour = "white", place = "centre",
                        grow = FALSE) +
    theme(legend.position = "none")
        
    
    

     

  })
  
  
  
  output$plot8 <- renderPlotly({
    
   
    
    country_rnum <- input$slider_rcountry
    print(country_rnum)
    
     slt_rcountry <-as.data.frame(recoveryplotdata) %>% 
      top_n(n=country_rnum, w=numofrecoveries) 
    
    slt_rcountry <- as_tibble(slt_rcountry)
    
    
    plotworldrecoveries <- fom_recovery_cases%>%
      filter(country %in% c(slt_rcountry$country))
    
    
                
    dminn <-as.Date(min_date, format = "%Y-%m-%d")
    dmaxx <-as.Date("2020-04-27", format = "%Y-%m-%d")
    pt8 <- ggplot(plotworldrecoveries, aes(date, numofrecoveries, col=country,group=1, text= paste0( "date:", ymd(date),
                                                                          "<br>","country:", country,                                    
                                                                          "<br>","recoveries:", numofrecoveries )))+  
      geom_line(size=0.6, alpha=0.6) +
      geom_point()+
      scale_x_date(breaks = "7 days",limits=c(dminn,dmaxx), labels = date_format("%b-%d")) +
      scale_y_continuous(labels = comma)+ 
      labs(#title="Recoveries by Date",
        x= "",
        y= "Recoveries",
        #subtitle="", 
        caption="source: Johns Hopkins University Center for Systems Science and Engineering (JHU CCSE)") +
      
      theme(axis.text.x = element_text(angle=85, vjust=0.5), legend.position = "none", legend.title = element_blank(),
            panel.background = element_rect(fill = "#f2f2f2", colour = "#333333",
                                            size = 2, linetype = "solid"),
            panel.grid.major.x= element_blank())
    #pt8
     ptdl8 <-direct.label(pt8,method="last.points" )
    
    
   
    ggplotly(pt8, tooltip="text")%>%
      layout(legend = list(
        orientation = "h",
        x=0.5,
        y=-0.5
      )
      )%>% config(displayModeBar=FALSE)
    
  })
  

  
  output$plot9 <- renderPlotly({
    dminn <-as.Date(min_date, format = "%Y-%m-%d")
    dmaxx <-as.Date("2020-04-27", format = "%Y-%m-%d")
    
    country_num <- input$slider_country
    print(country_num)
    
    slt_country <-as.data.frame(totalofdeaths) %>% 
      top_n(n=country_num, w=numofdeaths) 
      
    slt_country <- as_tibble(slt_country)
 
    
    plotworlddeaths <- formatted_df%>%
      filter(country %in% c(slt_country$country))


    
    pt9 <- ggplot(plotworlddeaths, aes(date, numofdeaths, col=country,group=1, text= paste0( "date:", ymd(date),
                                                                                                    "<br>","country:", country,                                    
                                                                                                    "<br>","deaths:", numofdeaths )))+  
      geom_line(size=0.6, alpha=0.6) +
      geom_point()+
      scale_x_date(breaks = "4 days",limits=c(dminn,dmaxx), labels = date_format("%b-%d")) +
      scale_y_continuous(labels = comma)+ 
      labs(#title="Recoveries by Date",
        x= "",
        y= "Deaths",
        #subtitle="", 
        caption="source: Johns Hopkins University Center for Systems Science and Engineering (JHU CCSE)") +
      
      theme(axis.text.x = element_text(angle=85, vjust=0.5), legend.position = "none", legend.title = element_blank(),
            panel.background = element_rect(fill = "#f2f2f2", colour = "#333333",
                                            size = 2, linetype = "solid"),
            panel.grid.major.x= element_blank())
    #pt8
    #ptdl8 <-direct.label(pt9,method="last.points" )
    
    
    
    ggplotly(pt9, tooltip="text")%>%
      layout(legend = list(
        orientation = "h",
        x=0.5,
        y=-0.5
      )
      )%>% config(displayModeBar=FALSE)
  })
  

  
}

