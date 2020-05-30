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
library(highcharter)
library(countrycode)
#library(tmap)
#library(leaflet)
#data("World")

options(scipen = 999)

#setwd("C:\\Users\\Allan\\OneDrive\\Documents\\Covid-19 S\\")
#setwd("C:\\Users\\Domino\\Documents\\Covid19\\")
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

server <- function(input, output, session) { 
  
  
  observeEvent(input$sidebar,{
    addClass(selector = "body", class = "sidebar-collapse")
    removeClass(selector = "body", class = "sidebar-open")
  })
  
  max_date <- "2020-05-20"
  min_date <- "2020-01-22"
  
  
  formatted_df <- df %>%
    gather(date, numofdeaths, 'X43852':'X43971', convert = TRUE) %>%
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
    gather(date, numofcases, 'X43852':'X43971', convert = TRUE) %>%
    mutate(date = sub("X", " ", date))  %>%
    rename(country = Country.Region)  %>%
    mutate(date = as.numeric(date)) %>%
    mutate(date = as.Date(date, origin="1899-12-30")) %>%
    select(country, date, numofcases) %>%
    group_by(country, date) %>%
    summarize(numofcases = sum(numofcases))
  
  
  fom_recovery_cases<- recovery_cases %>%
    gather(date, numofrecoveries, 'X43852':'X43971', convert = TRUE) %>%
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
  
  output$plot1 <- renderHighchart({
    
    pltconfirmdata <- fom_confirmed_cases %>% 
      filter(date >= min_date) %>%
      group_by(date) %>% 
      summarise(numofconfirmedcases= sum(numofcases))%>% 
      mutate(new_cases = numofconfirmedcases -lag(numofconfirmedcases, default = first(numofconfirmedcases))) 
    
    pltconfirmdata <- as.data.frame(pltconfirmdata)
    
      highchart() %>% 
      hc_add_series(pltconfirmdata,
                    "area",
                    hcaes(y=numofconfirmedcases, x=date), name="Cases") %>% 
      hc_xAxis(type = "datetime", dateTimeLabelFormats = list(day = '%d of %b'))%>%
      hc_yAxis(title = list(text = "Cases")) %>% 
      hc_tooltip(shared=TRUE, borderWidth=5) %>% 
      hc_plotOptions(line = list(
        marker = list(
          lineWidth = 2,
          lineColor = NULL
        )            
      ))
    
    
    
    
    
    
  })
  
  
  output$plot2 <- renderHighchart({ 
    
    pltcountry <- as.data.frame(totalofdeaths)%>% 
                 top_n(n=5, w=numofdeaths) 
    
    
    top_country_deaths <- formatted_df %>% 
                          filter(country %in% c(pltcountry$country)) %>% 
                          rename(y=numofdeaths)
    
    top_country_deaths <- as.data.frame(top_country_deaths)
    
    
    hc_world_deaths <- highchart() %>% 
                       hc_xAxis(type = "datetime", dateTimeLabelFormats = list(day = '%d of %b'))%>%
                       hc_yAxis(title = list(text = "Deaths")) %>% 
                       hc_add_series(top_country_deaths,
                       "line",
                       hcaes(y=y, x=date, group='country')) %>% 
                       hc_tooltip(shared=TRUE, borderWidth=5) %>% 
                       hc_plotOptions(line = list(
                       marker = list(
                       lineWidth = 2,
                       lineColor = NULL
                       )            
                       ))
    
    
    
    hc_world_deaths
    
  })
  
  output$plot3 <- renderHighchart({
    
    rpltcountry <- as.data.frame( recoveryplotdata) %>% 
                   top_n(n=5, w=numofrecoveries) 
    
    rpltcountry <- as_tibble(rpltcountry)
    print(rpltcountry)
    
    plotdata2 <- fom_recovery_cases %>%
                 filter(date >= "2020-01-22" & country %in% c(rpltcountry$country)) %>% 
                rename(y=numofrecoveries)
    
    print(plotdata2)
    plotdata2 <- as.data.frame(plotdata2)
    
    
    hc_world_recoveries <- highchart() %>% 
      hc_xAxis(type = "datetime", dateTimeLabelFormats = list(day = '%d of %b'))%>%
      hc_yAxis(title = list(text = "Recoveries")) %>% 
      hc_add_series(plotdata2,
                    "line",
                    hcaes(y=y,x=date, group='country')) %>% 
      hc_tooltip(shared=TRUE, borderWidth=5) %>% 
      hc_plotOptions(line = list(
        marker = list(
          lineWidth = 2,
          lineColor = NULL
        )            
      ))
    
    
    hc_world_recoveries
    
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
  output$plot4 <- renderHighchart({
    
    
    kenyaplotdata <- fom_confirmed_cases%>%
      filter(date >="2020-03-12" & country == c("Kenya")) %>% 
      arrange(date) %>% 
      mutate(new_cases = numofcases -lag(numofcases, default = first(numofcases)))
    
    
    hc_kenya_cases <- highchart() %>% 
      hc_xAxis(type = "datetime", dateTimeLabelFormats = list(day = '%d of %b'))%>%
      hc_yAxis(title = list(text = "Cases")) %>% 
      hc_add_series(kenyaplotdata,
                    "area",
                    hcaes(y=numofcases,x=date), name="Cases") %>% 
      hc_tooltip(shared=FALSE, borderWidth=5) %>% 
      hc_plotOptions(line = list(
        marker = list(
          lineWidth = 2,
          lineColor = NULL
        )            
      ))
    
    
    hc_kenya_cases
    
    
    
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
  
  output$plot6 <- renderHighchart({
    
    kenyadeathsplotdata <- formatted_df%>%
      filter(date >="2020-03-12" & country == "Kenya") %>% 
      arrange(date) %>% 
      mutate(new_deaths = numofdeaths -lag(numofdeaths, default = first(numofdeaths)))
  
    hc_kenya_deaths <- highchart() %>% 
      hc_xAxis(type = "datetime", dateTimeLabelFormats = list(day = '%d of %b'))%>%
      hc_yAxis(title = list(text = "Cases")) %>% 
      hc_add_series(kenyadeathsplotdata,
                    "line",
                    hcaes(y=numofdeaths,x=date), name="Deaths", showInLegend=FALSE) %>% 
      hc_tooltip(shared=FALSE, borderWidth=5) %>% 
      hc_plotOptions(line = list(
        marker = list(
          lineWidth = 2,
          lineColor = NULL
        )            
      ))
    
    
    hc_kenya_deaths
    
      
   
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
    dmaxx <-as.Date("2020-05-25", format = "%Y-%m-%d")
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
    dmaxx <-as.Date("2020-05-25", format = "%Y-%m-%d")
    
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
  
  
  
  
  output$world_tmap <- renderHighchart({
    
    
    
    world_tb <- formatted_df %>% 
      filter(date == max_date) 
    
    
    world_tb <- tibble(country=world_tb$country, numofdeaths=world_tb$numofdeaths)
    world_tb
    
    
    
    world_confirmed_tb <- fom_confirmed_cases %>%
      filter(date == max_date)
    
    
    world_confimed_tb <- tibble(country=world_confirmed_tb$country, numofcases=world_confirmed_tb$numofcases)
    
    
    
    world_recovered_tb <- fom_recovery_cases %>% 
      filter(date == max_date) 
    
    world_recovered_tb <-  tibble(country=world_recovered_tb$country, numofrecoveries=world_recovered_tb$numofrecoveries)
    world_recovered_tb
    
    
    alt_worlddata <- merge(world_tb, world_confimed_tb, by="country")
    alt_worlddata <- merge(alt_worlddata, world_recovered_tb, by="country")
    
    
    alt_worlddata <- alt_worlddata %>% 
      mutate(iso_a3 =countrycode(country,"country.name", "genc3c"))  %>% 
      select(iso_a3, country,numofcases, numofdeaths,numofrecoveries) 
    
    
    
    highchart() %>% 
      hc_chart(backgroundColor="#203644") %>% 
      hc_add_series_map(worldgeojson,
                        alt_worlddata,  
                        value = "numofcases", 
                        joinBy = c("iso3","iso_a3"), name="Country",
                        showInLegend=FALSE) %>% 
      hc_tooltip(useHTML=TRUE,
                 headerFormat="",
                 pointFormat="<b>{point.name}</b><br>
                                    Cases: {point.numofcases}<br>
                                    Deaths:{point.numofdeaths}<br>
                                    Recoveries:{point.numofrecoveries}",
                 borderWidth=5) %>% 
      hc_plotOptions(series = list(showInLegend = FALSE)) %>% 
      hc_mapNavigation(enabled = TRUE) %>%
      hc_credits(enabled = TRUE, 
                 text = "Sources:Johns Hopkins University Center
                       for Systems Science and Engineering (JHU CCSE)", style = list(fontSize = "10px")) %>% 
      hc_colorAxis(minColor = "#e6f7ff", maxColor = "#0099e6", showInLegend=FALSE )
    
  })
  
  
}

