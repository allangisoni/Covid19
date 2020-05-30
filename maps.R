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
library(worldgeojson)
library(countrycode)

#options(scipen = 999)

options(highcharter.options=list(lang=list(thousandsSep="*")))
hcopts<- getOption("highcharter.options")
hcopts
hcopts$lang$thousandsSep <- "*"
options(highcharter.options = hcopts)

#setwd("C:\\Users\\Allan\\OneDrive\\Documents\\Covid-19 S\\")
#getwd()
#setwd("C:\\Users\\85036758\\Documents\\Covid19\\")
df <- read.csv("data/deaths.csv", header = TRUE, stringsAsFactors = FALSE)
#df <- read.csv("data\\deaths.csv", header = TRUE, stringsAsFactors = FALSE)
confirmed_cases <- read.csv("data/confirmed.csv", header = TRUE, stringsAsFactors = FALSE)
#confirmed_cases <- read.csv("data\\confirmed.csv", header = TRUE, stringsAsFactors = FALSE)
recovery_cases <- read.csv("data/recovered.csv", header = TRUE, stringsAsFactors = FALSE)
#recovery_cases <- read.csv("data\\recovered.csv", header = TRUE, stringsAsFactors = FALSE)



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



world_tb <- formatted_df %>% 
  filter(date == max_date) 
  #%>% 
 # mutate(iso_a3 =countrycode(country,"country.name", "genc3c" ))

world_tb <- tibble(country=world_tb$country, numofdeaths=world_tb$numofdeaths)
world_tb






world_confirmed_tb <- fom_confirmed_cases %>%
  filter(date == max_date)
  #%>% 
  #mutate(iso_a3 =countrycode(country,"country.name", "genc3c" ))

world_confimed_tb <- tibble(country=world_confirmed_tb$country, numofcases=world_confirmed_tb$numofcases)
world_confimed_tb

#cnullcountries <- sum(is.na(world_confirmed_tb$iso_a3))
#cnullcountries



world_recovered_tb <- fom_recovery_cases %>% 
  filter(date == max_date) 
  #%>% 
  #mutate(iso_a3 =countrycode(country,"country.name", "genc3c" ))
world_recovered_tb <-  tibble(country=world_recovered_tb$country, numofrecoveries=world_recovered_tb$numofrecoveries)
world_recovered_tb

#alt_worlddata           
#alt_worlddata <- merge(alt_worlddata, world_tb, by="iso_a3")
alt_worlddata <- merge(world_tb, world_confimed_tb, by="country")
alt_worlddata <- merge(alt_worlddata, world_recovered_tb, by="country")


alt_worlddata <- alt_worlddata %>% 
  mutate(iso_a3 =countrycode(country,"country.name", "genc3c")) %>% 
  select(iso_a3,country,numofcases, numofdeaths,numofrecoveries)
  


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
                       borderWidth=5,
                       borderColor="#203644") %>% 
            hc_mapNavigation(enabled = TRUE) %>%
            hc_credits(enabled = TRUE, 
                       text = "Sources:Johns Hopkins University Center
                       for Systems Science and Engineering (JHU CCSE)", style = list(fontSize = "10px")) %>% 
            hc_plotOptions(series = list(showInLegend = FALSE)) %>% 
            hc_colorAxis(minColor = "#e6f7ff", maxColor = "#0099e6", showInLegend=FALSE )


pltcountry <-world_tb%>% 
  top_n(n=5, w=numofdeaths) 


top_country_deaths <- formatted_df %>% 
                   filter(country %in% c(pltcountry$country)) %>% 
                   rename(y=numofdeaths)
top_country_deaths <- as.data.frame(top_country_deaths)




hc_world_deaths <- highchart() %>% 
                   hc_xAxis(type = "datetime", dateTimeLabelFormats = list(day = '%d of %b'))%>%
                   hc_yAxis(title = list(text = "Deaths")) %>% 
                   hc_add_series(top_country_deaths,
                                 "spline",
                                 hcaes(y=top_country_deaths$y,x=top_country_deaths$date, group='country')
                                 ) %>% 
                   hc_tooltip(shared=TRUE, borderWidth=5) %>% 
                   hc_plotOptions(line = list(
                                              marker = list(
                                                lineWidth = 2,
                                                lineColor = NULL
                                              )            
                   ))
                   
                   
               
hc_world_deaths
?hc_plotOptions
?rnorm

hcmap() %>% 
  hc_add_series(data =alt_worlddata , type = "mapbubble",
                minSize = 0, maxSize = 30) %>% 
  hc_motion(enabled = TRUE, series = 1, labels = 1:n,
            loop = TRUE, autoPlay = TRUE, 
            updateInterval = 1000, magnet = list(step =  1)) %>% 
  hc_plotOptions(series = list(showInLegend = FALSE))


recoveryplotdata <- fom_recovery_cases %>%
                    filter(date == max_date) %>%
                    as_tibble()
recoveryplotdata
rpltcountry <- as.data.frame( recoveryplotdata) %>% 
               top_n(n=5, w=numofrecoveries) 

rpltcountry <- as_tibble(rpltcountry)
print(rpltcountry)

plotdata2 <- fom_recovery_cases %>%
              filter(country %in% c(rpltcountry$country)) %>% 
               rename(y=numofrecoveries)

print(plotdata2)
plotdata2 <- as.data.frame(plotdata2)


hc_world_rec <- highchart() %>% 
                hc_xAxis(type = "datetime", dateTimeLabelFormats = list(day = '%d of %b')) %>%
                hc_yAxis(title = list(text = "Recoveries")) %>% 
                hc_add_series(plotdata2, "line",hcaes(x=plotdata2$date,y=plotdata2$y, group='country')) %>% 
                hc_tooltip(shared=TRUE, borderWidth=5) %>% 
                hc_plotOptions(line = list(
                marker = list(
                lineWidth = 2,
                lineColor = NULL
               )            
               ))


hc_world_rec
