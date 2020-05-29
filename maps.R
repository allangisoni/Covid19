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

world_tb <- tibble(country=world_df$country, numofdeaths=world_tb$numofdeaths)
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
  select(country,numofcases, numofdeaths,numofrecoveries) 


highchart() %>% 
            hc_chart(backgroundColor="#203644") %>% 
            hc_add_series_map(worldgeojson,
                              alt_worlddata,  
                              value = "numofcases", 
                              joinBy = c("name", "country"), name="Country",
                              showInLegend=FALSE) %>% 
            hc_tooltip(useHTML=TRUE,
                       headerFormat="",
                       pointFormat="<b>{point.name}</b><br>
                                    Cases: {point.numofcases}<br>
                                    Deaths:{point.numofdeaths}<br>
                                    Recoveries:{point.numofrecoveries}",
                       borderWidth=5) %>% 
            hc_plotOptions(series = list(showInLegend = FALSE)) %>% 
            hc_colorAxis(minColor = "#595959", maxColor = "#404040", showInLegend=FALSE )

hcmap() %>% 
  hc_add_series(data =alt_worlddata , type = "mapbubble",
                minSize = 0, maxSize = 30) %>% 
  hc_motion(enabled = TRUE, series = 1, labels = 1:n,
            loop = TRUE, autoPlay = TRUE, 
            updateInterval = 1000, magnet = list(step =  1)) %>% 
  hc_plotOptions(series = list(showInLegend = FALSE))

