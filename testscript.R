library(tmap)
library(countrycode)
library(leaflet)
library(tidyr)
library(dplyr)
data("World")
#?World



setwd("C:\\Users\\Allan\\OneDrive\\Documents\\Covid-19 S\\")
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




  world_df <- df %>%
    gather(date, numofdeaths, 'X43852':'X43969', convert = TRUE) %>%
    mutate(date = sub("X", " ", date))  %>%
    rename(country = Country.Region)  %>%
    mutate(date = as.numeric(date)) %>%
    mutate(date = as.Date(date, origin="1899-12-30")) %>%
    select(country, date, numofdeaths) %>%

    group_by(country, date) %>%
    summarize(numofdeaths = sum(numofdeaths))
  
  world_df 
  summary(world_df)
  
  
  world_tb <- world_df %>% 
    filter(date == "2020-05-18") %>% 
    mutate(iso_a3 =countrycode(country,"country.name", "genc3c" ))
  
  world_tb <- tibble(iso_a3=world_df$iso_a3, numofdeaths=world_df$numofdeaths)
  world_tb
  
  nullcountries <- sum(is.na(world_df$iso_a3))
  nullcountries
  
  
  
  
  world_confirmed<- confirmed_cases %>%
    gather(date, numofcases, 'X43852':'X43969', convert = TRUE) %>%
    mutate(date = sub("X", " ", date))  %>%
    rename(country = Country.Region)  %>%
    mutate(date = as.numeric(date)) %>%
    mutate(date = as.Date(date, origin="1899-12-30")) %>%
    select(country, date, numofcases) %>%
    group_by(country, date) %>%
    summarize(numofcases = sum(numofcases))
  
  summary(world_confirmed) 
  
  
  world_confirmed_tb <- world_confirmed %>%
                       filter(date == "2020-05-18") %>% 
                       mutate(iso_a3 =countrycode(country,"country.name", "genc3c" ))
  
  world_confimed_tb <- tibble(iso_a3=world_confirmed$iso_a3, numofcases=world_confirmed$numofcases)
  world_confimed_tb
  
  cnullcountries <- sum(is.na(world_confirmed$iso_a3))
  cnullcountries
  
 
  world_recovered <- recovery_cases %>%
    gather(date, numofrecoveries, 'X43852':'X43969', convert = TRUE) %>%
    mutate(date = sub("X", " ", date))  %>%
    rename(country = Country.Region)  %>%
    mutate(date = as.numeric(date)) %>%
    mutate(date = as.Date(date, origin="1899-12-30")) %>%
    select(country, date, numofrecoveries) %>%
    group_by(country, date) %>%
    summarize(numofrecoveries = sum(numofrecoveries))
  
  world_recovered 
  
  world_recovered_tb <- world_recovered %>% 
                        filter(date == "2020-05-18") %>% 
                        mutate(iso_a3 =countrycode(country,"country.name", "genc3c" ))
  world_recovered_tb <-  tibble(iso_a3=world_recovered_tb$iso_a3, numofrecoveries=world_recovered_tb$numofrecoveries)
  world_recovered_tb
  
  alt_worlddata <- World           
  alt_worlddata <- merge(alt_worlddata, world_tb, by="iso_a3")
  alt_worlddata <- merge(alt_worlddata, world_confimed_tb, by="iso_a3")
  alt_worlddata <- merge(alt_worlddata, world_recovered_tb, by="iso_a3")
  
  #alt_worlddata$new_continent <- sapply( alt_worlddata$name, switch,"Brazil"="South America", "Kenya" ="Africa") 
  
  alt_worlddata <- alt_worlddata %>% 
                   select(iso_a3,name,area,continent,numofcases, numofdeaths,numofrecoveries)
    


tmap_mode("view")
tmap_style("white")
tmap_options(bg.color= "#203644")

map1 <- tm_shape(alt_worlddata) +
  tm_polygons(col = "gray41", id="name") +
  tm_shape(alt_worlddata) +
  tm_bubbles("numofcases", col="firebrick3", id="name", size =0.3, scale =1.5, popup.vars=c( "Confirmed"="numofcases",
                                                                                             "Deaths" ="numofdeaths",
                                                                                             "Recoveries"= "numofrecoveries"))+
  tm_borders("white", lwd = .5)+
  tm_scale_bar()+
  #tm_text("new_continent", size = "area")+
  tm_format("World")+
  tm_layout(bg.color = "#203644",outer.bg.color ="#203644" , inner.margins = c(0, .20, .0, .60)) 

map1
lf <- tmap_leaflet(map1) %>% 
      add_markers(2.2945, 48.8582, popup = "Eiffel tower")




