rm(list = ls())
library(janitor)
library(lubridate)
library(tidyr)
library(dplyr)
#library(ggplot2)
library(tidyverse)
library(plotly)
library(reshape2)
library(grid)
library(gridExtra)

options(scipen = 999)

#setwd("C:\\Users\\Allan\\OneDrive\\Documents\\Covid19\\")
getwd()

df <- read.csv("data\\deaths.csv", header = TRUE, stringsAsFactors = FALSE)
confirmed_cases <- read.csv("data\\covid19_confirmed.csv", header = TRUE, stringsAsFactors = FALSE)
recovery_cases <- read.csv("data\\covid19_recovered.csv", header = TRUE, stringsAsFactors = FALSE)
print(df)
print(confirmed_cases)


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

uniquecountries <-formatted_df%>%
                  filter( date == "2020-04-14") %>%
                  select(country, numofdeaths)

summary(plotdata)

plotdata$country <- as.character(plotdata$country)

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
#fig <- ggplotly(pt, tooltip = "text")
#fig




plotdata1 <- formatted_df%>%
  filter(numofdeaths >2000 & date >= "2020-03-14")

plotdata1$date <- ymd(plotdata1$date )
plotdata1$date <- as.Date(plotdata1$date, format = "%Y-%m-%d" )
minn <- min(plotdata1$date)
maxx <- max(plotdata1$date)


pt2 <- ggplot(plotdata1, aes(date, numofdeaths, col= country))+  
       geom_line(size=0.6, alpha=0.6) +
       geom_point()+
          scale_x_date(breaks = "2 days") +
          labs(title="Deaths by Date",
                      x= "Date",
                      y= "Num of Deaths",
                        #subtitle="", 
                      caption="source:  Johns Hopkins University Center for Systems Science and Engineering (JHU CCSE)") +
  
theme(axis.text.x = element_text(angle=85, vjust=0.5))

pt2

totalofdeaths  <- formatted_df %>%
                  filter(date == "2020-04-14")

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

covid_df <- cbind.data.frame(country=uniquecountries$country,  confirmed_cases=confirmedplotdata$numofcases,
                             deaths=uniquecountries$numofdeaths, recoveries=recoveryplotdata$numofrecoveries )
covid_df
