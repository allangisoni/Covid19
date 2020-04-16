rm(list = ls())
library(janitor)
library(lubridate)
library(tidyr)
library(dplyr)
library(ggplot2)
library(plotly)

options(scipen = 999)

setwd("C:\\Users\\Allan\\OneDrive\\Documents\\Covid19\\")

df <- read.csv("data\\deaths.csv", header = TRUE, stringsAsFactors = FALSE)
print(df)


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

plotdata <- formatted_df%>%
            filter(numofdeaths >3000  & date == "2020-04-14")

plotdata1 <- formatted_df%>%
  filter(numofdeaths >1000 & date >= "2020-03-14")

pt <- ggplot(plotdata, aes(country, numofdeaths))
pt + geom_bar(stat = "identity", width=.3, fill="tomato3") +
     labs(title="Deaths per Country", 
          x= "Country",
          y= "Num of Deaths",
          #subtitle="", 
         caption="source:  Johns Hopkins University Center for Systems Science and Engineering (JHU CCSE)") + 
         theme(axis.text.x = element_text(angle=65, vjust=0.6))


pt2 <- ggplot(plotdata1, aes(date, numofdeaths))
pt2 + geom_line(aes(col= country)) +
  labs(title="Deaths by Date",
       x= "Date",
       y= "Num of Deaths",
       #subtitle="", 
       caption="source:  Johns Hopkins University Center for Systems Science and Engineering (JHU CCSE)") +
      # scale_x_date(date_labels = "%b/%d")
       scale_x_date(date_minor_breaks = "1 day") + 
  theme(axis.text.x = element_text(angle=65, vjust=0.6))

#fig <- ggplotly(pt2)
#fig



