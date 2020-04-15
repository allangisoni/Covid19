rm(list = ls())
library(janitor)
library(lubridate)
library(tidyr)
library(dplyr)



df <- read.csv("data\\deaths.csv", header = TRUE, stringsAsFactors = FALSE)
print(df)


formatted_df <- df %>%
      gather(date, numofdeaths, 'X43852':'X43935', convert = TRUE) %>%
      mutate(date = sub("X", " ", date))  %>%
      mutate(date = as.numeric(date)) %>%
      mutate(date = as.Date(date, origin="1899-12-30")) %>%
     #filter(date >= "2020-04-14") %>%
      select(Country.Region, date, numofdeaths) %>%
      group_by(Country.Region, date) %>%
      summarize(numofdeaths = sum(numofdeaths))

formatted_df
summary(formatted_df)


