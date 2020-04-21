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
library(FSA)
library(rsconnect)

packageVersion("plotly")

options(scipen = 999)

setwd("C:\\Users\\Allan\\OneDrive\\Documents\\Covid19\\")
#setwd("C:\\Users\\85036758\\Documents\\Covid19\\")
getwd()

df <- read.csv("data\\deaths.csv", header = TRUE, stringsAsFactors = FALSE)
confirmed_cases <- read.csv("data\\confirmed.csv", header = TRUE, stringsAsFactors = FALSE)
recovery_cases <- read.csv("data\\recovered.csv", header = TRUE, stringsAsFactors = FALSE)
print(df)
print(confirmed_cases)


formatted_df <- df %>%
      gather(date, numofdeaths, 'X43852':'X43941', convert = TRUE) %>%
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
  gather(date, numofcases, 'X43852':'X43941', convert = TRUE) %>%
  mutate(date = sub("X", " ", date))  %>%
  rename(country = Country.Region)  %>%
  mutate(date = as.numeric(date)) %>%
  mutate(date = as.Date(date, origin="1899-12-30")) %>%
  select(country, date, numofcases) %>%
  group_by(country, date) %>%
  summarize(numofcases = sum(numofcases))


fom_recovery_cases<- recovery_cases %>%
  gather(date, numofrecoveries, 'X43852':'X43941', convert = TRUE) %>%
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

pltconfirmdata <- fom_confirmed_cases %>% 
              filter(date >= "2020-03-14") %>%
              group_by(date) %>% 
              summarise(numofconfirmedcases= sum(numofcases))
pltconfirmdata
  

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

covid_df <- covid_df %>% 
  rename(Country = country, Confirmed_Cases = confirmed_cases, Deaths=deaths,
         Recoveries=recoveries) %>% 
  mutate(Active =Confirmed_Cases-Deaths-Recoveries) %>% 
  filter(Country=="Kenya")
covid_df

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
ggplot(covid_df, aes(ymax=ymax, ymin=ymin, xmax=4, xmin=3, fill=category)) +
  geom_rect() +
  geom_label( x=3.5, aes(y=labelPosition, label=label), size=3) +
  scale_fill_brewer(palette="RdBu") +
  coord_polar(theta="y") +
  xlim(c(2, 4)) + theme_void()+ theme(legend.position = "none")

formatted_df1 <- formatted_df %>% 
               filter(country==c("Kenya", "US"))


dminn <-as.Date("2020-01-23", format = "%Y-%m-%d")
dmaxx <-as.Date("2020-04-20", format = "%Y-%m-%d")
pt9 <- ggplot(formatted_df1, aes(date, numofdeaths, text= paste0( "date:", ymd(date),
                                                                                      "<br>","country:", country,                                    
                                                                                      "<br>","deaths:", numofdeaths )))+  
  geom_line(size=0.6, alpha=0.6, aes(group=country)) +
  geom_dl(aes(label=country), method = "last.points")+
 # geom_point()+
  scale_x_date(breaks = "7 days",limits=c(dminn,dmaxx), labels = date_format("%b-%d")) +
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
pt9


formatted_df2 <- formatted_df %>% 
  filter(country == "Kenya") %>% 
  arrange(date) %>% 
  mutate(new_deaths = numofdeaths -lag(numofdeaths, default = first(numofdeaths))) 

test<- fom_confirmed_cases %>% 
  filter(date >= "2020-01-22") %>%
  group_by(date) %>% 
  summarise(numofconfirmedcases= sum(numofcases)) %>% 
  mutate(new_cases = numofconfirmedcases -lag(numofconfirmedcases, default = first(numofconfirmedcases))) 



slt_country <- as.data.frame(totalofdeaths) %>% 
              top_n(10)
              

slt_country <- as_tibble(slt_country)
#print(pltcountry)

?arrange
plotworlddeaths <- formatted_df %>%
                   filter(country %in% c(slt_country$country))%>% 
                   arrange(desc(date)) 
     
  #  filter(country == c(slt_country$country))


