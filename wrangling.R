library(shiny)
library(rvest)
library(lubridate)
library(stringr)
library(tidyverse)
library(dplyr)
library(ggplot2)
library(ggthemes)
library(shinythemes)


#######beginning of wrangling
url <- "https://en.wikipedia.org/wiki/List_of_post-election_Donald_Trump_rallies"
#extract table and remove day of week
h <- read_html(url)
tab <- h %>% html_nodes("table") %>% .[8]
tab <- tab %>% html_table() %>% .[[1]]
test <- tab$`Date of rally`
test <- test %>% str_replace("Monday, |Tuesday, |Wednesday, |Thursday, |Friday, |Saturday, |Sunday, ", "")

#next problem is the [###] at the end of the dates.
#all rows have at least one, so remove it
test <- test %>% substr(1, nchar(test) - 5)

pattern = nchar(test) > 20
test[pattern] <- test[pattern] %>% substr(1, nchar(test[pattern]) -5)
pattern = nchar(test) > 20
test[pattern] <- test[pattern] %>% substr(1, nchar(test[pattern]) -5)
pattern = nchar(test) > 20
test[pattern] <- test[pattern] %>% substr(1, nchar(test[pattern]) -5)
pattern = nchar(test) > 20
test[pattern] <- test[pattern] %>% substr(1, nchar(test[pattern]) -5)


#convert to dates
test <- mdy(test)
test <- test[1:67]
tab$`Date of rally` <- test

#get city and covid data
cities <- read.csv("data/uscities.csv")
covid <- read.csv("https://raw.githubusercontent.com/nytimes/covid-19-data/master/us-counties.csv")
covid$county[covid$county=="Newport News city"] <- "Newport News"

#combine rally data and city data 
new_test <- left_join(tab, cities, by=c("State" ="state_id", "City" = "city"))
#three  didn't match due to being townships not real cities
new_test[60, 9] <- "Macomb"
new_test[52, 9] <- "Oakland"
new_test[18, 9] <- "Beaver"

new_test <- new_test %>% select(c("Date of rally", "City", "State", "county_name", "population", "state_name")) %>% rename(date = "Date of rally")
covid$date <- ymd(covid$date)
new_test$date <- ymd(new_test$date)

#adding state names to fix NAs
new_test[60,6] <- "Michigan"
new_test[52,6] <- "Michigan"
new_test[18,6] <- "Pennsylvania"

#adjacent county data
adjacent <- read.table("https://www2.census.gov/geo/docs/reference/county_adjacency.txt", sep="\t", fill=FALSE, strip.white=TRUE)[,c(1,3)]
adjacent <- adjacent %>% rename(county=V1,adj_counties=V3)
adjacent$adj_counties[adjacent$adj_counties == "Do\xb1a Ana County, NM"] <- "Dona Ana County, NM"
adjacent$county[adjacent$county == "Do\xb1a Ana County, NM"] <- "Dona Ana County, NM"

#adjacent <- adjacent[-c(12459, 12460, 12461, 12462, 12463, 21751, 21752, 21753, 21754, 21755),]
adjacent <- adjacent[1:21721,]


for (i in (1:nrow(adjacent))){
  if (i != (nrow(adjacent))){
    if (nchar(adjacent$county[i+1])==0){
      adjacent$county[i+1]<-adjacent$county[i]
    }
  }
}

#adjacent <- adjacent[-c(12511),]
#adjacent <- adjacent[-c(12528),]
#adjacent$V3 <- adjacent$V3[!(adjacent$V3=="Do<b1>a Ana County, NM")]

adjacent$county_name <- substr(adjacent$county, 1, nchar(adjacent$county)-11)
adjacent$county_state <- substr(adjacent$county, nchar(adjacent$county)-2, nchar(adjacent$county))
adjacent$adj_county_name <- substr(adjacent$adj_counties, 1, nchar(adjacent$adj_counties)-11)
adjacent$adj_county_state <- substr(adjacent$adj_counties, nchar(adjacent$adj_counties)-1, nchar(adjacent$adj_counties))
adjacent[adjacent=="Cars"] <- "Carson City"
adjacent[adjacent=="Newport News city"] <- "Newport News"
adjacent[adjacent=="Newport Ne"] <- "Newport News"

#state abbreviations data set
state_abs <- read_csv("data/state_abbrevs.csv")
adjacent <- left_join(adjacent, state_abs, by=c("adj_county_state" = "Code"))

#census data with county populations
county_pop <- read_csv("data/county-pop.csv")
county_pop <- county_pop %>% select(c("STNAME", "CTYNAME", "POPESTIMATE2019")) %>% 
  rename("state"="STNAME", "county"="CTYNAME", "population"="POPESTIMATE2019")
county_pop$county[1835]<-"Dona Ana County"
county_pop$county <- substr(county_pop$county, 1, nchar(county_pop$county)-7)
county_pop$county[county_pop$county=="Cars"]<- "Carson City"
county_pop$county[county_pop$county=="Newport Ne"] <- "Newport News"
covid <- left_join(covid, county_pop, by=c("county", "state"))
covid <- covid %>% mutate(cases_per_thous = cases*1000 / population)

new_test <- new_test %>% mutate(city_state = paste(City, State, sep=", "))


######## end wrangling