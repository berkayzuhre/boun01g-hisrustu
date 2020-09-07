library(tidyverse)
library(lubridate)
library(dplyr)
library(cluster)
library(factoextra)
library(sp) 
library(openxlsx)

library(tseries)
library(forecast)
library(readxl)
library(data.table)

library(ggplot2)
library(car)

evds = read.csv("https://raw.githubusercontent.com/pjournal/boun01g-hisrustu/gh-pages/housing_data_EVDS.csv")

mapPositions = read.xlsx("https://raw.githubusercontent.com/pjournal/boun01g-hisrustu/gh-pages/tr.xlsx")

TUR <- readRDS("TUR_1_sp.rds")
TUR_fixed <- fortify(TUR)

evds[c(90:100),c(1:5)]



names_and_numbers <- tibble(id=rownames(TUR@data),
                            city=TUR@data$NAME_1) %>% 
  left_join(mapPositions, by ="city")

final_map <- left_join(TUR_fixed, names_and_numbers, by = "id")

convertName<-function(x,y){
  x <- gsub(".*- (.+) -.*", "\\1", x)
  x <- gsub("\u008d", "i", x)
  x <- gsub("€", "C", x)
  x <- gsub("ž", "S", x)
  x <- gsub("Ÿ", "s", x)
  x <- gsub("\u0081", "u", x)
  x <- gsub("˜", "I", x)
  x <- gsub("§", "g", x)
  x <- gsub("”", "o", x)
  x <- paste(y, x, sep="_")
  return(x)
}
datainfo = evds[c(92:1080),]
cityNames<-evds[c(95:258),2]
firstHandCityNames<-cityNames[c(1:82)]
secondHandCityNames<-cityNames[c(83:164)]
evds = evds[c(1:91),]
firstHandCityNames<-convertName(firstHandCityNames,"F")
secondHandCityNames<-convertName(secondHandCityNames,"S")
allNames= c("Tarih",firstHandCityNames,secondHandCityNames)
names(evds)<-allNames

for(i in c(1:165)){
  evds[,i] <- gsub(",","",evds[,i])
}
evds[, c(2:165)] <- sapply(evds[, c(2:165)], as.numeric)
names(evds)[1] = "Date"

evds$Date = paste(evds$Date,"01",sep="-")
evds = evds %>%
  mutate(Date=as.Date(Date, format="%Y-%m-%d")) %>%
  mutate(Year=year(Date), Month=month(Date))

find_season <- function(mon){
  season = NULL
  
  if(between(mon, 3, 5)){
    season = "Spring"
  } else if(between(mon, 6, 8)){
    season = "Summer"
  } else if(between(mon, 9, 11)){
    season = "Fall"
  } else{
    season = "Winter"
  }
  
  return(season)
}
evds$Season = sapply(evds$Month, find_season)

names(evds)

evds[,c(2)] - rowSums(evds[,c(3:83)], na.rm = FALSE)
evds[,c(2)] - rowSums(evds[,c(3:83)], na.rm = TRUE)
evds[is.na(evds)] <- 0

View(evds)