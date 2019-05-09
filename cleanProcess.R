library(shiny)
library(dplyr)
library(ggplot2)
library(tidyr)
library(leaflet)
library(shinydashboard)
library(googleVis)
library(leaflet)
library(DT)


#Load Data
df = read.csv('crime.csv')

#check NA
sapply(df, function(x) sum(is.na(x)))

#Data clean
#Shooting
df = df %>% 
  mutate(SHOOTING = ifelse(SHOOTING == 'Y', '1', '0'))

#Change District factor
levels(df$DISTRICT) = c('Missing',"A1","A15","A7","B2","B3","C11","C6","D14","D4","E13","E18","E5")

#columns to lower case
names(df) = sapply(names(df), tolower)

#write df in to new csv file
write.csv(df, file = 'df.csv')
