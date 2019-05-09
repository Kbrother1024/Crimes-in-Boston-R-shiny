library(shiny)
library(dplyr)
library(ggplot2)
library(tidyr)
library(leaflet)
library(shinydashboard)
library(googleVis)
library(leaflet)
library(DT)
library(rgdal)
library(geojsonio)
library(shinythemes)
library(RColorBrewer)
library(shinyWidgets)
library(tm)
library(wordcloud)
library(memoise)
library(wordcloud2)
library(rsconnect)
library(htmltools)
library(dygraphs)
library(plotly)

# date_trend %>%
#   group_by(date, district) %>%
#   summarise(date_crime_count = n()) %>% 
#   ggplot(aes(x = date, y = date_crime_count)) +
#   geom_smooth(aes(color = district), se = F)

#read dataframe
df = read.csv('df.csv', stringsAsFactors = F)

#read geojson file
bostonDist <-
  geojsonio::geojson_read("Police_Districts.geojson", what = "sp")

#filter crime that do not have address
dist = df %>%
  filter(!is.na(long) &
           !is.na(lat) &
           !is.na(reporting_area) &
           district != 'Missing')

#filter date remove time
date_trend = df %>%
  mutate(date = as.Date(df$occurred_on_date, format = "%Y-%m-%d"))

#clean offense_code_group make it short for word cloud
dist_word = df %>%
  mutate(
    offense_code_group = gsub(
      pattern = 'Motor Vehicle Accident Response',
      replacement = "Motor Accident",
      x = offense_code_group
    )
  ) %>%
  mutate(
    offense_code_group = gsub(
      pattern = "HUMAN TRAFFICKING - INVOLUNTARY SERVITUDE",
      replacement = "INVOLUNTARY SERVITUDE",
      x = offense_code_group
    )
  ) %>%
  mutate(
    offense_code_group = gsub(
      pattern = "Burglary - No Property Taken",
      replacement = "Burglary NPT",
      x = offense_code_group
    )
  ) %>%
  mutate(offense_code_group = gsub(" ", "", x = offense_code_group, fixed = TRUE))
