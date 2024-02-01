#get the data
library(tidyverse)
library(lubridate)
library(deltamapr)
library(sf)
library(ggmap)
library(readxl)

library(httr)
library(readxl)

smelt = read_excel("Running Delta Smelt Catch_2023-09-05.xlsx", sheet = "Delta Smelt Catch Data")

names(smelt)

smelt2 = mutate(smelt, Year = year(SampleDate),
               BroodYear = case_when(LifeStage %in% c("Adult", "Adult*") & yday(SampleDate) <200 ~ Year,
                                     TRUE ~ Year)) %>%
  st_as_sf(coords = c("LongitudeStart", "LatitudeStart"), crs = 4326, remove = FALSE) 

ggplot(smelt2)+
  geom_sf(data = WW_Delta)+
  geom_sf(aes(color = LifeStage))+
  facet_wrap(~BroodYear)

map <- leaflet() %>%
  addProviderTiles(providers$OpenStreetMap)
map %>%
  addPolygons(data = WW_Delta, weight = .8, color = "grey", opacity =1) %>%
  addCircleMarkers(data=smelt2, lng = smelt2$LongitudeStart, lat = smelt2$LatitudeStart,
                   label = ~ReleaseEvent, radius =5, opacity =0, fillOpacity = .8,
                   fillColor = ifelse(test = smelt2$LifeStage == "Adult",  # if this...
                     yes = "red",  
                     no = ifelse(
                       test = smelt2$LifeStage == "Juvenile",  
                       yes = "blue",  
                       no = "yellow"  
                     )))

save(smelt2, file = "Smelt2.RData")
