#get the data
library(tidyverse)
library(lubridate)
library(deltamapr)
library(sf)
library(ggmap)
library(readxl)
library(rsconnect)
library(httr)
library(readxl)
library(leaflet)



smelt = read_excel("Running Delta Smelt Catch_2023-09-05.xlsx", sheet = "Delta Smelt Catch Data")

names(smelt)

smelt2 = mutate(smelt, Year = year(SampleDate), Month = month(SampleDate), MonthYear = paste(Year, Month),
               BroodYear = case_when(LifeStage %in% c("Adult", "Adult*") & yday(SampleDate) <200 ~ Year-1,
                                     LifeStage == "Juvenile" & yday(SampleDate) <90 ~ Year-1,
                                     TRUE ~ Year)) %>%
  filter(!is.na(LatitudeStart)) %>%
  st_as_sf(coords = c("LongitudeStart", "LatitudeStart"), crs = 4326, remove = FALSE) 

smelt2 = mutate(smelt2, wildcultured = case_when(MarkCode == "None" ~ "Unmarked",
                                                 TRUE ~ "Cultured"))

ggplot(smelt2)+
  geom_sf(data = WW_Delta)+
  geom_sf(aes(color = LifeStage))+
  facet_wrap(~BroodYear)

pal <- colorFactor(
  palette = c("darkred", "yellow", "lightgreen", "orange"),
  domain = smelt2$LifeStage
)


pal2 <- colorFactor(
  palette = c("darkred", "yellow"),
  levels = c("Cultured", "Unmarked")
)

map <- leaflet() %>%
  addProviderTiles(providers$OpenStreetMap)


smelt3 = filter(smelt2, MethodCode == "Salvage")

map %>%
  addPolygons(data = WW_Delta, weight = .8, color = "grey", opacity =1) %>%
  addCircleMarkers(data=smelt2, lng = smelt2$LongitudeStart, lat = smelt2$LatitudeStart,
                   label = ~ReleaseEvent, radius =5, opacity =0, fillOpacity = 1,
                   fillColor = ~pal2(wildcultured), stroke = TRUE, color = "black") %>%
  addLegend(data = smelt2, "bottomright", pal = pal2, values = ~wildcultured,
            title = "Lifestage",
            opacity = 1
  )
ggplot(smelt3, aes(x = Survey, fill = LifeStage))+ geom_bar()+ylab("Number of Fish")+
  xlab("Salvage Facility")+ theme_bw()+scale_y_continuous(breaks = seq(0,nrow(smelt3), by =2))

save(smelt2, file = "Smelt2.RData")

smelt2$LifeStage = as.factor(smelt2$LifeStage)
data=filter(smelt2, SampleDate > ymd("2024-06-01"))

col = data$LifeStage
pal <- colorFactor(
  palette = c("darkred",
              "yellow", "lightgreen"),
  levels =  sort(unique(smelt2$LifeStage))
)

map %>%
  addPolygons(data = WW_Delta, weight = .8, color = "grey", opacity =1) %>%
  addCircleMarkers(data=data, 
                   lng = data$LongitudeStart, lat = data$LatitudeStart,
                   radius =5, opacity =1, fillOpacity = 1, weight =1,
                   color = "black",
                   fillColor = pal(col),
                   stroke = TRUE) #%>%
 # addLegend(data = smelt2, "bottomright", pal = pal, values = ~LifeStage,
 #           title = "Lifestage",
#            opacity = 1
 # )
unique(data$LifeStage)
