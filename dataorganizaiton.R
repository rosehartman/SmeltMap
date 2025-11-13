#get the data
library(tidyverse)
library(lubridate)
#l#ibrary(deltamapr)
library(sf)
library(ggmap)
library(readxl)
library(rsconnect)
library(httr)
library(readxl)
library(leaflet)
library(wql)

#WW_Delta = deltamapr::WW_Delta

#save(WW_Delta, file = "WW_Delta.RData")

smelt = read_excel("Running Delta Smelt Catch.xlsx", sheet = "Delta Smelt Catch Data")

names(smelt)

smelt2 = mutate(smelt, Year = year(SampleDate), Month = month(SampleDate), MonthYear = paste(Year, Month),
               BroodYear = case_when(LifeStage %in% c("Adult", "Adult*") & yday(SampleDate) <200 ~ Year-1,
                                     LifeStage == "Juvenile" & yday(SampleDate) <90 ~ Year-1,
                                     TRUE ~ Year)) %>%
  filter(!is.na(LatitudeStart)) %>%
  st_as_sf(coords = c("LongitudeStart", "LatitudeStart"), crs = 4326, remove = FALSE) 

#what are the life stage cut offs?
test = group_by(smelt, LifeStage) %>%
  summarize(minfl = min(as.numeric(ForkLength), na.rm =T), maxfl = max(as.numeric(ForkLength), na.rm =T))

smelt2 = mutate(smelt2, wildcultured = case_when(MarkCode == "None" ~ "Unmarked",
                                                 is.na(MarkCode) ~ "Unmarked",
                                                 MarkCode == "Unmarked - probably cultured" ~ "Unmarked - probably cultured",
                                                 TRUE ~ "Cultured"),
                LifeStage = case_when(is.na(LifeStage) & as.numeric(ForkLength) <21 ~ "Larva",
                                      is.na(LifeStage) & as.numeric(ForkLength) >21 & as.numeric(ForkLength) <58 ~ "Juvenile",
                                      TRUE ~ LifeStage),
                Salinity = ec2pss(SpecificConductance/1000, 25))

ggplot(smelt2, aes(x = Salinity, fill = as.factor(Month))) + geom_histogram()

ggplot(smelt2, aes(x = Turbidity_FNU, fill = as.factor(Month))) + geom_histogram()

ggplot(smelt2, aes(x = WaterTemp, fill = as.factor(Month))) + geom_histogram()



ggplot(smelt2)+
  geom_bar(aes(x = BroodYear, fill = wildcultured))+
  facet_wrap(~LifeStage)

smelt2 %>%
  st_drop_geometry() %>%
  group_by(BroodYear, wildcultured, LifeStage) %>%
  summarize(N = n())

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
data=filter(smelt2, SampleDate > ymd("2024-11-01"))

col = data$ReleaseEvent
pal <- colorFactor(
  palette = c("darkred",
              "yellow", "lightgreen", "purple", "blue", "orange", "tan", "grey"),
  levels =  sort(unique(data$ReleaseEvent))
)

map %>%
  addPolygons(data = WW_Delta, weight = .8, color = "grey", opacity =1) %>%
  addCircleMarkers(data=data, 
                   lng = data$LongitudeStart, lat = jitter(data$LatitudeStart),
                   radius =5, opacity =1, fillOpacity = 1, weight =1,
                   color = "black",
                   fillColor = pal(col),
                   stroke = TRUE) %>%
  addLegend(data = data, "bottomright", pal = pal, values = ~ReleaseEvent,
           title = "Release Event",
            opacity = 1
  )
unique(data$LifeStage)

#fiture out how to change icon shpe by value

deployApp()

ggplot()+
  geom_sf(data = WW_Delta)+
  geom_sf(data = data, aes(color = ReleaseEvent), size =2)+
coord_sf(xlim = c(-122.2, -121.5), ylim = c(37.8, 38.55))

ggplot(data,aes(x = SampleDate, fill = ReleaseEvent))+ geom_bar()
ggplot(data,aes(x =  ReleaseEvent))+ geom_bar()

library(wql)
smeltsal = filter(smelt2, LifeStage %in% c("Juvenile", "Adult")) %>%
  mutate(EC = as.numeric(SpecificConductance), Year = year(SampleDate), Month = month(SampleDate),
    Salinity = ec2pss(EC/1000, 25))  %>%
  filter(Month %in% c(6:11))

ggplot(smeltsal, aes(x = Salinity, y = as.numeric(ForkLength), color = as.factor(Year)))  +geom_point()

ggplot(smeltsal, aes(x = Salinity, fill = as.factor(Year))) + geom_histogram()

smeltsal = smeltsal %>%
  mutate(Count =1) %>%
  arrange(Salinity) %>%
  mutate(totsmelt = cumsum(Count))

smeltsal = mutate(smeltsal, percent = totsmelt/sum(smeltsal$Count))

ggplot(smeltsal, aes(x = Salinity, y = percent)) + geom_line()+
  geom_vline(xintercept = 4, linetype =2)+ geom_vline(xintercept =6, linetype =2)


ggplot()+
  geom_sf(data = WW_Delta)+
  geom_sf(data  =filter(smeltsal, Salinity >5.6), aes(color = as.factor(Year)))+
  geom_sf_text(data  =filter(smeltsal, Salinity >5.6), aes(label = SampleDate), 
               hjust =0, nudge_y = 0.01)+
  coord_sf(ylim = c(38, 38.2), xlim = c(-122.1, -121.8))

SDWSCsmelt = filter(smelt2, SubRegion %in% c("Sac DW Ship Channel", "Upper Sacramento River Ship Channel",
                                             "Lower Sacramento River Ship Channel"))
SDWSCcount = group_by(SDWSCsmelt, LifeStage, ID, wildcultured) %>%
  summarise(n = n())

SDWSCcount = SDWSCsmelt %>%
  mutate(Month = month(SampleDate)) %>%
  st_drop_geometry() %>%
  filter(is.na(ID) |ID != "Hypomensis sp.") %>%
  group_by(LifeStage, ID, wildcultured, Month) %>%
  summarise(n = n())
sum(SDWSCcount$n)
