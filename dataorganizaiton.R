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

save(smelt2, file = "Smelt2.RData")
ggplot(smelt2, aes(x = Salinity, fill = as.factor(Month))) + geom_histogram()

ggplot(smelt2, aes(x = Turbidity_FNU, fill = as.factor(Month))) + geom_histogram()

ggplot(smelt2, aes(x = WaterTemp, fill = as.factor(Month))) + geom_histogram()


ggplot(filter(smelt2, BroodYear == 2025), aes(x = Salinity, fill = as.factor(Month))) + geom_histogram()


ggplot(filter(smelt2, BroodYear == 2025), aes(x = SampleDate, y = Salinity)) + geom_point()

ggplot(filter(smelt2, BroodYear == 2025, SampleDate > ymd("2025-11-19"))) + geom_sf(data = WW_Delta)+
  geom_sf()+ coord_sf(xlim = c(-122.2, -121.9), ylim = c(38.1, 38.2))

#fiture out how to change icon shpe by value

deployApp()
# 
# library(ggspatial)
# ggplot()+
#   geom_sf(data = WW_Delta)+
#     geom_sf_label(data = data, aes(label = paste(month(SampleDate), mday(SampleDate), sep ="/")), nudge_x = 0.02,
#                   nudge_y = c(0,0,-0.010, 0,0,0))+
#   geom_sf(data = data, aes(fill = LifeStage), size =3, shape = 21, color = "black")+
# coord_sf(xlim = c(-122.1, -121.75), ylim = c(38, 38.3))+
#   annotation_scale()+
#   annotation_north_arrow(location = "tl")+
#   theme_bw()+
#   theme(legend.position = "inside", legend.position.inside = c(.9,.9,.9,.9),
#         legend.background = element_rect(color = "black"))+ ylab(NULL)+ xlab(NULL)
# 
# ggsave("SummerFallSmelt2025.tiff", width = 5, height =6)
# 
# ggplot(data,aes(x = SampleDate, fill = ReleaseEvent))+ geom_bar()
# 
# 
# 
# 
# library(wql)
# smeltsal = filter(smelt2, LifeStage %in% c("Juvenile", "Adult")) %>%
#   mutate(EC = as.numeric(SpecificConductance), Year = year(SampleDate), Month = month(SampleDate),
#     Salinity = ec2pss(EC/1000, 25))  %>%
#   filter(Month %in% c(6:11))
# 
# ggplot(smeltsal, aes(x = Salinity, y = as.numeric(ForkLength), color = as.factor(Year)))  +geom_point()
# 
# ggplot(smeltsal, aes(x = Salinity, fill = as.factor(Year))) + geom_histogram()
# 
# smeltsal = smeltsal %>%
#   mutate(Count =1) %>%
#   arrange(Salinity) %>%
#   mutate(totsmelt = cumsum(Count))
# 
# smeltsal = mutate(smeltsal, percent = totsmelt/sum(smeltsal$Count))
# 
# ggplot(smeltsal, aes(x = Salinity, y = percent)) + geom_line()+
#   geom_vline(xintercept = 4, linetype =2)+ geom_vline(xintercept =6, linetype =2)
# 
# 
# ggplot()+
#   geom_sf(data = WW_Delta)+
#   geom_sf(data  =filter(smeltsal, Salinity >5.6), aes(color = as.factor(Year)))+
#   geom_sf_text(data  =filter(smeltsal, Salinity >5.6), aes(label = SampleDate), 
#                hjust =0, nudge_y = 0.01)+
#   coord_sf(ylim = c(38, 38.2), xlim = c(-122.1, -121.8))
# 
# SDWSCsmelt = filter(smelt2, SubRegion %in% c("Sac DW Ship Channel", "Upper Sacramento River Ship Channel",
#                                              "Lower Sacramento River Ship Channel"))
# SDWSCcount = group_by(SDWSCsmelt, LifeStage, ID, wildcultured) %>%
#   summarise(n = n())
# 
# SDWSCcount = SDWSCsmelt %>%
#   mutate(Month = month(SampleDate)) %>%
#   st_drop_geometry() %>%
#   filter(is.na(ID) |ID != "Hypomensis sp.") %>%
#   group_by(LifeStage, ID, wildcultured, Month) %>%
#   summarise(n = n())
# sum(SDWSCcount$n)
