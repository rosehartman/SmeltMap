#summary of EDSM water quality

library(tidyverse)

edsm2 = read_csv("214edsmdailyreport24aug29_0.csv") %>%
  mutate(Date = mdy(SampleDate), Month = month(Date), 
         WaterTemp = WaterTempTop,
         Turbidity = TurbidityTop)
edsm = read_csv("41edsmdailyreport24sep06.csv")
str(edsm)
edsm = mutate(edsm, Date = mdy(SampleDate), Month = month(Date))

edsm3 = bind_rows(edsm, edsm2) %>%
  mutate(Stratum = case_when(Stratum == "Cache Slough LI" ~ "Cache Slough/LI",
                             TRUE ~ Stratum))

edsm3s = select(edsm3, SampleID, Month, Date, Stratum, Turbidity, WaterTemp) %>%
  distinct()

ggplot(edsm3s, aes(x = as.factor(Month), y = Turbidity))+
  geom_boxplot()+
  facet_wrap(~Stratum)+
  ylab("Turbidity (NTU)")+
  xlab("Month")+
  coord_cartesian(ylim = c(0,200))+
  geom_hline(yintercept = 12, linetype =2, color = "red")+
  theme_bw()+
  scale_x_discrete(labels = c("Apr", "May", "Jun", "Jul", "Aug", "Sep"))

ggsave("")

ggplot(edsm3s, aes(x = as.factor(Month), y = WaterTemp))+
  geom_boxplot()+
  facet_wrap(~Stratum)+
  ylab("Water Temperature (c)")+
  geom_hline(yintercept = 22, linetype =2, color = "red")+
  xlab("Month (2024)")+
  theme_bw()+
  scale_x_discrete(labels = c("Apr", "May", "Jun", "Jul", "Aug", "Sep"))
