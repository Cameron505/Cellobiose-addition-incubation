## Enzyme analysis

source("code/0-packages.R")
#source("code/fticrrr/a-functions_processing.R")


EZ = read.csv("Data/Sample_summary.csv")
EZ<- data.frame(EZ[1:80,1:4],EZ[1:80,19:23])

EZ2<- melt(EZ, id.vars=c("Harvest.ID","Date","Cadd","Temp"))
EZ2$Temp<- factor(EZ2$Temp, levels= c("-10","-6","-2","2","6"))

EZERROR = EZ2 %>%
  group_by(Temp, variable) %>%
  summarize(mean_value = mean(value, na.rm = TRUE),
            se = sqrt(var(value, na.rm = TRUE) / sum(!is.na(value)))) %>%
  ungroup()

ggplot(EZERROR,aes(x = Temp, y = mean_value, fill=Temp))+
  geom_bar(stat = "identity")+
  geom_errorbar(aes(ymin = mean_value - se, ymax = mean_value + se), lwd = 1, width = 1, color="black")+
  facet_wrap(~ variable, ncol = 3,scales = "free")+
  scale_fill_manual(values=cbPalette)+
  theme_kp()
ggsave("Graphs/cellobiose_Enzymes.png", device= "png")

EZCARBON = EZERROR %>%
  filter(variable== "BG" | variable== "CBH")

ggplot(EZCARBON,aes(x = Temp, y = mean_value, fill=Temp))+
  geom_bar(stat = "identity")+
  geom_errorbar(aes(ymin = mean_value - se, ymax = mean_value + se), lwd = 0.8, width = 0.7, color="black")+
  facet_wrap(~ variable, ncol = 2,scales = "free")+
  scale_fill_manual(values=cbPalette)+
  ylab("Enzyme activity per g dry soil")+
  theme_kp()
ggsave("Graphs/Carbon_Enzymes.png")

EZNUT = EZERROR %>%
  filter(variable== "LAP" | variable== "NAG" |variable== "PHOS")

ggplot(EZNUT,aes(x = Temp, y = mean_value, fill=Temp))+
  geom_bar(stat = "identity")+
  geom_errorbar(aes(ymin = mean_value - se, ymax = mean_value + se), lwd = 1, width = 1, color="black")+
  facet_wrap(~ variable, ncol = 3,scales = "free")+
  scale_fill_manual(values=cbPalette)+
  theme_kp()
