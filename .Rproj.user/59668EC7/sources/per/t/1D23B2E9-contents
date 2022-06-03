GC<- read.csv("data/Weintraub_GCMS.csv")
GC2<- read.csv("data/Weintraub_GCMS_Short.csv")


GC2$Temp<- factor(GC2$Temp, levels= c("-10","-6","-2","2","6","Pre"))

GCCB = GC2 %>%
  group_by(Temp, Cadd) %>%
  summarize(mean_value = mean(Cellobiose, na.rm = TRUE),
            se = sqrt(var(Cellobiose, na.rm = TRUE) / sum(!is.na(Cellobiose)))) %>%
  ungroup()

ggplot(GC2,aes(x = Temp, y = Cellobiose, fill=Temp))+
  stat_summary(fun=mean, geom="bar")+
  stat_summary(geom="errorbar")+
  scale_fill_manual(values=cbPalette)+
  ylab("Relative Cellobiose concentrations")+
  xlab("incubation temperature")+
  theme_kp()+
  theme(axis.text.y = element_blank())
ggsave("Graphs/cellobiose_relative amount.png", device= "png")

ggplot(GC2,aes(x = Temp, y = D.glucose, fill=Temp))+
  stat_summary(fun=mean, geom="bar")+
  stat_summary(geom="errorbar")+
  scale_fill_manual(values=cbPalette)+
  ylab("Relative Glucose concentrations")+
  xlab("incubation temperature")+
  theme_kp()+
  theme(axis.text.y = element_blank())


ggplot(GC2,aes(x = Temp, y = D.arabinose, fill=Temp))+
  stat_summary(fun=mean, geom="bar")+
  stat_summary(geom="errorbar")+
  scale_fill_manual(values=cbPalette)+
  ylab("Relative Arabinose concentrations")+
  xlab("incubation temperature")+
  theme_kp()+
  theme(axis.text.y = element_blank())


ggplot(GC2,aes(x = Temp, y = N.acetyl.D.glucosamine, fill=Temp))+
  stat_summary(fun=mean, geom="bar")+
  stat_summary(geom="errorbar")+
  scale_fill_manual(values=cbPalette)+
  ylab("Relative N-acetyl glucosamine concentrations")+
  xlab("incubation temperature")+
  theme_kp()+
  theme(axis.text.y = element_blank())

ggplot(GC2,aes(x = Temp, y = L.leucine, fill=Temp))+
  stat_summary(fun=mean, geom="bar")+
  stat_summary(geom="errorbar")+
  scale_fill_manual(values=cbPalette)+
  ylab("Relative L.leucine concentrations")+
  xlab("incubation temperature")+
  theme_kp()+
  theme(axis.text.y = element_blank())

ggplot(GC2,aes(x = Temp, y = D.xylose, fill=Temp))+
  stat_summary(fun=mean, geom="bar")+
  stat_summary(geom="errorbar")+
  scale_fill_manual(values=cbPalette)+
  ylab("Relative Xylose concentrations")+
  xlab("incubation temperature")+
  theme_kp()+
  theme(axis.text.y = element_blank())
