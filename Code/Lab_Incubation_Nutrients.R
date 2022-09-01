library(ggplot2)
library(doBy)
library(plotrix)
library(interactions)
library(ggthemes)

#read in the data
incubation.data <- read.csv("laboratory_incubation_nutrients.csv")

#calculate summary stats for all response variables
incubation.data.summary <- summaryBy(data=incubation.data, TN+MBC+MBN+NH4+NO3+PO4+TFPA+TRS~Time+Temp+Cadd, 
                                     FUN= c(length, mean, std.error))

#make Cadd a factor for plotting purposes
incubation.data.summary$Cadd <- as.factor(incubation.data.summary$Cadd)

#set the order for the x-axes
incubation.data.summary$Temp <- factor(incubation.data.summary$Temp, levels=c("Pre", "-10", "-6", "-2", "2", "6"))

#calculate means of pre-treatment nutrients
pretreatment.nutrients <- subset(incubation.data, Time=="Pre")
NO3.pre <- mean(pretreatment.nutrients$NO3)
NH4.pre <- mean(pretreatment.nutrients$NH4)
TFPA.pre <- mean(pretreatment.nutrients$TFPA)

#plot the effects of temp and labile C addition for each response variable
ggplot(data=subset(incubation.data.summary, Time=="Post"), aes(x=Cadd, y=NO3.mean, color=Temp))+
  geom_point(size=3.0)+
  geom_errorbar(aes(ymin=NO3.mean-NO3.std.error, ymax=NO3.mean+NO3.std.error), width = 0.25)+
  facet_wrap(~Temp)+
  theme_gdocs()+
  theme(plot.background=element_blank())+
  scale_color_colorblind()+
  xlab("Labile C Addition (mg C/g dry soil)")+
  ylab(expression('NO'[3]^{"-"}* ~'Availability (µg N/g dry soil)'))+
  theme(strip.text.x = element_text(size = 12, color = "black", face = "bold", hjust=0.5))+
  theme(legend.position="none")

ggplot(data=subset(incubation.data.summary, Time=="Post"), aes(x=Cadd, y=NH4.mean, color=Temp))+
  geom_point(size=3.0)+
  geom_errorbar(aes(ymin=NH4.mean-NH4.std.error, ymax=NH4.mean+NH4.std.error), width = 0.25)+
  facet_wrap(~Temp)+
  theme_gdocs()+
  theme(plot.background=element_blank())+
  scale_color_colorblind()+
  xlab("Labile C Addition (mg C/g dry soil)")+
  ylab(expression('NH'[4]^{"+"}* ~'Availability (µg N/g dry soil)'))+
  theme(strip.text.x = element_text(size = 12, color = "black", face = "bold", hjust=0.5))+
  theme(legend.position="none")

ggplot(data=subset(incubation.data.summary, Time=="Post"), aes(x=Cadd, y=TN.mean, color=Temp))+
  geom_point(size=3.0)+
  geom_errorbar(aes(ymin=TN.mean-TN.std.error, ymax=TN.mean+TN.std.error), width = 0.25)+
  facet_wrap(~Temp)+
  theme_gdocs()+
  theme(plot.background=element_blank())+
  scale_color_colorblind()+
  xlab("Labile C Addition (mg C/g dry soil)")+
  ylab("Total N Availability (µg N/g dry soil)")+
  theme(strip.text.x = element_text(size = 12, color = "black", face = "bold", hjust=0.5))+
  theme(legend.position="none")

ggplot(data=subset(incubation.data.summary, Time=="Post"), aes(x=Cadd, y=PO4.mean, color=Temp))+
  geom_point(size=3.0)+
  geom_errorbar(aes(ymin=PO4.mean-PO4.std.error, ymax=PO4.mean+PO4.std.error), width = 0.25)+
  facet_wrap(~Temp)+
  theme_gdocs()+
  theme(plot.background=element_blank())+
  scale_color_colorblind()+
  xlab("Labile C Addition (mg C/g dry soil)")+
  ylab("Orthophosphate-P Availability (µg P/g dry soil)")+
  theme(strip.text.x = element_text(size = 12, color = "black", face = "bold", hjust=0.5))+
  theme(legend.position="none")

ggplot(data=subset(incubation.data.summary, Time=="Post"), aes(x=Cadd, y=TFPA.mean, color=Temp))+
  geom_point(size=3.0)+
  geom_errorbar(aes(ymin=TFPA.mean-TFPA.std.error, ymax=TFPA.mean+TFPA.std.error), width = 0.25)+
  facet_wrap(~Temp)+
  theme_gdocs()+
  theme(plot.background=element_blank())+
  scale_color_colorblind()+
  xlab("Labile C Addition (mg C/g dry soil)")+
  ylab("TFPA Availability (nmol Leucine Equiv./g dry soil)")+
  theme(strip.text.x = element_text(size = 12, color = "black", face = "bold", hjust=0.5))+
  theme(legend.position="none")

ggplot(data=subset(incubation.data.summary, Time=="Post"), aes(x=Cadd, y=MBC.mean, color=Temp))+
  geom_point(size=3.0)+
  geom_errorbar(aes(ymin=MBC.mean-MBC.std.error, ymax=MBC.mean+MBC.std.error), width = 0.25)+
  facet_wrap(~Temp)+
  theme_gdocs()+
  theme(plot.background=element_blank())+
  scale_color_colorblind()+
  xlab("Labile C Addition (mg C/g dry soil)")+
  ylab("Microbial Biomass C (µg C/g dry soil)")+
  theme(strip.text.x = element_text(size = 12, color = "black", face = "bold", hjust=0.5))+
  theme(legend.position="none")
  
ggplot(data=subset(incubation.data.summary, Time=="Post"), aes(x=Cadd, y=MBN.mean, color=Temp))+
  geom_point(size=3.0)+
  geom_errorbar(aes(ymin=MBN.mean-MBN.std.error, ymax=MBN.mean+MBN.std.error), width = 0.25)+
  facet_wrap(~Temp)+
  theme_gdocs()+
  theme(plot.background=element_blank())+
  scale_color_colorblind()+
  xlab("Labile C Addition (mg C/g dry soil)")+
  ylab("Microbial Biomass N (µg N/g dry soil)")+
  theme(strip.text.x = element_text(size = 12, color = "black", face = "bold", hjust=0.5))+
  theme(legend.position="none")

ggplot(data=subset(incubation.data.summary, Time=="Post"), aes(x=Cadd, y=TRS.mean, color=Temp))+
  geom_point(size=3.0)+
  geom_errorbar(aes(ymin=TRS.mean-TRS.std.error, ymax=TRS.mean+TRS.std.error), width = 0.25)+
  facet_wrap(~Temp)+
  theme_gdocs()+
  theme(plot.background=element_blank())+
  scale_color_colorblind()+
  xlab("Labile C Addition (mg C/g dry soil)")+
  ylab("Total Reducing Sugars (mg sugar Equiv./g dry soil)")+
  theme(strip.text.x = element_text(size = 12, color = "black", face = "bold", hjust=0.5))+
  theme(legend.position="none")

#Models: Effects of C addition and temperature for each response variable
incubation.data.Cadd.Temp <- subset(incubation.data, Time=="Post")
incubation.data.Cadd.Temp$Time <- droplevels(incubation.data.Cadd.Temp$Time)
incubation.data.Cadd.Temp$Temp <- as.numeric(levels(incubation.data.Cadd.Temp$Temp))[incubation.data.Cadd.Temp$Temp]


NO3.model <- lm(data=incubation.data.Cadd.Temp, NO3~ Temp + Cadd + Temp*Cadd)
summary(NO3.model)
interact_plot(model = NO3.model, pred = Cadd, modx = Temp, modx.values = c(-10, -6, -2, 2, 6), 
              colors= c("#000000","#E69F00","#56B4E9","#009E73","#F0E442"), interval=TRUE)+
  geom_hline(yintercept=NO3.pre, color="darkred", size=1.0)+
  theme_gdocs()+
  theme(plot.background=element_blank())+
  theme(legend.position="top", legend.direction = "horizontal")+
  xlab("Labile C Addition (mg C/g dry soil)")+
  ylab(expression('NO'[3]^{"-"}* ~'Availability (µg N/g dry soil)'))

NO3.model.sub.zero <- lm(data=subset(incubation.data.Cadd.Temp, Temp<0), NO3~ Temp + Cadd + Temp*Cadd)
summary(NO3.model.sub.zero)
interact_plot(model = NO3.model.sub.zero, pred = Cadd, modx = Temp, modx.values = c(-10, -6, -2), 
              colors= c("#000000","#E69F00","#56B4E9"), interval=TRUE)+
  geom_hline(yintercept=NO3.pre, color="darkred", size=1.0)+
  theme_gdocs()+
  theme(plot.background=element_blank())+
  theme(legend.position="top", legend.direction = "horizontal")+
  xlab("Labile C Addition (mg C/g dry soil)")+
  ylab(expression('NO'[3]^{"-"}* ~'Availability (µg N/g dry soil)'))


NH4.model <- lm(data=incubation.data.Cadd.Temp, NH4~ Temp + Cadd + Temp*Cadd)
summary(NH4.model)
interact_plot(model = NH4.model, pred = Cadd, modx = Temp, modx.values = c(-10, -6, -2, 2, 6), 
              colors= c("#000000","#E69F00","#56B4E9","#009E73","#F0E442"), interval=TRUE)+
  geom_hline(yintercept=NH4.pre, color="darkred", size=1.0)+
  theme_gdocs()+
  theme(plot.background=element_blank())+
  theme(legend.position="top", legend.direction = "horizontal")+
  xlab("Labile C Addition (mg C/g dry soil)")+
  ylab(expression('NH'[4]^{"+"}* ~'Availability (µg N/g dry soil)'))

NH4.model.sub.zero <- lm(data=subset(incubation.data.Cadd.Temp, Temp<0), NH4~ Temp + Cadd + Temp*Cadd)
summary(NH4.model.sub.zero)
interact_plot(model = NH4.model.sub.zero, pred = Cadd, modx = Temp, modx.values = c(-10, -6, -2), 
              colors= c("#000000","#E69F00","#56B4E9"), interval=TRUE)+
  geom_hline(yintercept=NH4.pre, color="darkred", size=1.0)+
  theme_gdocs()+
  theme(plot.background=element_blank())+
  theme(legend.position="top", legend.direction = "horizontal")+
  xlab("Labile C Addition (mg C/g dry soil)")+
  ylab(expression('NH'[4]^{"+"}* ~'Availability (µg N/g dry soil)')) 


TFPA.model <- lm(data=incubation.data.Cadd.Temp, TFPA~ Temp + Cadd + Temp*Cadd)
summary(TFPA.model)
interact_plot(model = TFPA.model, pred = Cadd, modx = Temp, modx.values = c(-10, -6, -2, 2, 6), 
              colors= c("#000000","#E69F00","#56B4E9","#009E73","#F0E442"), interval=TRUE)+
  geom_hline(yintercept=TFPA.pre, color="darkred", size=1.0)+
  theme_gdocs()+
  theme(plot.background=element_blank())+
  theme(legend.position="top", legend.direction = "horizontal")+
  xlab("Labile C Addition (mg C/g dry soil)")+
  ylab("TFPA Availability (nmol Leucine Equiv./g dry soil)")

TFPA.model.sub.zero <- lm(data=subset(incubation.data.Cadd.Temp, Temp<0), TFPA~ Temp + Cadd + Temp*Cadd)
summary(TFPA.model.sub.zero)
interact_plot(model = TFPA.model.sub.zero, pred = Cadd, modx = Temp, modx.values = c(-10, -6, -2), 
              colors= c("#000000","#E69F00","#56B4E9"), interval=TRUE)+
  geom_hline(yintercept=TFPA.pre, color="darkred", size=1.0)+
  theme_gdocs()+
  theme(plot.background=element_blank())+
  theme(legend.position="top", legend.direction = "horizontal")+
  xlab("Labile C Addition (mg C/g dry soil)")+
  ylab("TFPA Availability (nmol Leucine Equiv./g dry soil)")  