source("code/0-packages.R")
#pracma
A<-read.csv("Data/CB_RES.csv")
A$Date<-as.Date(A$Date,"%m/%d/%Y")
cbPalette <- c("#000000", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")
A$JD <- strftime(A$Date, format = "%j")
A$JD<-as.numeric(A$JD)
A$Res.22<-A$Average*24     #change from per hour to per day
A$JD2<-A$JD-178
CUM<-A %>% 
  group_by(Cadd,Temperature,Sample) %>%
  summarize(val=cumtrapz(JD2,Res.22),D=JD2)

CUM2<-A %>% 
  group_by(Cadd,Temperature,Sample) %>%
  summarize(val=trapz(JD2,Res.22))

CUM2$TCR<-CUM2$val*12.005 #Multiply by average dry mass
CUM$TCR<-CUM$val*12.005







OQN<-CUM[CUM$Temperature<=0,]
OQP<-CUM[CUM$Temperature>=0,]
OQN$Cadd<-as.factor(OQN$Cadd)
OQP$Cadd<-as.factor(OQP$Cadd)
CUM$Cadd<-as.factor(CUM$Cadd)

CUMNEG<-ggplot(OQN, aes(x=D, y=TCR, color= OQN$Cadd))+
  stat_summary(fun = mean,geom = "point",lwd=4) +
  stat_summary(fun.data = mean_se, geom = "errorbar", lwd=1, width = 0.5)+
  facet_wrap(~Temperature)+
  theme_light()+
  scale_colour_manual(values=cbPalette)+
  scale_fill_manual(values=cbPalette)+
  ylab(expression(paste( " ",mu,"g-C")))+
  xlab("incubation day")+
  labs(color="C-addition")+
  theme(legend.direction = 'vertical', 
        legend.key = element_rect(size=2),
        legend.key.size = unit(3, 'lines'),
        panel.grid.major = element_blank(), 
        legend.title=element_text(size=40,colour='black'),
        panel.grid.minor = element_blank(),
        legend.text=element_text(size=35),
        axis.title.y= element_text(size=40, colour='black'),
        axis.text.y = element_text(size=40, colour='black'),
        axis.ticks = element_line(colour = "black", size=2),
        axis.ticks.length = unit(0.3, "cm"),
        axis.title.x=element_text(size=40, colour='black', vjust=-0.1),
        axis.text.x = element_text(size=40, colour='black', angle=45, vjust= 0.5),
        panel.border = element_rect(colour = "black", fill=NA, size=2),
        plot.title = element_text( size=40, face="bold.italic"),
        plot.margin=unit(c(1,1,1,1), "cm"))+
  ggtitle("Soil Respiration (below freezing)")

CUMPOS<-ggplot(OQP, aes(x=D, y=TCR, color=Cadd))+
  stat_summary(fun.y = mean,geom = "point",lwd=4) +
  stat_summary(fun.data = mean_se, geom = "errorbar", lwd=1, width = 0.5)+
  facet_wrap(~Temperature)+
  theme_light()+
  scale_colour_manual(values=cbPalette)+
  scale_fill_manual(values=cbPalette)+
  ylab(expression(paste( " ",mu,"g-C")))+
  xlab("incubation day")+
  labs(color="C-addition")+
  theme(legend.direction = 'vertical', 
        legend.key = element_rect(size=2),
        legend.key.size = unit(3, 'lines'),
        panel.grid.major = element_blank(), 
        legend.title=element_text(size=40,colour='black'),
        panel.grid.minor = element_blank(),
        legend.text=element_text(size=35),
        axis.title.y= element_text(size=40, colour='black'),
        axis.text.y = element_text(size=40, colour='black'),
        axis.ticks = element_line(colour = "black", size=2),
        axis.ticks.length = unit(0.3, "cm"),
        axis.title.x=element_text(size=40, colour='black', vjust=-0.1),
        axis.text.x = element_text(size=40, colour='black', angle=45, vjust= 0.5),
        panel.border = element_rect(colour = "black", fill=NA, size=2),
        plot.title = element_text( size=40, face="bold.italic"),
        plot.margin=unit(c(1,1,1,1), "cm"))+
  ggtitle("Soil Respiration (above freezing)")

setwd("C:\\Users\\Cameron M\\OneDrive - University of Toledo\\R working folder\\Projects\\Cellulose addition\\Res")
ggsave('cellobiose addition C-respiration Negative.png', plot=CUMNEG, width= 20, height= 12)
ggsave('cellobiose addition C-respiration posotive.png', plot=CUMPOS, width= 20, height= 12)


















CUM<-ggplot(CUM, aes(x=D, y=TCR, color=Cadd))+
  stat_summary(fun.y = mean,geom = "point",lwd=3) +
  stat_summary(fun.data = mean_se, geom = "errorbar", lwd=1, width = 0.5)+
  facet_wrap(~Temperature)+
  theme_light()+
  scale_colour_manual(values=cbPalette)+
  scale_fill_manual(values=cbPalette)+
  ylab(expression(paste( " ",mu,"g-C")))+
  xlab("Date")+
  theme(legend.direction = 'vertical', 
        legend.key = element_rect(size=2),
        legend.key.size = unit(3, 'lines'),
        panel.grid.major = element_blank(), 
        legend.title=element_text(size=30,colour='black'),
        panel.grid.minor = element_blank(),
        legend.text=element_text(size=25),
        axis.title.y= element_text(size=30, colour='black'),
        axis.text.y = element_text(size=20, colour='black'),
        axis.ticks = element_line(colour = "black", size=2),
        axis.ticks.length = unit(0.3, "cm"),
        axis.title.x=element_text(size=30, colour='black'),
        axis.text.x = element_text(size=20, colour='black'),
        panel.border = element_rect(colour = "black", fill=NA, size=2),
        plot.title = element_text( size=30, face="bold.italic"),
        plot.margin=unit(c(1,1,1,1), "cm"))+
  ggtitle("Soil Respiration")

setwd("C:/Users/Cameron M/University of Toledo/Weintraub, Michael - ESE Lab Stuff/Grad Students/Cameron/Experiments/Cellulose addition")

ggsave('cellulose addition C-respiration.png', plot=CUM, width= 20, height= 12)




```