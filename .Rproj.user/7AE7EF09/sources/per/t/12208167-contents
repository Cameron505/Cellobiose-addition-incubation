
cbPalette <- c("#88CCEE", "#CC6677", "#DDCC77", "#117733", "#332288", "#AA4499", "#44AA99", "#999933", "#882255", "#661100", "#6699CC", "#888888")
theme_CKM <- function() {  # this for all the elements common across plots
  theme_bw() %+replace%
    theme(legend.text = element_text(size = 40),
          legend.title = element_text(size = 30),
          legend.key.size = unit(1.5, 'lines'),
          legend.background = element_rect(colour = NA),
          panel.border = element_rect(color="black",size=1.5, fill = NA),
          
          plot.title = element_text(hjust = 0, size = 50),
          axis.text = element_text(size = 30, color = "black"),
          axis.title = element_text(size = 40, face = "bold", color = "black"),
          
          panel.grid.major = element_blank(), 
          panel.grid.minor = element_blank(),
          axis.line = element_line(colour = "black"),
          
          # formatting for facets
          panel.background = element_blank(),
          strip.background = element_rect(colour="white", fill="white"), #facet formatting
          panel.spacing.x = unit(1.5, "lines"), #facet spacing for x axis
          panel.spacing.y = unit(1.5, "lines"), #facet spacing for x axis
          strip.text.x = element_text(size=40, face="bold"), #facet labels
          strip.text.y = element_text(size=40, face="bold", angle = 270) #facet labels
    )
}





EZ = read.csv("Data/Sample_summary.csv")


ET<-ggplot(EZ, aes(x=Temp, y=TFPA, fill=Cadd))+
  geom_bar(stat='identity',position='dodge')+
  ggtitle("")
  ylab(expression(paste("")))+
  xlab("")+
  ggtitle("")+
  scale_fill_manual(values=cbPalette)+
  theme_CKM()
