#Data organization for arctic data center

EZ = read.csv("Data/Sample_summary.csv")
A<-read.csv("Data/CB_RES.csv")
EZ<- EZ %>%
  rename(Temperature=Temp) 

A$Temperature<- as.character(A$Temperature)
A$Cadd<- as.character(A$Cadd)


Data<-EZ%>%
  full_join(A)

write.csv(Data, "Data/Cellobiose_Incubation.csv")
