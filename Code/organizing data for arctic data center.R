
respiration_data$pre_inc<-as.character(respiration_data$pre_inc)
respiration_data$Inc_temp<-as.character(respiration_data$Inc_temp)
colnames(respiration_data)[3] ="ID"
SK<-sample_key %>%
  dplyr::rename(pre_inc=Pre.incubation,Inc_temp=Incubation.temperauture)
SK$pre_inc<-as.character(SK$pre_inc)
SK$Inc_temp<-as.character(SK$Inc_temp)


Data<-nutrients_data %>%
  full_join(respiration_data)%>%
  full_join(SK)




write.csv(Data, "Data/Antecedent_Temperature_Incubation.csv")
