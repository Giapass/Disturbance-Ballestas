library(dplyr)
path_Data<-'./Data/Fisheries/'
landings<-read.csv(paste0(path_Data,'Daily_landingsAnchovy0020MPA.csv'),header=TRUE)
landings$Date <- as.POSIXct(landings$Date)
landings%>%ggplot(aes(x=Date,y=Landing_tm))+
  geom_line(aes(color=Port))
