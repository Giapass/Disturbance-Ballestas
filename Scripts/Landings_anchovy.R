path_Data<-'./Data/Fisheries/'
#Data from 2000 to 2014
datadeb00_14 <- read.csv2("Data/Fisheries/datadeb00_14.csv", header=TRUE)
datadeb00_14<- datadeb00_14%>%mutate_if(is.factor, as.character)%>%mutate_if(is.character, as.numeric)
datadeb00_14$Date <- as.POSIXct(strptime(paste(datadeb00_14$Day,datadeb00_14$Month,datadeb00_14$Year,sep='/'), format='%d/%m/%Y'), tz = "UTC")
MPA_ports<-c('TAMBODMORA','PISCO')
deb00_14_MPA<-datadeb00_14%>%dplyr::filter(Year>=2011)%>%select(-TOTAL)%>%
  gather(Port, Landing_tm,-Date,-Year, -Month,-Day)%>%dplyr::filter(Port %in% MPA_ports)
#Plot daily MPA
deb00_14_MPA%>%ggplot(aes(x=Date,y=Landing_tm))+
  geom_line(aes(color=Port))

#Data from 2015 to 2020
datadeb15_20<- read.csv2("Data/Fisheries/IndustrialAnchovetaSardinaFisheryJuvenile.csv", 
                         header=TRUE)
str(datadeb15_20)
datadeb15_20$Date<-as.POSIXct(strptime(datadeb15_20$Date,'%d.%m.%Y'), tz = "UTC")
datadeb15_20ANC<-datadeb15_20%>%mutate(Landings=as.numeric(as.character(Value)))%>%
                 filter(Species.Name=='ANCHOVETA')%>%rename(Type=Type.of.Fishingvessel)%>%
                 select(-Value)%>%spread(Parameter.Name,Landings)%>%
                 rename(N_Vessel=`NÂ°Emb`,N_samplingVessel=`NÂ°Emb.muestr.`,
                        Landings_tm=`Desemb. (t)`,perc_Juveniles=`% juveniles`)
write.csv(datadeb15_20ANC,'Daily_landingsAnchovy1520.csv')
datadeb15_20ANC$Month<-as.factor(format(datadeb15_20ANC$Date,'%m'))
datadeb15_20ANC$Year<-as.factor(format(datadeb15_20ANC$Date,'%Y'))
deb15_20_TAMBO<-datadeb15_20ANC%>%transmute(Date=Date,Port=as.character(Port.Name),
                                          Landing_tm=Landings_tm,Type=Type)%>%filter(Port=="T. de Mora")%>%
                                          group_by(Port,Date)%>%summarize(Landings_tm=sum(Landing_tm))
deb15_20_PISCO<-datadeb15_20ANC%>%transmute(Date=Date,Port=as.character(Port.Name),
                                            Landing_tm=Landings_tm,Type=Type)%>%filter(Port=="Pisco")%>%
  group_by(Port,Date)%>%summarize(Landings_tm=sum(Landing_tm))
date_complete<-data.frame(as.POSIXct(seq.Date(as.Date('2015-04-01'), by = 'day', len = 1920),tz = "UTC"))
names(date_complete)<-'Date'
deb15_20_TAMBO <- merge(x = date_complete, y = deb15_20_TAMBO, all.x = TRUE)
deb15_20_PISCO <- merge(x = date_complete, y = deb15_20_PISCO, all.x = TRUE)
deb15_20_TAMBO <-deb15_20_TAMBO%>%mutate(Port="T. de Mora",Landings_tm=if_else(is.na(Landings_tm), 0, Landings_tm))
deb15_20_PISCO <-deb15_20_PISCO%>%mutate(Port="Pisco",Landings_tm=if_else(is.na(Landings_tm), 0, Landings_tm))
deb15_20_MPA<-rbind(deb15_20_TAMBO,deb15_20_PISCO)
deb15_20_MPA$Port[deb15_20_MPA$Port == "T. de Mora"] <- "TAMBODMORA"
deb15_20_MPA$Port[deb15_20_MPA$Port == "Pisco"] <- "PISCO"
deb15_20_MPA<-deb15_20_MPA%>%transmute(Date,Year=as.integer(format(Date,'%Y')),
                         Month=as.integer(format(Date,'%m')),
                         Day=as.integer(format(Date,'%d')),
                         Port,Landing_tm=Landings_tm)
deb15_20_MPA%>%ggplot(aes(x=Date,y=Landing_tm))+
  geom_line(aes(color=Port))
deb00_20_MPA<-rbind(deb00_14_MPA,deb15_20_MPA)
write.csv(deb00_20_MPA,'Daily_landingsAnchovy1520MPA.csv')

datadeb15_20ANCM<-datadeb15_20ANC%>%group_by(Year,Month,Port.Name)%>%
  summarise(Monthly_landings=sum(Landings_tm,na.rm =TRUE),Monthly_Vessel=sum(N_samplingVessel,na.rm =TRUE))%>%
  mutate(Date=as.POSIXct(strptime(paste(rep(1,length(Monthly_Vessel)),Month,Year,sep='/'),'%d/%m/%Y')))
write.csv(datadeb15_20ANCM,'Monthly_landingsAnchovy1520.csv')

datadeb15_20ANCM%>%ggplot(aes(x=Date,y=Monthly_landings))+
  geom_line(aes(color=Port.Name))
Month_all<-datadeb15_20ANC%>%group_by(Year,Month)%>%
  summarise(Monthly_landings=sum(Landings_tm,na.rm =TRUE),Monthly_Vessel=sum(N_samplingVessel,na.rm =TRUE),
            Mean_Juveniles=mean(perc_Juveniles,na.rm =TRUE))%>%
  mutate(Date=as.POSIXct(strptime(paste(rep(1,length(Monthly_Vessel)),Month,Year,sep='/'),'%d/%m/%Y')))


plot(Month_all$Date,Month_all$Monthly_landings,type='b',ylab="",yaxt="n")
axis(side=2)
legend("topright", legend=c("Landings", "Perc_juveniles"),
       col=c(1, 2), lty=1:2, cex=0.8)
par(new = TRUE)
plot(Month_all$Date,Month_all$Mean_Juveniles,type='b',col=2,ylab="",yaxt="n")
axis(side=4)
datadeb15_20ANC%>%ggplot(aes(x=Date,y=Landings_tm))+
  geom_line(aes(color=Port.Name))+facet_wrap(~Type)
datadeb15_20ANC%>%ggplot(aes(x=Date,y=perc_Juveniles))+
  geom_line(aes(color=Port.Name))+facet_wrap(~Type)
datadeb15_20ANC%>%ggplot(aes(x=Date,y=N_Vessel))+
  geom_line(aes(color=Port.Name))+facet_wrap(~Type)

datadeb15_20SAR<-datadeb15_20%>%mutate(Landings=as.numeric(as.character(Value)))%>%
  filter(Species.Name=='SARDINA')%>%rename(Type=Type.of.Fishingvessel)%>%
  select(-Value)%>%spread(Parameter.Name,Landings)%>%
  rename(N_Vessel=`NÂ°Emb`,N_samplingVessel=`NÂ°Emb.muestr.`,
         Landings_tm=`Desemb. (t)`,perc_Juveniles=`% juveniles`)