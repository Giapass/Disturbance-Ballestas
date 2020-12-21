library(readr)
library(cowplot)
library(tidyr)
library(ggplot2)
library(dplyr)
library(patchwork)
# path_Data<-'./Data/Seabirds/'
# agro03_18<-read.csv2(paste0(path_Data,"CENSO DE AVES 2003 - 2019 OFICIAL.csv"),header=TRUE)
# names(agro03_18)
# agro03_18<-agro03_18%>%mutate(Date=as.POSIXct(strptime(paste(agro03_18$Ano,
#                         agro03_18$Mes,rep(15,nrow(agro03_18)),sep='-'),
#                         format='%Y-%m-%d')))
# target<-c('CHN','CHC','CHS','BA')
# MPA_south<-agro03_18%>%dplyr::filter(I.P %in% target)
# MPA_south<-MPA_south%>%transmute(Colony=I.P,Month=Mes,Year=Ano,Date,NBrC=GNR,
#                                  BrC=GR,EgC=GH,ChC=GP,JuC=GJ,
#                                  TotC=TOTAL.GUANAY.POR.ISLA,
#                                  NBrB=PNR,BrB=PR,EgB=PH,ChB=PP,JuB=PJ,
#                                  TotB=TOTAL.PIQUERO.POR.ISLA,
#                                  NBrP=ANR,BrP=AR,EgP=AH,ChP=AP,JuP=AJ,
#                                  TotP=TOTAL.ALCATRAZ.POR.ISLA)
# mariano00_02<- read.csv2(paste0(path_Data,'Data Mariano Valverde 2000-2002.csv'), header= TRUE)
# head(mariano00_02)
# mariano00_02<-mariano00_02%>%mutate(Date=as.POSIXct(strptime(paste(Ano,MES,rep(15,nrow(mariano00_02)),sep='-'),
#                                                              format='%Y-%m-%d')))
# target<-c('CHN','CHC','CHS','BA')
# MPA_south00<-mariano00_02%>%dplyr::filter(I.P %in% target)
# MPA_south00<-MPA_south00%>%transmute(Colony=I.P,Month=MES,Year=Ano,Date,NBrC=NA,
#                                      BrC=NA,EgC=NA,ChC=NA,JuC=NA,
#                                      TotC=Guanay,
#                                      NBrB=NA,BrB=NA,EgB=NA,ChB=NA,JuB=NA,
#                                      TotB=Piquero,
#                                      NBrP=NA,BrP=NA,EgP=NA,ChP=NA,JuP=NA,
#                                      TotP=Alcatraz)
# MPA_south<-rbind(MPA_south00,MPA_south)
# write.csv(MPA_south,paste0(path_Data,'MPA_south_seabirds.csv'),row.names = FALSE)
MPA_south<-read.csv(paste0(path_Data,'MPA_south_seabirds.csv'),header = TRUE)
head(MPA_south)
#Cormorant total
MPA_south<-MPA_south%>%mutate(Date=as.POSIXct(Date))
plot_1<-MPA_south%>%ggplot(aes(x=Date,y=TotC,group=Colony))+
  geom_line(aes(colour=Colony))+geom_point(aes(colour=Colony))+
  ggtitle("Cormorant total abundance")
#Booby total
plot_1<-MPA_south%>%ggplot(aes(x=Date,y=TotB,group=Colony))+
  geom_line(aes(colour=Colony))+geom_point(aes(colour=Colony))+
  ggtitle("Booby total abundance")
#Pelican total
plot_3<-MPA_south%>%ggplot(aes(x=Date,y=TotP,group=Colony))+
  geom_line(aes(colour=Colony))+geom_point(aes(colour=Colony))+
  ggtitle("Pelican total abundance")
#Cormorant Breeding
plot_4<-MPA_south%>%ggplot(aes(x=Date,y=BrC,group=Colony))+
  geom_line(aes(colour=Colony))+geom_point(aes(colour=Colony))+
  ggtitle("Cormorant breeder abundance")
#Booby Breeding
plot_5<-MPA_south%>%ggplot(aes(x=Date,y=BrB,group=Colony))+
  geom_line(aes(colour=Colony))+geom_point(aes(colour=Colony))+
  ggtitle("Booby breeder abundance")
#Pelican Breeding
plot_6<-MPA_south%>%ggplot(aes(x=Date,y=BrP,group=Colony))+
  geom_line(aes(colour=Colony))+geom_point(aes(colour=Colony))+
  ggtitle("Pelican breeder abundance")
(plot_1|plot_2|plot_3)/(plot_4| plot_5|plot_6)




