library(ggplot2)
library(dplyr)
#Upload the data
data<-read.csv2('./Data/Tourism/Visit_Ballestas0820.csv',header=TRUE)
names(data)
data<-data%>%filter(Year>2011)
data$Month<-as.integer(data$Month)#convert the months in factors
data$Date<-as.POSIXct(strptime(paste(data$Year,data$Month,rep(1,nrow(data)),sep='-'),'%Y-%m-%d')) 
#Plots
plot(data$Date,data$N_touristMPA,type='b')
ggplot(data,mapping = aes(x=Date,y=N_touristMPA))+
  geom_line()+ geom_point()+geom_smooth(method='lm', se = FALSE)
ggplot(data,mapping = aes(x=Date,y=N_touristMPA,factor=Month))+
  geom_line()+ geom_point()+geom_smooth(method='lm')+
  facet_wrap(~Month)
ggplot(data,mapping = aes(x=Month,y=N_touristMPA))+
  geom_point()+geom_line()+
  facet_wrap(~Year)
#FIt a linear regression of the whole serie 
  fit= lm(N_touristMPA~1+seq(1:nrow(data)), data=data)
  summary(fit)
#Time series analysis
ts_data<-ts(data$N_touristMP, start=c(2011,1,1),end=c(2020,1,1),frequency = 12)
dec_data<-decompose(ts_data)
acf(data$N_touristMPA)#autocorrelation function
pacf(data$N_touristMPA)#partial autocorrelation function

