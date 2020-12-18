library(stars)
library(ggplot2)
library(viridis)
library(gganimate)
library(lubridate)
library(raster)
library(tibble)
library(gifski)
path_data<-('./Data/Environmental/')
df2011<-read_ncdf(paste0(path_data,"ERA52011_10m.nc"))
df2011
st_get_dimension_values(df2011, "time")#get values of time dimension
df2011t<-st_set_dimensions(df2011, "time",
                       values=with_tz(st_get_dimension_values(df2011, "time"), tz = "America/Lima"),
                       names = "time2")
#-----------The whole Peru-------#
#df2011b<-df2011t %>% dplyr::select(u10,v10)#%>% adrop()
by_t = "1 day"
df2011_day<-aggregate(df2011t, by = by_t, FUN = mean) 
df2011_day$ws<-sqrt((df2011_day[[1]])^2+(df2011_day[[2]])^2)
st_get_dimension_values(df2011_day, "time")
pe <- getData('GADM', country='PE', level=0)#peru shapefile
pe_sf = st_as_sf(pe)
#Plot 3 days wind speed
ggplot() + geom_stars(data = df2011_day[3,1:3,,]) +# select bands
  geom_sf(data = pe_sf)  +
  coord_sf(xlim=c(-84,-70),ylim=c(-21,-3))+#xlim=c(-77.4,-76),ylim=c(-14.7,-12.7) area of influence
  scale_y_discrete(name = "Latitude", expand=c(0,0)) +
  scale_x_discrete(name = "Longitude",expand=c(0,0))+
  scale_fill_gradientn(colors = viridis::viridis(40))+
  theme_light() + 
  facet_wrap(~time)
#all the year day per day
g<-ggplot() + geom_stars(data = df2011_day[3,183:213,,])+ #Only July
  geom_sf(data = pe_sf)  +
  coord_sf(xlim=c(-84,-70),ylim=c(-21,-3))+
  scale_y_discrete(name = "Latitude", expand=c(0,0)) +
  scale_x_discrete(name = "Longitude",expand=c(0,0))+
  scale_fill_gradientn(colors = viridis::viridis(40))+
  theme_light()
#Animate?
pr<-g +labs(title = "{closest_state}")+transition_states(states = time)
animate(pr, renderer = gifski_renderer("ERA5_ws2011_July.gif"))
#-----------Area of influence----#
MPA_all<-read.csv2("Data/Maps/Area_MPA.csv", header=TRUE)
MPA_ballestas<-MPA_all%>%dplyr::filter(MPA=='Ballestas')
MPA_ballestas<-MPA_ballestas%>%dplyr::mutate_if(is.factor,as.character)%>%dplyr::mutate_if(is.character,as.numeric)
Ballestas_st<- st_as_sf(MPA_ballestas[,4:5], coords = c("Longitude", "Latitude"), crs = st_crs(df2011t))
bb_ballestas<-st_as_sfc(st_bbox(Ballestas_st))
area_b = st_sfc(st_buffer(st_centroid(bb_ballestas), 0.25), crs = st_crs(df2011t))
MPA_chincha<-MPA_all%>%dplyr::filter(MPA=='Chincha')
MPA_chincha<-MPA_chincha%>%dplyr::mutate_if(is.factor,as.character)%>%dplyr::mutate_if(is.character,as.numeric)
chincha_st<- st_as_sf(MPA_chincha[,4:5], coords = c("Longitude", "Latitude"), crs = st_crs(df2011t))
bb_chincha<-st_as_sfc(st_bbox(chincha_st))
#Module u and v
df2011_u<-df2011t[1][st_bbox(area_b)]
df2011_v<-df2011t[2][st_bbox(area_b)]
df2011_AOI<-c(df2011_u,df2011_v)
image(df2011_AOI[1,,,1])#just to check the subset
plot(bb_ballestas,col = NA, border = 'red', add = TRUE, lwd = 2)
plot(bb_chincha,col = NA, border = 'red', add = TRUE, lwd = 2)
#agregation time and space
df2011_AOI_area<-aggregate(df2011_AOI, by = area_b, FUN = mean) 
by_t = "1 day"
df2011_AOI_t<-aggregate(df2011_AOI_area, by = by_t , FUN = mean) 
df2011_AOI_t$ws<-sqrt((df2011_AOI_t[[1]])^2+(df2011_AOI_t[[2]])^2)
ts2011_AOI<-tibble(st_get_dimension_values(df2011_AOI_t, "time"),df2011_AOI_t$ws)
names(ts2011_AOI)<-c('time','ws')
ggplot()+geom_line(data=ts2011_AOI,aes(x=time, y=ws))
#-----------Extended area-#
area_b111= st_sfc(st_buffer(st_centroid(bb_ballestas), 1), crs = st_crs(df2011t))
bb_ballestas<-st_as_sfc(st_bbox(Ballestas_st))
bb_chincha<-st_as_sfc(st_bbox(chincha_st))
#Module u and v
df2011_u111<-df2011t[1][st_bbox(area_b111)]
df2011_v111<-df2011t[2][st_bbox(area_b111)]
df2011_AOI111<-c(df2011_u111,df2011_v111)
image(df2011_AOI111[1,,,1])#just to check the subset
plot(bb_ballestas,col = NA, border = 'red', add = TRUE, lwd = 2)
plot(bb_chincha,col = NA, border = 'red', add = TRUE, lwd = 2)
#agregation time and space
df2011_AOI111_area<-aggregate(df2011_AOI111, by = area_b111, FUN = mean) 
by_t = "1 day"
df2011_AOI111_t<-aggregate(df2011_AOI111_area, by = by_t , FUN = mean) 
df2011_AOI111_t$ws<-sqrt((df2011_AOI111_t[[1]])^2+(df2011_AOI_t[[2]])^2)
ts2011_AOI111<-tibble(st_get_dimension_values(df2011_AOI111_t, "time"),df2011_AOI111_t$ws)
names(ts2011_AOI111)<-c('time','ws')
ggplot()+geom_line(data=ts2011_AOI111,aes(x=time, y=ws))
