library(sf)
library(ncdf4)
library(tidyverse)
library(lubridate)
library(viridis)
#Read
path_data<-('./Data/Environmental/')
nc <- nc_open(paste0(path_data,"ERA52011_10m.nc"))
#extract lon and lat
lat <- ncvar_get(nc,'latitude')
lon <- ncvar_get(nc,'longitude')
dim(lat);dim(lon)
lat <- ncvar_get(nc,'latitude')
lon <- ncvar_get(nc,'longitude')
dim(lat);dim(lon)
#extract the time
t <- ncvar_get(nc, "time")
#time unit: hours since 1900-01-01
ncatt_get(nc,'time')
#convert the hours into date + hour
#as_datetime() function of the lubridate package needs seconds
timestamp <- as_datetime(c(t*60*60),origin="1900-01-01")
#temperatures in K from july 2018
head(timestamp)
#import the data
data_u <- ncvar_get(nc,"u10")
data_v <- ncvar_get(nc,"u10")
filled.contour(data_u[,,1])
#To select the point closer to the islands
#create all the combinations of lon-lat
lonlat <- expand.grid(lon=lon,lat=lat)
#we must convert the coordinates in a spatial object sf
#we also indicate the coordinate system in EPSG code
coord <- st_as_sf(lonlat,coords=c("lon","lat"))%>%
  st_set_crs(4326)
#we do the same with our coordinate of Madrid
psj <- st_point(c(-3.691944,40.418889))%>%
  st_sfc()%>%
  st_set_crs(4326)
#plot all points
plot(st_geometry(coord))
plot(psj,add=TRUE,pch = 3, col = 'red')
#add the distance to the points
coord <- mutate(coord,dist=st_distance(coord,psj))
#create a distance matrix with the same dimensions as our data
dist_mat <- matrix(coord$dist,dim(data)[-3])
#the arrayInd function is useful to obtain the row and column indexes
mat_index <- as.vector(arrayInd(which.min(dist_mat), dim(dist_mat)))
#we extract the time serie and change the unit from K to ºC
#we convert the time in date + hour
df <- data.frame(ta=data[mat_index[1],mat_index[2],],time=timestamp)%>%
  mutate(ta=ta-273.15,time=ymd_hms(time))
#Visualize time series
ggplot(df,aes(time,ta))+geom_line()+
  labs(y="Temperature (ºC)",x="")+ theme_bw()

#2 OPTION Convert to dataframe
#Fron Netcdf to dataframe
data<-tibble(name=attributes(nc$var)$names) %>%
  bind_cols(map_df(.$name,ncatt_get,nc=nc)) %>%
  mutate(values=map(name,ncvar_get,nc=nc))
#Wind speed for every pixel
newMetric<-data_frame(name='ws10',units='m s**-1',long_name='Wind Speed',
                      values=list(sqrt(data$values[data$name=='u10'][[1]]^2+data$values[data$name=='v10'][[1]]^2)))
data<-bind_rows(data,newMetric)
#create an empty dataframe
df<-expand.grid(lon=lon,lat=lat,timestamp=timestamp,name=data$name) %>%
  mutate(coord=factor(paste(lon,lat,'/')))
#pull the data field in 2 dimensions
df<-data %>%
  pull(values) %>%
  unlist() %>%
  as_tibble() %>%
  bind_cols(df)
head(df)
#
df %>%filter(between(lat,-21,-3) & between(lon,-84,-70)) %>%
  left_join(select(data,name,long_name),'name') %>%
  ggplot(aes(timestamp,value,color=coord))+
  geom_line()+
  facet_wrap(~long_name,ncol=1,scales='free')+
  theme_minimal()+
  theme(legend.position = 'bottom')+
  scale_color_viridis(discrete = T)


#close the conection with the ncdf file
nc_close(nc)