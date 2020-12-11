# First, we should load the packages that we will use. Use install.packages() to   
# download and install the new version of rWind (v1.0.2)
library(rWind)
library(fields)  
library(shape)  
library(rworldmap)  
library(lubridate) 
library(rworldxtra)
library(raster)

library("rnaturalearth") # mapas del mundo
library("rnaturalearthdata") # mapas del mundo 
library(stars) # read rasters
library(plotly) # interactive graphs
library(gganimate) # animations
library(htmlwidgets) # guardar gráfico interactivo en html
library(transformr) # para que funcione la animación del objeto espacial

path_data<-'./Data/'
# Now, we use lubridate package to create a sequence of dates/times (each three  
# hours)  
years<-seq(2012,2013,1)
for(i in 1:years){
 dt <- seq(ymd_hms(paste(years[i],1,1,21,00,00, sep="-"), tz = "America/Lima"),  
          ymd_hms(paste(years[i],12,31,18,00,00, sep="-"), tz = "America/Lima"),by="3 hours")#time
 wind_series <- wind.dl_2(dt,-84,-70,-21,-3)  #wind.dl_2 to download the whole time series
 wind_series_layer <- wind2raster(wind_series)  #wind2raster function adapted to work with this lists of wind
 saveRDS(wind_series_layer,file = paste0(path_data,'Environmental/',paste('Wind_', years[i],'.rds',sep = "")))
}
dt_649[mat < 0.1] <- NA
#Read year by year
Wind_raster<-readRDS(paste0(path_data,'Environmental/',paste('Wind_','2012','.rds',sep = "")))
dir_h<-sapply(Wind_raster, '[[', 1)#read speed
speed_h<-sapply(Wind_raster, '[[', 2)#read speed

for(i in seq(1,length(speed_h),8)){
  averageWind0<- windAver(speed.data=speed_h[c(i:i+7)],dir.data=dir_h[c(i:i+7)])
  avSpeed0<-st_as_stars(averageWind0[[1]]$layer)
  assign(paste('d',i,sep='_'),avSpeed0)
}
#d_649[[1]]<-ifelse(d_649[[1]]>0.1,NA,d_649[[1]])#only 2012
new<-c(d_1,d_9,d_17,d_25,d_33,d_41,d_49,d_57,d_65,d_73,d_81,d_89,d_97,d_105,
       d_113,d_121,d_129,d_137,d_145,d_153,d_161,d_169,d_177,d_185,d_193,d_201,
       d_209,d_217,d_225,d_233,d_241,d_249,d_257,d_265,d_273,d_281,d_289,d_297,
       d_305,d_313,d_321,d_329,d_337,d_345,d_353,d_361,d_369,d_377,d_385,d_393,
       d_401,d_409,d_417,d_425,d_433,d_441,d_449,d_457,d_465,d_473,d_481,d_489,
       d_497,d_505,d_513,d_521,d_529,d_537,d_545,d_553,d_561,d_569,d_577,d_585,
       d_593,d_601,d_609,d_617,d_625,d_633,d_641,d_649,d_657,d_665,d_673,d_681,
       d_689,d_697,d_705,d_713,d_721,d_729,d_737,d_745,d_753,d_761,d_769,d_777,
       d_785,d_793,d_801,d_809,d_817,d_825,d_833,d_841,d_849,d_857,d_865,d_873,
       d_881,d_889,d_897,d_905,d_913,d_921,d_929,d_937,d_945,d_953,d_961,d_969,
       d_977,d_985,d_993,d_1001,d_1009,d_1017,d_1025,d_1033,d_1041,d_1049,d_1057,
       d_1065,d_1073,d_1081,d_1089,d_1097,d_1105,d_1113,d_1121,d_1129,d_1137,d_1145,
       d_1153,d_1161,d_1169,d_1177,d_1185,d_1193,d_1201,d_1209,d_1217,d_1225,d_1233,
       d_1241,d_1249,d_1257,d_1265,d_1273,d_1281,d_1289,d_1297,d_1305,d_1313,d_1321,
       d_1329,d_1337,d_1345,d_1353,d_1361,d_1369,d_1377,d_1385,d_1393,d_1401,d_1409,
       d_1417,d_1425,d_1433,d_1441,d_1449,d_1457,d_1465,d_1473,d_1481,d_1489,d_1497,
       d_1505,d_1513,d_1521,d_1529,d_1537,d_1545,d_1553,d_1561,d_1569,d_1577,d_1585,
       d_1593,d_1601,d_1609,d_1617,d_1625,d_1633,d_1641,d_1649,d_1657,d_1665,d_1673,
       d_1681,d_1689,d_1697,d_1705,d_1713,d_1721,d_1729,d_1737,d_1745,d_1753,d_1761,
       d_1769,d_1777,d_1785,d_1793,d_1801,d_1809,d_1817,d_1825,d_1833,d_1841,d_1849,
       d_1857,d_1865,d_1873,d_1881,d_1889,d_1897,d_1905,d_1913,d_1921,d_1929,d_1937,
       d_1945,d_1953,d_1961,d_1969,d_1977,d_1985,d_1993,d_2001,d_2009,d_2017,d_2025,
       d_2033,d_2041,d_2049,d_2057,d_2065,d_2073,d_2081,d_2089,d_2097,d_2105,d_2113,
       d_2121,d_2129,d_2137,d_2145,d_2153,d_2161,d_2169,d_2177,d_2185,d_2193,d_2201,
       d_2209,d_2217,d_2225,d_2233,d_2241,d_2249,d_2257,d_2265,d_2273,d_2281,d_2289,
       d_2297,d_2305,d_2313,d_2321,d_2329,d_2337,d_2345,d_2353,d_2361,d_2369,d_2377,
       d_2385,d_2393,d_2401,d_2409,d_2417,d_2425,d_2433,d_2441,d_2449,d_2457,d_2465,
       d_2473,d_2481,d_2489,d_2497,d_2505,d_2513,d_2521,d_2529,d_2537,d_2545,d_2553,
       d_2561,d_2569,d_2577,d_2585,d_2593,d_2601,d_2609,d_2617,d_2625,d_2633,d_2641,
       d_2649,d_2657,d_2665,d_2673,d_2681,d_2689,d_2697,d_2705,d_2713,d_2721,d_2729,
       d_2737,d_2745,d_2753,d_2761,d_2769,d_2777,d_2785,d_2793,d_2801,d_2809,d_2817,
       d_2825,d_2833,d_2841,d_2849,d_2857,d_2865,d_2873,d_2881,d_2889,d_2897,d_2905,
       d_2913,along=3)

peru <- ne_countries(scale = 10, country = "Peru", returnclass = "sf") # en clase sf
write_stars(adrop(new[1]), "wind_speed_2012.tif")
#Plot by days
#all peru
p1<-ggplot() + geom_stars(data =new[,,,1] ) +# select bands
  #facet_wrap(~new_dim) +
  geom_sf(data = peru)  +
  coord_sf(xlim=c(-84,-70),ylim=c(-21,-3))+
  scale_y_discrete(name = "Latitude", expand=c(0,0)) +
  scale_x_discrete(name = "Longitude",expand=c(0,0))+
  scale_fill_gradientn(colors = viridis::viridis(40))+
  theme_light()
library(animation)
saveGIF({
  for (i in 1:365){  
    a <- ggplot() + geom_stars(data =new[,,,i] ) +# select bands
      geom_sf(data = peru)  +
      coord_sf(xlim=c(-84,-70),ylim=c(-21,-3))+
      scale_y_discrete(name = "Latitude", expand=c(0,0)) +
      scale_x_discrete(name = "Longitude",expand=c(0,0))+
      scale_fill_gradientn(colors = viridis::viridis(50),limits=c(0,16))+
      theme_light()+ggtitle(seq(as.Date("2012/1/1"), as.Date("2012/12/31"), "days")[i])# paste('Day',i,sep='_')
    print(a)}
}, interval = .5, movie.name="wind_speed2012.gif")


#area
ggplot() + 
  geom_stars(data=new[,,,1], interpolate=TRUE)+
  scale_fill_gradientn(colors = viridis::viridis(40))+
  geom_sf(data = peru)  +
  coord_sf(xlim=c(-78,-75),ylim=c(-14.5,-13))+
  scale_y_discrete(name = "Latitude", expand=c(0,0)) +
  scale_x_discrete(name = "Longitude",expand=c(0,0))+
  theme_light()


