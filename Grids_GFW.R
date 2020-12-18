library(raster)
library(sf)
library(rnaturalearth)
library(rgdal)
library(lubridate)
library(sp)
library(stars)
library(ggspatial)
#Grid Peru 10km
xy_general<-data.frame(X =c(-84,-70), Y = c(-21,-3))
coordinates(xy_general) <- c("X", "Y")#coordinates in spatial format
proj4string(xy_general) <- CRS("+proj=longlat +datum=WGS84")#assign WGS84 projection to coordinates
st_crs(24891)$proj4string#projection EPSG:24891, Peru onshore,WGS84 bounds: -81.41,-18.35, -68.67,-0.03 
utm_general<- spTransform(xy_general,CRS("+proj=utm +zone=18 ellps=WGS84 +units=km"))#transform from latlong (in °) to utm (in km)
#All the coast
grd10 <- expand.grid(x = seq(from = utm_general@coords[1], to =utm_general@coords[2], by = 10),y = seq(from = utm_general@coords[3], to =utm_general@coords[4], by = 10))
coordinates(grd10) <- ~ x+y
gridded(grd10) <- TRUE#Grid as SpatialPixels
proj4string(grd10) <- CRS("+proj=utm +zone=18 ellps=WGS84 +units=km")
g10k <- as(grd10, 'SpatialPolygons')
g10k_df <- as(g10k, "SpatialPolygonsDataFrame")
writeOGR(obj=g10k_df, dsn="./Data/Maps", layer="peru_grid10", driver="ESRI Shapefile")
#The AOI, one degree around Ballestas
MPA_all<-read.csv2("Data/Maps/Area_MPA.csv", header=TRUE)
MPA_ballestas<-MPA_all%>%dplyr::filter(MPA=='Ballestas')
MPA_ballestas<-MPA_ballestas%>%dplyr::mutate_if(is.factor,as.character)%>%dplyr::mutate_if(is.character,as.numeric)
Ballestas_st<- st_as_sf(MPA_ballestas[,4:5], coords = c("Longitude", "Latitude"), crs = st_crs(xy_general))
bb_ballestas<-st_as_sfc(st_bbox(Ballestas_st))
area_b111= st_sfc(st_buffer(st_centroid(bb_ballestas), 1), crs = st_crs(xy_general))
xy_AOI<-as_Spatial(area_b111)
utm_AOI<-spTransform(xy_AOI, CRS("+proj=utm +zone=18 ellps=WGS84 +units=km"))
#Grid AOI
grd10_AOI <- expand.grid(x = seq(from = utm_AOI@bbox[1],to =utm_AOI@bbox[3], by = 10),
                     y = seq(from = utm_AOI@bbox[2],to =utm_AOI@bbox[4], by = 10))
coordinates(grd10_AOI) <- ~ x+y
gridded(grd10_AOI) <- TRUE#Grid as SpatialPixels
proj4string(grd10_AOI) <- CRS("+proj=utm +zone=18 ellps=WGS84 +units=km")
g10k_AOI <- as(grd10_AOI, 'SpatialPolygons')
g10k_AOI_df <- as(g10k_AOI, "SpatialPolygonsDataFrame")
#Plot
pe <- getData('GADM', country='PE', level=0)#peru shapefile
pe_sf = st_as_sf(pe)
pe_sf =st_transform(pe_sf, "+proj=utm +zone=18 ellps=WGS84 +units=km")
ggplot(st_as_sf(g10k_AOI_df)) +
  geom_sf(col=2)+geom_sf(data =pe_sf)+
  coord_sf(xlim=c(236.0712,456.0712),ylim=c(-1634.572,-1404.572))
writeOGR(obj=g10k_AOI_df, dsn="./Data/Maps", layer="Ica_grid10", driver="ESRI Shapefile")

