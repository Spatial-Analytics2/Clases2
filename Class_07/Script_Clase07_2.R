rm(list = ls(all.names = T))
###Class 07 - Spatial Statistics 3###
## Author: Esteban Lopez
## Course: Spatial Analytics 
## Program: Master in Business Analytics
## Institution: Universidad Adolfo Ibáñez

packageList<-c('tidyverse','leaflet','data.table','rgdal','ggplot2','deldir','sp','RColorBrewer','KernSmooth','googleway')
sapply(packageList, FUN = require, character.only=TRUE)

#--------------------------------------------------#
# 1. Cargando Mapas
#--------------------------------------------------#

# Mapa accidentes en bicicleta 2016
# Fuente: http://www.ide.cl/descarga/capas/item/accidentes-en-bicicleta-ano-2016.html?category_id=64 
biciMap<-readOGR(dsn = "Class_07/Bici_personaGS2016_2/Bici_personaGS2016_2.shp")

#exploring the file
biciMap@proj4string # proyección
proj4string(biciMap) # proyección
biciMap@coords # coordenadas 1
coordinates(biciMap) # coordenadas lat (Y) long (X)
View(biciMap@data) # datos

# Mapa comunas Santiago (ciudad)
#Fuente: http://www.censo2017.cl/servicio-de-mapas/

ComunasR13Map<-readOGR("Class_07/ComunasR13/COMUNA_C17.shp")
ComunasR13Map@proj4string
#Static base plot
plot(biciMap)
plot(ComunasR13Map,add=T)

#Interactive/live Plot
leaflet(biciMap)%>%
  addTiles()%>%
  addMarkers() 

leaflet(ComunasR13Map)%>%
  addTiles()%>%
  addPolygons()

leaflet(ComunasR13Map)%>%
  addTiles()%>%
  addPolygons()%>%
  addMarkers(biciMap,lng = biciMap@coords[,1],lat = biciMap@coords[,2])

#--------------------------------------------------#
# 2. Mapa por comuna con más accidentes
#--------------------------------------------------#

# Creando un mapa con las comunas que tienen datos

head(ComunasR13Map@data)
head(biciMap@data)

table(ComunasR13Map@data$COMUNA%in%biciMap@data$COD_COMUNA)

ComunasR13MapBici<-ComunasR13Map[ComunasR13Map@data$COMUNA%in%biciMap@data$COD_COMUNA,]

leaflet(ComunasR13MapBici)%>%
  addTiles()%>%
  addPolygons()%>%
  addMarkers(biciMap,lng = biciMap@coords[,1],lat = biciMap@coords[,2])

# añadiendo tabla de estadisticos descriptivos al mapa
class(biciMap@data)
biciData<-data.table(biciMap@data)
NumAccBici<-biciData[,.(NumAccBici=.N),by=.(COD_COMUNA)]

ComunasR13MapBici@data<-merge(ComunasR13MapBici@data,NumAccBici,by.x="COMUNA",by.y="COD_COMUNA",sort=F,all.x=T)

head(ComunasR13MapBici@data)
head(NumAccBici)

#creating colors
pColor<-colorQuantile(palette = brewer.pal(4,name = "Reds"),domain =range(ComunasR13MapBici@data$NumAccBici,na.rm = T),n = 5)

#creating labels
labels <- sprintf(
  "<strong>Comuna: %s </strong><br/>Acc.Bicicleta: %g ",
  ComunasR13MapBici$NOM_COMUNA, ComunasR13MapBici$NumAccBici
) %>% lapply(htmltools::HTML)

#grafico de colores por comuna
leaflet(ComunasR13MapBici)%>%
  addProviderTiles(provider = "CartoDB")%>%
  addPolygons(fillColor = ~pColor(NumAccBici),color='white',weight=1,fillOpacity = 0.7,highlight = highlightOptions(
    weight = 5,
    color = "#666",
    fillOpacity = 0.7,
    bringToFront = TRUE),label=labels)

#--------------------------------------------------#
# 3. seleccionar las comunas con más accidentes
#--------------------------------------------------#
class(biciMap@data)
biciData<-data.table(biciMap@data)
biciData[,.N,by=.(COMUNA1)]

#crear mapa solo de la comuna con más accidentes
biciMapStgo<-biciMap[biciMap$COMUNA1=="SANTIAGO" | biciMap$COMUNA1=="PROVIDENCIA" ,]
leaflet(biciMapStgo)%>%
  addProviderTiles(provider = "CartoDB")%>%
  addMarkers() 

#--------------------------------------------------#
# 4. Mapa de densidad de accidentes
#--------------------------------------------------#
#Fuente: https://gis.stackexchange.com/questions/168886/r-how-to-build-heatmap-with-the-leaflet-package

kde2d <- bkde2D(biciMapStgo@coords,
              bandwidth=c(.00225, .00225))
CL <- contourLines(kde2d$x1 , kde2d$x2 , kde2d$fhat)

# EXTRACT CONTOUR LINE LEVELS
LEVS <- as.factor(sapply(CL, `[[`, "level"))
NLEV <- length(levels(LEVS))

# CONVERT CONTOUR LINES TO POLYGONS
pgons <- lapply(1:length(CL), function(i)
  Polygons(list(Polygon(cbind(CL[[i]]$x, CL[[i]]$y))), ID=i))
spgons <- SpatialPolygons(pgons)

# Leaflet map with polygons
leaflet(spgons) %>% 
  addProviderTiles(provider = "CartoDB")%>%
  addPolygons(color = heat.colors(NLEV, NULL)[LEVS])


#--------------------------------------------------#
# 5. Densidad de Accidentes VS. Localización de Bike Stores
#--------------------------------------------------#
#Para bajar datos de Google Maps se necesita activar una API
#Dos paquetes como opciones: googleway y GoogleDataSearch
#bikeShops<-GooglePlaceSearch("accesorios para bicicletas, Santiago de Chile",apikey = "_____")
#bikeShops2<-google_places("accesorios para bicicletas, Santiago de Chile",key =  "_____",language = 'es' )
#saveRDS(bikeShops,"bikeShopsStgo.rds")

bikeShops<-readRDS("Class_07/bikeShopsStgo.rds")
bikeShops$store_lat<-as.numeric(bikeShops$store_lat)
bikeShops$store_lon<-as.numeric(bikeShops$store_lon)

leaflet(spgons) %>%   addProviderTiles(provider = "CartoDB")%>%
  addPolygons(color = heat.colors(NLEV, NULL)[LEVS])%>%
  addMarkers(lng = bikeShops$store_lon,lat = bikeShops$store_lat)

#--------------------------------------------------#
# 6. Voronoi Triangulation for market Areas
#--------------------------------------------------#

window<-as.numeric(c(attributes(spgons)$bbox[1,],attributes(spgons)$bbox[2,]))
z<-deldir(bikeShops[,c('store_lon','store_lat')],rw = window)
w <- deldir::tile.list(z)
polys <- vector(mode = "list", length = length(w))
for (i in seq(along = polys)) {
  pcrds <- cbind(w[[i]]$x, w[[i]]$y)
  pcrds <- rbind(pcrds, pcrds[1, ])
  polys[[i]] <- Polygons(list(Polygon(pcrds)), as.character(i))
}

marketAreas <- SpatialPolygons(polys)

leaflet(marketAreas) %>%
  addProviderTiles(provider = "CartoDB")%>%
  addPolygons()%>%
  addMarkers(lng = bikeShops$store_lon,lat = bikeShops$store_lat) %>%
  addPolygons(data = spgons,color = heat.colors(NLEV, NULL)[LEVS])%>%
  addCircleMarkers(lng = biciMapStgo@coords[,1],lat=biciMapStgo@coords[,2],color = "grey",fillColor = "grey",radius = 0.001)

