######################################################
#     Clase 8: Spatial Analytics Toolbox
#         PARTE 1: Satellite Imagery
#               Esteban Lopez Ochoa
#     Magister en Business Analytics - UAI
# DISCLAIMER: This code is for demostrative purposes only
# any missuse is the sole responsability of the ip address
# ownner.
######################################################


# paquetes
packages<-c('satellite','raster','rgdal','leaflet','RColorBrewer','data.table','RStoolbox')
sapply(packages,FUN = require,character.only=T)
#install.packages('satellite')
#install.packages('RStoolbox')

#cargando datos
path<- "/Users/estebanlopezochoa/Dropbox/Documents/005 Teaching/007 Workshops/002 DataScience and RegionalScience/01 SatelliteImagery/LC08_L1TP_001076_20180228_20180308_01_T1/"

dir(path)
#path<- paste0(getwd(),"/Class_08/LC08_L1TP_001076_20180228_20180308_01_T1/")
files <- list.files(path,pattern=glob2rx("LC08*.TIF"), full.names = T)
sat<-satellite(files)

sat1<-readMeta(paste0(path,"LC08_L1TP_001076_20180228_20180308_01_T1_MTL.txt"))
sat1<-stackMeta(paste0(path,"LC08_L1TP_001076_20180228_20180308_01_T1_MTL.txt"),allResolutions = T)

sat<-stack(sat) # stacking layers

# loading the panchromatic layer (15 mts resolution)

pan<-raster(files[10])

#-------------------------------------
#analyzing data
#-------------------------------------
names(sat) 
nlayers(sat)
res(sat)
res(pan)
ncell(sat)
dim(sat)

#-------------------------------------
#plotting
#-------------------------------------
plotRGB(sat, r=4,g=3,b=2,stretch='lin') #True color composite
plotRGB(sat, r=5,g=4,b=3,stretch='lin') #NIR false color composite
plotRGB(sat, r=7,g=5,b=1,stretch='lin') #SWIR false color composite
plotRGB(sat, r=10,g=7,b=3,stretch='lin') # TIR false composite

#-------------------------------------
#Spatial Subset - selecting Antofagasta
#-------------------------------------
#drawExtent()
e<-c(347303.5,361594.6,-2632700,-2594829)

sat.anf<-crop(sat,e)

plotRGB(sat.anf, r=3,g=2,b=1,stretch='lin')
plotRGB(sat.anf, r=4,g=3,b=2,stretch='lin')

e1<-c(352702.6,360973.6,-2627103,-2600194)

sat.anf<-crop(sat.anf,e1)
pan.anf<-crop(pan,e1)

#-------------------------------------
#plotting subset data
#-------------------------------------
plotRGB(sat.anf, r=4,g=3,b=2,stretch='lin') # true color
plotRGB(sat.anf,r=5,g=5,b=5,stretch='lin') # band 5 Near Infra Red (NIR)
plotRGB(sat.anf, r=5,g=4,b=3,stretch='lin') # Vegetation false color composite - healthy vegetation in red
plotRGB(sat.anf, r=7,g=5,b=1,stretch='lin') # SWIF false composite - healthy vegetaion in brigth green
plotRGB(sat.anf, r=9,g=9,b=9,stretch='lin') #ground not visible, only high reflectance at high altitude features are captured like clouds or mountain traits.
plotRGB(sat.anf, r=10,g=10,b=11,stretch='lin') # Thermal Infra Red (TIR)
plotRGB(sat.anf, r=10,g=7,b=3,stretch='lin') # TIR false composite

# reference: https://landsat.gsfc.nasa.gov/landsat-8/landsat-8-bands/ 

#-------------------------------------
# Normalized Difference Vegetation Index (NDVI)
#-------------------------------------
# i=NIR and k=red are bands depending on satellite

#NDVI = (NIR - red)/(NIR + red)

vi <- function(img, i, k){
    bi <- img[[i]]
    bk <- img[[k]]
    vi <- (bi-bk)/(bk+bi)
    return(vi)
}

#For landstat8, NIR=5, red=4
ndvi<-vi(sat.anf,5,4)

plot(ndvi, col = rev(terrain.colors(30)), main = 'NDVI from LandSat8')




################################################
# End Part 1: Do part 2 and come back
################################################

#-------------------------------------
#Extract Raster Values based on a point-shapefile
#-------------------------------------
arriendos<-data.table(readRDS("Class_08/arriendos.rds"))
coords<-arriendos[,c("coords.2.","coords.1.")]
proj.CL <- "+proj=longlat +datum=WGS84 +no_defs"

coords<-SpatialPointsDataFrame(coords,proj4string = CRS(proj.CL),data=arriendos)
coords<-spTransform(coords,CRSobj = sat.anf@crs)

sat.anf.pointvalues<-extract(ndvi,coords,buffer=10,fun=max,na.rm=T,df=T)

coords@data<-cbind(coords@data,sat.anf.pointvalues[,2])
names(coords@data)[13]<-"NDVI"

#-------------------------------------
# Visualization
#-------------------------------------
coords<-spTransform(coords,CRSobj = proj.CL)

pColor<-colorQuantile(palette = brewer.pal(6,name = "Reds"),domain =range(coords$precio,na.rm = T),n=6)

leaflet(coords@data)%>%
  addTiles(group="OSM defautlt")%>%
  addProviderTiles(providers$Stamen.TonerLite, group = "Toner Lite")%>%
  addCircleMarkers(~coords.2.,~coords.1.,color =pColor(coords$precio),stroke=F,fillOpacity = 0.5,label = ~as.character(precio),group = "Arriendos")%>%
  addRasterImage(ndvi,opacity = 0.5,colors = rev(terrain.colors(5)),group="Raster")%>%
    addLayersControl(
      baseGroups = c("OSM (default)", "Toner Lite"),
      overlayGroups = c("Raster", "Arriendos"),
      options = layersControlOptions(collapsed = FALSE)
    )

#-------------------------------------
# Creating a polygon-shapefile from raster values
#-------------------------------------

averdes<-rasterToPolygons(ndvi,fun=function(x){x>0.3})
averdes<-rasterToPolygons(ndvi,fun=function(x){x>=0.2})
averdes<-spTransform(averdes,CRSobj = proj.CL)
averdes@data

leaflet(averdes)%>%
  addProviderTiles(providers$OpenStreetMap.BlackAndWhite, group = "Toner Lite")%>%
  addPolygons(group="Areas Verdes",label =~as.character(round(layer,2)))%>%
  addRasterImage(ndvi>=0.2,opacity = 0.8,colors = rev(terrain.colors(5)),group="Raster")%>%
  addLayersControl(
    overlayGroups = c("Raster","Areas Verdes"),
    options = layersControlOptions(collapsed = FALSE)
  )

#-------------------------------------
###loading actual areas verdes from ANF PRC
#source: http://www.ide.cl/descarga/capas/item/zonificacion-plan-regulador-intercomunal-del-borde-costero-region-de-antofagasta.html
#-------------------------------------
prc_anf<-readOGR("Class_08/PRC_Antofagasta/PRC_Antofagasta.shp")
av<-grepl('Verde',prc_anf@data$NOMBRE)
table(prc_anf@data$NOMBRE[av])
prc_averdes<-prc_anf[av,]

prc_averdes<-spTransform(prc_averdes,CRSobj = proj.CL)
leaflet(prc_averdes)%>%
  addProviderTiles(providers$OpenStreetMap.BlackAndWhite, group = "Toner Lite")%>%
  addPolygons(group="Areas Verdes")%>%
  addRasterImage(ndvi>=0.2,opacity = 0.8,colors = rev(terrain.colors(5)),group="Raster")%>%
  addLayersControl(
    overlayGroups = c("Raster","Areas Verdes"),
    options = layersControlOptions(collapsed = FALSE)
  )


#Conclusion: No siempre los catastros de uso de suelo son una buena representacion de la realidad. 
