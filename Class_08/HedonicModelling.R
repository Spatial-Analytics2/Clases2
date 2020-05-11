######################################################
#       Clase 8: Spatial Analytics Toolbox
#       PARTE 3: Housing Price Hedonic Modelling
#               Esteban Lopez Ochoa
#       Magister en Business Analytics - UAI
# DISCLAIMER: This code is for demostrative purposes only
# any missuse is the sole responsability of the ip address
# ownner.
######################################################

# paquetes
packages<-c('McSpatial','sp','devtools','rgeos')
sapply(packages,FUN = require,character.only=T)

# Calculando distancias
# Euclidianas
coords@data$distEu_av_sat<-geoshape(longvar = coords@data$coords.2.,latvar = coords@data$coords.1.,pointfile = averdes)*0.621371
coords@data$distEu_av_prc<-geoshape(longvar = coords@data$coords.2.,latvar = coords@data$coords.1.,pointfile = prc_averdes)*0.621371

#-------------------------------------
# OLS
#-------------------------------------
dataANF<-data.table(coords@data)
dataANF[,ln_precio:=log(precio)]
head(sort(dataANF$distEu_av_sat,decreasing = T))


f1<-formula(ln_precio ~ bedrooms + bathrooms + casa + area_util + distEu_av_sat)
f2<-formula(ln_precio ~ bedrooms + bathrooms + casa + area_util + distEu_av_prc)

m1<-lm(f1,data = dataANF)
m2<-lm(f2,data = dataANF)
summary(m1)
summary(m2)

# Distance to CBD
dataANF[,dist_CBD:=(geodistance(longvar = coords.2.,latvar = coords.1.,lotarget =-70.39475,latarget = -23.59240)$dist)*0.621371]
dataANF[,rooms:=bedrooms+bathrooms]

f3<-formula(ln_precio ~ rooms + casa + distEu_av_sat + dist_CBD)
m3<-lm(f3,data = dataANF)
summary(m3)


#accounting for locations (raw for distance to the ocean)
f4<-formula(ln_precio ~ rooms+ casa +distEu_av_sat + dist_CBD+coords.1.)
m4<-lm(f4,data = dataANF)
summary(m4)

#accounting for both locations (raw for distance to the ocean and north-south)
f5<-formula(ln_precio ~ rooms+ casa +distEu_av_sat+coords.1.+coords.2.)
m5<-lm(f5,data = dataANF)
summary(m5)

#-------------------------------------
# Conditional Parametric Regression
#-------------------------------------
# Locally Weighted Regression (LWR)
#   GWR is a special case of LWR when the long and lat are the weighting variables

names(dataANF)[grepl("coords",x = names(dataANF))]<-c("lat","lon")

f6<-formula(ln_precio ~ rooms+ casa + area_util+ distEu_av_sat + dist_CBD)
m6<-cparlwr(f6,nonpar = ~lon+lat,distance = "Latlong",data=dataANF,targetobs = "alldata")
summary(m6$xcoef)


f7<-formula(ln_precio ~ rooms+ casa + area_util+ distEu_av_sat + dist_CBD)
m7<-cparlwr(f7,nonpar = ~lon,data=dataANF)
summary(m7$xcoef)


#-------------------------------------
# Comparing  Model fitted values
#-------------------------------------

dataANF[,m3.fit:=predict(m3,type='response')]
dataANF[,m6.fit:=m6$yhat]


plot(density(dataANF$m3.fit),col='red')
lines(density(dataANF$m6.fit),col='blue')
lines(density(dataANF$ln_precio))

#assesing the marginal predicted values of a single variable
dataANF[,m3.fit_distEu_av_sat:=m3$coefficients['distEu_av_sat']*distEu_av_sat]
dataANF[,m6.fit_distEu_av_sat:=m6$xcoef[,'distEu_av_sat']*distEu_av_sat]

plot(dataANF$distEu_av_sat,dataANF$m6.fit_distEu_av_sat,col='blue',type='p')
points(dataANF$distEu_av_sat,dataANF$m3.fit_distEu_av_sat,col='red',type='p')


#-------------------------------------
# Visualizing Results
#-------------------------------------

dataANF[,m6.Coeff_rooms:=m6$xcoef[,2]]
dataANF[,m6.Coeff_casa:=m6$xcoef[,3]]
dataANF[,m6.Coeff_distEu_av_sat:=m6$xcoef[,5]]
dataANF[,m6.Coeff_dist_CBD:=m6$xcoef[,6]]

pColor<-colorQuantile(palette = brewer.pal(7,name = "YlOrRd"),domain =range(dataANF$m6.Coeff_distEu_av_sat,na.rm = T),n=7)

leaflet(dataANF)%>%
  addTiles(group="OSM defautlt")%>%
  addProviderTiles(providers$Stamen.TonerLite, group = "Toner Lite")%>%
  addCircleMarkers(~lon,~lat,color =pColor(dataANF$m6.Coeff_distEu_av_sat),stroke=F,fillOpacity = 0.5,label = ~as.character(round(m6.Coeff_distEu_av_sat,2)),group = "Distancia AV Coef")%>%
  addRasterImage(ndvi,opacity = 0.5,colors = rev(terrain.colors(5)),group="Raster")%>%
  addLayersControl(
    baseGroups = c("OSM (default)", "Toner Lite"),
    overlayGroups = c("Raster", "Distancia AV Coef"),
    options = layersControlOptions(collapsed = FALSE)
  )
