rm(list = ls(all.names = T))
options(scipen = 999)
###Class 08 - Spatial Statistics 3###
## Author: Esteban Lopez
## Course: Spatial Analytics 
## Program: Master in Business Analytics
## Institution: Universidad Adolfo Ibáñez

library(ggplot2)
library(readstata13)
library(data.table)
library(readxl)


theme_cepr <- theme_bw() + theme(text = element_text(family = 'Bitter',size = 9),
                                 plot.title = element_blank(), #element_text(color = "seagreen4")  
                                 plot.subtitle = element_blank(),#element_text(color = "#7bb2ae"), 
                                 plot.caption = element_text(color = "#545453"), 
                                 plot.background = element_blank(),#element_rect(color = "black", fill = "white"), 
                                 axis.title.x =  element_blank(),
                                 axis.title.y = element_text(vjust = 1.7,hjust = 0.93,angle=90,color = "#545453",face = 'italic'),
                                 panel.border = element_blank(),#element_rect(color = "blue", size = 1.5, linetype = "dashed"), 
                                 panel.grid.major.x = element_blank(),#element_line(linetype = "dashed"), 
                                 panel.grid.major.y = element_line(linetype = "solid"), 
                                 panel.grid.minor.x = element_blank(),#element_line(linetype = "blank"), 
                                 panel.grid.minor.y = element_blank(),#element_line(linetype = "blank"), 
                                 panel.background = element_blank(),#element_rect(color = "red", fill = "grey90"), 
                                 legend.background = element_blank(),#element_rect(color = "red", fill = "blue"), 
                                 legend.position = 'top', 
                                 legend.justification = c("left", "top"), 
                                 legend.box.just = "left",
                                 legend.margin = margin(0, 0, 4, 0), 
                                 legend.direction = "horizontal",
                                 legend.title = element_blank(),
                                 legend.box.margin = margin(0,0,0,0),
                                 legend.box.spacing = unit(0,'cm'), 
                                 strip.text.x = element_text(face = "italic", hjust = 0, color = "#00A297", size = 10), 
                                 strip.text.y = element_text(face = "italic", color = "#00A297"), 
                                 strip.background = element_rect(linetype = "blank"), 
                                 strip.background.x = element_blank(), 
                                 strip.background.y = element_rect(fill = "grey"),
                                 strip.placement = 'inside')



EODValpo<-data.table(read.dta13("Class_07/Data_EOD_GV/Base_Valparaiso_sample2play.dta"))

prop<-data.table(read_xlsx("Class_07/Data_EOD_GV/Proposito.xlsx"))
  EODValpo$proposito<-factor(EODValpo$proposito,levels=prop$Id,labels = prop$Proposito)
modo<-data.table(read_xlsx("Class_07/Data_EOD_GV/Modo.xlsx"))
  EODValpo$modoviaje2<-car::recode(as.character(EODValpo$modoviaje),"c(1,17)='Auto';
                                   c(2,3,6,11,12,13,14)='Bus/Furgón';
                                   c(4,16)='Metro/Tren';
                                   c(5,7,15)='Taxi/Colectivo';
                                   c(9,10,18)='Bici/Moto';
                                   8='A Pie'")
  EODValpo$modoviaje2<-factor(EODValpo$modoviaje2)
  
modopripub<-data.table(read_xlsx("Class_07/Data_EOD_GV/ModoPriPub.xlsx"))
  modopripub[Id==3,ModoPriPub:="Público"]
EODValpo$modopripub<-factor(EODValpo$modopripub,levels=modopripub$Id,labels = modopripub$ModoPriPub)
  
act<-data.table(read_xlsx("Class_07/Data_EOD_GV/Actividad.xlsx"))
  EODValpo$Actividad<-factor(EODValpo$Actividad,levels=act$Id,labels = act$Actividad)
rel<-data.table(read_xlsx("Class_07/Data_EOD_GV/Relacion.xlsx"))
  EODValpo$Relacion<-factor(EODValpo$Relacion,levels=rel$Id,labels = rel$relacion)
sexo<-data.table(read_xlsx("Class_07/Data_EOD_GV/Sexo.xlsx"))
  EODValpo$Sexo<-factor(EODValpo$Sexo,levels=sexo$Id,labels = sexo$Sexo)
actDest<-data.table(read_xlsx("Class_07/Data_EOD_GV/ActividadDestino.xlsx"))
    EODValpo$Ocupacion<-factor(EODValpo$Ocupacion,levels=actDest$Id,labels = actDest$ActividadDestino)
  
EODValpo[,edad:=2013-AnoNac]
EODValpo<-EODValpo[edad>=15,]

EODValpo$tramo_edad[EODValpo$edad >= 15 & EODValpo$edad <= 29] <- "15 - 29 AÑOS"
EODValpo$tramo_edad[EODValpo$edad >= 30 & EODValpo$edad <= 44] <- "30 - 44 AÑOS"
EODValpo$tramo_edad[EODValpo$edad >= 45 & EODValpo$edad <= 59] <- "45 - 59 AÑOS"
EODValpo$tramo_edad[EODValpo$edad >= 60] <- "60 O MÁS AÑOS"

# Tiempo

EODValpo[,time:=as.ITime(EODValpo$tiempoviaje,format="%H:%M")]
EODValpo[,horas:=hour(time)]
EODValpo[,minutos:=minute(time)]
EODValpo[horas!=0,minutos2:=hour(time)*60]
EODValpo[horas==0,minutos2:=0]
EODValpo[,TiempoViajeMin:=minutos+minutos2]

# ---- Part 1: Main graphs ----

#ggplot(EODValpo,aes(x=proposito,y=Factor_Laboral/sum(Factor_Laboral,na.rm = T),fill=Sexo))+geom_bar(stat = 'identity')+coord_flip()

ggplot(EODValpo,aes(x=proposito,weight=Factor_Laboral,na.rm = T,fill=Sexo))+geom_bar(aes(y = (..count..)/sum(..count..)))+coord_flip() + theme_cepr

ggplot(EODValpo,aes(x=proposito,weight=Factor_Laboral,na.rm = T,fill=Sexo))+geom_bar(aes(y = (..count..)))+coord_flip() + theme_cepr

ggplot(EODValpo[proposito %in% c("Al trabajo"),],aes(x=Sexo,weight=Factor_Laboral,na.rm = T,fill=edad))+geom_bar(aes(y = (..count..)))+coord_flip()

ggplot(EODValpo,aes(x=modoviaje2,weight=Factor_Laboral,na.rm = T,fill=Sexo))+geom_bar(aes(y = (..count..)/sum(..count..)))+coord_flip() +theme_cepr

ggplot(EODValpo,aes(x=tramo_edad,weight=Factor_Laboral,na.rm = T,fill=Sexo))+geom_bar(aes(y = (..count..)/sum(..count..)))+coord_flip()+geom_text(aes(label=scales::percent((..count..)/sum(..count..)),y=(..count..)/sum(..count..)),stat='count',size=3,hjust=1)

ggplot(EODValpo[proposito=="Al trabajo"],aes(x=modoviaje2,weight=Factor_Laboral,na.rm = T,fill=tramo_edad))+geom_bar(aes(y = (..count..)/sum(..count..)))+coord_flip()+theme_cepr


ggplot(EODValpo[proposito=="Al trabajo"&TiempoViajeMin<150& !is.na(Factor_Laboral)],aes(x=TiempoViajeMin,weight=Factor_Laboral,na.rm = T,fill=tramo_edad))+geom_density(stat = 'density',position = position_dodge(),alpha=0.5)+theme_cepr +facet_wrap(~Ocupacion,scales = 'free_y')


ggplot(EODValpo[proposito=="Al trabajo"&TiempoViajeMin<150& !is.na(Factor_Laboral)& IngresoHogar<3000000 & Actividad=='Trabaja' & Ocupacion!='Educación'],aes(y=TiempoViajeMin,weight=Factor_Laboral,na.rm = T,fill=modopripub))+geom_boxplot(position = position_dodge(),alpha=0.5)+theme_cepr +facet_wrap(~Ocupacion)


ggplot(EODValpo,aes(x=edad,weight=Factor_Laboral,na.rm = T,fill=Sexo))+geom_histogram(bins = 20,stat = 'count')

ggplot(EODValpo[proposito %in% c("Al trabajo") & !is.na(Factor_Laboral)& IngresoHogar<5000000],aes(x=IngresoHogar,weight=Factor_Laboral,na.rm = T,fill=Sexo))+geom_density(alpha=0.5)+facet_wrap(~modoviaje2,scales = 'free_y')

ggplot(EODValpo[proposito %in% c("Al trabajo") & !is.na(Factor_Laboral)& IngresoHogar<3000000],aes(y=IngresoHogar,weight=Factor_Laboral,na.rm = T,fill=Sexo))+geom_boxplot(alpha=0.5)+facet_wrap(~modoviaje2,scales = 'free_x')

ggplot(EODValpo[proposito %in% c("Al trabajo") & !is.na(Factor_Laboral)& IngresoHogar<3000000],aes(y=edad,weight=Factor_Laboral,na.rm = T,fill=Sexo))+geom_boxplot(alpha=0.5)+facet_wrap(~modoviaje2,scales = 'free_x')

ggplot(EODValpo[proposito %in% c("Al trabajo") & !is.na(Factor_Laboral)& IngresoHogar<3000000],aes(y=edad,weight=Factor_Laboral,na.rm = T,fill=Sexo))+geom_boxplot(alpha=0.5)+facet_wrap(~Actividad,scales = 'free_x')


ggplot(EODValpo[proposito %in% c("Al trabajo") & !is.na(Factor_Laboral)& IngresoHogar<3000000 & Actividad=='Trabaja'],aes(y=edad,weight=Factor_Laboral,na.rm = T,fill=Sexo))+geom_boxplot(alpha=0.5)+facet_wrap(~Relacion,scales = 'free_x')


ggplot(EODValpo[proposito %in% c("Al trabajo") & !is.na(Factor_Laboral)& IngresoHogar<3000000 & Actividad=='Trabaja'],aes(y=IngresoHogar,weight=Factor_Laboral,na.rm = T,fill=Sexo))+geom_boxplot(alpha=0.5)+facet_wrap(~Ocupacion,scales = 'free_x')

ggplot(EODValpo[proposito %in% c("Al trabajo") & !is.na(Factor_Laboral)& IngresoHogar<3000000 & Actividad=='Trabaja'],aes(x=edad,y=IngresoHogar,weight=Factor_Laboral,na.rm = T,colour=modoviaje2))+geom_point(alpha=0.5)+facet_wrap(~Ocupacion,scales = 'free_x')

ggplot(EODValpo[proposito %in% c("Al trabajo") & !is.na(Factor_Laboral)& IngresoHogar<3000000 & Actividad=='Trabaja' & Ocupacion!='Educación'],aes(x=tramo_edad,fill=tramo_edad,y=IngresoHogar,weight=Factor_Laboral,na.rm = T))+geom_violin(alpha=0.5)+facet_wrap(~Ocupacion)+coord_flip()+theme_cepr

ggplot(EODValpo[proposito %in% c("Al trabajo") & !is.na(Factor_Laboral)& IngresoHogar<3000000 & Actividad=='Trabaja' & Ocupacion!='Educación' & TiempoViajeMin<150],aes(x=tramo_edad,fill=tramo_edad,y=TiempoViajeMin,weight=Factor_Laboral,na.rm = T))+geom_violin(alpha=0.5)+facet_wrap(~Ocupacion)+coord_flip()+theme_cepr


ggplot(EODValpo[proposito %in% c("Al trabajo") & !is.na(Factor_Laboral)& IngresoHogar<3000000 & Actividad=='Trabaja'],aes(x=tramo_edad,fill=tramo_edad,y=IngresoHogar,weight=Factor_Laboral,na.rm = T))+geom_boxplot(alpha=0.5)+facet_wrap(~modoviaje2)+coord_flip()

ggplot(EODValpo[proposito %in% c("Al trabajo") & !is.na(Factor_Laboral)& IngresoHogar<3000000 & Actividad=='Trabaja'],aes(x=tramo_edad,y=tiempoviaje,weight=Factor_Laboral,na.rm = T,fill=tramo_edad))+geom_violin(alpha=0.5)+facet_wrap(~Ocupacion,scales = 'free_x')


ggplot(EODValpo[proposito %in% c("Al trabajo") & !is.na(Factor_Laboral)],aes(x=edad,weight=Factor_Laboral,na.rm = T,fill=Sexo))+geom_density(alpha=0.5)+facet_wrap(~modoviaje2,scales = 'free_y')

#----- Part 2.0: Mapping Trabajadores por Origen y Destino ----

dataOD1<-EODValpo[proposito %in% c("Al trabajo"),.(Trabajadores_Origen=sum(Factor_Laboral,na.rm = T)),by=.(zonaorigen)]
dataOD2<-EODValpo[proposito %in% c("Al trabajo"),.(Trabajadores_Destino=sum(Factor_Laboral,na.rm = T)),by=.(zonadestino)]

vina<-readOGR("Class_07/Shapefiles/Zonas/Zonas_Vina_del_Mar.shp")
  vina<-as(vina,'sf')
valpo<-readOGR("Class_07/Shapefiles/Zonas/Zonas_Valparaiso.shp")
  valpo<-as(valpo,'sf')
concon<-readOGR("Class_07/Shapefiles/Zonas/Zonas_Concon.shp")
  concon<-as(concon,'sf')
quilpue<-readOGR("Class_07/Shapefiles/Zonas/Zonas_Quilpue.shp")
  quilpue<-as(quilpue,'sf')
villa<-readOGR("Class_07/Shapefiles/Zonas/Zonas_Villa_alemana.shp")
  villa<-as(villa,'sf')
  
  
vina<-merge(vina,dataOD1,by.x="ID_ZONA_1",by.y="zonaorigen",all.x=T,sort=F)
valpo<-merge(valpo,dataOD1,by.x="ID_ZONA_12",by.y="zonaorigen",all.x=T,sort=F)
concon<-merge(concon,dataOD1,by.x="ID_ZONA",by.y="zonaorigen",all.x=T,sort=F)
quilpue<-merge(quilpue,dataOD1,by.x="ID_ZONA",by.y="zonaorigen",all.x=T,sort=F)
villa<-merge(villa,dataOD1,by.x="ID_ZONA",by.y="zonaorigen",all.x=T,sort=F)

vina<-merge(vina,dataOD2,by.x="ID_ZONA_1",by.y="zonadestino",all.x=T,sort=F)
valpo<-merge(valpo,dataOD2,by.x="ID_ZONA_12",by.y="zonadestino",all.x=T,sort=F)
concon<-merge(concon,dataOD2,by.x="ID_ZONA",by.y="zonadestino",all.x=T,sort=F)
quilpue<-merge(quilpue,dataOD2,by.x="ID_ZONA",by.y="zonadestino",all.x=T,sort=F)
villa<-merge(villa,dataOD2,by.x="ID_ZONA",by.y="zonadestino",all.x=T,sort=F)

#Trabajadores en el Origen
library(classInt)
bquant_O <- classIntervals(dataOD1$Trabajadores_Origen, n = 5, style = "quantile")
vina$bquant_O<-cut(vina$Trabajadores_Origen,breaks = bquant_O$brks,dig.lab = 5)
valpo$bquant_O<-cut(valpo$Trabajadores_Origen,breaks = bquant_O$brks,dig.lab = 5)
concon$bquant_O<-cut(concon$Trabajadores_Origen,breaks = bquant_O$brks,dig.lab = 5)
villa$bquant_O<-cut(villa$Trabajadores_Origen,breaks = bquant_O$brks,dig.lab = 5)
quilpue$bquant_O<-cut(quilpue$Trabajadores_Origen,breaks = bquant_O$brks,dig.lab = 5)

ggplot()+
  geom_sf(data = vina,aes(fill = bquant_O, geometry = geometry), lwd=0.05) +
  geom_sf(data = valpo,aes(fill = bquant_O, geometry = geometry), lwd=0.05)+
  geom_sf(data = concon,aes(fill = bquant_O, geometry = geometry), lwd=0.05)+
  geom_sf(data = quilpue,aes(fill = bquant_O, geometry = geometry), lwd=0.05)+
  geom_sf(data = villa,aes(fill = bquant_O, geometry = geometry), lwd=0.05)+
  scale_fill_brewer(palette = "GnBu",na.value="#bdbdbd",name = 'No. de Personas \n(colores por quintil)')+
  labs(title = "Distribución Espacial Trabajadores por Origen", subtitle = "Gran Valparaíso",caption = "Fuente: Elaboración CEPR en base a Encuesta Origen y Destino 2012 - SECTRA") +   theme_minimal(base_size = 11)

#Trabajandores por Destino
library(classInt)
bquant_D <- classIntervals(dataOD2$Trabajadores_Destino, n = 5, style = "quantile")
vina$bquant_D<-cut(vina$Trabajadores_Destino,breaks = bquant_D$brks,dig.lab = 5)
valpo$bquant_D<-cut(valpo$Trabajadores_Destino,breaks = bquant_D$brks,dig.lab = 5)
concon$bquant_D<-cut(concon$Trabajadores_Destino,breaks = bquant_D$brks,dig.lab = 5)
villa$bquant_D<-cut(villa$Trabajadores_Destino,breaks = bquant_D$brks,dig.lab = 5)
quilpue$bquant_D<-cut(quilpue$Trabajadores_Destino,breaks = bquant_D$brks,dig.lab = 5)

ggplot()+
  geom_sf(data = vina,aes(fill = bquant_D, geometry = geometry), lwd=0.05) +
  geom_sf(data = valpo,aes(fill = bquant_D, geometry = geometry), lwd=0.05)+
  geom_sf(data = concon,aes(fill = bquant_D, geometry = geometry), lwd=0.05)+
  geom_sf(data = quilpue,aes(fill = bquant_D, geometry = geometry), lwd=0.05)+
  geom_sf(data = villa,aes(fill = bquant_D, geometry = geometry), lwd=0.05)+
  scale_fill_brewer(palette = "GnBu",na.value="#bdbdbd",name = 'No. de Personas \n(colores por quintil)')+
  labs(title = "Distribución Espacial Trabajadores por Destino", subtitle = "Gran Valparaíso",caption = "Fuente: Elaboración CEPR en base a Encuesta Origen y Destino 2012 - SECTRA") +   theme_minimal(base_size = 11) 



# library(mapview)
# library(classInt)
# bquant_O <- classIntervals(dataOD2$Trabajadores_Destino, n = 5, style = "quantile")
# 
# mapview(list(vina["Trabajadores_Destino"],valpo["Trabajadores_Destino"],concon["Trabajadores_Destino"],quilpue["Trabajadores_Destino"],villa["Trabajadores_Destino"]),col.regions=brewer.pal(n = 5,"Reds"),at=bquant_O$brks)
# 
# mapview(vina["Trabajadores_Destino"],col.regions=brewer.pal(n = 5,"Reds"),at=round(bquant_O$brks,0))

#----- Part 2: Mapping origings ----

library(rgdal)
library(sf)
library(RColorBrewer)

# ORIGEN
dataO1<-EODValpo[proposito %in% c("Al trabajo"),sum(Factor_Laboral,na.rm = T),by=.(zonaorigen,Ocupacion)]
dataO1[,wf:=sum(V1,na.rm = T),by=.(zonaorigen)]

dataO1[is.na(Ocupacion),Ocupacion:="Sin Sector"]

dataO1<-reshape(dataO1,v.names = c("V1","wf"),direction = 'wide',idvar = 'zonaorigen',timevar = "Ocupacion")

vina<-merge(vina,dataO1,by.x="ID_ZONA_1",by.y="zonaorigen",all.x=T,sort=F)
valpo<-merge(valpo,dataO1,by.x="ID_ZONA_12",by.y="zonaorigen",all.x=T,sort=F)
concon<-merge(concon,dataO1,by.x="ID_ZONA",by.y="zonaorigen",all.x=T,sort=F)
quilpue<-merge(quilpue,dataO1,by.x="ID_ZONA",by.y="zonaorigen",all.x=T,sort=F)
villa<-merge(villa,dataO1,by.x="ID_ZONA",by.y="zonaorigen",all.x=T,sort=F)

# DESTINO
dataD1<-EODValpo[proposito %in% c("Al trabajo"),sum(Factor_Laboral,na.rm = T),by=.(zonadestino,Ocupacion)]
dataD1[,wf:=sum(V1,na.rm = T),by=.(zonadestino)]

dataD1[is.na(Ocupacion),Ocupacion:="Sin Sector"]

dataD1<-reshape(dataD1,v.names = c("V1","wf"),direction = 'wide',idvar = 'zonadestino',timevar = "Ocupacion")

vina<-merge(vina,dataD1,by.x="ID_ZONA_1",by.y="zonadestino",all.x=T,sort=F)
valpo<-merge(valpo,dataD1,by.x="ID_ZONA_12",by.y="zonadestino",all.x=T,sort=F)
concon<-merge(concon,dataD1,by.x="ID_ZONA",by.y="zonadestino",all.x=T,sort=F)
quilpue<-merge(quilpue,dataD1,by.x="ID_ZONA",by.y="zonadestino",all.x=T,sort=F)
villa<-merge(villa,dataD1,by.x="ID_ZONA",by.y="zonadestino",all.x=T,sort=F)


# Salud - Origen
ggplot()+
  geom_sf(data = vina,aes(fill = V1.Salud.x, geometry = geometry), lwd=0.05) +
  geom_sf(data = concon,aes(fill = V1.Salud.x, geometry = geometry), lwd=0.05) +
  geom_sf(data = quilpue,aes(fill = V1.Salud.x, geometry = geometry), lwd=0.05) +
  geom_sf(data = villa,aes(fill = V1.Salud.x, geometry = geometry), lwd=0.05)+
  geom_sf(data = valpo,aes(fill = V1.Salud.x, geometry = geometry), lwd=0.05)  +scale_fill_distiller(palette = "YlGn",na.value="#bdbdbd",direction = 1,guide = "colourbar",name = 'No. de Personas') +labs(title = "Distribución Espacial Trabajadores Sector Salud - Origen", subtitle = "Gran Valparaíso",caption = "Fuente: Elaboración CEPR en base a Encuesta Origen y Destino 2012 - SECTRA") + theme_minimal(base_size = 11)


# Salud - Destino
ggplot()+
  geom_sf(data = vina,aes(fill = V1.Salud.y, geometry = geometry), lwd=0.05) +
  geom_sf(data = concon,aes(fill = V1.Salud.y, geometry = geometry), lwd=0.05) +
  geom_sf(data = quilpue,aes(fill = V1.Salud.y, geometry = geometry), lwd=0.05) +
  geom_sf(data = villa,aes(fill = V1.Salud.y, geometry = geometry), lwd=0.05)+
  geom_sf(data = valpo,aes(fill = V1.Salud.y, geometry = geometry), lwd=0.05)  +scale_fill_distiller( palette = "YlGn",na.value="#bdbdbd",direction = 1,guide = "colourbar",name = 'No. de Personas')+labs(title = "Distribución Espacial Trabajadores Sector Salud", subtitle = "Gran Valparaíso",caption = "Fuente: Elaboración CEPR en base a Encuesta Origen y Destino 2012 - SECTRA") + theme_minimal(base_size = 11)



# Comercio - Origen
ggplot()+
  geom_sf(data = vina,aes(fill = V1.Comercio.x, geometry = geometry), lwd=0.05) +
  geom_sf(data = concon,aes(fill = V1.Comercio.x, geometry = geometry), lwd=0.05) +
  geom_sf(data = quilpue,aes(fill = V1.Comercio.x, geometry = geometry), lwd=0.05) +
  geom_sf(data = villa,aes(fill = V1.Comercio.x, geometry = geometry), lwd=0.05)+
    geom_sf(data = valpo,aes(fill = V1.Comercio.x, geometry = geometry), lwd=0.05) +scale_fill_distiller( palette = "YlOrBr",na.value="#bdbdbd",direction = 1,guide = "colourbar",name = 'No. de Personas') +labs(title = "Distribución Espacial Trabajadores Sector Comercio - Origen", subtitle = "Gran Valparaíso") +theme_minimal(base_size = 11)

# Comercio - Destino
ggplot()+
  geom_sf(data = vina,aes(fill = V1.Comercio.y, geometry = geometry), lwd=0.05) +
  geom_sf(data = concon,aes(fill = V1.Comercio.y, geometry = geometry), lwd=0.05) +
  geom_sf(data = quilpue,aes(fill = V1.Comercio.y, geometry = geometry), lwd=0.05) +
  geom_sf(data = villa,aes(fill = V1.Comercio.y, geometry = geometry), lwd=0.05)+
  geom_sf(data = valpo,aes(fill = V1.Comercio.y, geometry = geometry), lwd=0.05)  +scale_fill_distiller( palette = "YlOrBr",na.value="#bdbdbd",direction = 1,guide = "colourbar",name = 'No. de Personas') +labs(title = "Distribución Espacial Trabajadores Sector Comercio", subtitle = "Gran Valparaíso") +theme_minimal(base_size = 11)



  # V1.Industria - Origen
  ggplot()+
    geom_sf(data = vina,aes(fill = V1.Industria.x, geometry = geometry), lwd=0.05) +
    geom_sf(data = concon,aes(fill = V1.Industria.x, geometry = geometry), lwd=0.05) +
    geom_sf(data = quilpue,aes(fill = V1.Industria.x, geometry = geometry), lwd=0.05) +
    geom_sf(data = villa,aes(fill = V1.Industria.x, geometry = geometry), lwd=0.05)+
    geom_sf(data = valpo,aes(fill = V1.Industria.x, geometry = geometry), lwd=0.05)  +scale_fill_continuous(low="thistle2", high="darkred", guide="colorbar",na.value="#bdbdbd") +  labs(title = "Distribución Espacial Trabajadores Sector Industria", subtitle = "Región de Valparaíso") +  theme_minimal(base_size = 11)
  
  
  
 
  
  
  # V1.Industria
  ggplot()+
    geom_sf(data = vina,aes(fill = V1.Industria.y, geometry = geometry), lwd=0.05) +
    geom_sf(data = concon,aes(fill = V1.Industria.y, geometry = geometry), lwd=0.05) +
    geom_sf(data = quilpue,aes(fill = V1.Industria.y, geometry = geometry), lwd=0.05) +
    geom_sf(data = villa,aes(fill = V1.Industria.y, geometry = geometry), lwd=0.05)+
    geom_sf(data = valpo,aes(fill = V1.Industria.y, geometry = geometry), lwd=0.05)  +scale_fill_continuous(low="thistle2", high="darkred", guide="colorbar",na.value="#bdbdbd") +  labs(title = "Distribución Espacial Trabajadores Sector Industria", subtitle = "Región de Valparaíso") +  theme_minimal(base_size = 11)
  

  
  
#### Auto Only

EODValpo<-data.table(read.dta13("/Users/estebanlopezochoa/Dropbox/Documents/003 Research/0000000Transporte_AEF/Bases de datos/Data Paper/Valparaiso.dta"))

prop<-data.table(read_xlsx("Class_07/Data_EOD_GV/Proposito.xlsx"))
EODValpo$proposito<-factor(EODValpo$proposito,levels=prop$Id,labels = prop$Proposito)
modo<-data.table(read_xlsx("Class_07/Data_EOD_GV/Modo.xlsx"))
EODValpo$modoviaje2<-car::recode(as.character(EODValpo$modoviaje),"c(1,17)='Auto';
                                   c(2,3,6,11,12,13,14)='Bus/Furgón';
                                   c(4,16)='Metro/Tren';
                                   c(5,7,15)='Taxi/Colectivo';
                                   c(9,10,18)='Bici/Moto';
                                   8='A Pie'")
EODValpo$modoviaje2<-factor(EODValpo$modoviaje2)

act<-data.table(read_xlsx("Class_07/Data_EOD_GV/Actividad.xlsx"))
EODValpo$Actividad<-factor(EODValpo$Actividad,levels=act$Id,labels = act$Actividad)
rel<-data.table(read_xlsx("Class_07/Data_EOD_GV/Relacion.xlsx"))
EODValpo$Relacion<-factor(EODValpo$Relacion,levels=rel$Id,labels = rel$relacion)
sexo<-data.table(read_xlsx("Class_07/Data_EOD_GV/Sexo.xlsx"))
EODValpo$Sexo<-factor(EODValpo$Sexo,levels=sexo$Id,labels = sexo$Sexo)
actDest<-data.table(read_xlsx("Class_07/Data_EOD_GV/ActividadDestino.xlsx"))
EODValpo$Ocupacion<-factor(EODValpo$Ocupacion,levels=actDest$Id,labels = actDest$ActividadDestino)

EODValpo[,edad:=2013-AnoNac]
EODValpo<-EODValpo[edad>=15,]

EODValpo$tramo_edad[EODValpo$edad >= 15 & EODValpo$edad <= 29] <- "15 - 29 AÑOS"
EODValpo$tramo_edad[EODValpo$edad >= 30 & EODValpo$edad <= 44] <- "30 - 44 AÑOS"
EODValpo$tramo_edad[EODValpo$edad >= 45 & EODValpo$edad <= 59] <- "45 - 59 AÑOS"
EODValpo$tramo_edad[EODValpo$edad >= 60] <- "60 O MÁS AÑOS"