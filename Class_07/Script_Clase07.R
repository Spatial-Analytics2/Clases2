###Class 07 - Spatial Statistics 2###
## Author: Esteban Lopez
## Course: Spatial Analytics 
## Program: Master in Business Analytics
## Institution: Universidad Adolfo Ibáñez


#---- Part 1: Global and local Spatial autocorrelation of Covid ----

#Covid
library(data.table)

archivos<-dir(path = "Class_06/producto2/")
COVID<-fread(input =paste0("Class_06/producto2/",archivos[1]))
names(COVID)[6]<-paste0("Confirmados_",substr(archivos[1],start = 1,stop = 10))

for(i in 2:length(archivos)){
  aa<-fread(input =paste0("Class_06/producto2/",archivos[i]))
  aa<-aa[,.(`Codigo comuna`,`Casos Confirmados`)]
  names(aa)[2]<-paste0("Confirmados_",substr(archivos[i],start = 1,stop = 10))
  COVID<-merge(COVID,aa,by="Codigo comuna",all.x=T,sort=F)
}
#View(COVID)

COVID[is.na(`Confirmados_2020-03-30`),`Confirmados_2020-03-30`:=0]

library(ggplot2)
ggplot(COVID,aes(x=`Confirmados_2020-03-30`,y=`Confirmados_2020-04-17`))+geom_point()+geom_smooth(method = lm)

ggplot(COVID,aes(x=`Confirmados_2020-04-10`,y=`Confirmados_2020-04-17`))+geom_point()+geom_smooth(method = lm)

ggplot(COVID,aes(x=`Confirmados_2020-04-15`,y=`Confirmados_2020-04-17`))+geom_point()+geom_smooth(method = lm)


#---- Intro 2 Spatial Autocorrelation  -------------------
#install.packages("chilemapas")
library(chilemapas)
library(data.table)
library(ggplot2)

comunas_rm<-mapa_comunas[mapa_comunas$codigo_region==13,]

comunas_rm<-merge(x = comunas_rm,y = COVID[`Codigo region`==13,],by.x="codigo_comuna",by.y="Codigo comuna",all.x=TRUE,sort=F)

comunas_rm<-as_Spatial(comunas_rm)

library(spdep)

nbs<-poly2nb(comunas_rm,queen = T)

w_rm<-nb2listw(nbs,style = "W")

plot(comunas_rm)
plot(nbs,coordinates(comunas_rm),add=T,col='blue',pch=".")

comunas_rm$Confirmados_2020.04.17_sl<-lag.listw(w_rm,comunas_rm$Confirmados_2020.04.17)

plot(comunas_rm$Confirmados_2020.04.17,comunas_rm$Confirmados_2020.04.17_sl)
identify(comunas_rm$Confirmados_2020.04.17,comunas_rm$Confirmados_2020.04.17_sl, comunas_rm$Comuna, cex = 0.8)

# Global Moran's I    

moran.test(comunas_rm$Confirmados_2020.04.17,listw = w_rm)

moran.plot(comunas_rm$Confirmados_2020.04.17,listw = w_rm,)

ggplot(comunas_rm@data,aes(x=Confirmados_2020.04.17,y=Confirmados_2020.04.17_sl))+geom_point()+geom_smooth(method = 'lm',se = F) +geom_smooth(method = 'loess',se = F,col='darkgreen') +geom_hline(yintercept = mean(comunas_rm@data$Confirmados_2020.04.17_sl),col='red') +geom_vline(xintercept = mean(comunas_rm@data$Confirmados_2020.04.17),col='red')


#### Local Moran

locM<-localmoran(x = comunas_rm$Confirmados_2020.04.17,listw = w_rm)
summary(locM)

meanConf<-mean(comunas_rm$Confirmados_2020.04.17)
meanConf_sl<-mean(comunas_rm$Confirmados_2020.04.17_sl)

comunas_rm$quad_sig <- 5
comunas_rm@data[(comunas_rm$Confirmados_2020.04.17 >= meanConf & comunas_rm$Confirmados_2020.04.17_sl >= meanConf_sl) & (locM[, 5] <= 0.1), "quad_sig"] <- 1
comunas_rm@data[(comunas_rm$Confirmados_2020.04.17 <= meanConf & comunas_rm$Confirmados_2020.04.17_sl <= meanConf_sl) & (locM[, 5] <= 0.1), "quad_sig"] <- 2
comunas_rm@data[(comunas_rm$Confirmados_2020.04.17 >= meanConf & comunas_rm$Confirmados_2020.04.17_sl <= meanConf_sl) & (locM[, 5] <= 0.1), "quad_sig"] <- 3
comunas_rm@data[(comunas_rm$Confirmados_2020.04.17 >= meanConf & comunas_rm$Confirmados_2020.04.17_sl <= meanConf_sl) & (locM[, 5] <= 0.1), "quad_sig"] <- 4

# Set the breaks for the thematic map classes
breaks <- seq(1, 5, 1)

# Set the corresponding labels for the thematic map classes
labels <- c("High-High", "Low-Low", "High-Low", "Low-High", "Not Signif.")

np <- findInterval(comunas_rm$quad_sig, breaks)

# Assign colors to each map class
colors <- c("red", "blue", "lightpink", "skyblue2", "white")
plot(comunas_rm, col = colors[np])  #colors[np] manually sets the color for each county
mtext("Local Moran's I", cex = 1.5, side = 3, line = 1)
legend("topright", legend = labels, fill = colors, bty = "n",cex = 0.9)

