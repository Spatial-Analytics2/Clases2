###Class 06 - Spatial Statistics 1###
## Author: Esteban Lopez
## Course: Spatial Analytics 
## Program: Master in Business Analytics
## Institution: Universidad Adolfo Ibáñez


# Spatial randomness

#install.packages('plot.matrix')
library(plot.matrix)
library(classInt)

a<-rnorm(100,0,1)
A<-matrix(a,10,10)
b_A<-classIntervals(a,n = 5,style = 'jenks')
plot(A, breaks = b_A$brks, key=NULL)

b<-a
B<-matrix(c(sample(b,15),sort(sample(b,25)),sample(b,15),sort(sample(b,15)),sample(b,10)),10,10)
#B<-matrix(sort(b),10,10)
b_B<-classIntervals(b,n = 5,style = 'jenks')
plot(B, breaks = b_B$brks, key=NULL)


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
View(COVID)

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

sl<-lag.listw(w_rm,comunas_rm$Confirmados_2020.04.17)

plot(comunas_rm$Confirmados_2020.04.17,sl)


# Optional Reading
# https://cran.r-project.org/web/packages/spdep/vignettes/nb.pdf
