###Class 03 - Data Management & Visualization###
## Author: Esteban Lopez
## Course: Spatial Analytics 
## Program: Master in Business Analytics
## Institution: Universidad Adolfo Ibáñez


#---- Part 1: Data Management  -------------------

# Reading an exporting data

library(readxl)
library(data.table)



casos<-data.table(read_excel("Class_02/2020-03-17-Casos-confirmados.xlsx",na = "—",trim_ws = TRUE,col_names = TRUE),stringsAsFactors = FALSE)

names(casos)
casos<-casos[Región=="Metropolitana",]

saveRDS(casos,"Class_03/casosRM.rds")

write.csv(casos,file = 'Class_03/CasosCovid_RM.csv',fileEncoding = 'UTF-8')

writexl::write_xlsx(casos,path = "Class_03/CasosenExcel.xlsx")

library(foreign)

write.dta



casosRM<-fread("Class_03/CasosCovid_RM.csv",header = T, showProgress = T,data.table = T)

casosRM[,table(Sexo)]
casosRM[Sexo=="Fememino",Sexo:="Femenino"]

casosRM[`Centro de salud`=="Clínica Alemana",`Centro de salud`:="Clinica Alemana"]
casosRM[,.N,by=.(`Centro de salud`)]

# Creating (factor) variables

class(casosRM$Sexo)

casosRM[,Sexo:=factor(Sexo)]

head(casosRM$Sexo)
head(as.numeric(casosRM$Sexo))

table(casosRM$Sexo)
casosRM[,.N,by=.(Sexo)]
casosRM[,.N,by=.(Sexo,`Centro de salud`)]

#Collapsing by Centro de Salud 

names(casosRM)
obj1<-casosRM[,.N,by=.(`Centro de salud`)]


obj1[,sum(N,na.rm = T)]

obj1[,porc:=N/sum(N,na.rm = T)]

# collapsing (colapsar) by average age


A<-casosRM[,.(AvAge=mean(Edad,na.rm = T)),by=.(`Centro de salud`)]

B<-casosRM[,.(Total_centro=.N),by=.(`Centro de salud`)]

C<-casosRM[Sexo=="Femenino",.(Total_Centro_Mujeres=.N),by=.(`Centro de salud`)]

D<-casosRM[Sexo=="Masculino",.(Total_Centro_Hombres=.N),by=.(`Centro de salud`)]

dim(A)
dim(B)
dim(C)
dim(D)


#merging data sets


AB<-merge(A,B,by = "Centro de salud",all = T,sort = F)


ABC<-merge(AB,C,by = "Centro de salud",all = T,sort = F)
ABCD<-merge(ABC,D,by = "Centro de salud",all = T,sort = F)

ABCD[,porc_mujeres:=Total_Centro_Mujeres/Total_centro]


# reshaping

E<-casosRM[,.(AvAge=mean(Edad,na.rm = T),`Casos confirmados`=.N),by=.(`Centro de salud`,Sexo)]

G<-reshape(E,direction = 'wide',timevar = 'Sexo',v.names = c('AvAge','Casos confirmados'),idvar = 'Centro de salud')

#---- Part 2: Visualization  -------------------

#Scatter plot
  #Base R 
plot(G$`Casos confirmados.Femenino`,G$`Casos confirmados.Masculino`)
text(x =G$`Casos confirmados.Femenino`,y=G$`Casos confirmados.Masculino`, G$`Centro de salud`,cex=0.5)

#ggplot2
p1<-ggplot(G,aes(x=`Casos confirmados.Femenino`,y=`Casos confirmados.Masculino`))+geom_point(aes(size=AvAge.Femenino,colour=AvAge.Masculino))+geom_text(aes(label=`Centro de salud`),size=2,check_overlap = T)
p1

#plotly
library(plotly)
ggplotly(p1)

# other useful ways to show data

#high charter
# http://jkunst.com/highcharter/index.html


#---- Part 3: Intro to Mapping  -------------------
#install.packages("chilemapas")
library(chilemapas)
library(data.table)
library(ggplot2)

zonas_censo<-data.table(censo_2017_zonas,stringsAsFactors = F)

poblacion_adulto_mayor_zonas<-zonas_censo[edad=="65 y mas",.(AdultosMayores=sum(poblacion)),by=.(geocodigo)]

zonas_valparaiso<-mapa_zonas[mapa_zonas$codigo_region=="05",]

zonas_valparaiso<-merge(zonas_valparaiso,codigos_territoriales[,c("codigo_comuna","nombre_comuna")],by="codigo_comuna",all.x=TRUE,sort=F)

zonas_valparaiso<-zonas_valparaiso[zonas_valparaiso$codigo_comuna%in%c("05101","05109"),]

zonas_valparaiso<-merge(zonas_valparaiso,poblacion_adulto_mayor_zonas,by="geocodigo",all.x=TRUE,sort=F)


#plotting
library(RColorBrewer)
paleta <- rev(brewer.pal(n = 9,name = "Reds"))


ggplot(zonas_valparaiso) + 
  geom_sf(aes(fill = AdultosMayores, geometry = geometry)) +
  scale_fill_gradientn(colours = rev(paleta), name = "Poblacion\nadulto mayor") +
  labs(title = "Poblacion de 65 años y más", subtitle = "Valparaíso y Viña del Mar") +
  theme_minimal(base_size = 11)

# creating a fake spatial distribution of adult population in space
zonas_valparaiso2<-cbind(zonas_valparaiso[,c("geocodigo","codigo_comuna","codigo_provincia","codigo_region","geometry")],"AdultosMayores"=sample(zonas_valparaiso$AdultosMayores,size = length(zonas_valparaiso$AdultosMayores)))


ggplot(zonas_valparaiso2) + 
  geom_sf(aes(fill = AdultosMayores, geometry = geometry)) +
  scale_fill_gradientn(colours = rev(paleta), name = "Poblacion\nadulto mayor") +
  labs(title = "Poblacion de 65 años y más", subtitle = "Valparaíso y Viña del Mar") +
  theme_minimal(base_size = 13)

#comparing histograms of the same variable

hist(zonas_valparaiso$AdultosMayores,main = "Histograma Adultos Mayores Viña-Valpo")

hist(zonas_valparaiso2$AdultosMayores,main = "Histograma Adultos Mayores Viña-Valpo")
