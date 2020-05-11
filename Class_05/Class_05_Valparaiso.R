###Class 06 - Spatial Statistics 1###
## Author: Esteban Lopez
## Course: Spatial Analytics 
## Program: Master in Business Analytics
## Institution: Universidad Adolfo Ibáñez


#Covid
library(data.table)

archivos<-dir(path = "/Users/estebanlopezochoa/Dropbox/Documents/006 Library/01 Chile/Datos-COVID19/output/producto2/")
COVID<-fread(input =paste0("/Users/estebanlopezochoa/Dropbox/Documents/006 Library/01 Chile/Datos-COVID19/output/producto2/",archivos[1]))
names(COVID)[6]<-paste0("Confirmados_",substr(archivos[1],start = 1,stop = 10))

for(i in 2:length(archivos)-1){
  aa<-fread(input =paste0("/Users/estebanlopezochoa/Dropbox/Documents/006 Library/01 Chile/Datos-COVID19/output/producto2/",archivos[i]))
  aa<-aa[,.(`Codigo comuna`,`Casos Confirmados`)]
  names(aa)[2]<-paste0("Confirmados_",substr(archivos[i],start = 1,stop = 10))
  COVID<-merge(COVID,aa,by="Codigo comuna",all.x=T,sort=F)
  print(paste("done with", archivos[i]))
}
View(COVID)

COVID[,`Confirmados_2020-03-30.y`:=NULL]

COVID[is.na(`Confirmados_2020-05-04`),`Confirmados_2020-05-04`:=0]


COVID_Valpo<-COVID[`Codigo region`==5,]
COVID_Valpo[,`Codigo comuna`:=paste0("0",`Codigo comuna`)]

### Mapa Niveles

# Choropleth maps
library(sf)
library(chilemapas)
library(classInt)

comunas_r5<-mapa_comunas[mapa_comunas$codigo_region=="05",]

comunas_r5<-merge(x = comunas_r5,y = COVID_Valpo,by.x="codigo_comuna",by.y="Codigo comuna",all.x=TRUE,sort=F)


paleta <- brewer.pal(n = 5,name = "Reds")

ggplot(comunas_r5) + 
  geom_sf(aes(fill = `Confirmados_2020-05-04`, geometry = geometry)) +
  coord_sf(xlim = c(-72, -70), ylim = c(-34, -32), expand = FALSE) +
  scale_fill_gradientn(colours = paleta, name = "No. Casos") +
  labs(title = "Casos Confirmados", subtitle = "Región de Valparaíso - 2020-05-04") +
  theme_minimal(base_size = 11)


# Quantile Choropleth plot (Equal share)
breaks_quantile <- classIntervals(comunas_r5$`Confirmados_2020-05-04`, n = 5, style = "quantile")

comunas_r5$casos_quantile<-cut(comunas_r5$`Confirmados_2020-05-04`,breaks = breaks_quantile$brks,include.lowest = T)

ggplot(comunas_r5) + 
  geom_sf(aes(fill = casos_quantile, geometry = geometry)) +
  coord_sf(xlim = c(-72, -70), ylim = c(-34, -32), expand = FALSE) +
  scale_fill_brewer(palette = "Reds")+
  labs(title = "Casos Confirmados", subtitle = "Región de Valparaíso - 2020-05-04") +
  theme_minimal(base_size = 11)



# Mapping Rates
#concepts
# O_i: Observed Events at spatial unit i  (`Casos Confirmados`)
# P_i: Population at risk at spatial unit i (Poblacion)
# r_i:  Risk rate at spatial unit i (O_i/P_i)
# AR:  Average Risk Rate (sum(O_i)/sum(P_i))
# E_i: Expected Events at spatial unit i (r*P_i)
# R_i: Relative Risk at spatial unit i (Excess Risk)  (O_i/E_i)

comunas_r5$r_i<-comunas_r5$`Confirmados_2020-05-04`/comunas_r5$Poblacion

div_pal<-brewer.pal(name =  "RdBu",n = 5)

ggplot(comunas_r5) + 
  geom_sf(aes(fill = r_i, geometry = geometry)) +
  coord_sf(xlim = c(-72, -70), ylim = c(-34, -32), expand = FALSE) +
  geom_sf_text(aes(label = Comuna, geometry = geometry),cex=1.7) +
  scale_fill_gradient2(low = div_pal[5], mid = "white",
                       high = div_pal[1],name = "Tasa Riesgo") +
  labs(title = "Tasa Cruda de Riesgo", subtitle = "Región de Valparaíso - 2020-05-04") +  theme_minimal(base_size = 11)

# Tasa relativa
comunas_r5$AR<-sum(comunas_r5$`Confirmados_2020-05-04`,na.rm = T)/sum(comunas_r5$Poblacion,na.rm = T)

comunas_r5$E_i<-comunas_r5$Poblacion*comunas_r5$AR

comunas_r5$R_i<-comunas_r5$`Confirmados_2020-05-04`/comunas_r5$E_i

ggplot(comunas_r5) + 
  geom_sf(aes(fill = R_i, geometry = geometry)) +
  coord_sf(xlim = c(-72, -70), ylim = c(-34, -32), expand = FALSE) +
  scale_fill_gradient2(low = div_pal[5], mid = "white",
                       high = div_pal[1],name = "Tasa Relativa",midpoint=1) +
  labs(title = "Tasa Relativa de Riesgo", subtitle = "Región de Valparaíso - 2020-05-04") +
  theme_minimal(base_size = 11)


### Poblacion trabajadora

ODRM<-readRDS("Class_05/ConmutacionR5.rds")
class(ODRM)
View(ODRM)
View(comunas_rm)
names(ODRM)
names(ODRM)<-c("destino",'origen','Total')
ODRM[,origen:=paste0("0",origen)]
ODRM[,destino:=paste0("0",destino)]

library(circlize)
library(chorddiag)
# Creamos matriz de conmutación, donde las filas representan en qué comuna viven y las columna en que columna trabajan:

MATRIZ_OD_R5 <- with(ODRM, tapply(Total, list(destino,origen), FUN=sum))

MATRIZ_OD_R5[is.na(MATRIZ_OD_R5)] <- 0 # Cambiamos NAs de las matrices por 0s.

#chorddiag(MATRIZ_OD_RM[-44,], type = "directional")

Trabajadores_origen<-ODRM[,.(Trab_origen=sum(Total,na.rm = T)),by=.(origen)]

Trabajadores_destino<-ODRM[,.(Trab_destino=sum(Total,na.rm = T)),by=.(destino)]

comunas_r5<-merge(comunas_r5,Trabajadores_origen,by.x='codigo_comuna',by.y='origen',all.x=T,sort=F)

comunas_r5<-merge(comunas_r5,Trabajadores_destino,by.x='codigo_comuna',by.y='destino',all.x=T,sort=F)

# Diferencias Poblacionales

# Natural Breaks (Jenks) - Poblacion total
breaks_jenks_t <- classIntervals(comunas_r5$Poblacion, n = 5, style = "jenks")

comunas_r5$pop_jenks<-cut(comunas_r5$Poblacion,breaks = breaks_jenks_t$brks)

ggplot(comunas_r5) + 
  geom_sf(aes(fill = pop_jenks, geometry = geometry)) +
  coord_sf(xlim = c(-72, -70), ylim = c(-34, -32), expand = FALSE) +
  scale_fill_brewer(palette = "YlOrBr")+
  labs(title = "Población", subtitle = "Región de Valparaíso - 2020-05-04") +
  theme_minimal(base_size = 11)

# Natural Breaks (Jenks) - Trabajadores por Origen
breaks_jenks_o <- classIntervals(comunas_r5$Trab_origen.y, n = 5, style = "jenks")

comunas_r5$trab_jenks_o<-cut(comunas_r5$Trab_origen.y,breaks = breaks_jenks_o$brks)

ggplot(comunas_r5) + 
  geom_sf(aes(fill = trab_jenks_o, geometry = geometry)) +
  coord_sf(xlim = c(-72, -70), ylim = c(-34, -32), expand = FALSE) +
  scale_fill_brewer(palette = "YlOrBr")+
  labs(title = "Trabajadores por Origen", subtitle = "Región de Valparaíso - 2020-05-04") +
  theme_minimal(base_size = 11)


# Natural Breaks (Jenks) - Trabajadores por Destino
breaks_jenks_d <- classIntervals(comunas_r5$Trab_destino.y, n = 5, style = "jenks")

comunas_r5$trab_jenks_d<-cut(comunas_r5$Trab_destino.y,breaks = breaks_jenks_d$brks)

ggplot(comunas_r5) + 
  geom_sf(aes(fill = trab_jenks_d, geometry = geometry)) +
  coord_sf(xlim = c(-72, -70), ylim = c(-34, -32), expand = FALSE) +
  scale_fill_brewer(palette = "YlOrBr")+
  labs(title = "Trabajadores por Destino", subtitle = "Región de valparaíso - 2020-05-04") +
  theme_minimal(base_size = 11)


# Risk by origen

comunas_r5$r_i_TO<-comunas_r5$`Confirmados_2020-05-04`/comunas_r5$Trab_origen.y

comunas_r5$AR_TO<-sum(comunas_r5$`Confirmados_2020-05-04`,na.rm = T)/sum(comunas_r5$Trab_origen.y,na.rm = T)

comunas_r5$E_i_TO<-comunas_r5$Trab_origen.y*comunas_r5$AR_TO

comunas_r5$R_i_TO<-comunas_r5$`Confirmados_2020-05-04`/comunas_r5$E_i_TO

ggplot(comunas_r5) + 
  geom_sf(aes(fill = R_i_TO, geometry = geometry)) +
  coord_sf(xlim = c(-72, -70), ylim = c(-34, -32), expand = FALSE) +
  scale_fill_gradient2(low = div_pal[5], mid = "white",
                       high = div_pal[1],name = "Tasa Relativa",midpoint=1) +
  labs(title = "Tasa Relativa de Riesgo - Trabajadores Origen", subtitle = "Región de Valparaíso - 2020-05-04") +
  theme_minimal(base_size = 11)


# Risk by destino

comunas_r5$r_i_TD<-comunas_r5$`Confirmados_2020-05-04`/comunas_r5$Trab_destino.y

comunas_r5$AR_TD<-sum(comunas_r5$`Confirmados_2020-05-04`,na.rm = T)/sum(comunas_r5$Trab_destino.y,na.rm = T)

comunas_r5$E_i_TD<-comunas_r5$Trab_destino.y*comunas_r5$AR_TD

comunas_r5$R_i_TD<-comunas_r5$`Confirmados_2020-05-04`/comunas_r5$E_i_TD

ggplot(comunas_r5) + 
  geom_sf(aes(fill = R_i_TD, geometry = geometry)) +
  coord_sf(xlim = c(-72, -70), ylim = c(-34, -32), expand = FALSE) +
  scale_fill_gradient2(low = div_pal[5], mid = "white",
                       high = div_pal[1],name = "Tasa Relativa",midpoint=1) +
  labs(title = "Tasa Relativa de Riesgo - Trabajadores Destino", subtitle = "Región de Valparaíso - 2020-05-04") +
  theme_minimal(base_size = 11)
