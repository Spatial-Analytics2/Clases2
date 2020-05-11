# devtools::install_github("mattflor/chorddiag") # paquete chordding no se puede instalar por medios normales
library(circlize)
library(chorddiag)
library(data.table)

Tabla_19_19 <- data.table(readRDS(file = "Ayud_03/Tabla_19_19.rds"),stringsAsFactors = F)

## A continuación, se genera matriz origen-destino laborales en la quinta región (sin separar por industria):

  BASE_OD_R5 <- data.table(Tabla_19_19[ano_trimestre == 2019 & 
                                    mes_central == 5 & 
                                    cae_general_red == "Ocupados" & 
                                    (b18_codigo %between% c(5000,5999) | ## nos quedamos con todos los que viven en la RM 
                                    r_p_c %between% c(5000,5999)),       ## o bien con aquellos que trabajan en la RM
                                    .(Total = sum(round(V1))), 
                                    by = .(b18_codigo, r_p_c)])

  # Modificamos los valores comunales para clasificar a aquellos que no viven/trabajan en la XIII región:

  BASE_OD_R5[!b18_codigo %between% c(5000,5999), b18_codigo:="Sale de RV"]
  BASE_OD_R5[!r_p_c %between% c(5000,5999), r_p_c:="Entra a RV"]

  # Creamos matriz de conmutación, donde las filas representan en qué comuna viven y las columna en que columna trabajan:

  MATRIZ_OD_R5 <- with(BASE_OD_R5, tapply(Total, list(b18_codigo, r_p_c), FUN=sum))

  MATRIZ_OD_R5[is.na(MATRIZ_OD_R5)] <- 0 # Cambiamos NAs de las matrices por 0s.

  chorddiag(MATRIZ_OD_R5[completeRow,completeCol], type = "directional")
  
saveRDS(BASE_OD_R5,file = "Class_05/ConmutacionR5.rds")



  