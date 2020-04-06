# Tarea 2: Lab 02 Data Management en R

#instalar y cargar el paquete "car"
install.packages("car")
help(package="car")
library(car)
library(data.table)

#cargar los datos preinstalados "Chile"
?Chile
Chile
plebicito<-Chile

plot(Chile)

# 0. ¿De qué clase es el objeto Chile?

class(Chile)

# 1. ¿De qué clase es cada variable?

class(Chile$population)

apply(Chile,FUN=class,MARGIN = 2)

str(Chile)

# 2. Cree una nueva variable que se llame income1000, que sea igual al ingreso en miles de pesos

Chile$income1000<- Chile$income/1000
class(Chile)

# 3. Cuente cuántas personas votaron por el si, y cuántas por el no

table(Chile$vote)

# 4. Cree un nuevo objeto tipo data.table en base al objeto Chile, que se llame Chile2

Chile2<-data.table(Chile)

# 5. Borre la variable statusquo
Chile2<-Chile2[,.(region,population,sex,education,income1000)]
Chile2[,statusquo:=NULL]

# 7. Cree una nueva variable de ingreso per cápita
  # 7.1. Reemplace los NAs de income por ceros
table(is.na(Chile2$income))

Chile2[is.na(income),income:=0]

# 8. Cree una variable que tenga un 1 si age>65 y 0 en el caso contrario
str(Chile2)
Chile2[age>65, Viejitos:=1]
Chile2[is.na(Viejitos), Viejitos:=0]
Chile2[age>65,]$Viejitos<-1

Chile2$Viejitos2<-as.numeric(Chile2$age>65)

# 9. Cree un nuevo objeto que muestre los valores promedio de ingreso y edad por region y sexo
#     Nota: Si tiene observaciones con NA, por qué es esto? Cómo lo arreglamos?

# ObjetoDataTable[<cosas por filas aqui>,<cosas por columnas>,<by>]

Chile2[,.(Prom_ingreso=mean(income,na.rm = T),Prom_edad=mean(age,na.rm = T)),by=.(region,sex)]

by(Chile2$income,Chile2[,c("region","sex")],FUN = mean)

mean(Chile2[region=="SA" & sex=="M",age],na.rm = TRUE)



#10. loops: Solo para propósitos demostrativos, no entra en la tarea!

for(i in 1:10){
  print(paste("Estoy contando de 1 en 1 al 10, y voy en el", i))
  Sys.sleep(3)
}


regiones<-names(table(Chile$region))
sexo<-names(table(Chile$sex))

Chile2<-data.table(Chile)
for(r in 1:length(regiones)){
  for(s in 1:length(sexo)){
    prom.inc<-Chile2[region==regiones[r] & sex==sexo[s],mean(income,na.rm = T)]
    prom.age<-Chile2[region==regiones[r] & sex==sexo[s],mean(age,na.rm = T)]
    plot(Chile2[region==regiones[r] & sex==sexo[s],.(age,income)])
    title(main = paste("Region", regiones[r], "y Sexo", sexo[s]),sub = paste("Ingreso Promedio:", prom.inc, "| Edad Promedio:", prom.age))
    abline(h = prom.inc, col='red')
    abline(v = prom.age, col='blue')
    Sys.sleep(2)
  }
}