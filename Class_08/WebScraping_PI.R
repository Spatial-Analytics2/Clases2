######################################################
#       Clase 8: Spatial Analytics Toolbox
#              PARTE 2: Web Scraping
#               Esteban Lopez Ochoa
#       Magister en Business Analytics - UAI
# DISCLAIMER: This code is for demostrative purposes only
# any missuse is the sole responsability of the ip address
# ownner.
######################################################


# paquetes
packages<-c('rvest','httr','XML','data.table','devtools')
sapply(packages,FUN = require,character.only=T)
vignette("selectorgadget")
#https://cran.r-project.org/web/packages/rvest/vignettes/selectorgadget.html

#-------------------------------------
#scraping departamentos
#-------------------------------------

  url<-'https://www.portalinmobiliario.com/arriendo/departamento/antofagasta-antofagasta?ca=3&ts=1&mn=1&or=&sf=1&sp=0&at=0&pg=1'  

  #extracting raw prices
  list<-read_html(url)
  prices<-html_nodes(list,".price__fraction")%>%html_text()
  prices<-gsub(pattern ="." ,replacement = "",x = prices,fixed = T)
  

#extracting links to specific properties
list<-read_html(url)%>%
  html_nodes(".item__info") %>%
  html_nodes(".stack_column_item")%>%
  html_children()%>%
  html_attr("href") %>%
  as.data.frame(stringsAsFactors=FALSE) %>%
  unique()

list<-list[!is.na(list),]


# Nivel de Scrape: Propiedad

suburl<-as.character(list[1])
urlprop <- read_html(suburl)


#Precio
precio <- urlprop %>% 
  html_nodes(".price-tag-fraction") %>%
  html_text()%>%
  gsub(pattern="\\D+",replacement="")%>%
  as.numeric()


#Main atributes
  area<- urlprop %>% 
    html_nodes(".specs-item") %>%
    html_text()%>%
    gsub(pattern="\\D+", replacement=" ")%>%
    trimws(which = "both")
  
  area_total<-as.numeric(area[1])
  area_util<-as.numeric(area[2])
  bedrooms<-as.numeric(area[3])
  bathrooms<-as.numeric(area[4])
  parking<-as.numeric(area[5])
  storage<-as.numeric(area[6])
  
  
  # extras
  amenidades <- urlprop %>% 
    html_nodes(".ui-view-more__content") %>%
    html_text()%>%
    gsub(pattern="\n", replacement="")%>%
    trimws(which = "both")%>%
    strsplit(split ="\t")%>%unlist()
  
  
  #Direccion
  dir<- urlprop %>% 
    html_nodes(".map-address") %>%
    html_text()

  sector<- urlprop %>% 
    html_nodes(".map-location") %>%
    html_text()
  
  # id & fecha publicacion
  data<-urlprop %>% 
    html_nodes(".info") %>%
    html_text()
  
  id<-as.numeric(gsub(data[1],pattern="\\D+",replacement = ""))
  date<-gsub(data[2],pattern="Publicada: ",replacement = "")
  
  
  
  # doing a loop 
  
  #setting data storage
  departamentos_arriendo<-NULL
  
  #scraping departamentos # 8 paginas
  for(j in 1:8){
    url<-paste0('https://www.portalinmobiliario.com/arriendo/departamento/antofagasta-antofagasta?ca=3&ts=1&mn=1&or=&sf=1&sp=0&at=0&pg=',j)  
    
    #extracting links of all pages
    list<-read_html(url)%>%
      html_nodes(".item__info") %>%
      html_nodes(".stack_column_item")%>%
      html_children()%>%
      html_attr("href") %>%
      as.data.frame(stringsAsFactors=FALSE) %>%
      unique()
    
    list<-list[!is.na(list),]
    
    
    #loop for each specific properties in page i
    for(i in 1:NROW(list)){
      
      #print counter
      print(paste("Estoy en pagina:",j,", Propiedad:",i))
      Sys.sleep(3)
      
      #scraping level: Propiedad
      suburl<-as.character(list[i])
      urlprop <- read_html(suburl)
      
      #Precio
      precio <- urlprop %>% 
        html_nodes(".price-tag-fraction") %>%
        html_text()%>%
        gsub(pattern="\\D+",replacement="")%>%
        as.numeric()
      
      #Main atributes
      area<- urlprop %>% 
        html_nodes(".specs-item") %>%
        html_text()%>%
        gsub(pattern="\\D+", replacement=" ")%>%
        trimws(which = "both")
      
      area_total<-as.numeric(area[1])
      area_util<-as.numeric(area[2])
      bedrooms<-as.numeric(area[3])
      bathrooms<-as.numeric(area[4])
      parking<-as.numeric(area[5])
      storage<-as.numeric(area[6])
      
      # extras
      # amenidades <- urlprop %>% 
      #   html_nodes(".ui-view-more__content") %>%
      #   html_text()%>%
      #   gsub(pattern="\n", replacement="")%>%
      #   trimws(which = "both")%>%
      #   strsplit(split ="\t")%>%unlist()
      
      
      #Direccion
      dir<- urlprop %>% 
        html_nodes(".map-address") %>%
        html_text()
      
      sector<- urlprop %>% 
        html_nodes(".map-location") %>%
        html_text()
      
      # id & fecha publicacion
      data<-urlprop %>% 
        html_nodes(".info") %>%
        html_text()
      
      id<-as.numeric(gsub(data[1],pattern="\\D+",replacement = ""))
      date<-gsub(data[2],pattern="Publicada: ",replacement = "")
      
      data_all<-data.frame(id,date,precio,bedrooms,bathrooms,area_total,area_util,parking,storage,dir,sector)    
    
      #storing the data
      departamentos_arriendo<-rbind(departamentos_arriendo,data_all)
    }
  }

saveRDS(departamentos_arriendo,file = "departamentos_arriendo.rds")



