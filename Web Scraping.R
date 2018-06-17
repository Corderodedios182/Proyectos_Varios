rm(list = ls())
#El siguiente script extaera los datos que se encuentran en el Directorio de la pagina (http://www.fisica.unam.mx/)
#Se busca guardar las fotografias y datos.
library(rvest) ; library(tidyverse) ; library(stringr) ; require(magick) 

#C:\Users\Finanzas\Downloads\Proyectos_SS-master\Directorio
Ruta <- readline("Introduce una Ruta Donde se guardaran las Publicaciones y las Fotos : ") 
setwd(Ruta)
dir()

Fotos <- as.character(readline("Introduce el Nombre de una Carpeta para guardar solo las Fotos:"))
Fotos_Direccion <- dir.create(Fotos)

Fotos <- paste0(Ruta,'\\',Fotos)

#Paso 0 (Se descarga la Informacion de la estructura HTML)
url <- "http://w2.fisica.unam.mx/directory/info/search_researchers?page=1"
url_html <- read_html(url) #Lemos la info html

tabla_0 <- html_table(url_html) #Extraemos la informacion que se encuentra entre <table></table>
tabla_0 <- data.frame(tabla_0)

tabla_0 <- data.frame(tabla_0[,2])

tabla_0["page"] <- 1
colnames(tabla_0) <- c("info","page")
Listado <- tabla_0 #Base con los datos

#for para leer todos los datos

for (i in (2:34)){
eval(parse(text = paste0('url <- "http://w2.fisica.unam.mx/directory/info/search_researchers?page=',i,'"')))
url_html <- read_html(url)

tabla <- html_table(url_html)
tabla <- data.frame(tabla)

tabla <- data.frame(tabla[,2])
tabla["page"] <- i
colnames(tabla) <- c("info","page")

Listado <- rbind(Listado,tabla)
}

#########################################
#Limpieza y Estructura para el DataFrame#
#########################################

#Especificamos los valores por los que queremos separar
Columnas <- c('Departamento:', ' Cargo:', 'Teléfono:', 'Fax:', 'Intercom:', 'Ubicación:')

#Base para Separarla Inormacion
test_0 <- Listado ; colnames(test_0) <- c('X2','page')

#For que separa los datos de acuerdo al Vector Columnas
for(i in 1:6){
  eval(parse(text = paste0( 'test_',i,' <- data.frame(str_split_fixed(test_',i-1,'$X2,"',Columnas[i],'",2))')))
  }

#Creacion de la Base
Listado_F <- data.frame(Page = Listado$page,
                        Profesor = test_1$X1,
                        Departamento = test_2$X1,
                        Cargo = test_3$X1,
                        Telefono = trimws(test_4$X1),
                        Fax = test_5$X1,
                        Intercom = test_6$X1,
                        Ubicacion = test_6$X2)

rm(list=ls(pattern="^test"))

Listado_F <- separate(Listado_F, col = Profesor, into = c("Prof","Grado"), sep = ",") 

Listado_F$Prof <- trimws(Listado_F$Prof)
Listado_F$Grado <- trimws(Listado_F$Grado)

Listado_F <- unite(Listado_F, Profesor, Grado, Prof, sep = " ")

Listado_F$Departamento <- str_replace(Listado_F$Departamento,",","")

Listado_F$Fax <- str_replace(Listado_F$Fax,",","")

##########
#Imagenes#
##########
#Se descarga la Url de las Imagenes

url <- "http://w2.fisica.unam.mx/directory/info/search_researchers?page=1"
url_html <- read_html(url) #Lemos la info html

  for(i in 1:4) {
  eval(parse(text = paste0('img_',i,' <- xml_attrs(xml_child(xml_child(xml_child(xml_child(xml_child(url_html, 2), 5), ',i,'), 1), 1))')))
  eval(parse(text = paste0('img_',i,' <- data.frame(img_',i,')')))
  }
f <- data.frame(t(cbind(img_1,img_2,img_3,img_4)))

for(i in 2:33){
eval(parse(text = paste0('url <- "http://w2.fisica.unam.mx/directory/info/search_researchers?page=',i,'"')))
url_html <- read_html(url) #Lemos la info html

for(i in 1:4) {
  eval(parse(text = paste0('img_',i,' <- xml_attrs(xml_child(xml_child(xml_child(xml_child(xml_child(url_html, 2), 5), ',i,'), 1), 1))')))
  eval(parse(text = paste0('img_',i,' <- data.frame(img_',i,')')))
}

eval(parse(text = paste0('f_',i,' <- data.frame(t(cbind(img_1,img_2,img_3,img_4)))')))

eval(parse(text = paste0('f <- rbind(f,f_',i,')')))
  }
    
rm(list=ls(pattern="^img_"))

#Jalar ultimas imagenes y colocar bien la URL

url <- "http://w2.fisica.unam.mx/directory/info/search_researchers?page=33"
url_html <- read_html(url) #Lemos la info html

img_1 <- xml_attrs(xml_child(xml_child(xml_child(xml_child(xml_child(url_html, 2), 5), 1), 1), 1))
img_1 <- data.frame((t(img_1)))

f <- rbind(f, img_1)
f <- data.frame(Imagen_Url = paste0("http://w2.fisica.unam.mx",f$src))

#Unimos Directorio con las Imagenes
Tabla_Final <- cbind(Listado_F,Imagen_url = f[,"Imagen_Url"])

rm(f,f_4,img_1,Listado,Listado_F,tabla,tabla_0)

Tabla_Final <- data.frame(Tabla_Final)

Test <- Tabla_Final$Profesor
Test <- data.frame(Profesor = trimws(strsplit(Test,split = " ")))
Test$Profesor <- gsub("\"","",Test$Profesor)
Test$Profesor <- gsub(",","",Test$Profesor)
Test$Profesor <- gsub("c\\(","",Test$Profesor)
Test$Profesor <- gsub(")","",Test$Profesor)

Tabla_Final$Profesor <- Test$Profesor

write.csv(Tabla_Final,"Tabla_final.csv",row.names = FALSE)

#Descarga de las Imagenes a una carpeta
setwd(Fotos)

Profesor <- paste0(Tabla_Final[,1]," ",Tabla_Final[,2]) #Fijos
links <- Tabla_Final[,9] #Fijos

for(i in 1:40){
eval(parse(text = paste0('Imagen_',i,' <- image_read(as.character(links[',i,']))'))) # Ciclo
eval(parse(text = paste0('image_write(Imagen_',i,',paste0(as.character(Profesor[',i,']),".jpg"))'))) #Ciclo
eval(parse(text = paste0('rm(Imagen_',i,')')))
}

#No se abre la conexion y no puedo descargar esta imagen :(
#Imagen_41 <- image_read(as.character(links[41]),strip = 1) # Ciclo
#image_write(Imagen_41,paste0(as.character(Profesor[41]),".jpg"))
#print(Imagen_41)  

for(i in 42:129){
  eval(parse(text = paste0('Imagen_',i,' <- image_read(as.character(links[',i,']))'))) # Ciclo
  eval(parse(text = paste0('image_write(Imagen_',i,',paste0(as.character(Profesor[',i,']),".jpg"))'))) #Ciclo
  eval(parse(text = paste0('rm(Imagen_',i,')')))
}

