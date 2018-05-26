rm(list = ls())
#El siguiente script extaera los datos que se encuentran en el Directorio de la pagina (http://www.fisica.unam.mx/)
#Se busca guardar las fotografias y datos.
library(rvest)
#Paso 0
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

  #-Separar los datos de la columna uno y dejar especificadas las columnas.

library(stringr)
#Especificamos los valores por los que queremos separar
Columnas <- c('Departamento:', ' Cargo:', 'Teléfono:', 'Fax:', 'Intercom:', 'Ubicación:')

#Base para Separarla Inormacion
test_0 <- Listado ; colnames(test_0) <- c('X2','page')

#For que separa los datos de acuerdo al Vector Columnas
for(i in 1:6){
  eval(parse(text = paste0( 'test_',i,' <- data.frame(str_split_fixed(test_',i-1,'$X2,"',Columnas[i],'",2))')))
  }

#Creacion de la Base
Listado_F <- data.frame(Profesor = test_1$X1,
                        Departamento = test_2$X1,
                        Cargo = test_3$X1,
                        Telefono = test_4$X1,
                        Fax = test_5$X1,
                        Intercom = test_6$X1,
                        Ubicacion = test_6$X2)

rm(list=ls(pattern="^test"))

  #Imagenes
  #-Revisar la inforamcion de las imagenes.

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

url <- "http://w2.fisica.unam.mx/directory/info/search_researchers?page=34"
url_html <- read_html(url) #Lemos la info html

img_1 <- xml_attrs(xml_child(xml_child(xml_child(xml_child(xml_child(url_html, 2), 5), 1), 1), 1))
img_1 <- data.frame((t(img_1)))

f <- rbind(f, img_1)

#Unimos
Tabla_Final <- cbind(Listado_F,f)

rm(f,f_4,img_1,Listado,Listado_F,tabla,tabla_0)
