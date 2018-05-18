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

#Siguientes pasos;
 #-Separar los datos de la columna uno y dejar especificadas las columnas.
 #-Revisar la inforamcion de las imagenes.
