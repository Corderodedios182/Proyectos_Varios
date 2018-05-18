rm(list = ls())

#El siguiente script traira la informacion JSON  sobre las publicaciones de 1959 a 2018,que se encuentran en los servidores privados.

#install.packages("httr") ; install.packages("jsonlite")

library(httr) ; library(jsonlite)

#Funcion que me permite ver los datos que se encuentran en la URL (http://132.248.7.24:3000/web_site/user_articles.json?year=año).
#Solo es necesarion especificar el año.

Datos_JSON <- function(año){
  
  url <- paste0("http://132.248.7.24:3000/web_site/user_articles.json?year=", año) #Url que ira cambiando de acuerdo al año
  
  response <- GET(url, user_agent("Hola, Soy el chico que realiza Servicio Social con Carlos Lopez")) #Solicitamos acceso al servidor
  
  if(http_error(response)){  #if para verificar que no exista un error al momento de acceder al servidor
    
    stop("Hubo un error con la conexion.")
    
  } else { 
    
    result <- data.frame(fromJSON(content(response,as = "text")))  #Jalamos los datos JSON que nos arrojo el servidor (funcion content), pasamos a una tabla (funcion fromJSON), convertimos a data.frame.
    
    return(result) 
    
  }
}

#Base para validacion de los datos, eh encontrado 10 variables en todos los ejercicio que eh realizado
Base_F <- Datos_JSON(1959) #Tiene 2 datos y son correctos.

for (i in 1960:2018) {
  eval(parse(text = paste0('Base_',i,' <- Datos_JSON(',i,')')))
  eval(parse(text = paste0('Base_F <- rbind(Base_F,Base_',i,')')))
  eval(parse(text = paste0('rm(Base_',i,')')))
}

#Siguientes pasos; 
  # - Realizar analis y graficas de los datos.






