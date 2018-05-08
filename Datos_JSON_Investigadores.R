rm(list = ls())

#install.packages("httr") ; install.packages("jsonlite")

library(httr) ; library(jsonlite)

#Funcion que me permite ver los datos que se encuentran en la URL (http://132.248.7.24:3000/web_site/user_articles.json?year=) especificando el año.
#Pasando de formato JSON a DataFrame.

Datos_JSON <- function(año){
  
  url <- paste0("http://132.248.7.24:3000/web_site/user_articles.json?year=", año) 
  
  response <- GET(url, user_agent("Hola, Soy el chico que realiza Servicio Social con Carlos Lopez"))
  
  if(http_error(response)){ 
    
    stop("Hubo un error con la conexion.")
    
  } else { 
    
    result <- fromJSON(content(response,as = "text")) 
    
    return(result) 
    
  }
}

#Base para validacion de los datos
Base <- data.frame(Datos_JSON(1959)) #Tiene 2 datos y son correctos.
Base <- data.frame(Datos_JSON(2010)) #Tiene 1 datos y es correcto.
Base <- data.frame(Datos_JSON(2015)) #Tiene 205 datos y son correctos.







