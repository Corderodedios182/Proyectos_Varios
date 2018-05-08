rm(list = ls())

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
Base <- Datos_JSON(1959) #Tiene 2 datos y son correctos.
Base <- Datos_JSON(2010) #Tiene 1 datos y es correcto.
Base <- Datos_JSON(2015) #Tiene 205 datos y son correctos.


#Siguientes pasos; 
  # - Ir por todas las Base de 1959 a 2018.
  # - Validar que todas tengan el mismo formato.
  # - Unir las bases de datos en una mismas.
  # - Realizar analis y graficas de los datos.






