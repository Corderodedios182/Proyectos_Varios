#Paqueterias a Utilizar
list.of.packages <- c("httr","jsonlite","xml2","dplyr","tidyr","lubridate")

new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]

if(length(new.packages) != 0){
  install.packages(new.packages)
} else if (!is.logical(length(new.packages) != 0 & (new.packages %in% "sweetalertR"))){
  devtools::install_github("timelyportfolio/sweetalertR") 
}

#lectura de multiples paqueterias
lapply(list.of.packages, require, character.only = TRUE)

#############
#Brand Index#
#############

Login <- POST("https://api.brandindex.com/v0/login?email=nalleli.torres@omd.com&password=data1046")

names(Login)
Login$url

#Contenido de la llamada Login

texto <- content(Login, as = "text", encoding = "UTF-8")
texto

texto_parsed <- content(Login, as = "parsed")
texto_parsed
names(texto_parsed)

##################
#Company Defaults#
##################

Company_Defaults <- GET("https://api.brandindex.com/v0/company/defaults")

names(Company_Defaults)
Company_Defaults$url

#Contenido de la llamada Company Defaults

texto_company <- content(Company_Defaults, as = "text", encoding = "UTF-8")
texto_company

texto_parsed_company <- content(Company_Defaults, as = "parsed")
texto_parsed_company
names(texto_parsed_company)

texto_parsed_company$data[[1]]






























