library(readxl)
rm(list = ls())
setwd("C:/Users/Carlos.Flores/Documents/Entregables_Urgentes/Qualitas_Adcuality")
#getwd()
#dir()
#version
#system.file()
#Sys.info()
#sys.
carpeta = list.files( paste(getwd(),sep=""))

base = matrix(nrow = 0,ncol = 7)

for (i in seq(length(list.files( paste(getwd(),sep=""))))) {
  print(i)
  archivos = list.files( paste(getwd(),'/',carpeta[i],sep=""))
  for (j in seq(list.files( paste(getwd(),'/',carpeta[i],sep="")))) {
    for (k in seq(excel_sheets(paste(getwd(),'/',carpeta[i],'/',archivos[j],sep="")))) {
      print(c(j,k))
      print(paste('Carpeta: ',carpeta[i],'Archivo: ',archivos[j],'Hoja: ',excel_sheets(paste(getwd(),'/',carpeta[i],'/',archivos[j],sep=""))[k],'Archivo: ', archivos[j]))
      x = as.matrix(read_excel(paste(getwd(),'/',carpeta[i],'/',list.files( paste(getwd(),'/',carpeta[i],sep=""))[j],sep = ""), sheet = excel_sheets(paste(getwd(),'/',carpeta[i],'/',archivos[j],sep=""))[k]))
      f = substr(as.character(data.frame(na.omit(x[,6]))[2,1]),6,16)
      y = cbind(na.omit(x[1:dim(x)[1],2:4]),colnames(x)[2],f,carpeta[i],archivos[j])
      colnames(y) = c('V1','V2','V3','V4','V5','V6','V7')
      base = rbind(base,y)
    } 
  }
}

colnames(base) = c('Medida','Inversión','Impresiones','Hoja','Fecha','Marca','Archivo')

base = data.frame(base)

base[,5] = as.Date(paste(substr(base[,5],2,5),'/',substr(base[,5],7,8),'/',substr(base[,5],10,11), sep = ""))

View(base)

write.csv(base,file = paste(getwd(),'/','Seguros_Top10_17Y18.CSV',sep = ""),row.names = F)














