################################ API de Brand Index ##############################
# Autor: Luis Fernando Torres Pineda                                             #
# Fecha: 07 de enero 2019                                                        #
##################################################################################

#Paqueterias a Utilizar

list.of.packages <- c("httr","jsonlite","xml2","dplyr","tidyr","lubridate","string","rjson","RODBC")

new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]

if(length(new.packages) != 0){
  install.packages(new.packages)
} else if (!is.logical(length(new.packages) != 0 & (new.packages %in% "sweetalertR"))){
  devtools::install_github("timelyportfolio/sweetalertR") 
}

#lectura de multiples paqueterias
lapply(list.of.packages, require, character.only = TRUE)

#con<-odbcConnect("SQLSERVER-MX")


API_Bran_Index <- function(){
  
  Region = 'mx';Sector =  "Automotive";Sector1 = 'Technology and Telecom'; Scoring = 'total'#; inicio = "2017-01-01"; fin = "2017-12-01"
  
  # Fechas
  
  
  if(substring(wday(Sys.Date(), label = TRUE),1,3) == 'lun' ){
    inicio <- c(Sys.Date()-10)
    fin <- c(Sys.Date()-4)
  }else if(substring(wday(Sys.Date(), label = TRUE),1,3) != 'lun'){
    dias <- data.frame(matrix(nrow = 7, ncol = 2))
    dias[,1] <- substring(data.frame(table(wday(Sys.Date(),label = T)))[,1],1,3)
    dias[8,] <- 'dom'
    dias <- dias[-1,]
    dias[,2] <- seq(dim(dias)[1])
    row.names(dias) <- seq(dim(dias)[1])
    contador <- dias[as.numeric(row.names(dias)[is.element(dias[,1],substring(wday(Sys.Date(), label = TRUE),1,3))]),2]
    inicio <- Sys.Date()-((contador-1)+10)
    fin <- Sys.Date()-((contador-1)+4)
  }
  
  
  
  #############
  #Brand Index#
  #############
  
  # Entramos a la página de Brand Index
  
  #Login <- POST("https://api.brandindex.com/v0/login?email=nalleli.torres@omd.com&password=data1046")
  Login <- POST("https://api.brandindex.com/v0/login?email=Sergio.diaz@omnicommediagroup.com&password=Omnicom2018")
  
  texto_parsed <- content(Login, as = "parsed")
  
  
  ##################
  #Company Defaults#
  ##################
  
  Company_Defaults <- GET(texto_parsed$data[[1]])
  
  texto_parsed_company <- content(Company_Defaults, as = "parsed")
  
  
  ##################
  #Company Regions#
  ##################
  
  Company_Regions <- GET(texto_parsed_company$data$regions)
  
  texto_parsed_company_regions <- content(Company_Regions, as = "parsed")
  
  catalogo_regions <-data.frame(matrix(nrow = 0,ncol = 2)) 
  
  for (i in seq(length(texto_parsed_company_regions$data))) {
    catalogo_regions[i,1] <- names(texto_parsed_company_regions$data[i])
    catalogo_regions[i,2] <- texto_parsed_company_regions$data[[i]][1]
    
  }
  
  colnames(catalogo_regions) <- c('Id','URL')
  
  
  
  
  ##################
  #Company Sectors#
  ##################
  
  Company_Sectors <- GET(texto_parsed_company_regions$data[[as.numeric(row.names(catalogo_regions)[is.element(catalogo_regions[,1],Region)])]])
  
  texto_parsed_company_sectors <- content(Company_Sectors, as = "parsed")
  
  catalogo_sectors <-data.frame(matrix(nrow = 0,ncol = 2))
  
  for (i in seq(length(texto_parsed_company_sectors$data))) {
    j <- (dim(catalogo_sectors)[1]+1):(dim(catalogo_sectors)[1]+dim(t(data.frame(texto_parsed_company_sectors$data[i])))[1])
    catalogo_sectors[j,1] <- names(texto_parsed_company_sectors$data[i])
    catalogo_sectors[j,2] <- as.character(t(data.frame(texto_parsed_company_sectors$data[i]))[,1])
  }
  
  
  texto_parsed_company_sectors$data[[length(unique(catalogo_sectors[1:as.numeric(row.names(catalogo_sectors)[is.element(catalogo_sectors[,2],Sector)]),1]))]]
  
  
  ########################
  #Company Custom Sectors#
  ########################
  
  Company_Custom_Sectors <- GET(paste("https://api.brandindex.com/v0/company/custom-sectors?region=",Region,sep = ""))
  
  texto_parsed_company_custom_sectors <- content(Company_Custom_Sectors, as = "parsed")
  
  catalogo_custom_sectors <-data.frame(matrix(nrow = 0,ncol = 2)) 
  
  for (i in seq(length(texto_parsed_company_custom_sectors$data))) {
    j <- (dim(catalogo_custom_sectors)[1]+1):(dim(catalogo_custom_sectors)[1]+dim(t(data.frame(texto_parsed_company_custom_sectors$data[i])))[1])
    catalogo_custom_sectors[j,1] <- names(texto_parsed_company_custom_sectors$data[i])
    catalogo_custom_sectors[j,2] <- as.character(t(data.frame(texto_parsed_company_custom_sectors$data[i]))[,1])
  }
  
  
  
  ########################
  #Company Brand Sectors#
  ########################
  
  
  Company_Brand_Sectors <- GET(paste("https://api.brandindex.com/v0/company/brands?sector","=",as.numeric(catalogo_sectors[as.numeric(row.names(catalogo_sectors)[is.element(catalogo_sectors[,2],Sector)]),1]),"&region=",Region,sep = ""))
  
  Company_Brand_Sectors_att <- GET(paste("https://api.brandindex.com/v0/company/brands?sector","=",as.numeric(catalogo_sectors[as.numeric(row.names(catalogo_sectors)[is.element(catalogo_sectors[,2],Sector1)]),1]),"&region=",Region,sep = ""))
  

  texto_parsed_company_brand_sectors <- content(Company_Brand_Sectors, as = "parsed")

  texto_parsed_company_brand_sectors_att <- content(Company_Brand_Sectors_att, as = "parsed")
    
  catalogo_brand_sectors <-data.frame(matrix(nrow = 0,ncol = 3)) 
  
  catalogo_brand_sectors_att <-data.frame(matrix(nrow = 0,ncol = 3)) 
  
  for (i in seq(length(texto_parsed_company_brand_sectors$data))) {
    j <- (dim(catalogo_brand_sectors)[1]+1):(dim(catalogo_brand_sectors)[1]+dim(t(data.frame(texto_parsed_company_brand_sectors$data[i])))[1])
    catalogo_brand_sectors[j,1] <- names(texto_parsed_company_brand_sectors$data[i])
    catalogo_brand_sectors[j,2] <- names(texto_parsed_company_brand_sectors$data[i][[1]])
    catalogo_brand_sectors[j,3] <- as.character(t(data.frame(texto_parsed_company_brand_sectors$data[i]))[,1])
  }
  
  
  for (i in seq(length(texto_parsed_company_brand_sectors_att$data))) {
    j <- (dim(catalogo_brand_sectors_att)[1]+1):(dim(catalogo_brand_sectors_att)[1]+dim(t(data.frame(texto_parsed_company_brand_sectors_att$data[i])))[1])
    catalogo_brand_sectors_att[j,1] <- names(texto_parsed_company_brand_sectors_att$data[i])
    catalogo_brand_sectors_att[j,2] <- names(texto_parsed_company_brand_sectors_att$data[i][[1]])
    catalogo_brand_sectors_att[j,3] <- as.character(t(data.frame(texto_parsed_company_brand_sectors_att$data[i]))[,1])
  }
  
  colnames(catalogo_brand_sectors) <- c('Id','Variable','Value')
  
  colnames(catalogo_brand_sectors_att) <- c('Id','Variable','Value')
  
  
  ##############################
  #Company Brand Custom Sectors# #Sector Financiero
  ##############################
  
  Company_Brand_Custom_Sectors <- GET("https://api.brandindex.com/v0/company/brands?custom_sector=-162&region=mx")
  
  texto_parsed_company_brand_custom_sectors <- content(Company_Brand_Custom_Sectors, as = "parsed")
  
  catalogo_brand_custom_sectors <-data.frame(matrix(nrow = 0,ncol = 3))
  
  for (i in seq(length(texto_parsed_company_brand_custom_sectors$data))) {
    j <- (dim(catalogo_brand_custom_sectors)[1]+1):(dim(catalogo_brand_custom_sectors)[1]+dim(t(data.frame(texto_parsed_company_brand_custom_sectors$data[i])))[1])
    catalogo_brand_custom_sectors[j,1] <- names(texto_parsed_company_brand_custom_sectors$data[i])
    catalogo_brand_custom_sectors[j,2] <- names(texto_parsed_company_brand_custom_sectors$data[i][[1]])
    catalogo_brand_custom_sectors[j,3] <- as.character(t(data.frame(texto_parsed_company_brand_custom_sectors$data[i]))[,1])
    
  }
  
  colnames(catalogo_brand_custom_sectors) <- c('Id','Variable','Value') 
  
  ##############################
  # Demographic filters        #
  ##############################
  
  
  Demographic_Filters <- GET(paste("https://api.brandindex.com/v0/filters/demos?region=",Region,sep = ""))
  
  texto_parsed_demographics_filters <- content(Demographic_Filters, as = "parsed")
  
  catalogo_demographic_filters <-data.frame(matrix(nrow = 0,ncol = 5)) 
  
  for (i in seq(length(texto_parsed_demographics_filters$data))) {
    j <- (dim(catalogo_demographic_filters)[1]+1):(dim(catalogo_demographic_filters)[1]+dim(t(data.frame(texto_parsed_demographics_filters$data[i])))[1])
    catalogo_demographic_filters[j,1] <- names(texto_parsed_demographics_filters$data[i])
    catalogo_demographic_filters[j,2] <- names(texto_parsed_demographics_filters$data[i][[1]])
    catalogo_demographic_filters[j,3] <- as.character(t(data.frame(texto_parsed_demographics_filters$data[i]))[,1])
  }
  
  
  meta <- data.frame(matrix(nrow = 0, ncol = 2))
  
  for (i in seq(length(texto_parsed_demographics_filters$meta))) {
    meta[i,1] <- names(texto_parsed_demographics_filters$meta)[i]
    meta[i,2] <- texto_parsed_demographics_filters$meta[i]
  }
  
  
  for (i in seq(length(texto_parsed_demographics_filters$data))) {
    catalogo_demographic_filters[catalogo_demographic_filters[,1] == names(texto_parsed_demographics_filters$data)[i],4:5] <- meta[meta[,1] == names(texto_parsed_demographics_filters$data)[i],]
  }
  
  
  
  
  
  #############################
  # Demographic filter        #
  #############################
  
  catalogo_demographic_filter <-data.frame(matrix(nrow = 0,ncol = 4)) 
  
  for (k in seq(length(unique(catalogo_demographic_filters[,1])))) {
    
    Demographic_Filter <- GET(paste("https://api.brandindex.com/v0/filters/demo?region=",Region,"&sector=",as.numeric(catalogo_sectors[as.numeric(rownames(catalogo_sectors)[is.element(catalogo_sectors[,2],Sector)]),1]),"&name=",unique(catalogo_demographic_filters[,1])[k],sep = ""))
    
    texto_parsed_demographics_filter <- content(Demographic_Filter, as = "parsed")
    
    for (i in seq(length(texto_parsed_demographics_filter$data))) {
      j <- (dim(catalogo_demographic_filter)[1]+1):(dim(catalogo_demographic_filter)[1]+dim(t(data.frame(texto_parsed_demographics_filter$data[i])))[1])
      catalogo_demographic_filter[j,1] <- names(texto_parsed_demographics_filter$data[i])
      if( length(names(texto_parsed_demographics_filter$data[i][[1]])) == 0){
        catalogo_demographic_filter[j,2] <- 0
      }else if( length(names(texto_parsed_demographics_filter$data[i][[1]])) != 0){
        catalogo_demographic_filter[j,2] <- names(texto_parsed_demographics_filter$data[i][[1]])
      }
      
      catalogo_demographic_filter[j,3] <- as.character(t(data.frame(texto_parsed_demographics_filter$data[i]))[,1])
      catalogo_demographic_filter[j,4] <- unique(catalogo_demographic_filters[,1])[k]
    }
    
  }
  
  colnames(catalogo_demographic_filter) <- c('Variables','Value')
  
  
  
  #########################
  # Metric filters        #
  #########################
  
  
  
  Metric_Filters <- GET("https://api.brandindex.com/v0/filters/metrics")
  
  texto_parsed_metric_filters <- content(Metric_Filters, as = "parsed")
  
  catalogo_metric_filters <-data.frame(matrix(nrow = 0,ncol = 5)) 
  
  for (i in seq(length(texto_parsed_metric_filters$data))) {
    j <- (dim(catalogo_metric_filters)[1]+1):(dim(catalogo_metric_filters)[1]+dim(t(data.frame(texto_parsed_metric_filters$data[i])))[1])
    catalogo_metric_filters[j,1] <- names(texto_parsed_metric_filters$data[i])
    catalogo_metric_filters[j,2] <- names(texto_parsed_metric_filters$data[i][[1]])
    catalogo_metric_filters[j,3] <- as.character(t(data.frame(texto_parsed_metric_filters$data[i]))[,1])
  }
  
  meta <- data.frame(matrix(nrow = 0, ncol = 2))
  
  for (i in seq(length(texto_parsed_metric_filters$meta))) {
    meta[i,1] <- names(texto_parsed_metric_filters$meta)[i]
    meta[i,2] <- texto_parsed_metric_filters$meta[i]
  }
  
  
  for (i in seq(length(texto_parsed_metric_filters$data))) {
    catalogo_metric_filters[catalogo_metric_filters[,1] == names(texto_parsed_metric_filters$data)[i],4:5] <- meta[meta[,1] == names(texto_parsed_metric_filters$data)[i],]
  }
  
  
  
  
  ############################
  # Composite filters        #
  ############################
  
  
  Composite_Filters <- GET(paste("https://api.brandindex.com/v0/filters/composite-filters?region=",Region,sep = ""))
  
  texto_parsed_composite_filters <- content(Composite_Filters, as = "parsed")
  
  catalogo_composite_filters <-data.frame(matrix(nrow = 0,ncol = 3)) 
  
  for (i in seq(length(texto_parsed_composite_filters$data))) {
    j <- (dim(catalogo_composite_filters)[1]+1):(dim(catalogo_composite_filters)[1]+dim(t(data.frame(texto_parsed_composite_filters$data[i])))[1])
    catalogo_composite_filters[j,1] <- names(texto_parsed_composite_filters$data[i])
    catalogo_composite_filters[j,2] <- names(texto_parsed_composite_filters$data[i][[1]])
    catalogo_composite_filters[j,3] <- as.character(t(data.frame(texto_parsed_composite_filters$data[i]))[,1])
  }
  
  
  
  ############################
  # Composite filter        #
  ############################
  
  
  Composite_Filter <- GET("https://api.brandindex.com/v0/filters/composite-filter?id=59977cde29ebcd00037629bd")
  
  texto_parsed_composite_filter <- content(Composite_Filter, as = "parsed")
  
  
  #######################
  # Metrics data        #
  #######################
  
  
  Metrics_Data <- GET("https://api.brandindex.com/v0/metrics")
  
  texto_parsed_metric_data <- content(Metrics_Data, as = "parsed")
  
  catalogo_metrics_data <-data.frame(matrix(nrow = 0,ncol = 2)) 
  
  for (i in seq(length(texto_parsed_metric_data$data))) {
    j <- (dim(catalogo_metrics_data)[1]+1):(dim(catalogo_metrics_data)[1]+dim(t(data.frame(texto_parsed_metric_data$data[i])))[1])
    catalogo_metrics_data[j,1] <- names(texto_parsed_metric_data$data[i])
    #catalogo_composite_filter[j,2] <- names(texto_parsed_composite_filter[2][[1]][[1]])
    catalogo_metrics_data[j,2] <- as.character(t(data.frame(texto_parsed_metric_data$data[i]))[,1])
  }
  
  
  colnames(catalogo_metrics_data) <- c('Variable','Value')
  
  
  
  #######################
  # Timeline brands     #
  #######################
  
  #catalogo_metric_filters
 
  
  
  
  
  #if(day(Sys.Date()) != 30 | day(Sys.Date()) != 31){
  #  if(substring(wday(Sys.Date(), label = TRUE),1,3) != 'lun' ){
  #    inicio <-  Sys.Date()-60
  #    fin <-  Sys.Date()-30
  #  }else if(substring(wday(Sys.Date(), label = TRUE),1,3) == 'lun' ){
  #    inicio <- c(Sys.Date()-67,Sys.Date())
  #    fin <- c(Sys.Date()-30,Sys.Date())
  #  }
  #}else if(day(Sys.Date()) == 30 | day(Sys.Date()) == 31){
  #  if(substring(wday(Sys.Date(), label = TRUE),1,3) != 'lun' ){
  #   inicio <-  Sys.Date()-60
  #    fin <-  Sys.Date()-30
  #  }else if(substring(wday(Sys.Date(), label = TRUE),1,3) == 'lun' ){
  #    inicio <- c(Sys.Date()-60,Sys.Date())
  #    fin <- c(Sys.Date()-30,Sys.Date())
  #  }
  #}
  
  catalogo <- catalogo_demographic_filter[catalogo_demographic_filter[,1] == "codes",]
  catalogo <- catalogo[catalogo[,1] == 'codes',]
  metricas <- c("age4_mx","agegranular","gender","income_mx")
  row.names(catalogo) <- 1:dim(catalogo)[1]
  catalogo <- catalogo[as.numeric(rownames(catalogo)[is.element(catalogo[,4],metricas)]),]
  row.names(catalogo) <- 1:dim(catalogo)[1]
  
  catalogo_att <- catalogo
  catalogo_renault <- catalogo
  
  income <- catalogo[catalogo[,4] == 'income_mx',2:4]
  income[,3] <- c('E','D','C','C+','B','A','NA')
  rownames(income) <- 1:dim(income)[1]
  
  catalogo_marca <- catalogo_brand_sectors[catalogo_brand_sectors[,2] == 'name',]
  row.names(catalogo_marca) <- 1:dim(catalogo_marca)[1]
  catalogo_marca <- catalogo_marca[-c(25,30),]
  row.names(catalogo_marca) <- 1:dim(catalogo_marca)[1]
  
  modelos <- data.frame(matrix(nrow = dim(catalogo)[1],ncol = 12))
  catalogo <- cbind(catalogo,modelos)
  
  att <- data.frame(matrix(nrow = dim(catalogo_att)[1], ncol = 6))
  catalogo_att <- cbind(catalogo_att,att)
  
  renault <- data.frame(matrix(nrow = dim(catalogo_renault)[1], ncol = 6))
  catalogo_renault <- cbind(catalogo_renault,renault)
  
  catalogo_marca_att <- catalogo_brand_sectors_att[catalogo_brand_sectors_att[,2] == 'name',]
  row.names(catalogo_marca_att) <- 1:dim(catalogo_marca_att)[1]
  catalogo_marca_att <- catalogo_marca_att[-c(25,30),]
  row.names(catalogo_marca_att) <- 1:dim(catalogo_marca_att)[1]
  
 
  
  MARCH <- c(18:49,income[as.numeric(row.names(income)[is.element(income[,3],c('A','B','C','C+'))]),2],c('Male','Female'))
  KICKS <- c(25:50,income[as.numeric(row.names(income)[is.element(income[,3],c('A','B','C','C+'))]),2],c('Male','Female'))
  LEAF <- c(30:40,income[as.numeric(row.names(income)[is.element(income[,3],c('A','B'))]),2],c('Male','Female'))
  VERSA <- c(25:35,income[as.numeric(row.names(income)[is.element(income[,3],c('A','B','C','C+'))]),2],c('Male','Female'))
  SENTRA <- c(25:45,income[as.numeric(row.names(income)[is.element(income[,3],c('A','B','C','C+'))]),2],c('Male','Female'))
  MAXIMA <- c(39:49,income[as.numeric(row.names(income)[is.element(income[,3],c('A','B','C+'))]),2],c('Male'))
  ALTIMA <- c(30:40,income[as.numeric(row.names(income)[is.element(income[,3],c('A','B'))]),2],c('Male'))
  XTRAIL <- c(25:45,income[as.numeric(row.names(income)[is.element(income[,3],c('A','B','C','C+'))]),2],c('Male','Female'))
  MURANO <- c(45:55,income[as.numeric(row.names(income)[is.element(income[,3],c('A','B'))]),2],c('Male','Female'))
  PATHFINDER <- c(35:40,income[as.numeric(row.names(income)[is.element(income[,3],c('A','B','C+'))]),2],c('Male','Female'))
  FRONTIER <- c(19:55,income[as.numeric(row.names(income)[is.element(income[,3],c('A','B','C','C+'))]),2],c('Male','Female'))
  MASIVO <- c(c(18:79,'80+'),income[as.numeric(row.names(income)[is.element(income[,3],c('A','B','C','C+','D','E','NA'))]),2],c('Male','Female'))
  POSTPAGO <- c(18:45,income[as.numeric(row.names(income)[is.element(income[,3],c('A','B','C','C+'))]),2],c('Male','Female'))
  PREPAGO <- c(15:55,income[as.numeric(row.names(income)[is.element(income[,3],c('C','C+'))]),2],c('Male','Female'))
  UNEFON <- c(15:55,income[as.numeric(row.names(income)[is.element(income[,3],c('C','D'))]),2],c('Male','Female'))
  MASIVO_ATT <- c(c(18:79,'80+'),income[as.numeric(row.names(income)[is.element(income[,3],c('A','B','C','C+','D','E','NA'))]),2],c('Male','Female'))
  CURRENT_MASIVO <- c(c(18:79,'80+'),income[as.numeric(row.names(income)[is.element(income[,3],c('A','B','C','C+','D','E','NA'))]),2],c('Male','Female'))
  NON_CURRENT_MASIVO <- c(c(18:79,'80+'),income[as.numeric(row.names(income)[is.element(income[,3],c('A','B','C','C+','D','E','NA'))]),2],c('Male','Female'))
  #MASIVO_RENAULT <- c(c(19:44),income[as.numeric(row.names(income)[is.element(income[,3],c('A','B','C','C+'))]),2],c('Male','Female')) 
  #STEPWAY <- c(c(25:34),income[as.numeric(row.names(income)[is.element(income[,3],c('A','B','C','C+'))]),2],c('Male','Female'))
  #DUSTER <- c(c(35:54),income[as.numeric(row.names(income)[is.element(income[,3],c('A','B','C','C+'))]),2],c('Male','Female'))
  #CAPTUR <- c(c(25:54),income[as.numeric(row.names(income)[is.element(income[,3],c('A','B','C+'))]),2],c('Male','Female'))
  #KOLEOS <- c(c(35:54),income[as.numeric(row.names(income)[is.element(income[,3],c('A','B','C+'))]),2],c('Male','Female'))
  #OROCH <- c(c(35:54),income[as.numeric(row.names(income)[is.element(income[,3],c('A','B','C','C+'))]),2],c('Male','Female'))
  
  
  catalogo[as.numeric(rownames(catalogo)[is.element(catalogo[,3],MARCH)]),5] <- 'MARCH'
  catalogo[as.numeric(rownames(catalogo)[is.element(catalogo[,3],KICKS)]),6] <- 'KICKS'
  catalogo[as.numeric(rownames(catalogo)[is.element(catalogo[,3],LEAF)]),7] <- 'LEAF'
  catalogo[as.numeric(rownames(catalogo)[is.element(catalogo[,3],VERSA)]),8] <- 'VERSA'
  catalogo[as.numeric(rownames(catalogo)[is.element(catalogo[,3],SENTRA)]),9] <- 'SENTRA'
  catalogo[as.numeric(rownames(catalogo)[is.element(catalogo[,3],MAXIMA)]),10] <- 'MAXIMA'
  catalogo[as.numeric(rownames(catalogo)[is.element(catalogo[,3],ALTIMA)]),11] <- 'ALTIMA'
  catalogo[as.numeric(rownames(catalogo)[is.element(catalogo[,3],XTRAIL)]),12] <- 'X-TRAIL'
  catalogo[as.numeric(rownames(catalogo)[is.element(catalogo[,3],MURANO)]),13] <- 'MURANO'
  catalogo[as.numeric(rownames(catalogo)[is.element(catalogo[,3],PATHFINDER)]),14] <- 'PATHFINDER'
  catalogo[as.numeric(rownames(catalogo)[is.element(catalogo[,3],FRONTIER)]),15] <- 'FRONTIER'
  catalogo[as.numeric(rownames(catalogo)[is.element(catalogo[,3],MASIVO)]),16] <- 'MASIVO'
  catalogo_att[as.numeric(rownames(catalogo_att)[is.element(catalogo_att[,3],POSTPAGO)]),5] <- 'POSTPAGO' 
  catalogo_att[as.numeric(rownames(catalogo_att)[is.element(catalogo_att[,3],PREPAGO)]),6] <- 'PREPAGO'
  catalogo_att[as.numeric(rownames(catalogo_att)[is.element(catalogo_att[,3],UNEFON)]),7] <- 'UNEFON'
  catalogo_att[as.numeric(rownames(catalogo_att)[is.element(catalogo_att[,3],MASIVO_ATT)]),8] <- 'MASIVO_ATT'
  catalogo_att[as.numeric(rownames(catalogo_att)[is.element(catalogo_att[,3],CURRENT_MASIVO)]),9] <- 'CURRENT MASIVO' 
  catalogo_att[as.numeric(rownames(catalogo_att)[is.element(catalogo_att[,3],NON_CURRENT_MASIVO)]),10] <- 'NON_CURRENT_MASIVO' 
  #catalogo_renault[as.numeric(rownames(catalogo_renault)[is.element(catalogo_renault[,3],MASIVO_RENAULT)]),5] <- 'MASIVO_RENAULT' 
  #catalogo_renault[as.numeric(rownames(catalogo_renault)[is.element(catalogo_renault[,3],STEPWAY)]),6] <- 'STEPWAY' 
  #catalogo_renault[as.numeric(rownames(catalogo_renault)[is.element(catalogo_renault[,3],DUSTER)]),7] <- 'DUSTER'
  #catalogo_renault[as.numeric(rownames(catalogo_renault)[is.element(catalogo_renault[,3],CAPTUR)]),8] <- 'CAPTUR' 
  #catalogo_renault[as.numeric(rownames(catalogo_renault)[is.element(catalogo_renault[,3],KOLEOS)]),9] <- 'KOLEOS'
  #catalogo_renault[as.numeric(rownames(catalogo_renault)[is.element(catalogo_renault[,3],OROCH)]),10] <- 'OROCH' 
  
  scoring <- Scoring
  sector <- as.numeric(catalogo_sectors[row.names(catalogo_sectors)[is.element(catalogo_sectors[,2],Sector)],1])
  sector_att <- as.numeric(catalogo_sectors[row.names(catalogo_sectors)[is.element(catalogo_sectors[,2],Sector1)],1])
  
  # Espezamos a generar la url empezando por los demográficos
  #if(length(as.numeric(na.omit(catalogo[,c(1:4,i)])[na.omit(catalogo[,c(1:4,i)])[,4] == unique(na.omit(catalogo[,c(1:4,i)])[,4])[j],2]))==1){length(as.numeric(na.omit(catalogo[,c(1:4,i)])[na.omit(catalogo[,c(1:4,i)])[,4] == unique(na.omit(catalogo[,c(1:4,i)])[,4])[j],2]))}else if(length(as.numeric(na.omit(catalogo[,c(1:4,i)])[na.omit(catalogo[,c(1:4,i)])[,4] == unique(na.omit(catalogo[,c(1:4,i)])[,4])[j],2]))>1){(length(as.numeric(na.omit(catalogo[,c(1:4,i)])[na.omit(catalogo[,c(1:4,i)])[,4] == unique(na.omit(catalogo[,c(1:4,i)])[,4])[j],2]))-1)}
  
  url_demo <- data.frame(matrix(nrow = 0,ncol = 2))
  
  for (i in 5:dim(catalogo)[2]) {
    for (j in 1:length(unique(na.omit(catalogo[,c(1:4,i)])[,4]))) {
      demo_id <-""
      
      if(length(as.numeric(na.omit(catalogo[,c(1:4,i)])[na.omit(catalogo[,c(1:4,i)])[,4] == unique(na.omit(catalogo[,c(1:4,i)])[,4])[j],2]))==1){
        for (k in 1:if(length(as.numeric(na.omit(catalogo[,c(1:4,i)])[na.omit(catalogo[,c(1:4,i)])[,4] == unique(na.omit(catalogo[,c(1:4,i)])[,4])[j],2]))==1){length(as.numeric(na.omit(catalogo[,c(1:4,i)])[na.omit(catalogo[,c(1:4,i)])[,4] == unique(na.omit(catalogo[,c(1:4,i)])[,4])[j],2]))}else if(length(as.numeric(na.omit(catalogo[,c(1:4,i)])[na.omit(catalogo[,c(1:4,i)])[,4] == unique(na.omit(catalogo[,c(1:4,i)])[,4])[j],2]))>1){(length(as.numeric(na.omit(catalogo[,c(1:4,i)])[na.omit(catalogo[,c(1:4,i)])[,4] == unique(na.omit(catalogo[,c(1:4,i)])[,4])[j],2]))-1)}) {
          demo_id <- paste(demo_id,as.numeric(na.omit(catalogo[,c(1:4,i)])[na.omit(catalogo[,c(1:4,i)])[,4] == unique(na.omit(catalogo[,c(1:4,i)])[,4])[j],2])[k],sep = "")
        }
        
        url_demo[dim(url_demo)[1]+1,1:2] <- c(paste(unique(na.omit(catalogo[,c(1:4,i)])[,4])[j],".",demo_id,sep = ""),unique(na.omit(catalogo[,c(1:4,i)])[na.omit(catalogo[,c(1:4,i)])[,4] == unique(na.omit(catalogo[,c(1:4,i)])[,4])[j],5]))
        
      }else if(length(as.numeric(na.omit(catalogo[,c(1:4,i)])[na.omit(catalogo[,c(1:4,i)])[,4] == unique(na.omit(catalogo[,c(1:4,i)])[,4])[j],2]))>=1){
        for (k in 1:if(length(as.numeric(na.omit(catalogo[,c(1:4,i)])[na.omit(catalogo[,c(1:4,i)])[,4] == unique(na.omit(catalogo[,c(1:4,i)])[,4])[j],2]))==1){length(as.numeric(na.omit(catalogo[,c(1:4,i)])[na.omit(catalogo[,c(1:4,i)])[,4] == unique(na.omit(catalogo[,c(1:4,i)])[,4])[j],2]))}else if(length(as.numeric(na.omit(catalogo[,c(1:4,i)])[na.omit(catalogo[,c(1:4,i)])[,4] == unique(na.omit(catalogo[,c(1:4,i)])[,4])[j],2]))>1){(length(as.numeric(na.omit(catalogo[,c(1:4,i)])[na.omit(catalogo[,c(1:4,i)])[,4] == unique(na.omit(catalogo[,c(1:4,i)])[,4])[j],2]))-1)}) {
          demo_id <- paste(demo_id,as.numeric(na.omit(catalogo[,c(1:4,i)])[na.omit(catalogo[,c(1:4,i)])[,4] == unique(na.omit(catalogo[,c(1:4,i)])[,4])[j],2])[k],",",sep = "")
        }
        demo_id <- paste(demo_id,as.numeric(na.omit(catalogo[,c(1:4,i)])[na.omit(catalogo[,c(1:4,i)])[,4] == unique(na.omit(catalogo[,c(1:4,i)])[,4])[j],2])[length(as.numeric(na.omit(catalogo[,c(1:4,i)])[na.omit(catalogo[,c(1:4,i)])[,4] == unique(na.omit(catalogo[,c(1:4,i)])[,4])[j],2]))],sep = "")
        
        url_demo[dim(url_demo)[1]+1,1:2] <- c(paste(unique(na.omit(catalogo[,c(1:4,i)])[,4])[j],".",demo_id,sep = ""),unique(na.omit(catalogo[,c(1:4,i)])[na.omit(catalogo[,c(1:4,i)])[,4] == unique(na.omit(catalogo[,c(1:4,i)])[,4])[j],5]))
        
      }
    
    }
  }
  
  
  url_demo_total <- data.frame(matrix(nrow = 1,ncol = 3))
  
  
  for (i in 1:length(unique(url_demo[,2]))) {
    url_demo_modelo <- ""
    for (j in 2:length(url_demo[url_demo[,2] == unique(url_demo[,2])[i],1])) {
      url_demo_modelo <- paste(url_demo_modelo,url_demo[url_demo[,2] == unique(url_demo[,2])[i],1][j],sep = ":/")
    }
    url_demo_modelo <- paste(url_demo[url_demo[,2] == unique(url_demo[,2])[i],1][1],url_demo_modelo,sep = "")
    url_demo_total[i,c(1,2)] <- c(url_demo_modelo,unique(url_demo[,2])[i])
  }
  
  
  # URL para AT&T
  
  url_demo_att <- data.frame(matrix(nrow = 0,ncol = 2))
  
  for (i in 5:dim(catalogo_att)[2]) {
    for (j in 1:length(unique(na.omit(catalogo_att[,c(1:4,i)])[,4]))) {
      demo_id <-""
      
      if(length(as.numeric(na.omit(catalogo_att[,c(1:4,i)])[na.omit(catalogo_att[,c(1:4,i)])[,4] == unique(na.omit(catalogo_att[,c(1:4,i)])[,4])[j],2]))==1){
        for (k in 1:if(length(as.numeric(na.omit(catalogo_att[,c(1:4,i)])[na.omit(catalogo_att[,c(1:4,i)])[,4] == unique(na.omit(catalogo_att[,c(1:4,i)])[,4])[j],2]))==1){length(as.numeric(na.omit(catalogo_att[,c(1:4,i)])[na.omit(catalogo_att[,c(1:4,i)])[,4] == unique(na.omit(catalogo_att[,c(1:4,i)])[,4])[j],2]))}else if(length(as.numeric(na.omit(catalogo_att[,c(1:4,i)])[na.omit(catalogo_att[,c(1:4,i)])[,4] == unique(na.omit(catalogo_att[,c(1:4,i)])[,4])[j],2]))>1){(length(as.numeric(na.omit(catalogo_att[,c(1:4,i)])[na.omit(catalogo_att[,c(1:4,i)])[,4] == unique(na.omit(catalogo_att[,c(1:4,i)])[,4])[j],2]))-1)}) {
          demo_id <- paste(demo_id,as.numeric(na.omit(catalogo_att[,c(1:4,i)])[na.omit(catalogo_att[,c(1:4,i)])[,4] == unique(na.omit(catalogo_att[,c(1:4,i)])[,4])[j],2])[k],sep = "")
        }
        
        url_demo_att[dim(url_demo_att)[1]+1,1:2] <- c(paste(unique(na.omit(catalogo_att[,c(1:4,i)])[,4])[j],".",demo_id,sep = ""),unique(na.omit(catalogo_att[,c(1:4,i)])[na.omit(catalogo_att[,c(1:4,i)])[,4] == unique(na.omit(catalogo_att[,c(1:4,i)])[,4])[j],5]))
        
      }else if(length(as.numeric(na.omit(catalogo_att[,c(1:4,i)])[na.omit(catalogo_att[,c(1:4,i)])[,4] == unique(na.omit(catalogo_att[,c(1:4,i)])[,4])[j],2]))>=1){
        for (k in 1:if(length(as.numeric(na.omit(catalogo_att[,c(1:4,i)])[na.omit(catalogo_att[,c(1:4,i)])[,4] == unique(na.omit(catalogo_att[,c(1:4,i)])[,4])[j],2]))==1){length(as.numeric(na.omit(catalogo_att[,c(1:4,i)])[na.omit(catalogo_att[,c(1:4,i)])[,4] == unique(na.omit(catalogo_att[,c(1:4,i)])[,4])[j],2]))}else if(length(as.numeric(na.omit(catalogo_att[,c(1:4,i)])[na.omit(catalogo_att[,c(1:4,i)])[,4] == unique(na.omit(catalogo_att[,c(1:4,i)])[,4])[j],2]))>1){(length(as.numeric(na.omit(catalogo_att[,c(1:4,i)])[na.omit(catalogo_att[,c(1:4,i)])[,4] == unique(na.omit(catalogo_att[,c(1:4,i)])[,4])[j],2]))-1)}) {
          demo_id <- paste(demo_id,as.numeric(na.omit(catalogo_att[,c(1:4,i)])[na.omit(catalogo_att[,c(1:4,i)])[,4] == unique(na.omit(catalogo_att[,c(1:4,i)])[,4])[j],2])[k],",",sep = "")
        }
        demo_id <- paste(demo_id,as.numeric(na.omit(catalogo_att[,c(1:4,i)])[na.omit(catalogo_att[,c(1:4,i)])[,4] == unique(na.omit(catalogo_att[,c(1:4,i)])[,4])[j],2])[length(as.numeric(na.omit(catalogo_att[,c(1:4,i)])[na.omit(catalogo_att[,c(1:4,i)])[,4] == unique(na.omit(catalogo_att[,c(1:4,i)])[,4])[j],2]))],sep = "")
        
        url_demo_att[dim(url_demo_att)[1]+1,1:2] <- c(paste(unique(na.omit(catalogo_att[,c(1:4,i)])[,4])[j],".",demo_id,sep = ""),unique(na.omit(catalogo_att[,c(1:4,i)])[na.omit(catalogo_att[,c(1:4,i)])[,4] == unique(na.omit(catalogo_att[,c(1:4,i)])[,4])[j],5]))
        
      }
      
    }
  }
  
  
  url_demo_att_total <- data.frame(matrix(nrow = 1,ncol = 3))
  
  
  for (i in 1:length(unique(url_demo_att[,2]))) {
    url_demo_att_modelo <- ""
    for (j in 2:length(url_demo_att[url_demo_att[,2] == unique(url_demo_att[,2])[i],1])) {
      url_demo_att_modelo <- paste(url_demo_att_modelo,url_demo_att[url_demo_att[,2] == unique(url_demo_att[,2])[i],1][j],sep = ":/")
    }
    url_demo_att_modelo <- paste(url_demo_att[url_demo_att[,2] == unique(url_demo_att[,2])[i],1][1],url_demo_att_modelo,sep = "")
    url_demo_att_total[i,c(1,2)] <- c(url_demo_att_modelo,unique(url_demo_att[,2])[i])
  }
  
 
  
  # Media movil por modelo March,Kicks,Leaf,Versa,Sentra,Maxima,Altima, X-Trail,Murano,Pathfinder,Frontier
  
  url_demo_total[,3] <- c(28,42,84,63,42,84,84,42,84,84,28,28)
  
  #url_demo_att_total[,3] <-c(70,70,42,28)
  url_demo_att_total[,3] <-c(70,70,42,28,84,28)
  
  #url_demo_total_renault[,3] <- c(42,42,42,42,42,42)
  
  catalogo_URL_JSON <- data.frame(matrix(nrow = 0,ncol = 3))
  catalogo_URL_CSV <- data.frame(matrix(nrow = 0,ncol = 3))
  
  catalogo_masivo_URL_JSON <- data.frame(matrix(nrow = 0,ncol = 3))
  catalogo_masivo_URL_CSV <- data.frame(matrix(nrow = 0,ncol = 3))
  
  catalogo_att_URL_JSON <- data.frame(matrix(nrow = 0,ncol = 3))
  catalogo_att_URL_CSV <- data.frame(matrix(nrow = 0,ncol = 3))
  
  catalogo_masivo_att_URL_JSON <- data.frame(matrix(nrow = 0,ncol = 3))
  catalogo_masivo_att_URL_CSV <- data.frame(matrix(nrow = 0,ncol = 3))
  
  #catalogo_renault_URL_JSON <- data.frame(matrix(nrow = 0,ncol = 3))
  #catalogo_renault_URL_CSV <- data.frame(matrix(nrow = 0,ncol = 3))
  
  #catalogo_masivo_renault_URL_JSON <- data.frame(matrix(nrow = 0,ncol = 3))
  #catalogo_masivo_renault_URL_CSV <- data.frame(matrix(nrow = 0,ncol = 3))
  
  for (i in 1:dim(catalogo_marca)[1]) {
    for (j in 1:(dim(url_demo_total)[1]-1)) {
      catalogo_URL_JSON[dim(catalogo_URL_JSON)[1]+1,1:3] <- c(paste("https://api.brandindex.com/v0/timeline/multi-brand-file.json?scoring=",scoring,"&start_date=",inicio,"&end_date=",fin,"&moving_average=",url_demo_total[j,3],"&brand=",Region,":",sector,":",catalogo_marca[i,1],":/",url_demo_total[j,1],sep = ""),catalogo_marca[i,1],url_demo_total[j,2])
      catalogo_URL_CSV[dim(catalogo_URL_CSV)[1]+1,1:3] <- c(paste("https://api.brandindex.com/v0/timeline/multi-brand-file.csv?scoring=",scoring,"&start_date=",inicio,"&end_date=",fin,"&moving_average=",url_demo_total[j,3],"&brand=",Region,":",sector,":",catalogo_marca[i,1],":/",url_demo_total[j,1],sep = ""),catalogo_marca[i,1],url_demo_total[j,2])
    }
  }
  
  for (i in 1:dim(catalogo_marca)[1]) {
    for (j in dim(url_demo_total)[1]) {
      catalogo_masivo_URL_JSON[dim(catalogo_masivo_URL_JSON)[1]+1,1:3] <- c(paste("https://api.brandindex.com/v0/timeline/multi-brand-file.json?scoring=",scoring,"&start_date=",inicio,"&end_date=",fin,"&moving_average=",url_demo_total[j,3],"&brand=",Region,":",sector,":",catalogo_marca[i,1],sep = ""),catalogo_marca[i,1],url_demo_total[j,2])
      catalogo_masivo_URL_CSV[dim(catalogo_masivo_URL_CSV)[1]+1,1:3] <- c(paste("https://api.brandindex.com/v0/timeline/multi-brand-file.csv?scoring=",scoring,"&start_date=",inicio,"&end_date=",fin,"&moving_average=",url_demo_total[j,3],"&brand=",Region,":",sector,":",catalogo_marca[i,1],sep = ""),catalogo_marca[i,1],url_demo_total[j,2])
    }
  }
  
  for (i in 1:dim(catalogo_marca_att)[1]) {
    for (j in seq(dim(url_demo_att_total)[1]-1)) {
      catalogo_att_URL_JSON[dim(catalogo_att_URL_JSON)[1]+1,1:3] <- c(paste("https://api.brandindex.com/v0/timeline/multi-brand-file.json?scoring=",scoring,"&start_date=",inicio,"&end_date=",fin,"&moving_average=",url_demo_att_total[j,3],"&brand=",Region,":",sector_att,":",catalogo_marca_att[i,1],":/",url_demo_att_total[j,1],sep = ""),catalogo_marca_att[i,1],url_demo_att_total[j,2])
      catalogo_att_URL_CSV[dim(catalogo_att_URL_CSV)[1]+1,1:3] <- c(paste("https://api.brandindex.com/v0/timeline/multi-brand-file.csv?scoring=",scoring,"&start_date=",inicio,"&end_date=",fin,"&moving_average=",url_demo_att_total[j,3],"&brand=",Region,":",sector_att,":",catalogo_marca_att[i,1],":/",url_demo_att_total[j,1],sep = ""),catalogo_marca_att[i,1],url_demo_att_total[j,2])
    }
  }
  
  for (i in 1:dim(catalogo_marca_att)[1]) {
    for (j in 4:dim(url_demo_att_total)[1]) {
      if(url_demo_att_total[j,2] == 'MASIVO_ATT'){
        url_att <- ''
      }else if(url_demo_att_total[j,2] == 'CURRENT MASIVO'){
        url_att <- paste(':/brand',as.numeric(catalogo_marca_att[as.numeric(row.names(catalogo_marca_att)[is.element(catalogo_marca_att[,3],'AT&T')]),1]),catalogo_metric_filters[grep('customer',catalogo_metric_filters[,4])[1],4],as.numeric(catalogo_metric_filters[as.numeric(row.names(catalogo_metric_filters)[is.element(catalogo_metric_filters[,3],'Current')]),2]), sep = '.')
      }else if(url_demo_att_total[j,2] == 'NON_CURRENT_MASIVO'){
        url_att <- paste(':/brand',as.numeric(catalogo_marca_att[as.numeric(row.names(catalogo_marca_att)[is.element(catalogo_marca_att[,3],'AT&T')]),1]),catalogo_metric_filters[grep('customer',catalogo_metric_filters[,4])[1],4],paste(as.numeric(catalogo_metric_filters[as.numeric(row.names(catalogo_metric_filters)[is.element(catalogo_metric_filters[,3],'Former')]),2]),as.numeric(catalogo_metric_filters[as.numeric(row.names(catalogo_metric_filters)[is.element(catalogo_metric_filters[,3],'Never (aware)')]),2]),as.numeric(catalogo_metric_filters[as.numeric(row.names(catalogo_metric_filters)[is.element(catalogo_metric_filters[,3],'Never (unaware)')]),2]), sep = ','), sep = '.')
      }
      
      catalogo_masivo_att_URL_JSON[dim(catalogo_masivo_att_URL_JSON)[1]+1,1:3] <- c(paste("https://api.brandindex.com/v0/timeline/multi-brand-file.json?scoring=",scoring,"&start_date=",inicio,"&end_date=",fin,"&moving_average=",url_demo_att_total[j,3],"&brand=",Region,":",sector_att,":",catalogo_marca_att[i,1],url_att ,sep = ""),catalogo_marca_att[i,1],url_demo_att_total[j,2])
      catalogo_masivo_att_URL_CSV[dim(catalogo_masivo_att_URL_CSV)[1]+1,1:3] <- c(paste("https://api.brandindex.com/v0/timeline/multi-brand-file.csv?scoring=",scoring,"&start_date=",inicio,"&end_date=",fin,"&moving_average=",url_demo_att_total[j,3],"&brand=",Region,":",sector_att,":",catalogo_marca_att[i,1],url_att ,sep = ""),catalogo_marca_att[i,1],url_demo_att_total[j,2])
    }
  }
  
 
  
  
  
  #as.numeric(catalogo_metric_filters[as.numeric(row.names(catalogo_metric_filters)[is.element(catalogo_metric_filters[,3],'Current')]),2])
  #as.numeric(catalogo_metric_filters[as.numeric(row.names(catalogo_metric_filters)[grep(catalogo_metric_filters[,4],'customer')]),2])
  #paste(as.numeric(catalogo_metric_filters[as.numeric(row.names(catalogo_metric_filters)[is.element(catalogo_metric_filters[,3],'Former')]),2]),as.numeric(catalogo_metric_filters[as.numeric(row.names(catalogo_metric_filters)[is.element(catalogo_metric_filters[,3],'Never (aware)')]),2]),as.numeric(catalogo_metric_filters[as.numeric(row.names(catalogo_metric_filters)[is.element(catalogo_metric_filters[,3],'Never (unaware)')]),2]), sep = '.')
  #catalogo_metric_filters[grep('customer',catalogo_metric_filters[,4])[1],4]
  
  ######### Vamos a extraer los datos de cada llamada #######
  
  
  ############################## MARCH #####################################################
  
  march_url <- catalogo_URL_CSV[catalogo_URL_CSV[,3] == toupper("march"),]
  rownames(march_url) <- seq(dim(march_url)[1])
  colnames(march_url) <- c('URL','Marca','Modelo')
  armadora <- 0
  Brand_Index_march <- data.frame(matrix(nrow = 0, ncol = 6))
  
  for (i in seq(dim(march_url)[1])) {
    print(i)
    armadora <- data.frame(content(GET(as.character(march_url[i,1])), as = "parsed"))
    armadora[,1] <- as.character(armadora[,1])
    for (j in seq(4,dim(armadora)[2])) {
      contador <- (dim(Brand_Index_march)[1]+1):(dim(Brand_Index_march)[1]+dim(armadora)[1])
      contador
      Brand_Index_march[contador,1:3] <- armadora[,c(2,1,3)]
      Brand_Index_march[contador,4] <- colnames(armadora)[j]
      Brand_Index_march[contador,5] <- armadora[,j]
    }
  }
  
  write.csv(Brand_Index_march, paste(getwd(),'/Brand Index March.csv',sep = ''),row.names = F)

  ############################## Kicks ###########################################################################
  
  kicks_url <- catalogo_URL_CSV[catalogo_URL_CSV[,3] == toupper("kicks"),]
  rownames(kicks_url) <- seq(dim(kicks_url)[1])
  colnames(kicks_url) <- c('URL','Marca','Modelo')
  armadora <- 0
  Brand_Index_kicks <- data.frame(matrix(nrow = 0, ncol = 6))
  
  for (i in seq(dim(kicks_url)[1])) {
    print(i)
    armadora <- data.frame(content(GET(as.character(kicks_url[i,1])), as = "parsed"))
    armadora[,1] <- as.character(armadora[,1])
    for (j in seq(4,dim(armadora)[2])) {
      contador <- (dim(Brand_Index_kicks)[1]+1):(dim(Brand_Index_kicks)[1]+dim(armadora)[1])
      contador
      Brand_Index_kicks[contador,1:3] <- armadora[,c(2,1,3)]
      Brand_Index_kicks[contador,4] <- colnames(armadora)[j]
      Brand_Index_kicks[contador,5] <- armadora[,j]
    }
  }
  
  write.csv(Brand_Index_kicks, paste(getwd(),'/Brand Index Kicks.csv',sep = ''),row.names = F)
  
  ############################## leaf ###########################################################################
  
  leaf_url <- catalogo_URL_CSV[catalogo_URL_CSV[,3] == toupper("leaf"),]
  rownames(leaf_url) <- seq(dim(leaf_url)[1])
  colnames(leaf_url) <- c('URL','Marca','Modelo')
  armadora <- 0
  Brand_Index_leaf <- data.frame(matrix(nrow = 0, ncol = 6))
  
  for (i in seq(dim(leaf_url)[1])) {
    print(i)
    armadora <- data.frame(content(GET(as.character(leaf_url[i,1])), as = "parsed"))
    armadora[,1] <- as.character(armadora[,1])
    for (j in seq(4,dim(armadora)[2])) {
      contador <- (dim(Brand_Index_leaf)[1]+1):(dim(Brand_Index_leaf)[1]+dim(armadora)[1])
      contador
      Brand_Index_leaf[contador,1:3] <- armadora[,c(2,1,3)]
      Brand_Index_leaf[contador,4] <- colnames(armadora)[j]
      Brand_Index_leaf[contador,5] <- armadora[,j]
    }
  }
  
  write.csv(Brand_Index_leaf, paste(getwd(),'/Brand Index Leaf.csv',sep = ''),row.names = F)
  
  ############################## versa ###########################################################################
  
  versa_url <- catalogo_URL_CSV[catalogo_URL_CSV[,3] == toupper("versa"),]
  rownames(versa_url) <- seq(dim(versa_url)[1])
  colnames(versa_url) <- c('URL','Marca','Modelo')
  armadora <- 0
  Brand_Index_versa <- data.frame(matrix(nrow = 0, ncol = 6))
  
  for (i in seq(dim(versa_url)[1])) {
    print(i)
    armadora <- data.frame(content(GET(as.character(versa_url[i,1])), as = "parsed"))
    armadora[,1] <- as.character(armadora[,1])
    for (j in seq(4,dim(armadora)[2])) {
      contador <- (dim(Brand_Index_versa)[1]+1):(dim(Brand_Index_versa)[1]+dim(armadora)[1])
      contador
      Brand_Index_versa[contador,1:3] <- armadora[,c(2,1,3)]
      Brand_Index_versa[contador,4] <- colnames(armadora)[j]
      Brand_Index_versa[contador,5] <- armadora[,j]
    }
  }
  
  write.csv(Brand_Index_versa, paste(getwd(),'/Brand Index Versa.csv',sep = ''),row.names = F)
  
  ############################## sentra ###########################################################################
  
  sentra_url <- catalogo_URL_CSV[catalogo_URL_CSV[,3] == toupper("sentra"),]
  rownames(sentra_url) <- seq(dim(sentra_url)[1])
  colnames(sentra_url) <- c('URL','Marca','Modelo')
  armadora <- 0
  Brand_Index_sentra <- data.frame(matrix(nrow = 0, ncol = 6))
  
  for (i in seq(dim(sentra_url)[1])) {
    print(i)
    armadora <- data.frame(content(GET(as.character(sentra_url[i,1])), as = "parsed"))
    armadora[,1] <- as.character(armadora[,1])
    for (j in seq(4,dim(armadora)[2])) {
      contador <- (dim(Brand_Index_sentra)[1]+1):(dim(Brand_Index_sentra)[1]+dim(armadora)[1])
      contador
      Brand_Index_sentra[contador,1:3] <- armadora[,c(2,1,3)]
      Brand_Index_sentra[contador,4] <- colnames(armadora)[j]
      Brand_Index_sentra[contador,5] <- armadora[,j]
    }
  }
  
  write.csv(Brand_Index_sentra, paste(getwd(),'/Brand Index Sentra.csv',sep = ''),row.names = F)
  
  ############################## maxima ###########################################################################
  
  maxima_url <- catalogo_URL_CSV[catalogo_URL_CSV[,3] == toupper("maxima"),]
  rownames(maxima_url) <- seq(dim(maxima_url)[1])
  colnames(maxima_url) <- c('URL','Marca','Modelo')
  armadora <- 0
  Brand_Index_maxima <- data.frame(matrix(nrow = 0, ncol = 6))
  
  for (i in seq(dim(maxima_url)[1])) {
    armadora <- data.frame(content(GET(as.character(maxima_url[i,1])), as = "parsed"))
    armadora[,1] <- as.character(armadora[,1])
    print(i)
    for (j in seq(4,dim(armadora)[2])) {
      contador <- (dim(Brand_Index_maxima)[1]+1):(dim(Brand_Index_maxima)[1]+dim(armadora)[1])
      contador
      Brand_Index_maxima[contador,1:3] <- armadora[,c(2,1,3)]
      Brand_Index_maxima[contador,4] <- colnames(armadora)[j]
      Brand_Index_maxima[contador,5] <- armadora[,j]
    }
  }
  
  write.csv(Brand_Index_maxima, paste(getwd(),'/Brand Index Maxima.csv',sep = ''),row.names = F)
  
  ############################## altima ###########################################################################
  
  altima_url <- catalogo_URL_CSV[catalogo_URL_CSV[,3] == toupper("altima"),]
  rownames(altima_url) <- seq(dim(altima_url)[1])
  colnames(altima_url) <- c('URL','Marca','Modelo')
  armadora <- 0
  Brand_Index_altima <- data.frame(matrix(nrow = 0, ncol = 6))
  
  for (i in seq(dim(altima_url)[1])) {
    armadora <- data.frame(content(GET(as.character(altima_url[i,1])), as = "parsed"))
    armadora[,1] <- as.character(armadora[,1])
    print(i)
    for (j in seq(4,dim(armadora)[2])) {
      contador <- (dim(Brand_Index_altima)[1]+1):(dim(Brand_Index_altima)[1]+dim(armadora)[1])
      contador
      Brand_Index_altima[contador,1:3] <- armadora[,c(2,1,3)]
      Brand_Index_altima[contador,4] <- colnames(armadora)[j]
      Brand_Index_altima[contador,5] <- armadora[,j]
    }
  }
  
  write.csv(Brand_Index_altima, paste(getwd(),'/Brand Index Altima.csv',sep = ''),row.names = F)
  
  ############################## xtrail ###########################################################################
  
  xtrail_url <- catalogo_URL_CSV[catalogo_URL_CSV[,3] == toupper("x-trail"),]
  rownames(xtrail_url) <- seq(dim(xtrail_url)[1])
  colnames(xtrail_url) <- c('URL','Marca','Modelo')
  armadora <- 0
  Brand_Index_xtrail <- data.frame(matrix(nrow = 0, ncol = 6))
  
  for (i in seq(dim(xtrail_url)[1])) {
    armadora <- data.frame(content(GET(as.character(xtrail_url[i,1])), as = "parsed"))
    armadora[,1] <- as.character(armadora[,1])
    print(i)
    for (j in seq(4,dim(armadora)[2])) {
      contador <- (dim(Brand_Index_xtrail)[1]+1):(dim(Brand_Index_xtrail)[1]+dim(armadora)[1])
      contador
      Brand_Index_xtrail[contador,1:3] <- armadora[,c(2,1,3)]
      Brand_Index_xtrail[contador,4] <- colnames(armadora)[j]
      Brand_Index_xtrail[contador,5] <- armadora[,j]
    }
  }
  
  write.csv(Brand_Index_xtrail, paste(getwd(),'/Brand Index XTrail.csv',sep = ''),row.names = F)
  
  ############################## murano ###########################################################################
  
  murano_url <- catalogo_URL_CSV[catalogo_URL_CSV[,3] == toupper("murano"),]
  rownames(murano_url) <- seq(dim(murano_url)[1])
  colnames(murano_url) <- c('URL','Marca','Modelo')
  armadora <- 0
  Brand_Index_murano <- data.frame(matrix(nrow = 0, ncol = 6))
  
  for (i in seq(dim(murano_url)[1])) {
    armadora <- data.frame(content(GET(as.character(murano_url[i,1])), as = "parsed"))
    armadora[,1] <- as.character(armadora[,1])
    print(i)
    for (j in seq(4,dim(armadora)[2])) {
      contador <- (dim(Brand_Index_murano)[1]+1):(dim(Brand_Index_murano)[1]+dim(armadora)[1])
      contador
      Brand_Index_murano[contador,1:3] <- armadora[,c(2,1,3)]
      Brand_Index_murano[contador,4] <- colnames(armadora)[j]
      Brand_Index_murano[contador,5] <- armadora[,j]
    }
  }
  
  write.csv(Brand_Index_murano, paste(getwd(),'/Brand Index Murano.csv',sep = ''),row.names = F)
  
  ############################## pathfinder ###########################################################################
  
  pathfinder_url <- catalogo_URL_CSV[catalogo_URL_CSV[,3] == toupper("pathfinder"),]
  rownames(pathfinder_url) <- seq(dim(pathfinder_url)[1])
  colnames(pathfinder_url) <- c('URL','Marca','Modelo')
  armadora <- 0
  Brand_Index_pathfinder <- data.frame(matrix(nrow = 0, ncol = 6))
  
  for (i in seq(dim(pathfinder_url)[1])) {
    armadora <- data.frame(content(GET(as.character(pathfinder_url[i,1])), as = "parsed"))
    armadora[,1] <- as.character(armadora[,1])
    print(i)
    for (j in seq(4,dim(armadora)[2])) {
      contador <- (dim(Brand_Index_pathfinder)[1]+1):(dim(Brand_Index_pathfinder)[1]+dim(armadora)[1])
      contador
      Brand_Index_pathfinder[contador,1:3] <- armadora[,c(2,1,3)]
      Brand_Index_pathfinder[contador,4] <- colnames(armadora)[j]
      Brand_Index_pathfinder[contador,5] <- armadora[,j]
    }
  }
  
  write.csv(Brand_Index_pathfinder, paste(getwd(),'/Brand Index Pathfinder.csv',sep = ''),row.names = F)
  
  ############################## frontier ###########################################################################
  
  frontier_url <- catalogo_URL_CSV[catalogo_URL_CSV[,3] == toupper("frontier"),]
  rownames(frontier_url) <- seq(dim(frontier_url)[1])
  colnames(frontier_url) <- c('URL','Marca','Modelo')
  armadora <- 0
  Brand_Index_frontier <- data.frame(matrix(nrow = 0, ncol = 6))
  
  for (i in seq(dim(frontier_url)[1])) {
    armadora <- data.frame(content(GET(as.character(frontier_url[i,1])), as = "parsed"))
    armadora[,1] <- as.character(armadora[,1])
    print(i)
    for (j in seq(4,dim(armadora)[2])) {
      contador <- (dim(Brand_Index_frontier)[1]+1):(dim(Brand_Index_frontier)[1]+dim(armadora)[1])
      contador
      Brand_Index_frontier[contador,1:3] <- armadora[,c(2,1,3)]
      Brand_Index_frontier[contador,4] <- colnames(armadora)[j]
      Brand_Index_frontier[contador,5] <- armadora[,j]
    }
  }
  
  write.csv(Brand_Index_frontier, paste(getwd(),'/Brand Index Frontier.csv',sep = ''),row.names = F)
  
  ############################## masivo ###########################################################################
  
  masivo_url <- catalogo_masivo_URL_CSV[catalogo_masivo_URL_CSV[,3] == toupper("masivo"),]
  rownames(masivo_url) <- seq(dim(masivo_url)[1])
  colnames(masivo_url) <- c('URL','Marca','Modelo')
  armadora <- 0
  Brand_Index_masivo <- data.frame(matrix(nrow = 0, ncol = 6))
  
  for (i in seq(dim(masivo_url)[1])) {
    armadora <- data.frame(content(GET(as.character(masivo_url[i,1])), as = "parsed"))
    armadora[,1] <- as.character(armadora[,1])
    print(i)
    for (j in seq(4,dim(armadora)[2])) {
      contador <- (dim(Brand_Index_masivo)[1]+1):(dim(Brand_Index_masivo)[1]+dim(armadora)[1])
      contador
      Brand_Index_masivo[contador,1:3] <- armadora[,c(2,1,3)]
      Brand_Index_masivo[contador,4] <- colnames(armadora)[j]
      Brand_Index_masivo[contador,5] <- armadora[,j]
    }
  }
  
  
  write.csv(Brand_Index_masivo, paste(getwd(),'/Brand Index Masivo.csv',sep = ''),row.names = F)
  
  ############################## postpago ###########################################################################
  
  postpago_url <- catalogo_att_URL_CSV[catalogo_att_URL_CSV[,3] == toupper("postpago"),]
  rownames(postpago_url) <- seq(dim(postpago_url)[1])
  colnames(postpago_url) <- c('URL','Marca','Modelo')
  armadora <- 0
  Brand_Index_postpago <- data.frame(matrix(nrow = 0, ncol = 6))
  
  for (i in seq(dim(postpago_url)[1])) {
    armadora <- data.frame(content(GET(as.character(postpago_url[i,1])), as = "parsed"))
    armadora[,1] <- as.character(armadora[,1])
    print(i)
    for (j in seq(4,dim(armadora)[2])) {
      contador <- (dim(Brand_Index_postpago)[1]+1):(dim(Brand_Index_postpago)[1]+dim(armadora)[1])
      contador
      Brand_Index_postpago[contador,1:3] <- armadora[,c(2,1,3)]
      Brand_Index_postpago[contador,4] <- colnames(armadora)[j]
      Brand_Index_postpago[contador,5] <- armadora[,j]
    }
  }
  
  write.csv(Brand_Index_postpago, paste(getwd(),'/Brand Index Postpago.csv',sep = ''),row.names = F)

  ############################## prepago ###########################################################################
  
  prepago_url <- catalogo_att_URL_CSV[catalogo_att_URL_CSV[,3] == toupper("prepago"),]
  rownames(prepago_url) <- seq(dim(prepago_url)[1])
  colnames(prepago_url) <- c('URL','Marca','Modelo')
  armadora <- 0
  Brand_Index_prepago <- data.frame(matrix(nrow = 0, ncol = 6))
  
  for (i in seq(dim(prepago_url)[1])) {
    armadora <- data.frame(content(GET(as.character(prepago_url[i,1])), as = "parsed"))
    armadora[,1] <- as.character(armadora[,1])
    print(i)
    for (j in seq(4,dim(armadora)[2])) {
      contador <- (dim(Brand_Index_prepago)[1]+1):(dim(Brand_Index_prepago)[1]+dim(armadora)[1])
      contador
      Brand_Index_prepago[contador,1:3] <- armadora[,c(2,1,3)]
      Brand_Index_prepago[contador,4] <- colnames(armadora)[j]
      Brand_Index_prepago[contador,5] <- armadora[,j]
    }
  }
  
  
  write.csv(Brand_Index_prepago, paste(getwd(),'/Brand Index Prepago.csv',sep = ''),row.names = F)
  
  ############################## unefon ###########################################################################
  
  unefon_url <- catalogo_att_URL_CSV[catalogo_att_URL_CSV[,3] == toupper("unefon"),]
  rownames(unefon_url) <- seq(dim(unefon_url)[1])
  colnames(unefon_url) <- c('URL','Marca','Modelo')
  armadora <- 0
  Brand_Index_unefon <- data.frame(matrix(nrow = 0, ncol = 6))
  
  for (i in seq(dim(unefon_url)[1])) {
    armadora <- data.frame(content(GET(as.character(unefon_url[i,1])), as = "parsed"))
    armadora[,1] <- as.character(armadora[,1])
    print(i)
    for (j in seq(4,dim(armadora)[2])) {
      contador <- (dim(Brand_Index_unefon)[1]+1):(dim(Brand_Index_unefon)[1]+dim(armadora)[1])
      contador
      Brand_Index_unefon[contador,1:3] <- armadora[,c(2,1,3)]
      Brand_Index_unefon[contador,4] <- colnames(armadora)[j]
      Brand_Index_unefon[contador,5] <- armadora[,j]
    }
  }
  
  write.csv(Brand_Index_unefon, paste(getwd(),'/Brand Index Unefon.csv',sep = ''),row.names = F)
  
  ############################## masivo_att ###########################################################################
  
  masivo_att_url <- catalogo_masivo_att_URL_CSV[catalogo_masivo_att_URL_CSV[,3] == toupper("masivo_att"),]
  rownames(masivo_att_url) <- seq(dim(masivo_att_url)[1])
  colnames(masivo_att_url) <- c('URL','Marca','Modelo')
  armadora <- 0
  Brand_Index_masivo_att <- data.frame(matrix(nrow = 0, ncol = 6))
  
  for (i in seq(dim(masivo_att_url)[1])) {
    armadora <- data.frame(content(GET(as.character(masivo_att_url[i,1])), as = "parsed"))
    armadora[,1] <- as.character(armadora[,1])
    print(i)
    for (j in seq(4,dim(armadora)[2])) {
      contador <- (dim(Brand_Index_masivo_att)[1]+1):(dim(Brand_Index_masivo_att)[1]+dim(armadora)[1])
      contador
      Brand_Index_masivo_att[contador,1:3] <- armadora[,c(2,1,3)]
      Brand_Index_masivo_att[contador,4] <- colnames(armadora)[j]
      Brand_Index_masivo_att[contador,5] <- armadora[,j]
    }
  }
  
  write.csv(Brand_Index_masivo_att, paste(getwd(),'/Brand Index Masivo ATT.csv',sep = ''),row.names = F)
  
  ############################## current_masivo ###########################################################################
  
  current_masivo_att_url <- catalogo_masivo_att_URL_CSV[catalogo_masivo_att_URL_CSV[,3] == toupper("current masivo"),]
  rownames(current_masivo_att_url) <- seq(dim(current_masivo_att_url)[1])
  colnames(current_masivo_att_url) <- c('URL','Marca','Modelo')
  armadora <- 0
  Brand_Index_current_masivo <- data.frame(matrix(nrow = 0, ncol = 6))
  
  for (i in seq(dim(current_masivo_att_url)[1])) {
    armadora <- data.frame(content(GET(as.character(current_masivo_att_url[i,1])), as = "parsed"))
    armadora[,1] <- as.character(armadora[,1])
    print(i)
    for (j in seq(4,dim(armadora)[2])) {
      contador <- (dim(Brand_Index_current_masivo)[1]+1):(dim(Brand_Index_current_masivo)[1]+dim(armadora)[1])
      contador
      Brand_Index_current_masivo[contador,1:3] <- armadora[,c(2,1,3)]
      Brand_Index_current_masivo[contador,4] <- colnames(armadora)[j]
      Brand_Index_current_masivo[contador,5] <- armadora[,j]
    }
  }
  
  write.csv(Brand_Index_current_masivo, paste(getwd(),'/Brand Index Current Masivo.csv',sep = ''),row.names = F)
  
  ############################## non_current_masivo ###########################################################################
  
  non_current_masivo_att_url <- catalogo_masivo_att_URL_CSV[catalogo_masivo_att_URL_CSV[,3] == toupper("non_current_masivo"),]
  rownames(non_current_masivo_att_url) <- seq(dim(non_current_masivo_att_url)[1])
  colnames(non_current_masivo_att_url) <- c('URL','Marca','Modelo')
  armadora <- 0
  Brand_Index_non_current_masivo <- data.frame(matrix(nrow = 0, ncol = 6))
  
  for (i in seq(dim(non_current_masivo_att_url)[1])) {
    armadora <- data.frame(content(GET(as.character(non_current_masivo_att_url[i,1])), as = "parsed"))
    armadora[,1] <- as.character(armadora[,1])
    print(i)
    for (j in seq(4,dim(armadora)[2])) {
      contador <- (dim(Brand_Index_non_current_masivo)[1]+1):(dim(Brand_Index_non_current_masivo)[1]+dim(armadora)[1])
      contador
      Brand_Index_non_current_masivo[contador,1:3] <- armadora[,c(2,1,3)]
      Brand_Index_non_current_masivo[contador,4] <- colnames(armadora)[j]
      Brand_Index_non_current_masivo[contador,5] <- armadora[,j]
    }
  }
  
  write.csv(Brand_Index_non_current_masivo, paste(getwd(),'/Brand Index Non Current Masivo.csv',sep = ''),row.names = F)
  
  ############################## Union de tabla final ###########################################################################

  
  Brand_Index_march[,6] <- 'MARCH'
  Brand_Index_kicks[,6] <- 'KICKS'
  Brand_Index_leaf[,6] <- 'LEAF'
  Brand_Index_versa[,6] <- 'VERSA'
  Brand_Index_sentra[,6] <- 'SENTRA'
  Brand_Index_maxima[,6] <- 'MAXIMA'
  Brand_Index_altima[,6] <- 'ALTIMA'
  Brand_Index_xtrail[,6] <- 'XTRAIL'
  Brand_Index_murano[,6] <- 'MURANO'
  Brand_Index_pathfinder[,6] <- 'PATHFINDER'
  Brand_Index_frontier[,6] <- 'FRONTIER'
  Brand_Index_masivo[,6] <- 'MASIVO'
  Brand_Index_postpago[,6] <- 'POSTPAGO'
  Brand_Index_prepago[,6] <- 'PREPAGO'
  Brand_Index_unefon[,6] <- 'UNEFON'
  Brand_Index_masivo_att[,6] <- 'MASIVO_ATT'
  Brand_Index_current_masivo[,6] <- 'CURRENT_MASIVO'
  Brand_Index_non_current_masivo[,6] <- 'NON_CURRENT_MASIVO'
  
  Brand_Index_Final <- rbind(Brand_Index_march,
                             Brand_Index_kicks,
                             Brand_Index_leaf,
                             Brand_Index_versa,
                             Brand_Index_sentra,
                             Brand_Index_maxima,
                             Brand_Index_altima,
                             Brand_Index_xtrail,
                             Brand_Index_murano,
                             Brand_Index_pathfinder,
                             Brand_Index_frontier,
                             Brand_Index_masivo,
                             Brand_Index_postpago,
                             Brand_Index_prepago,
                             Brand_Index_unefon,
                             Brand_Index_masivo_att,
                             Brand_Index_current_masivo,
                             Brand_Index_non_current_masivo
                             )

  metrica <- data.frame(matrix(nrow = dim(Brand_Index_Final)[1], ncol = 6))
  
  metrica[,1] <- Brand_Index_Final[,3]
  
  metrica[,3] <- Brand_Index_Final[,1]
  
  for (i in seq(dim(catalogo_metrics_data)[1])) {
    
    metrica[as.numeric(row.names(metrica)[is.element(metrica[,1], catalogo_metrics_data[i,1])]),2] <- catalogo_metrics_data[i,2]
    
  }
  
  for (i in seq(dim(catalogo_marca)[1])) {
    
    metrica[as.numeric(row.names(metrica)[is.element(as.numeric(metrica[,3]),as.numeric(catalogo_marca[i,1]))]),4] <- catalogo_marca[i,3]
    metrica[as.numeric(row.names(metrica)[is.element(as.numeric(metrica[,3]),as.numeric(catalogo_marca_att[i,1]))]),4] <- catalogo_marca_att[i,3]
    
  }
  
  Brand_Index_Final[,3] <- metrica[,2]
  
  Brand_Index_Final[,1] <- metrica[,4]
  
  Brand_Index_Final$sSector <- NA
  
  #Brand_Index_Final[as.numeric(row.names(Brand_Index_Final)[is.element(Brand_Index_Final[,6],c('POSTPAGO','PREPAGO','UNEFON','MASIVO_ATT','CURRENT_MASIVO','NON_CURRENT_MASIVO'))]),7] <- Sector1
  
  Brand_Index_Final[as.numeric(row.names(Brand_Index_Final)[is.element(Brand_Index_Final[,6],c('POSTPAGO','PREPAGO','UNEFON','MASIVO_ATT','CURRENT_MASIVO','NON_CURRENT_MASIVO'))]),7] <- Sector1
  
  Brand_Index_Final[is.na(Brand_Index_Final[,7]),7] <- Sector
  
  Brand_Index_Final$nDate_Run <- as.character(Sys.Date())
  
  colnames(Brand_Index_Final) <- c('sBrand','dDate','sMetric','sProperty','nValue','sFilter','sSector','dUpdate')
  
  Brand_Index_Final[as.numeric(row.names(Brand_Index_Final)[is.na(Brand_Index_Final[,5])]),5] <- 0
  
#  for (i in dim(Brand_Index_Final)[1]) {
#    x <-sqlQuery(con,paste("insert into Competencia.dbo.f_BrandIndex_MEX", "(",paste(names(Brand_Index_Final), collapse = ','),")","values","(", paste(paste("'",Brand_Index_Final[i,],"'",sep  = ""),collapse = ','), ")",sep = ' '))
#  }
  
  #write.csv(Brand_Index_Final, paste(getwd(),'/Brand Index Final.csv',sep = ''),row.names = F)
  
  #write.csv(catalogo_marca, paste(getwd(),'/Catalogo.csv',sep = ''),row.names = F)
  
  save(Brand_Index_Final,file ="C:/Users/Luis.TorresP/Desktop/Códigos de R/API Brand Index/Bran_Index.rda")
  
  return(Brand_Index_Final)
}

Brand_Index <- API_Bran_Index()


