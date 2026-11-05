#################################################################
## Procesamiento mean sin preescalado: normalización en folds ##
#################################################################

#################################################################
##                     3. Consolidar datos                     ##
#################################################################

preparar_data_consolidada <- function() {
  
  #################################################################
  ##                         3. Datos.                           ##
  #################################################################
  
  # INFORMACION INEC
  
  data_inec_2022 = read_excel('../input/Data/Covariables_Actualizadas_2010_enviar/covariables/covariables_2022.xlsx')
  data_inec_2023 = read_excel('../input/Data/Covariables_Actualizadas_2010_enviar/covariables/covariables_2023.xlsx')
  #.-data_inec_2024 = read_excel('../input/Data//Covariables_Actualizadas_2010_enviar/covariables/covariables_2024.xlsx')
  
  CodCanton = data_inec_2023$CodCanton
  Canton = data_inec_2023$Canton
  
  # 2. ENVIRONMENTAL
  
  clima_canton = read_stata('../input/Data/ENVIRONMENTAL/Clima_Ecuador_canton.dta')
  defo_canton =  read_stata('../input/Data/ENVIRONMENTAL/Defo_Ecuador_canton.dta', encoding="latin1")
  
  clima_canton <- clima_canton %>% 
    mutate(CodCanton=DPA_CANTON) %>% 
    group_by(year,CodCanton) %>% 
    mutate(sd_precipitacion=sd(precipitacion,na.rm = T),
           max_temperature=max(temp_max),
           min_temperature=min(temp_min)) %>% 
    summarise_if(is.numeric, mean, na.rm = TRUE) %>% 
    filter(year==2023 | year==2022) %>% 
    select(year,CodCanton,sd_precipitacion,max_temperature,min_temperature)
  # Temperature missing for 2010
  
  defo_canton_2021 <- defo_canton %>% 
    mutate(CodCanton=DPA_CANTON) %>% 
    filter(year==2021) %>% 
    select(year,CodCanton,arealoss,areagain,areatreecover,
           gas_net,gas_emissions,area)
  
  
  defo_canton_2010 <- defo_canton %>% 
    mutate(CodCanton=DPA_CANTON) %>% 
    filter(year==2010) %>% 
    select(year,CodCanton,arealoss,areagain,areatreecover,
           gas_net,gas_emissions,area)
  
  # MERGE CON ENVIRONMENTAL
  
  names(data_inec_2022)[names(data_inec_2022) == 'CodCanton...2'] <- 'CodCanton'
  
  
  # merge con clima canton
  data1<-left_join(data_inec_2022,clima_canton[clima_canton$year == 2022,-1],
                   by = c("CodCanton"),keep = F)
  data2<-left_join(data_inec_2023,clima_canton[clima_canton$year == 2023,-1],
                   by = c("CodCanton"),keep = F)
  #.-data3<-left_join(data_inec_2024,clima_canton[clima_canton$year == 2024,-1],
  #.-                 by = c("CodCanton"),keep = F)
  
  
  # merge con deforestacion canton (asumir que los niveles se mantiene hasta 2024)
  data1<-left_join(data1,defo_canton_2021[,-1],
                   by = c("CodCanton"),keep = F)
  data2<-left_join(data2,defo_canton_2021[,-1],
                   by = c("CodCanton"),keep = F)
  #.-data3<-left_join(data3,defo_canton_2021[,-1],
  #.-                 by = c("CodCanton"),keep = F)
  
  # Impute missing values con la media para 3 cantones (Ok Gustavo)
  data1<- data1 %>% mutate(across(   
    c(sd_precipitacion, max_temperature, min_temperature,arealoss,areagain,areatreecover,
      gas_net,gas_emissions,area),
    ~ ifelse(is.na(.), mean(., na.rm = TRUE), .)))
  data2<- data2 %>% mutate(across(   
    c(sd_precipitacion, max_temperature, min_temperature,arealoss,areagain,areatreecover,
      gas_net,gas_emissions,area),
    ~ ifelse(is.na(.), mean(., na.rm = TRUE), .)))
  #.-data3<- data3 %>% mutate(across(   
  #.-  c(sd_precipitacion, max_temperature, min_temperature,arealoss,areagain,areatreecover,
  #.-    gas_net,gas_emissions,area),
  #.-  ~ ifelse(is.na(.), mean(., na.rm = TRUE), .)))
  
  # MERGE CON REMOTE
  
  trafico_interno_mean  <- read_csv(paste0("../input/Data/MOBILE/roads_canton_mean.csv"),show_col_types = FALSE)
  trafico_nacional_mean <- read_csv(paste0("../input/Data/MOBILE/roads_canton_national25_mean.csv"),show_col_types = FALSE)

  trafico_interno_sum  <- read_csv(paste0("../input/Data/MOBILE/roads_canton_sum.csv"),show_col_types = FALSE)
  trafico_nacional_sum <- read_csv(paste0("../input/Data/MOBILE/roads_canton_national25_sum.csv"),show_col_types = FALSE)
  
  data_mobile <- left_join(trafico_interno_mean,trafico_nacional_mean[,-c(2,3)],by = "canton",keep = F)
  
  data_mobile <- data_mobile %>% rename(CodCanton=canton) %>% arrange(CodCanton)
  
  data_mobile$CodCanton <- c(paste0(0,data_mobile$CodCanton[nchar(data_mobile$CodCanton)==3]),
    paste0(data_mobile$CodCanton[nchar(data_mobile$CodCanton)==4]))
  names(data_mobile)
  # Merge con datos INEC  
  data1<-left_join(data1,data_mobile[,-c(2,3)],by = "CodCanton",keep = F)
  data2<-left_join(data2,data_mobile[,-c(2,3)],by = "CodCanton",keep = F)
  
  # Cantones con missing
  data1[is.na(data1$viajes.x),c("CodCanton","Canton")]
  data2[is.na(data1$viajes.x),c("CodCanton","Canton")]
  
  # check cor antes
  #round(cor(data1$nbicenso2022,data1[,(1150-18):1150],use = "na.or.complete"),2)
  
  # Impute missing values con la mediana para 12 cantones (no cambia distribución ni la cor con la variable dep.)
  
  data1<- data1 %>% mutate(across(   
    c("viajes.x",           
      "viajeros.x",          "monday.x"        ,    "tuesday.x"       ,    "wednesday.x"   ,     
      "thursday.x",          "friday.x"        ,    "saturday.x"      ,    "sunday.x"     ,      
      "weekday_all.x",       "weekday_morning.x"  , "weekday_afternoon.x" ,"weekday_evening.x"  ,
      "weekday_night.x",     "weekend_all.x"    ,   "weekend_morning.x" ,  "weekend_afternoon.x",
      "weekend_evening.x",   "weekend_night.x"   ,  "viajes.y"          ,  "viajeros.y"      ,   
      "monday.y"          ,  "tuesday.y"         ,  "wednesday.y"     ,    "thursday.y"     ,    
      "friday.y"           , "saturday.y"        ,  "sunday.y"        ,    "weekday_all.y"   ,   
      "weekday_morning.y" ,  "weekday_afternoon.y", "weekday_evening.y" ,  "weekday_night.y"   , 
      "weekend_all.y"      , "weekend_morning.y"  , "weekend_afternoon.y", "weekend_evening.y"  ,
      "weekend_night.y"  ),
    ~ ifelse(is.na(.), 0, .))) #median(., na.rm = TRUE)
  
  data2<- data2 %>% mutate(across(   
    c("viajes.x",           
      "viajeros.x",          "monday.x"        ,    "tuesday.x"       ,    "wednesday.x"   ,     
      "thursday.x",          "friday.x"        ,    "saturday.x"      ,    "sunday.x"     ,      
      "weekday_all.x",       "weekday_morning.x"  , "weekday_afternoon.x" ,"weekday_evening.x"  ,
      "weekday_night.x",     "weekend_all.x"    ,   "weekend_morning.x" ,  "weekend_afternoon.x",
      "weekend_evening.x",   "weekend_night.x"   ,  "viajes.y"          ,  "viajeros.y"      ,   
      "monday.y"          ,  "tuesday.y"         ,  "wednesday.y"     ,    "thursday.y"     ,    
      "friday.y"           , "saturday.y"        ,  "sunday.y"        ,    "weekday_all.y"   ,   
      "weekday_morning.y" ,  "weekday_afternoon.y", "weekday_evening.y" ,  "weekday_night.y"   , 
      "weekend_all.y"      , "weekend_morning.y"  , "weekend_afternoon.y", "weekend_evening.y"  ,
      "weekend_night.y"  ),
    ~ ifelse(is.na(.), 0, .)))
  
  #check cor después
  #round(cor(data1$nbicenso2022,data1[,(1150-18):1150],use = "na.or.complete"),2)
  
  data1 <- data1[,-1:-6] # solo considerar X
  data2 <- data2[,-1:-4] # solo considerar X
  #.-data3 <- data3[,-1:-3] # solo considerar X
  #.-data4 <- data4[,-1:-2] # solo considerar X
  #.-data4 <- data4[,-ncol(data4)] # solo considerar X
  
  
  # Y
  Y_2022_censo<-data_inec_2022$nbicenso2022
  Y_2023_mef<-data_inec_2023$NBIMEF
  Y_2023_enemdu<-data_inec_2023$NBIENEMDU
  #.-Y_nbicenso2010<-data_inec_2010$nbicenso2010
  
  # Diccionario
  
  diccionario = read_excel('../input/Data/Covariables_Actualizadas_2010_enviar/diccionario_covariables_all.xlsx')
  diccionario = diccionario[,1:9]
  # Add diccionario para variables ambientales
  colnames(diccionario)
  add_diccionario_environ<-data.frame(matrix(nrow=9,ncol=9))
  names(add_diccionario_environ)<-colnames(diccionario)
  
  add_diccionario_environ$Area <- rep(x = "Sector cambio climático y deforestación",9)
  add_diccionario_environ$operacionestadistica <- c(rep(x = "Cambio climático",3),
                                                    rep(x = "Deforestación",6))
  add_diccionario_environ$Indicador <-c("sd_precipitacion", "max_temperature", 
                                        "min_temperature","arealoss","areagain","areatreecover",
                                        "gas_net","gas_emissions","area")
  
  add_diccionario_environ$en_2022<-rep(T,9)
  add_diccionario_environ$en_2023<-rep(T,9)
  add_diccionario_environ$en_2024<-rep(T,9)
  add_diccionario_environ$en_2010<-c(rep(F,3),rep(T,6))
  
  
  diccionario<-rbind(diccionario,add_diccionario_environ)
  
  
  # Add diccionario para variables telefonicas
  colnames(diccionario)
  add_diccionario_mobil<-data.frame(matrix(nrow=38,ncol=9))
  names(add_diccionario_mobil)<-colnames(diccionario)
  names(data_mobile)[-1:-3]
  add_diccionario_mobil$Area <- rep(x = "Datos móbiles",38)
  add_diccionario_mobil$operacionestadistica <- c(rep(x = "Datos Telefónica",38))
  add_diccionario_mobil$Indicador <-names(data_mobile)[-1:-3]
  
  add_diccionario_mobil$en_2022<-rep(T,38)
  add_diccionario_mobil$en_2023<-rep(T,38)
  
  
  diccionario<-rbind(diccionario,add_diccionario_mobil)
  #tail(diccionario,19)
  
    # Variables en 2022
    variables_in<-intersect(diccionario$Indicador[diccionario$en_2022],
                            diccionario$Indicador[diccionario$en_2023])
    #.-variables_in<-intersect(variables_in,
    #.-                        diccionario$Indicador[diccionario$en_2024])
    
    
    categoria_pri <- diccionario$Area[match(variables_in, diccionario$Indicador)][-1]
    categoria_sec <- diccionario$operacionestadistica[match(variables_in, diccionario$Indicador)][-1]
    
    # Considerar mismas variables
    X1<-data1[,colnames(data1) %in% variables_in]
    X2<-data2[,colnames(data2) %in% variables_in]
    #.-X3<-data3[,colnames(data3) %in% variables_in]
    
    
    # Missing data
    p <- .001 # porcentaje permitido de missings (NO MISSING) INEC CHECK
    
    missing_1<-as.matrix(apply(apply(X1,2, is.na),2,sum))
    missing_2<-as.matrix(apply(apply(X2,2, is.na),2,sum))
    #.-missing_3<-as.matrix(apply(apply(X3,2, is.na),2,sum))
    
    X1=X1[,c(missing_1<221*p)]
    X2=X2[,c(missing_2<221*p)]
    #.-X3=X3[,c(missing_3<221*p)]
    
    variables_in_2<-intersect(colnames(X1),colnames(X2))
    #.-variables_in_2<-intersect(variables_in_2,colnames(X3))
    
    # El escalado original eliminaba indirectamente las columnas constantes.
    # Se reproduce esa selección explícitamente, sin transformar los valores.
    has_variation <- function(x) {
      values <- unique(x[!is.na(x)])
      length(values) > 1L
    }
    varies_2022 <- vapply(X1[, variables_in_2, drop = FALSE], has_variation, logical(1))
    varies_2023 <- vapply(X2[, variables_in_2, drop = FALSE], has_variation, logical(1))
    variables_in_2 <- variables_in_2[varies_2022 & varies_2023]
    
    X1=X1[,names(X1) %in% variables_in_2]
    X2=X2[,names(X2) %in% variables_in_2]
    #.-X3=X3[,names(X3) %in% variables_in_2]
    
    # Diccionarios y datos finales #
    categoria_pri <- diccionario$Area[match(variables_in_2, diccionario$Indicador)]
    categoria_sec <- diccionario$operacionestadistica[match(variables_in_2, diccionario$Indicador)]
    
    X1f<-X1
    X2f<-X2
    #.-X3f<-X3
    ################################
    
    return(list("X1"=X1f,"X2"=X2f,
                #.-"X3"=X3f,
                "categoria_pri"=categoria_pri,
                "categoria_sec"=categoria_sec,
                "Y_2022_censo"=data_inec_2022$nbicenso2022,
                "Y_2023_mef"=data_inec_2023$NBIMEF,
                "Y_2023_enemdu"=data_inec_2023$NBIENEMDU,
                #.-"Y_2024_enemdu"=data_inec_2024$NBIenemdu,
                "CodCanton" = data_inec_2023$CodCanton,
                "Canton" = data_inec_2023$Canton))

}
