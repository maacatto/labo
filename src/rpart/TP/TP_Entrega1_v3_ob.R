#'------------------------------------------------------------------------------
# INICIO ----
#'------------------------------------------------------------------------------
#formato sin notacion cientifica
options(scipen = 50)

#limpio la memoria
rm( list=ls() )  #remove all objects
gc()             #garbage collection


#Arbol elemental con libreria  rpart
#Debe tener instaladas las librerias  data.table  ,  rpart  y  rpart.plot

#cargo las librerias que necesito
require("data.table")
require("rlist")

require("rpart")
require("parallel")

#paquetes necesarios para la Bayesian Optimization
require("DiceKriging")
require("mlrMBO")
require("rpart.plot")
require("ggplot2")
require("readr")

require("dplyr")


seeds <- c( 807299, 962041, 705689, 909463, 637597 )

#Aqui se debe poner la carpeta de la materia de SU computadora local
setwd("G:\\My Drive\\Facultad\\Maestria DM\\Expecializacion\\DMEyF\\Carpetas")  #Establezco el Working Directory

dir_salidas="./labo/src/rpart/TP/exp"
dir.create( dir_salidas )


#'------------------------------------------------------------------------------
# 1. Lectura de datos ----
#'------------------------------------------------------------------------------


# cargo el dataset
# dataset  <- fread("./datasets/competencia1_2022.csv")

#'------------------------------------------------------------------------------
# 2. Funciones Auxiliares ----
#'------------------------------------------------------------------------------

#particionar agrega una columna llamada fold a un dataset que consiste en una particion estratificada segun agrupa
# particionar( data=dataset, division=c(70,30), agrupa=clase_ternaria, seed=semilla)   crea una particion 70, 30 

particionar  <- function( data,  division, agrupa="",  campo="fold", start=1, seed=NA )
{
  if( !is.na(seed) )   set.seed( seed )
  
  bloque  <- unlist( mapply(  function(x,y) { rep( y, x )} ,   division,  seq( from=start, length.out=length(division) )  ) )  
  
  data[ , (campo) :=  sample( rep( bloque, ceiling(.N/length(bloque))) )[1:.N],
        by= agrupa ]
}

ArbolEstimarGanancia  <- function( semilla, param_basicos )
{
  #particiono estratificadamente el dataset
  particionar( dataset, division=c(7,3), agrupa="clase_ternaria", seed= semilla )  #Cambiar por la primer semilla de cada uno !
  
  #genero el modelo
  modelo  <- rpart("clase_ternaria ~ .",     #quiero predecir clase_ternaria a partir del resto
                   data= dataset[ fold==1],  #fold==1  es training,  el 70% de los datos
                   xval= 0,
                   control= param_basicos )  #aqui van los parametros del arbol
  
  #aplico el modelo a los datos de testing
  prediccion  <- predict( modelo,   #el modelo que genere recien
                          dataset[ fold==2],  #fold==2  es testing, el 30% de los datos
                          type= "prob") #type= "prob"  es que devuelva la probabilidad
  
  #prediccion es una matriz con TRES columnas, llamadas "BAJA+1", "BAJA+2"  y "CONTINUA"
  #cada columna es el vector de probabilidades 
  
  
  #calculo la ganancia en testing  qu es fold==2
  ganancia_test  <- dataset[ fold==2, 
                             sum( ifelse( prediccion[, "BAJA+2"]  >  0.025,
                                          ifelse( clase_ternaria=="BAJA+2", 78000, -2000 ),
                                          0 ) )]
  
  #escalo la ganancia como si fuera todo el dataset
  ganancia_test_normalizada  <-  ganancia_test / 0.3
  
  return( ganancia_test_normalizada )
}

ArbolesMontecarlo  <- function( semillas,  param_basicos )
{
  ganancias  <- mcmapply( ArbolEstimarGanancia, 
                          semillas,   #paso el vector de semillas, que debe ser el primer parametro de la funcion ArbolEstimarGanancia
                          MoreArgs= list( param_basicos),  #aqui paso el segundo parametro
                          SIMPLIFY= FALSE,
                          mc.cores= 16 )  #se puede subir a 5 si posee Linux o Mac OS
  
  #media de las ganancias
  return(  unlist(ganancias) )
}


## Optimizacion Bayesiana ----
#fold_test  tiene el numero de fold que voy a usar para testear, entreno en el resto de los folds
#param tiene los hiperparametros del arbol


ArbolFinal <- function( columnas="clase_ternaria ~ .", datos, param)
{
  modelo <- rpart(columnas, 
                  data= datos,  #entreno en todo MENOS el fold_test que uso para testing
                  xval= 0,
                  control= param )
  return(modelo)
}

ArbolSimple  <- function( fold_test, data, cols, param )
{
  #genero el modelo
  modelo <- ArbolFinal(cols, data, param)
  #modelo  <- rpart("clase_ternaria ~ .", 
  #                 data= data[ fold != fold_test, ],  #entreno en todo MENOS el fold_test que uso para testing
  #                 xval= 0,
  #                 control= param )
  
  #aplico el modelo a los datos de testing
  prediccion  <- predict( modelo, 
                          data[ fold==fold_test, ],  #aplico el modelo sobre los datos de testing
                          type= "prob")   #quiero que me devuelva probabilidades
  
  prob_baja2  <- prediccion[, "BAJA+2"]  #esta es la probabilidad de baja
  
  #calculo la ganancia
  ganancia_testing  <- data[ fold==fold_test ][ prob_baja2 > 1/40,  
                                                sum( ifelse( clase_ternaria=="BAJA+2", 78000, -2000 ) )] 
  
  return( ganancia_testing )  #esta es la ganancia sobre el fold de testing, NO esta normalizada
}
#'------------------------------------------------------------------------------

ArbolesCrossValidation  <- function( data, cols, param, qfolds, pagrupa, semilla )
{
  divi  <- rep( 1, qfolds )  # generalmente  c(1, 1, 1, 1, 1 )  cinco unos
  
  particionar( data, divi, seed=semilla, agrupa=pagrupa )  #particiono en dataset en folds
  
  ganancias  <- mcmapply( ArbolSimple, 
                          seq(qfolds), # 1 2 3 4 5
                          MoreArgs= list( data, cols, param), 
                          SIMPLIFY= FALSE,
                          mc.cores= 1 )   #se puede subir a qfolds si posee Linux o Mac OS
  
  data[ , fold := NULL ]
  
  #devuelvo la primer ganancia y el promedio
  ganancia_promedio  <- mean( unlist( ganancias ) )   #promedio las ganancias
  ganancia_promedio_normalizada  <- ganancia_promedio * qfolds  #aqui normalizo la ganancia
  
  return( ganancia_promedio_normalizada )
}
#'------------------------------------------------------------------------------
#esta funcion solo puede recibir los parametros que se estan optimizando
#el resto de los parametros, lamentablemente se pasan como variables globales

EstimarGanancia  <- function( x )
{
  GLOBAL_iteracion  <<-  GLOBAL_iteracion + 1
  
  xval_folds  <- 5
  ganancia  <- ArbolesCrossValidation( dataset, columnas,
                                       param= x, #los hiperparametros del arbol
                                       qfolds= xval_folds,  #la cantidad de folds
                                       pagrupa= "clase_ternaria",
                                       semilla= ksemilla_azar )
  
  #logueo 
  xx  <- x
  xx$xval_folds  <-  xval_folds
  xx$ganancia  <- ganancia
  xx$iteracion <- GLOBAL_iteracion
  loguear( xx,  arch= archivo_log )
  
  return( ganancia )
}

#'------------------------------------------------------------------------------
#graba a un archivo los componentes de lista
#para el primer registro, escribe antes los titulos

loguear  <- function( reg, arch=NA, folder="./work/", ext=".csv", verbose=TRUE )
{
  archivo  <- arch
  if( is.na(arch) )  archivo  <- paste0( folder, substitute( reg), ext )
  
  if( !file.exists( archivo ) )  #Escribo los titulos
  {
    linea  <- paste0( "fecha,", 
                      paste( list.names(reg), collapse="," ), "\n" )
    
    cat( linea, file=archivo )
  }
  
  linea  <- paste0( format(Sys.time(), "%Y%m%d %H%M%S"),  ",",     #la fecha y hora
                    gsub( ", ", ",", toString( reg ) ),  "\n" )
  
  cat( linea, file=archivo, append=TRUE )  #grabo al archivo
  
  if( verbose )  cat( linea )   #imprimo por pantalla
}

#'------------------------------------------------------------------------------

OptBayesiana <- function(dataset,columnas, GLOBAL_iteracion, archivo_log, archivo_BO) {
#Atencion: Requiere los parametros como variables globales, con ese nombre
  #Defino la  Optimizacion Bayesiana
  
  # kBO_iter  <- 100   #cantidad de iteraciones de la Optimizacion Bayesiana
  # 
  # hs  <- makeParamSet(
  #   makeNumericParam("cp"       , lower= -1   , upper=    0.1),
  #   makeIntegerParam("minsplit" , lower=  1L  , upper= 8000L),  #la letra L al final significa ENTERO
  #   makeIntegerParam("minbucket", lower=  1L  , upper= 4000L),
  #   makeIntegerParam("maxdepth" , lower=  3L  , upper=   20L),
  #   forbidden = quote( minbucket > 0.5*minsplit ) )             # minbuket NO PUEDE ser mayor que la mitad de minsplit
  # 
  # ksemilla_azar  <- seeds[0]   #cambiar por la primer semilla
  # 
  
  ##creo la carpeta donde va el experimento
  ## HT  representa  Hiperparameter Tuning
  #dir.create( "./exp/",  showWarnings = FALSE ) 
  #dir.create( "./exp/TP/", showWarnings = FALSE )
  ##setwd("./exp/TP/")   #Establezco el Working Directory DEL EXPERIMENTO
  
  
  #archivo_log  <- "./exp/TP/HT321.txt"
  #archivo_BO   <- "./exp/TP/HT321.RDATA"
  
  #leo si ya existe el log, para retomar en caso que se se corte el programa
  GLOBAL_iteracion  <- 0
  
  if( file.exists(archivo_log) )
  {
    tabla_log  <- fread( archivo_log )
    GLOBAL_iteracion  <- nrow( tabla_log )
  }
  
  
  
  #Aqui comienza la configuracion de la Bayesian Optimization
  
  funcion_optimizar  <- EstimarGanancia
  
  configureMlr( show.learner.output= FALSE)
  
  #configuro la busqueda bayesiana,  los hiperparametros que se van a optimizar
  #por favor, no desesperarse por lo complejo
  obj.fun  <- makeSingleObjectiveFunction(
    fn=       funcion_optimizar,
    minimize= FALSE,   #estoy Maximizando la ganancia
    noisy=    TRUE,
    par.set=  hs,
    has.simple.signature = FALSE
  )
  
  ctrl  <- makeMBOControl( save.on.disk.at.time= 120,  save.file.path= archivo_BO)
  ctrl  <- setMBOControlTermination(ctrl, iters= kBO_iter )
  ctrl  <- setMBOControlInfill(ctrl, crit= makeMBOInfillCritEI())
  
  surr.km  <- makeLearner("regr.km", predict.type= "se", covtype= "matern3_2", control= list(trace= TRUE))
  
  #inicio la optimizacion bayesiana
  if( !file.exists( archivo_BO ) ) {
    
    run  <- mbo( fun=     obj.fun, 
                 learner= surr.km,
                 control= ctrl)
    
  } else  run  <- mboContinue( archivo_BO )   #retomo en caso que ya exista
  
}



#'------------------------------------------------------------------------------
# 3. FEATURE ENGINEERING ----
#'-----------------------------------------------------------------------------

dataset_fe <- fread("./datasets/competencia1_2022.csv")


## imputar nulos? -----  
#dataset_fe <- dataset_fe[,lapply(.SD,function(x){ifelse(is.na(x),-1,x)})]


## Agrega columnas con expresiones logicas surgidas de arbol pequeño (BAJA+2)  ----
  # ctrx_quarter < 14 & mcuentas_saldo < −1256.1 & cprestamos_personales >=2 & Visa_status <8 & mcuenta_corriente < −1.1319e+6
  # ctrx_quarter < 14 & mcuentas_saldo >= −1256.1 & mcaja_ahorro < 2601.1 & Visa_fechaalta >= 4539 & mcaja_ahorro >= 2544.3
  dataset_fe$expresion1 <- ifelse(dataset_fe$ctrx_quarter < 14 & dataset_fe$mcuentas_saldo < -1256.1 & dataset_fe$cprestamos_personales >= 2 & dataset_fe$Visa_status < 8 & dataset_fe$mcuenta_corriente < -1131900.0 , 1, 0)
  dataset_fe[,sum(expresion1)]

  ## Data Drifting  ----
  #-mcomisiones_mantenimiento -Visa_mpagado?
  #mcomisiones_mantenimiento 
  #Master_mpagado
  #Visa_fultimo_cierre

  # corrijo manualmente el drifting de  Visa_fultimo_cierre
  dataset_fe[foto_mes==202103 & Visa_fultimo_cierre== 1, Visa_fultimo_cierre :=  4 ]
  dataset_fe[foto_mes==202103 & Visa_fultimo_cierre== 7, Visa_fultimo_cierre := 11 ]
  dataset_fe[foto_mes==202103 & Visa_fultimo_cierre==21, Visa_fultimo_cierre := 25 ]
  dataset_fe[foto_mes==202103 & Visa_fultimo_cierre==14, Visa_fultimo_cierre := 18 ]
  dataset_fe[foto_mes==202103 & Visa_fultimo_cierre==28, Visa_fultimo_cierre := 32 ]
  dataset_fe[foto_mes==202103 & Visa_fultimo_cierre==35, Visa_fultimo_cierre := 39 ]
  dataset_fe[foto_mes==202103 & Visa_fultimo_cierre> 39, Visa_fultimo_cierre := Visa_fultimo_cierre + 4 ]
  
  # corrijo manualmente el drifting de  Visa_fultimo_cierre
  dataset_fe[foto_mes==202103 & Master_fultimo_cierre== 1, Master_fultimo_cierre :=  4 ]
  dataset_fe[foto_mes==202103 & Master_fultimo_cierre== 7, Master_fultimo_cierre := 11 ]
  dataset_fe[foto_mes==202103 & Master_fultimo_cierre==21, Master_fultimo_cierre := 25 ]
  dataset_fe[foto_mes==202103 & Master_fultimo_cierre==14, Master_fultimo_cierre := 18 ]
  dataset_fe[foto_mes==202103 & Master_fultimo_cierre==28, Master_fultimo_cierre := 32 ]
  dataset_fe[foto_mes==202103 & Master_fultimo_cierre==35, Master_fultimo_cierre := 39 ]
  dataset_fe[foto_mes==202103 & Master_fultimo_cierre> 39, Master_fultimo_cierre := Master_fultimo_cierre + 4 ]

  #Corregir saldos por inflación
  
  
  
## n_Total_Pagos: deuda total por cliente (todas las tarjetas)----
  dataset_fe[,n_Total_Pagos := Visa_mpagado + Master_mpagado]
  dataset_fe[,n_Saldo_antes_pagos := mcuentas_saldo + n_Total_Pagos]

  
  
## n_visa_minimo_pagado: pagó el minimo? ----
  dataset_fe[, n_visa_minimo_pagado := ifelse(Visa_mpagado >= Visa_mpagominimo,1,0)]
  dataset_fe[, n_master_minimo_pagado := ifelse(Master_mpagado >= Master_mpagominimo,1,0)]
  dataset_fe[, n_tarjetas_minimos_pagado := ifelse(is.na(n_visa_minimo_pagado),0,n_visa_minimo_pagado) + ifelse(is.na(n_master_minimo_pagado),0,n_master_minimo_pagado)]
  
## Saldos
  dataset_fe[,n_monedas_saldo := mforex_buy - mforex_sell ]
  

## concentrar varios montos, misma naturaleza
  dataset_fe[,n_mpayroll := mpayroll + mpayroll2]
  
  dataset_fe[,n_minversion := minversion1_pesos + minversion1_dolares + minversion2]
  
  dataset_fe[,n_mprestamos := mprestamos_personales + mprestamos_prendarios + mprestamos_hipotecarios]
  dataset_fe[,n_mcomisiones := mcomisiones + mcomisiones_otras]
  dataset_fe[,n_mtotal_ganacia_banco := n_mcomisiones + mactivos_margen + mpasivos_margen]
  

  dataset_fe[,n_Total_Ingresos := n_mpayroll + mtransferencias_recibidas + mforex_sell ]
  
  dataset_fe[, n_tarjetas_saldo_total := ifelse(is.na(Visa_msaldototal),0,Visa_msaldototal) + ifelse(is.na(Master_msaldototal),0,Master_msaldototal)]
  
  
##monto unitarios de prestamos
  dataset_fe[,n_munitario_prestamos_personales := mprestamos_personales / cprestamos_personales]
  dataset_fe[, n_munitario_prestamos_personales_nonull := ifelse(is.na(n_munitario_prestamos_personales),0,n_munitario_prestamos_personales) ]
  
  dataset_fe[,n_munitario_prestamos_prendarios := mprestamos_prendarios / cprestamos_prendarios]
  dataset_fe[, n_munitario_prestamos_prendarios_nonull := ifelse(is.na(n_munitario_prestamos_prendarios),0,n_munitario_prestamos_prendarios) ]
  
  dataset_fe[,n_munitario_prestamos_hipotecarios := mprestamos_hipotecarios / cprestamos_hipotecarios]
  dataset_fe[, n_munitario_prestamos_hipotecarios_nonull := ifelse(is.na(n_munitario_prestamos_hipotecarios),0,n_munitario_prestamos_hipotecarios) ]

#edad
  dataset_fe[,n_antig_segun_edad := (cliente_antiguedad / 12) / cliente_edad]
  dataset_fe[,n_edad_alta := cliente_edad - (cliente_antiguedad / 12)]
  
  
##kmeans----
  # require(stats)
  # cantidad_clusters=30
  # 
  # #dtrain.numero_de_cliente = dtrain$numero_de_cliente
  # dataset_fe_scaled <- dataset_fe[,mcuentas_saldo,cdescubierto_preacordado] %>%  mutate_if(is.numeric, scale)
  # #dtrain$numero_de_cliente = dtrain.numero_de_cliente
  # CL  = kmeans(dataset_fe_scaled,cantidad_clusters)
  # dataset_fe$kmeans = CL$cluster
  # CL$cluster 
  # 
  # #Hist
  # ggplot(dataset_fe, aes(x=kmeans)) + 
  #   geom_histogram()
  
## * cantidad de productos. menos es mas probable que te vayas ---- 
  
## * binarizar ---- 
  # #Hist
  ggplot( dataset_fe[foto_mes==202101 & Visa_status>=6], aes(x=clase_ternaria)) + 
    geom_histogram(stat="count")
  
  dataset_fe[, n_visa_status_bin := ifelse(Visa_status>=6,1,0)]
  dataset_fe[, n_master_status_bin := ifelse(Master_status>=6,1,0)]
  dataset_fe[, n_trjs_cierre_tot := ifelse((n_visa_status_bin==1 & n_master_status_bin==1) |
                                                   (is.na(Visa_status) & n_master_status_bin==1) |
                                                   (is.na(Master_status) & n_visa_status_bin==1) ,2,1)]
  dataset_fe[, n_trjs_cierre_tot := ifelse(is.na(n_trjs_cierre_tot),0,n_trjs_cierre_tot)]

## Clase Binaria  ----
  #creo la clase_binaria SI={ BAJA+1, BAJA+2 }    NO={ CONTINUA }
  dataset_fe[ foto_mes==202101, 
           clase_binaria :=  ifelse( clase_ternaria=="CONTINUA", "NO", "SI" ) ]


## Agrego canaritos----

  #agrego 30 canaritos
  for( i in 1:30 ) dataset_fe[ , paste0("n_canarito", i ) :=  runif( nrow(dataset_fe)) ]


#'------------------------------------------------------------------------------
# 4. ENTRENAMIENTO ----
#'------------------------------------------------------------------------------

  #'------------------------------------------------------------------------------
  ## a  dTrain / dApply ----
  #'------------------------------------------------------------------------------
  dtrain  <- dataset_fe[ foto_mes==202101 ]  #defino donde voy a entrenar
  dapply  <- dataset_fe[ foto_mes==202103 ]  #defino donde voy a aplicar el modelo

  
  #'------------------------------------------------------------------------------
  ## b Correcciones 202103: Inflación----
  #'------------------------------------------------------------------------------
  #dtrain$mcuentas_saldo <- (dtrain$mcuentas_saldo - mean(dtrain$mcuentas_saldo)) / sd(dtrain$mcuentas_saldo)
  dapply$mcuentas_saldo <- dapply$mcuentas_saldo / 1.033 / 1.047
  dapply$mcuenta_corriente <- dapply$mcuenta_corriente / 1.033 / 1.048
  dapply$mprestamos_personales <- dapply$mprestamos_personales / 1.033 / 1.049
  dapply$mcaja_ahorro <- dapply$mcaja_ahorro / 1.033 / 1.050
  dapply$mactivos_margen <- dapply$mactivos_margen / 1.033 / 1.051
  dapply$mpasivos_margen <- dapply$mpasivos_margen / 1.033 / 1.052
  dapply$mtarjeta_visa_consumo <- dapply$mtarjeta_visa_consumo / 1.033 / 1.053
  dapply$mcomisiones <- dapply$mcomisiones / 1.033 / 1.054
  dapply$mcomisiones_otras <- dapply$mcomisiones_otras / 1.033 / 1.055
  dapply$Visa_msaldototal <- dapply$Visa_msaldototal / 1.033 / 1.056
  dapply$Visa_msaldopesos <- dapply$Visa_msaldopesos / 1.033 / 1.057
  dapply$mrentabilidad_annual <- dapply$mrentabilidad_annual / 1.033 / 1.058
  dapply$Visa_mpagominimo <- dapply$Visa_mpagominimo / 1.033 / 1.059
  dapply$mrentabilidad <- dapply$mrentabilidad / 1.033 / 1.060
  dapply$mpayroll <- dapply$mpayroll / 1.033 / 1.061
  dapply$mtransferencias_recibidas <- dapply$mtransferencias_recibidas / 1.033 / 1.062
  dapply$Visa_mpagospesos <- dapply$Visa_mpagospesos / 1.033 / 1.063
  dapply$Visa_mconsumospesos <- dapply$Visa_mconsumospesos / 1.033 / 1.064
  dapply$Visa_mconsumototal <- dapply$Visa_mconsumototal / 1.033 / 1.065
  dapply$mextraccion_autoservicio <- dapply$mextraccion_autoservicio / 1.033 / 1.066
  dapply$mautoservicio <- dapply$mautoservicio / 1.033 / 1.067
  dapply$mcaja_ahorro_dolares <- dapply$mcaja_ahorro_dolares / 1.033 / 1.068
  dapply$mplazo_fijo_dolares <- dapply$mplazo_fijo_dolares / 1.033 / 1.069
  dapply$mtransferencias_emitidas <- dapply$mtransferencias_emitidas / 1.033 / 1.070
  
  dapply$matm <- dapply$matm / 1.033 / 1.072
  dapply$mcheques_emitidos <- dapply$mcheques_emitidos / 1.033 / 1.073
  
  dapply$mcheques_depositados <- dapply$mcheques_depositados / 1.033 / 1.075
  dapply$matm_other <- dapply$matm_other / 1.033 / 1.076
  dapply$mtarjeta_master_consumo <- dapply$mtarjeta_master_consumo / 1.033 / 1.077
  
  dapply$mpagomiscuentas <- dapply$mpagomiscuentas / 1.033 / 1.079
  dapply$Visa_mlimitecompra <- dapply$Visa_mlimitecompra / 1.033 / 1.080
  dapply$minversion1_dolares <- dapply$minversion1_dolares / 1.033 / 1.081
  dapply$mcheques_depositados_rechazados <- dapply$mcheques_depositados_rechazados / 1.033 / 1.082
  dapply$Visa_mfinanciacion_limite <- dapply$Visa_mfinanciacion_limite / 1.033 / 1.083
  dapply$mttarjeta_master_debitos_automaticos <- dapply$mttarjeta_master_debitos_automaticos / 1.033 / 1.084
  dapply$minversion2 <- dapply$minversion2 / 1.033 / 1.085



# dtrain.numero_de_cliente = dtrain$numero_de_cliente
# dtrain <- dtrain[,!"numero_de_cliente"] %>%  mutate_if(is.numeric, scale)
# dtrain$numero_de_cliente = dtrain.numero_de_cliente
# 
# dapply.numero_de_cliente = dapply$numero_de_cliente
# dapply <- dapply[,!"numero_de_cliente"] %>%  mutate_if(is.numeric, scale)
# dapply$numero_de_cliente = dapply.numero_de_cliente



  #'------------------------------------------------------------------------------
  ## d rpart----
  #'------------------------------------------------------------------------------
  ### columnas <------ ----
  #columnas <- "clase_ternaria ~ . -clase_binaria"
  #columnas <- "clase_binaria ~ . -clase_ternaria"
  columnas <- "clase_binaria ~ . -clase_ternaria -mcomisiones_mantenimiento -Master_mpagominimo -Visa_mpagominimo"
  #columnas <- "clase_ternaria ~ . -clase_binaria -mcomisiones_mantenimiento -Master_mpagominimo -Visa_mpagominimo"
  
  
  ### hyperparametros <------ ----
  #PARAMS de 412_rpart_binaria_aplicar (mejores en kaggle)
  hyperparametros <- list(cp=          -0.54,#  -0.89
                          minsplit=  1073,   # 621
                          minbucket=  278,   # 309
                          maxdepth=     20 )  #  12
  # #22M
  # hyperparametros <- list(cp=          -0.307,#  -0.89
  #                         minsplit=  1707,   # 621
  #                         minbucket=  660,   # 309
  #                         maxdepth=     10 )  #  12
  # #28811000
  # hyperparametros <- list(cp=          -0.2332,#  -0.89
  #                         minsplit=  8,   # 621
  #                         minbucket=  4,   # 309
  #                         maxdepth=     12 )  #  12
  # #37791000
  # hyperparametros <- list(cp=          -0.3539,#  -0.89
                           # minsplit=  6,   # 621
                           # minbucket=  2,   # 309
                           # maxdepth=     20 )  #  12
#hyperparametros <- list(cp=          -0.723,#  -0.89
#                        minsplit=  315,   # 621
#                        minbucket=  156,   # 309
#                        maxdepth=     12 )  #  12
  #PARAMS mios, de Optimizacion Bayesiana con clase ternaria
  # cp=        -0.076525636,   #esto significa no limitar la complejidad de los splits
  # minsplit=  113,     #minima cantidad de registros para que se haga el split
  # minbucket= 53,     #tamaño minimo de una hoja
  # maxdepth=  6 )    #profundidad maxima del arbol
  
  
  #genero el modelo,  aqui se construye el arbol
  modelo  <- rpart(formula=   columnas,
                   data=      dtrain,  #los datos donde voy a entrenar
                   xval=      0,
                   control= hyperparametros)
  
  
## e PODA <------ ----
  modelo$frame[ modelo$frame$var %like% "canarito", "complexity"] <- -666
  modelo  <- prune(  modelo, -666 )
  
#'------------------------------------------------------------------------------
# 5. Analisis post-entrenamiento ----
#'------------------------------------------------------------------------------
# Features importance 
modelo$variable.importance

importances <- modelo$variable.importance/sum(modelo$variable.importance)
round(importances, 2)
importances_df <- data.table(attribute = names(importances), importance = importances)
row.names(importances_df) <- NULL
#Tabla
print.data.frame(importances_df, digits = 3)
write_csv(importances_df, path = paste0(dir_salidas,"/",format(Sys.time(), "%Y%m%d_%H%M%S_"),"featureImportance.csv"))

#Graficar e imprimir pdf
pdf(file = paste0(dir_salidas,"/",format(Sys.time(), "%Y%m%d_%H%M%S_"),".pdf"), width=28, height=4 )
  #Features importance
  ggplot(data=importances_df, aes(x=reorder(attribute,-importance), y=importance)) + 
    geom_bar(stat="identity", fill="steelblue") +
    labs(title = "Variable importance for monster type prediction") +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 60,vjust = 1, hjust=1), axis.text.y = element_text(size=15),
          axis.title.x = element_text(size = 20, color = "red"), axis.title.y = element_text(size = 20, color = "red"),
          plot.title=element_text(color = "black", size = 20, face = "bold", hjust = 0.5, vjust = 0))


  #grafico el arbol
  prp(modelo, extra=101, digits=6, branch=1, type=4, varlen=0, faclen=0)

dev.off()

#Boxplot
ggplot(dataset_fe[mcuentas_saldo<100000 & foto_mes==202101 & Visa_status>2], aes(x = clase_ternaria, y = Visa_status)) + 
  geom_boxplot() + labs(y = "accuracy")+ labs(x = "")

# ggplot(dataset_fe[ foto_mes==202101 & kmeans==2 ], aes(x =kmeans, y = mcuentas_saldo)) + 
#   geom_boxplot() + labs(y = "accuracy")+ labs(x = "")
# 
# #Hist
# ggplot( dataset_fe[foto_mes==202101 & Visa_status>=4], aes(x=clase_ternaria)) + 
#   geom_histogram(stat="count")


#'------------------------------------------------------------------------------
# 6. Predicción para Kaggle ----
#'------------------------------------------------------------------------------

#aplico el modelo a los datos nuevos
prediccion  <- predict( object= modelo,
                        newdata= dapply,
                        type = "prob")
#prediccion es una matriz con DOS columnas, llamadas "NO", "SI"
#cada columna es el vector de probabilidades 


#agrego a dapply una columna nueva que es la probabilidad de BAJA+2
dfinal  <- copy( dapply[ , list(numero_de_cliente) ] )
dfinal[ , prob_SI := prediccion[ , "SI"] ]


# por favor cambiar por una semilla propia
# que sino el Fiscal General va a impugnar la prediccion
set.seed(807299)  
dfinal[ , azar := runif( nrow(dapply) ) ]

# ordeno en forma descentente, y cuando coincide la probabilidad, al azar
setorder( dfinal, -prob_SI, azar )


archivos_kaggle=format(Sys.time(), "%Y%m%d_%H%M%S_")

#archiv de descripcion del submission
xx <- list("columnas"=columnas,"hyperparametros"=hyperparametros)
loguear( xx,  arch= paste0( dir_salidas,"/", archivos_kaggle, "descr.txt") )


#for( corte  in  c( 7500, 8000, 8500, 9000, 9500, 10000, 10500, 11000 ) )
for( corte  in  c(  9500) )
{
  dfinal_predicted <- dfinal
  
  #le envio a los  corte  mejores,  de mayor probabilidad de prob_SI
  dfinal_predicted[ , Predicted := 0L ]
  dfinal_predicted[ 1:corte , Predicted := 1L ]

  # ordeno de nuevo por numero_de_cliente
  setorder( dfinal_predicted, numero_de_cliente,Predicted )
  
  fwrite( dfinal_predicted[ , list(numero_de_cliente, Predicted) ], #solo los campos para Kaggle
          file= paste0( dir_salidas,"/",archivos_kaggle,  corte, ".csv"),
          sep=  "," )
}



-------



#Defino la  Optimizacion Bayesiana

kBO_iter  <- 100   #cantidad de iteraciones de la Optimizacion Bayesiana

hs  <- makeParamSet(
  makeNumericParam("cp"       , lower= -1   , upper=    0.1),
  makeIntegerParam("minsplit" , lower=  1L  , upper= 8000L),  #la letra L al final significa ENTERO
  makeIntegerParam("minbucket", lower=  1L  , upper= 4000L),
  makeIntegerParam("maxdepth" , lower=  3L  , upper=   14L),
  forbidden = quote( minbucket > 0.5*minsplit ) )             # minbuket NO PUEDE ser mayor que la mitad de minsplit

ksemilla_azar  <- seeds[1]   #cambiar por la primer semilla


archivo_log  <- paste0( dir_salidas,"/", "20220911_034709", "OB.txt")
archivo_BO   <- paste0( dir_salidas,"/", "20220911_034709", "OB..RDATA") 

#leo si ya existe el log, para retomar en caso que se se corte el programa
GLOBAL_iteracion  <- 0
dataset = dataset_fe
columnas <- "clase_ternaria ~ . -clase_binaria -mcomisiones_mantenimiento -Master_mpagominimo -Visa_mpagominimo"

OptBayesiana(dataset,columnas,GLOBAL_iteracion,archivo_log,archivo_BO)
