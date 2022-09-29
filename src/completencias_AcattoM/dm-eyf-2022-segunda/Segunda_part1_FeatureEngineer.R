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
require("yaml")


require("ranger")
require("randomForest")  #solo se usa para imputar nulos
require("parallel")

#paquetes necesarios para la Bayesian Optimization
require("DiceKriging")
require("mlrMBO")

require("rpart.plot")
require("ggplot2")
require("readr")

require("dplyr")
require("readODS")


seeds <- c( 807299, 962041, 705689, 909463, 637597 )

#Aqui se debe poner la carpeta de la materia de SU computadora local
setwd("G:\\My Drive\\Facultad\\Maestria DM\\Expecializacion\\DMEyF\\Carpetas")  #Establezco el Working Directory
#setwd("~/buckets/b1/")   #Establezco el Working Directory

dir_salidas="./exp/TP/"
dir.create( dir_salidas )

#'------------------------------------------------------------------------------
# 1. Lectura de datos ----
#'------------------------------------------------------------------------------


# cargo el dataset
#dataset_fe <- fread("./datasets/competencia1_2022.csv")

dataset_fe  <- fread("./datasets/competencia2_2022.csv.gz", stringsAsFactors= TRUE)


#diccionario <- read_ods("./datasets/DiccionarioDatos.ods")
#diccionario <- data.table(diccionario)
#var.monet <- diccionario[unidad=="pesos", .(campo)]

#'------------------------------------------------------------------------------
# 2. Funciones Auxiliares ----
#'------------------------------------------------------------------------------

crearCheckpoint <- function(path,filename) 
{
  require(rstudioapi)
  file.copy(rstudioapi::getSourceEditorContext()$path,
            to = file.path(dir_salidas,
                           paste0(archivos_kaggle, "antes.R")))
  documentSave()
  file.copy(rstudioapi::getSourceEditorContext()$path,
            to = file.path(dir_salidas,
                           paste0(archivos_kaggle, ".R")))
}



#'------------------------------------------------------------------------------
# 3. FEATURE ENGINEERING ----
#'-----------------------------------------------------------------------------


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
dataset_fe[foto_mes==202105 & Visa_fultimo_cierre== 1, Visa_fultimo_cierre :=  4 ]
dataset_fe[foto_mes==202105 & Visa_fultimo_cierre== 7, Visa_fultimo_cierre := 11 ]
dataset_fe[foto_mes==202105 & Visa_fultimo_cierre==21, Visa_fultimo_cierre := 25 ]
dataset_fe[foto_mes==202105 & Visa_fultimo_cierre==14, Visa_fultimo_cierre := 18 ]
dataset_fe[foto_mes==202105 & Visa_fultimo_cierre==28, Visa_fultimo_cierre := 32 ]
dataset_fe[foto_mes==202105 & Visa_fultimo_cierre==35, Visa_fultimo_cierre := 39 ]
dataset_fe[foto_mes==202105 & Visa_fultimo_cierre> 39, Visa_fultimo_cierre := Visa_fultimo_cierre + 4 ]

# corrijo manualmente el drifting de  Visa_fultimo_cierre
dataset_fe[foto_mes==202105 & Master_fultimo_cierre== 1, Master_fultimo_cierre :=  4 ]
dataset_fe[foto_mes==202105 & Master_fultimo_cierre== 7, Master_fultimo_cierre := 11 ]
dataset_fe[foto_mes==202105 & Master_fultimo_cierre==21, Master_fultimo_cierre := 25 ]
dataset_fe[foto_mes==202105 & Master_fultimo_cierre==14, Master_fultimo_cierre := 18 ]
dataset_fe[foto_mes==202105 & Master_fultimo_cierre==28, Master_fultimo_cierre := 32 ]
dataset_fe[foto_mes==202105 & Master_fultimo_cierre==35, Master_fultimo_cierre := 39 ]
dataset_fe[foto_mes==202105 & Master_fultimo_cierre> 39, Master_fultimo_cierre := Master_fultimo_cierre + 4 ]

#Corregir saldos por inflación
dataset_fe[foto_mes==202105, mcuentas_saldo := mcuentas_saldo / 1.033 / 1.047 ]
dataset_fe[foto_mes==202105, mcuenta_corriente := mcuenta_corriente / 1.033 / 1.047 ]
dataset_fe[foto_mes==202105, mprestamos_personales := mprestamos_personales / 1.033 / 1.047 ]
dataset_fe[foto_mes==202105, mcaja_ahorro := mcaja_ahorro / 1.033 / 1.047 ]
dataset_fe[foto_mes==202105, mactivos_margen := mactivos_margen / 1.033 / 1.047 ]
dataset_fe[foto_mes==202105, mpasivos_margen := mpasivos_margen / 1.033 / 1.047 ]
dataset_fe[foto_mes==202105, mtarjeta_visa_consumo := mtarjeta_visa_consumo / 1.033 / 1.047 ]
dataset_fe[foto_mes==202105, mcomisiones := mcomisiones / 1.033 / 1.047 ]
dataset_fe[foto_mes==202105, mcomisiones_otras := mcomisiones_otras / 1.033 / 1.047 ]
dataset_fe[foto_mes==202105, Visa_msaldototal := Visa_msaldototal / 1.033 / 1.047 ]
dataset_fe[foto_mes==202105, Visa_msaldopesos := Visa_msaldopesos / 1.033 / 1.047 ]
dataset_fe[foto_mes==202105, mrentabilidad_annual := mrentabilidad_annual / 1.033 / 1.047 ]
dataset_fe[foto_mes==202105, Visa_mpagominimo := Visa_mpagominimo / 1.033 / 1.047 ]
dataset_fe[foto_mes==202105, mrentabilidad := mrentabilidad / 1.033 / 1.047 ]
dataset_fe[foto_mes==202105, mpayroll := mpayroll / 1.033 / 1.047 ]
dataset_fe[foto_mes==202105, mtransferencias_recibidas := mtransferencias_recibidas / 1.033 / 1.047 ]
dataset_fe[foto_mes==202105, Visa_mpagospesos := Visa_mpagospesos / 1.033 / 1.047 ]
dataset_fe[foto_mes==202105, Visa_mconsumospesos := Visa_mconsumospesos / 1.033 / 1.047 ]
dataset_fe[foto_mes==202105, Visa_mconsumototal := Visa_mconsumototal / 1.033 / 1.047 ]
dataset_fe[foto_mes==202105, mextraccion_autoservicio := mextraccion_autoservicio / 1.033 / 1.047 ]
dataset_fe[foto_mes==202105, mautoservicio := mautoservicio / 1.033 / 1.047 ]
dataset_fe[foto_mes==202105, mcaja_ahorro_dolares := mcaja_ahorro_dolares / 1.033 / 1.047 ]
dataset_fe[foto_mes==202105, mplazo_fijo_dolares := mplazo_fijo_dolares / 1.033 / 1.047 ]
dataset_fe[foto_mes==202105, mtransferencias_emitidas := mtransferencias_emitidas / 1.033 / 1.047 ]

dataset_fe[foto_mes==202105, matm := matm / 1.033 / 1.047 ]
dataset_fe[foto_mes==202105, mcheques_emitidos := mcheques_emitidos / 1.033 / 1.047 ]

dataset_fe[foto_mes==202105, mcheques_depositados := mcheques_depositados / 1.033 / 1.047 ]
dataset_fe[foto_mes==202105, matm_other := matm_other / 1.033 / 1.047 ]
dataset_fe[foto_mes==202105, mtarjeta_master_consumo := mtarjeta_master_consumo / 1.033 / 1.047 ]

dataset_fe[foto_mes==202105, mpagomiscuentas := mpagomiscuentas / 1.033 / 1.047 ]
dataset_fe[foto_mes==202105, Visa_mlimitecompra := Visa_mlimitecompra / 1.033 / 1.047 ]
dataset_fe[foto_mes==202105, minversion1_dolares := minversion1_dolares / 1.033 / 1.047 ]
dataset_fe[foto_mes==202105, mcheques_depositados_rechazados := mcheques_depositados_rechazados / 1.033 / 1.047 ]
dataset_fe[foto_mes==202105, Visa_mfinanciacion_limite := Visa_mfinanciacion_limite / 1.033 / 1.047 ]
dataset_fe[foto_mes==202105, mttarjeta_master_debitos_automaticos := mttarjeta_master_debitos_automaticos / 1.033 / 1.047 ]
dataset_fe[foto_mes==202105, minversion2 := minversion2 / 1.033 / 1.047 ]
dataset_fe[foto_mes==202105, Visa_mpagominimo := Visa_mpagominimo / 1.033 / 1.047 ]
dataset_fe[foto_mes==202105, Master_mpagominimo := Master_mpagominimo / 1.033 / 1.047 ]



## n_Total_Pagos: deuda total por cliente (todas las tarjetas)----
dataset_fe[,n_Total_Pagos := Visa_mpagado + Master_mpagado]
dataset_fe[,n_Saldo_antes_pagos := mcuentas_saldo + n_Total_Pagos]



## n_visa_minimo_pagado: pagó el minimo? ----
dataset_fe[, n_visa_minimo_pagado := ifelse(-Visa_mpagospesos >= Visa_mpagominimo,2,1)]
dataset_fe[, n_master_minimo_pagado := ifelse(-Master_mpagospesos >= Master_mpagominimo,2,1)]
dataset_fe[, n_tarjetas_minimos_pagado := ifelse(is.na(n_visa_minimo_pagado),1,n_visa_minimo_pagado) + ifelse(is.na(n_master_minimo_pagado),1,n_master_minimo_pagado)]

## Saldos
dataset_fe[,n_monedas_saldo := mforex_buy - mforex_sell ]

dataset_fe[,n_transferencias_saldo := mtransferencias_recibidas - mtransferencias_emitidas ]
## Transferencias emitidas de todo el saldo?
dataset_fe[,n_dif_saldo_y_tranf_emitidas := mcuentas_saldo + mtransferencias_recibidas - mtransferencias_emitidas ]
## concentrar varios montos, misma naturaleza
dataset_fe[,n_mpayroll := mpayroll + mpayroll2]

dataset_fe[,n_minversion := minversion1_pesos + minversion1_dolares + minversion2]

dataset_fe[,n_mprestamos := mprestamos_personales + mprestamos_prendarios + mprestamos_hipotecarios]
dataset_fe[,n_mcomisiones := mcomisiones + mcomisiones_otras]
dataset_fe[,n_mtotal_ganacia_banco := mrentabilidad_annual / 12 -  mrentabilidad  ]


dataset_fe[,n_Total_Ingresos := n_mpayroll + mtransferencias_recibidas + mforex_sell ]
dataset_fe[,n_Total_Egresos := mtransferencias_emitidas + mforex_buy - Visa_mpagospesos - Master_mpagospesos + mpagomiscuentas]


dataset_fe[, n_tarjetas_saldo_total := ifelse(is.na(Visa_msaldototal),0,Visa_msaldototal) + ifelse(is.na(Master_msaldototal),0,Master_msaldototal)]


##monto unitarios de prestamos
dataset_fe[,n_munitario_prestamos_personales := mprestamos_personales / cprestamos_personales]
dataset_fe[, n_munitario_prestamos_personales_nonull := ifelse(is.na(n_munitario_prestamos_personales),0,n_munitario_prestamos_personales) ]

dataset_fe[,n_munitario_prestamos_prendarios := mprestamos_prendarios / cprestamos_prendarios]
dataset_fe[, n_munitario_prestamos_prendarios_nonull := ifelse(is.na(n_munitario_prestamos_prendarios),0,n_munitario_prestamos_prendarios) ]

dataset_fe[,n_munitario_prestamos_hipotecarios := mprestamos_hipotecarios / cprestamos_hipotecarios]
dataset_fe[, n_munitario_prestamos_hipotecarios_nonull := ifelse(is.na(n_munitario_prestamos_hipotecarios),0,n_munitario_prestamos_hipotecarios) ]

dataset_fe[,n_munitario_master_consumos := Master_mconsumototal / Master_cconsumos]
dataset_fe[, n_munitario_master_consumos_nonull := ifelse(is.na(n_munitario_master_consumos),0,n_munitario_master_consumos) ]

dataset_fe[,n_munitario_visa_consumos := Visa_mconsumototal / Visa_cconsumos]
dataset_fe[, n_munitario_visa_consumos_nonull := ifelse(is.na(n_munitario_visa_consumos),0,n_munitario_visa_consumos) ]

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
## * cierre de tarjeta, la persona saldo deudas de un mes a otro.
dataset_fe[, n_visa_deuda_saldopendiente :=  Visa_msaldopesos + Visa_msaldodolares - Visa_mpagado]
dataset_fe[, n_master_deuda_saldopendiente :=  Master_msaldopesos + Master_msaldodolares - Master_mpagado]
dataset_fe[, n_tarjetas_deuda_saldopendiente :=  n_visa_deuda_saldopendiente + n_master_deuda_saldopendiente]


## * binarizar ---- 
# #Hist
ggplot( dataset_fe[foto_mes==202103 & Visa_status>=6], aes(x=clase_ternaria)) + 
  geom_histogram(stat="count")

dataset_fe[, n_visa_status_bin := ifelse(Visa_status>=6,2,1)]
dataset_fe[, n_master_status_bin := ifelse(Master_status>=6,2,1)]
dataset_fe[, n_tarjetas_cierre_tot := ifelse((n_visa_status_bin==1 & n_master_status_bin==1) |
                                               (is.na(Visa_status) & n_master_status_bin==1) |
                                               (is.na(Master_status) & n_visa_status_bin==1) ,2,1)]
dataset_fe[, n_tarjetas_cierre_tot := ifelse(is.na(n_tarjetas_cierre_tot),0,n_tarjetas_cierre_tot)]

## Clase Binaria  ----
#creo la clase_binaria SI={ BAJA+1, BAJA+2 }    NO={ CONTINUA }
dataset_fe[ foto_mes==202103, 
            clase_binaria :=  ifelse( clase_ternaria=="CONTINUA", "NO", "SI" ) ]




dataset_fe$n_tarjetas_minimos_pagado


#'------------------------------------------------------------------------------
# 5. Grabar dataset final en csv -----
#'-----------------------------------------------------------------------------
#archivo_featureEngineer=format(Sys.time(), "%Y%m%d_%H%M%S_")
archivo_featureEngineer = "20220928_152726_part1_FeatEng.csv"

fwrite( dataset_fe, #solo los campos para Kaggle
        file= paste0( dir_salidas,"/",archivo_featureEngineer),
        sep=  "," )


#' #'------------------------------------------------------------------------------
#' # 5. OB -----
#' #'-----------------------------------------------------------------------------
#' # Creo dataset y columnas para la funcion
#' dataset  <- copy(dataset_fe)
#' dataset  <- dataset[ foto_mes==202103 ]
#' columnas <- "clase_binaria ~ . "
#' 
#' 
#' 
#' # (opcional) Defino la  Optimizacion Bayesiana -----
#' 
#' kBO_iter  <- 100   #cantidad de iteraciones de la Optimizacion Bayesiana
#' 
#' 
#' #Estructura que define los hiperparámetros y sus rangos
#' hs  <- makeParamSet(
#'           makeIntegerParam("num.trees" ,        lower=  100L, upper= 2500L),  #la letra L al final significa ENTERO
#'           makeIntegerParam("max.depth",         lower=    1L, upper=   30L),  # 0 significa profundidad infinita
#'           makeIntegerParam("min.node.size" ,    lower=    1L, upper=  500L),
#'           makeIntegerParam("mtry" ,             lower=    2L, upper=   50L))
#' 
#' ksemilla_azar  <- seeds[1]  #Aqui poner la propia semilla seeds[0]   #cambiar por la primer semilla
#' 
#' archivo_log  <- paste0( dir_salidas,"/", "20220911_034709", "OB.txt")
#' archivo_BO   <- paste0( dir_salidas,"/", "20220911_034709", "OB.RDATA") 
#' 
#' #leo si ya existe el log, para retomar en caso que se se corte el programa
#' GLOBAL_iteracion  <- 0
#' 
#' 
#' 
#' #OptBayesiana(dataset,columnas,GLOBAL_iteracion,archivo_log,archivo_BO)
#' #en estos archivos quedan los resultados
#' kbayesiana  <- archivo_log
#' klog        <- archivo_BO
#' 
#' #leo si ya existe el log, para retomar en caso que se se corte el programa
#' GLOBAL_iteracion  <- 0   #inicializo la variable global
#' 
#' #si ya existe el archivo log, traigo hasta donde llegue
#' if( file.exists(klog) )
#' {
#'   tabla_log  <- fread( klog )
#'   GLOBAL_iteracion  <- nrow( tabla_log )
#' }
#' 
#' 
#' 
#' #paso a trabajar con clase binaria POS={BAJA+2}   NEG={BAJA+1, CONTINUA}
#' dataset[ , clase_binaria := as.factor(ifelse( clase_ternaria=="BAJA+2", "POS", "NEG" )) ]
#' dataset[ , clase_ternaria := NULL ]  #elimino la clase_ternaria, ya no la necesito
#' 
#' 
#' #imputo los nulos, ya que ranger no acepta nulos
#' #Leo Breiman, ¿por que le temias a los nulos?
#' dataset  <- na.roughfix( dataset )
#' 
#' 
#' 
#' #Aqui comienza la configuracion de la Bayesian Optimization
#' 
#' configureMlr( show.learner.output = FALSE)
#' 
#' funcion_optimizar  <- EstimarGanancia_ranger
#' 
#' #configuro la busqueda bayesiana,  los hiperparametros que se van a optimizar
#' #por favor, no desesperarse por lo complejo
#' obj.fun  <- makeSingleObjectiveFunction(
#'   fn=       funcion_optimizar,
#'   minimize= FALSE,   #estoy Maximizando la ganancia
#'   noisy=    TRUE,
#'   par.set=  hs,
#'   has.simple.signature = FALSE
#' )
#' 
#' ctrl  <- makeMBOControl( save.on.disk.at.time= 120,  save.file.path= kbayesiana)
#' ctrl  <- setMBOControlTermination(ctrl, iters= kBO_iter )
#' ctrl  <- setMBOControlInfill(ctrl, crit= makeMBOInfillCritEI())
#' 
#' surr.km  <-  makeLearner("regr.km", predict.type= "se", covtype= "matern3_2", control= list(trace= TRUE))
#' 
#' #inicio la optimizacion bayesiana
#' if(!file.exists(kbayesiana)) {
#'   run  <- mbo(obj.fun, learner = surr.km, control = ctrl)
#' } else  run  <- mboContinue( kbayesiana )   #retomo en caso que ya exista
#' 
