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

#Parametros del script
PARAM  <- list()
PARAM$experimento  <- "DR9141_2"

PARAM$exp_input  <- "DR9141"

seeds <- c( 807299, 962041, 705689, 909463, 637597 )
# Meses de train y de predicción <---- -----
meses_train <- c(201901, 201902, 201903, 201904, 201905, 201906,
                 201907, 201908, 201909, 201910, 201911, 201912,
                 202001, 202002, 202003, 202004, 202005, 202006,
                 202007, 202008, 202009, 202010, 202011, 202012,
                 202101, 202102, 202103, 202104, 202105, 202106,
                 202107,
                 202108, 202109)
mes_predecir <- c(202109)

meses_train_test <- c(meses_train,mes_predecir)

#'------------------------------------------------------------------------------

#Aqui se debe poner la carpeta de la materia de SU computadora local
#setwd("G:\\My Drive\\Facultad\\Maestria DM\\Expecializacion\\DMEyF\\Carpetas")  #Establezco el Working Directory
setwd("~/buckets/b1/")   #Establezco el Working Directory

dir_salidas="./exp/TP_final/"
dir.create( dir_salidas )

#'------------------------------------------------------------------------------
# 1. Lectura de datos ----
#'------------------------------------------------------------------------------


# cargo el dataset
#dataset_fe <- fread("./datasets/competencia1_2022.csv")

#dataset_fe  <- fread("./datasets/competencia2_2022.csv.gz", stringsAsFactors= TRUE)

#cargo el dataset donde voy a entrenar
#esta en la carpeta del exp_input y siempre se llama  dataset.csv.gz
dataset_input  <- paste0( "./exp/", PARAM$exp_input, "/dataset.csv.gz" )
dataset_fe  <- fread( dataset_input )


#'------------------------------------------------------------------------------
# 2. Funciones Auxiliares ----
#'------------------------------------------------------------------------------
scriptName <- rstudioapi::getSourceEditorContext()$path
crearCheckpoint <- function(path,filename) 
{
  require(rstudioapi)
  # file.copy(rstudioapi::getSourceEditorContext()$path,
  #           to = file.path(path,
  #                          paste0(filename, "_antes.R")))
  documentSave()
  file.copy(scriptName,
            to = file.path(path,
                           paste0(filename, ".R")))
}

#------------------------------------------------------------------------------
#El cero se transforma en cero
#los positivos se rankean por su lado
#los negativos se rankean por su lado

drift_rank_cero_fijo  <- function( campos_drift )
{
  for( campo in campos_drift )
  {
    cat( campo, " " )
    dataset_fe[ get(campo) ==0, paste0(campo,"_rank") := 0 ]
    dataset_fe[ get(campo) > 0, paste0(campo,"_rank") :=   frank(  get(campo), ties.method="random")  / .N, by= foto_mes ]
    dataset_fe[ get(campo) < 0, paste0(campo,"_rank") :=  -frank( -get(campo), ties.method="random")  / .N, by= foto_mes ]
    dataset_fe[ , (campo) := NULL ]
  }
}

drift_rank_simple  <- function( campos_drift )
{
  for( campo in campos_drift )
  {
    cat( campo, " " )
    dataset_fe[ , paste0(campo,"_rank") :=  (frank( get(campo), ties.method="random") - 1) / ( .N -1 ), by= foto_mes]
    dataset_fe[ , (campo) := NULL ]
  }
}

#'------------------------------------------------------------------------------
# 3. FEATURE ENGINEERING ----
#'-----------------------------------------------------------------------------
columnas_a_quitar <- c()
col_aplicar_DF <- c()



## imputar nulos? -----  
#dataset_fe <- dataset_fe[,lapply(.SD,function(x){ifelse(is.na(x),-1,x)})]

## Crea nuevos features part1  ----
### n_antig_segun_edad ----
dataset_fe[, n_antig_segun_edad := (cliente_antiguedad / 12) / cliente_edad]
### n_edad_de_alta ----
dataset_fe[, n_edad_de_alta := cliente_edad - (cliente_antiguedad / 12)]


### n_tarjetas_deuda_saldopendiente ---- 
#* cantidad de productos. menos es mas probable que te vayas 
dataset_fe[, n_visa_deuda_saldopendiente :=  Visa_msaldopesos + Visa_msaldodolares - Visa_mpagado]
dataset_fe[, n_master_deuda_saldopendiente :=  Master_msaldopesos + Master_msaldodolares - Master_mpagado]
dataset_fe[, n_tarjetas_deuda_saldopendiente :=  n_visa_deuda_saldopendiente + n_master_deuda_saldopendiente]

## * cierre de tarjeta, la persona saldo deudas de un mes a otro.






### n_mpayroll_sobre_edad -----
dataset_fe[  , n_mpayroll_sobre_edad  := mpayroll / cliente_edad ]

col_aplicar_DF_antes <- c(colnames(dataset_fe))


### combino MasterCard y Visa -----
dataset_fe[ , mv_mfinanciacion_limite := rowSums( cbind( Master_mfinanciacion_limite,  Visa_mfinanciacion_limite) , na.rm=TRUE ) ]

dataset_fe[ , mv_Fvencimiento         := pmin( Master_Fvencimiento, Visa_Fvencimiento, na.rm = TRUE) ]
dataset_fe[ , mv_Finiciomora          := pmin( Master_Finiciomora, Visa_Finiciomora, na.rm = TRUE) ]
dataset_fe[ , mv_msaldototal          := rowSums( cbind( Master_msaldototal,  Visa_msaldototal) , na.rm=TRUE ) ]
dataset_fe[ , mv_msaldopesos          := rowSums( cbind( Master_msaldopesos,  Visa_msaldopesos) , na.rm=TRUE ) ]
dataset_fe[ , mv_msaldodolares        := rowSums( cbind( Master_msaldodolares,  Visa_msaldodolares) , na.rm=TRUE ) ]
dataset_fe[ , mv_mconsumospesos       := rowSums( cbind( Master_mconsumospesos,  Visa_mconsumospesos) , na.rm=TRUE ) ]
dataset_fe[ , mv_mconsumosdolares     := rowSums( cbind( Master_mconsumosdolares,  Visa_mconsumosdolares) , na.rm=TRUE ) ]
dataset_fe[ , mv_mlimitecompra        := rowSums( cbind( Master_mlimitecompra,  Visa_mlimitecompra) , na.rm=TRUE ) ]
dataset_fe[ , mv_madelantopesos       := rowSums( cbind( Master_madelantopesos,  Visa_madelantopesos) , na.rm=TRUE ) ]
dataset_fe[ , mv_madelantodolares     := rowSums( cbind( Master_madelantodolares,  Visa_madelantodolares) , na.rm=TRUE ) ]
dataset_fe[ , mv_fultimo_cierre       := pmax( Master_fultimo_cierre, Visa_fultimo_cierre, na.rm = TRUE) ]
dataset_fe[ , mv_mpagado              := rowSums( cbind( Master_mpagado,  Visa_mpagado) , na.rm=TRUE ) ]
dataset_fe[ , mv_mpagospesos          := rowSums( cbind( Master_mpagospesos,  Visa_mpagospesos) , na.rm=TRUE ) ]
dataset_fe[ , mv_mpagosdolares        := rowSums( cbind( Master_mpagosdolares,  Visa_mpagosdolares) , na.rm=TRUE ) ]
dataset_fe[ , mv_fechaalta            := pmax( Master_fechaalta, Visa_fechaalta, na.rm = TRUE) ]
dataset_fe[ , mv_mconsumototal        := rowSums( cbind( Master_mconsumototal,  Visa_mconsumototal) , na.rm=TRUE ) ]
dataset_fe[ , mv_cconsumos            := rowSums( cbind( Master_cconsumos,  Visa_cconsumos) , na.rm=TRUE ) ]
dataset_fe[ , mv_cadelantosefectivo   := rowSums( cbind( Master_cadelantosefectivo,  Visa_cadelantosefectivo) , na.rm=TRUE ) ]
dataset_fe[ , mv_mpagominimo          := rowSums( cbind( Master_mpagominimo,  Visa_mpagominimo) , na.rm=TRUE ) ]

  #columnas a aplicar data drifting
  col_aplicar_DF <- c(col_aplicar_DF,setdiff(colnames(dataset_fe),col_aplicar_DF_antes))



# ## Data Drifting  ----
# ### tarjeta_fultimo_cierre   ----
# dataset_fe[foto_mes==202102 & Visa_fultimo_cierre== -3, Visa_fultimo_cierre :=  4 ]
# dataset_fe[foto_mes==202102 & Visa_fultimo_cierre== 4, Visa_fultimo_cierre := 11 ]
# dataset_fe[foto_mes==202102 & Visa_fultimo_cierre==11, Visa_fultimo_cierre := 18 ]
# dataset_fe[foto_mes==202102 & Visa_fultimo_cierre==18, Visa_fultimo_cierre := 25 ]
# dataset_fe[foto_mes==202102 & Visa_fultimo_cierre==32, Visa_fultimo_cierre := 32 ]
# dataset_fe[foto_mes==202102 & Visa_fultimo_cierre==39, Visa_fultimo_cierre := 39 ]
# dataset_fe[foto_mes==202102 & Visa_fultimo_cierre> 39, Visa_fultimo_cierre := Visa_fultimo_cierre + 7 ]
# 
# dataset_fe[foto_mes==202102 & Master_fultimo_cierre==-3, Master_fultimo_cierre :=  4 ]
# dataset_fe[foto_mes==202102 & Master_fultimo_cierre== 4, Master_fultimo_cierre := 11 ]
# dataset_fe[foto_mes==202102 & Master_fultimo_cierre==11, Master_fultimo_cierre := 18 ]
# dataset_fe[foto_mes==202102 & Master_fultimo_cierre==18, Master_fultimo_cierre := 25 ]
# dataset_fe[foto_mes==202102 & Master_fultimo_cierre==32, Master_fultimo_cierre := 32 ]
# dataset_fe[foto_mes==202102 & Master_fultimo_cierre==39, Master_fultimo_cierre := 39 ]
# dataset_fe[foto_mes==202102 & Master_fultimo_cierre> 39, Master_fultimo_cierre := Master_fultimo_cierre + 7 ]
# ### tarjeta_fultimo_cierre   ----
# dataset_fe[foto_mes==202103 & Visa_fultimo_cierre== 1, Visa_fultimo_cierre :=  4 ]
# dataset_fe[foto_mes==202103 & Visa_fultimo_cierre== 7, Visa_fultimo_cierre := 11 ]
# dataset_fe[foto_mes==202103 & Visa_fultimo_cierre==14, Visa_fultimo_cierre := 18 ]
# dataset_fe[foto_mes==202103 & Visa_fultimo_cierre==21, Visa_fultimo_cierre := 25 ]
# dataset_fe[foto_mes==202103 & Visa_fultimo_cierre==28, Visa_fultimo_cierre := 32 ]
# dataset_fe[foto_mes==202103 & Visa_fultimo_cierre==35, Visa_fultimo_cierre := 39 ]
# dataset_fe[foto_mes==202103 & Visa_fultimo_cierre> 35, Visa_fultimo_cierre := Visa_fultimo_cierre + 4 ]
# 
# dataset_fe[foto_mes==202103 & Master_fultimo_cierre== 1, Master_fultimo_cierre :=  4 ]
# dataset_fe[foto_mes==202103 & Master_fultimo_cierre== 7, Master_fultimo_cierre := 11 ]
# dataset_fe[foto_mes==202103 & Master_fultimo_cierre==21, Master_fultimo_cierre := 25 ]
# dataset_fe[foto_mes==202103 & Master_fultimo_cierre==14, Master_fultimo_cierre := 18 ]
# dataset_fe[foto_mes==202103 & Master_fultimo_cierre==28, Master_fultimo_cierre := 32 ]
# dataset_fe[foto_mes==202103 & Master_fultimo_cierre==35, Master_fultimo_cierre := 39 ]
# dataset_fe[foto_mes==202103 & Master_fultimo_cierre> 35, Master_fultimo_cierre := Master_fultimo_cierre + 4 ]
# 
# 
# 
# 
# 
# ### tarjeta_fultimo_cierre   ----
# dataset_fe[foto_mes==202105 & Visa_fultimo_cierre== 5, Visa_fultimo_cierre :=  4 ]
# dataset_fe[foto_mes==202105 & Visa_fultimo_cierre== 12, Visa_fultimo_cierre := 11 ]
# dataset_fe[foto_mes==202105 & Visa_fultimo_cierre==19, Visa_fultimo_cierre := 18 ]
# dataset_fe[foto_mes==202105 & Visa_fultimo_cierre==26, Visa_fultimo_cierre := 25 ]
# dataset_fe[foto_mes==202105 & Visa_fultimo_cierre==33, Visa_fultimo_cierre := 32 ]
# dataset_fe[foto_mes==202105 & Visa_fultimo_cierre==44, Visa_fultimo_cierre := 39 ]
# dataset_fe[foto_mes==202105 & Visa_fultimo_cierre> 44, Visa_fultimo_cierre := Visa_fultimo_cierre - 1 ]
# 
# dataset_fe[foto_mes==202105 & Master_fultimo_cierre== 5, Master_fultimo_cierre :=  4 ]
# dataset_fe[foto_mes==202105 & Master_fultimo_cierre== 12, Master_fultimo_cierre := 11 ]
# dataset_fe[foto_mes==202105 & Master_fultimo_cierre==19, Master_fultimo_cierre := 18 ]
# dataset_fe[foto_mes==202105 & Master_fultimo_cierre==26, Master_fultimo_cierre := 25 ]
# dataset_fe[foto_mes==202105 & Master_fultimo_cierre==33, Master_fultimo_cierre := 32 ]
# dataset_fe[foto_mes==202105 & Master_fultimo_cierre==44, Master_fultimo_cierre := 39 ]
# dataset_fe[foto_mes==202105 & Master_fultimo_cierre> 44, Master_fultimo_cierre := Master_fultimo_cierre - 1 ]
# 
# 
# ### cliente_antiguedad-----
# dataset_fe[foto_mes==202102, cliente_antiguedad := cliente_antiguedad - 1 ]
# dataset_fe[foto_mes==202103, cliente_antiguedad := cliente_antiguedad - 2 ]
# dataset_fe[foto_mes==202105, cliente_antiguedad := cliente_antiguedad - 4 ]
### Inflación
# dataset_fe[foto_mes==202105, mcuentas_saldo := mcuentas_saldo / 1.033 / 1.047 ]
# dataset_fe[foto_mes==202105, mcuenta_corriente := mcuenta_corriente / 1.033 / 1.047 ]
# dataset_fe[foto_mes==202105, mprestamos_personales := mprestamos_personales / 1.033 / 1.047 ]
# dataset_fe[foto_mes==202105, mcaja_ahorro := mcaja_ahorro / 1.033 / 1.047 ]
# dataset_fe[foto_mes==202105, mactivos_margen := mactivos_margen / 1.033 / 1.047 ]
# dataset_fe[foto_mes==202105, mpasivos_margen := mpasivos_margen / 1.033 / 1.047 ]
# dataset_fe[foto_mes==202105, mtarjeta_visa_consumo := mtarjeta_visa_consumo / 1.033 / 1.047 ]
# dataset_fe[foto_mes==202105, mcomisiones := mcomisiones / 1.033 / 1.047 ]
# dataset_fe[foto_mes==202105, mcomisiones_otras := mcomisiones_otras / 1.033 / 1.047 ]
# dataset_fe[foto_mes==202105, Visa_msaldototal := Visa_msaldototal / 1.033 / 1.047 ]
# dataset_fe[foto_mes==202105, Visa_msaldopesos := Visa_msaldopesos / 1.033 / 1.047 ]
# dataset_fe[foto_mes==202105, mrentabilidad_annual := mrentabilidad_annual / 1.033 / 1.047 ]
# dataset_fe[foto_mes==202105, Visa_mpagominimo := Visa_mpagominimo / 1.033 / 1.047 ]
# dataset_fe[foto_mes==202105, mrentabilidad := mrentabilidad / 1.033 / 1.047 ]
# dataset_fe[foto_mes==202105, mpayroll := mpayroll / 1.033 / 1.047 ]
# dataset_fe[foto_mes==202105, mtransferencias_recibidas := mtransferencias_recibidas / 1.033 / 1.047 ]
# dataset_fe[foto_mes==202105, Visa_mpagospesos := Visa_mpagospesos / 1.033 / 1.047 ]
# dataset_fe[foto_mes==202105, Visa_mconsumospesos := Visa_mconsumospesos / 1.033 / 1.047 ]
# dataset_fe[foto_mes==202105, Visa_mconsumototal := Visa_mconsumototal / 1.033 / 1.047 ]
# dataset_fe[foto_mes==202105, mextraccion_autoservicio := mextraccion_autoservicio / 1.033 / 1.047 ]
# dataset_fe[foto_mes==202105, mautoservicio := mautoservicio / 1.033 / 1.047 ]
# dataset_fe[foto_mes==202105, mcaja_ahorro_dolares := mcaja_ahorro_dolares / 1.033 / 1.047 ]
# dataset_fe[foto_mes==202105, mplazo_fijo_dolares := mplazo_fijo_dolares / 1.033 / 1.047 ]
# dataset_fe[foto_mes==202105, mtransferencias_emitidas := mtransferencias_emitidas / 1.033 / 1.047 ]
# 
# dataset_fe[foto_mes==202105, matm := matm / 1.033 / 1.047 ]
# dataset_fe[foto_mes==202105, mcheques_emitidos := mcheques_emitidos / 1.033 / 1.047 ]
# 
# dataset_fe[foto_mes==202105, mcheques_depositados := mcheques_depositados / 1.033 / 1.047 ]
# dataset_fe[foto_mes==202105, matm_other := matm_other / 1.033 / 1.047 ]
# dataset_fe[foto_mes==202105, mtarjeta_master_consumo := mtarjeta_master_consumo / 1.033 / 1.047 ]
# 
# dataset_fe[foto_mes==202105, mpagomiscuentas := mpagomiscuentas / 1.033 / 1.047 ]
# dataset_fe[foto_mes==202105, Visa_mlimitecompra := Visa_mlimitecompra / 1.033 / 1.047 ]
# dataset_fe[foto_mes==202105, minversion1_dolares := minversion1_dolares / 1.033 / 1.047 ]
# dataset_fe[foto_mes==202105, mcheques_depositados_rechazados := mcheques_depositados_rechazados / 1.033 / 1.047 ]
# dataset_fe[foto_mes==202105, Visa_mfinanciacion_limite := Visa_mfinanciacion_limite / 1.033 / 1.047 ]
# dataset_fe[foto_mes==202105, mttarjeta_master_debitos_automaticos := mttarjeta_master_debitos_automaticos / 1.033 / 1.047 ]
# dataset_fe[foto_mes==202105, minversion2 := minversion2 / 1.033 / 1.047 ]
# dataset_fe[foto_mes==202105, Visa_mpagominimo := Visa_mpagominimo / 1.033 / 1.047 ]
# dataset_fe[foto_mes==202105, Master_mpagominimo := Master_mpagominimo / 1.033 / 1.047 ]




#'##' Rankeo + escalado + relativizar montos -----
library(tidyverse)
# descr <- dataset_fe[ foto_mes == 202103] %>%   summarise(Minimo = min(mcuentas_saldo), Q1 = quantile(mcuentas_saldo, probs = 0.25), Mediana = quantile(mcuentas_saldo, probs = 0.5), 
#                                                          Promedio = round(mean(mcuentas_saldo),0), Q3 = quantile(mcuentas_saldo, probs = 0.75),  Maximo = max(mcuentas_saldo), Desvio = round(sd(mcuentas_saldo),0))
# 
# dataset_fe <- dataset_fe[ foto_mes == 202103 & mcuentas_saldo < descr$Q1 , n_mcuentas_saldo_qntl := 1]
# dataset_fe <- dataset_fe[ foto_mes == 202103 & mcuentas_saldo >= descr$Q1 & mcuentas_saldo < descr$Mediana  , n_mcuentas_saldo_qntl := 2]
# dataset_fe <- dataset_fe[ foto_mes == 202103 & mcuentas_saldo >= descr$Mediana & mcuentas_saldo < descr$Q3  , n_mcuentas_saldo_qntl := 3]
# dataset_fe <- dataset_fe[ foto_mes == 202103 & mcuentas_saldo >= descr$Q3 , n_mcuentas_saldo_qntl := 4]
# dataset_fe <- dataset_fe[ foto_mes == 202103, n_mcuentas_saldo_scl := mcuentas_saldo - mean(mcuentas_saldo)]
# 
# descr <- dataset_fe[ foto_mes == 202105] %>%   summarise(Minimo = min(mcuentas_saldo), Q1 = quantile(mcuentas_saldo, probs = 0.25), Mediana = quantile(mcuentas_saldo, probs = 0.5), 
#                                                          Promedio = round(mean(mcuentas_saldo),0), Q3 = quantile(mcuentas_saldo, probs = 0.75),  Maximo = max(mcuentas_saldo), Desvio = round(sd(mcuentas_saldo),0))
# descr
# dataset_fe <- dataset_fe[ foto_mes == 202105 & mcuentas_saldo < descr$Q1 , n_mcuentas_saldo_qntl := 1]
# dataset_fe <- dataset_fe[ foto_mes == 202105 & mcuentas_saldo >= descr$Q1 & mcuentas_saldo < descr$Mediana  , n_mcuentas_saldo_qntl := 2]
# dataset_fe <- dataset_fe[ foto_mes == 202105 & mcuentas_saldo >= descr$Mediana & mcuentas_saldo < descr$Q3  , n_mcuentas_saldo_qntl := 3]
# dataset_fe <- dataset_fe[ foto_mes == 202105 & mcuentas_saldo >= descr$Q3 , n_mcuentas_saldo_qntl := 4]
# dataset_fe <- dataset_fe[ foto_mes == 202105, n_mcuentas_saldo_scl := mcuentas_saldo - mean(mcuentas_saldo)]
# 
# dataset_fe <- dataset_fe[ foto_mes == 202103, n_mcuentas_saldo_qntl_rnk := frank(n_mcuentas_saldo_qntl)]
# dataset_fe <- dataset_fe[ foto_mes == 202103 & mcuentas_saldo >=0 , n_mcuentas_saldo_qntl_rnk_gz := frank(n_mcuentas_saldo_qntl)]
# dataset_fe <- dataset_fe[ foto_mes == 202103 & mcuentas_saldo <0 , n_mcuentas_saldo_qntl_rnk_lz := frank(n_mcuentas_saldo_qntl)]
# 
# dataset_fe <- dataset_fe[ foto_mes == 202105, n_mcuentas_saldo_qntl_rnk := frank(n_mcuentas_saldo_qntl)]
# dataset_fe <- dataset_fe[ foto_mes == 202105 & mcuentas_saldo >=0 , n_mcuentas_saldo_qntl_rnk_gz := frank(n_mcuentas_saldo_qntl)]
# dataset_fe <- dataset_fe[ foto_mes == 202105 & mcuentas_saldo <0 , n_mcuentas_saldo_qntl_rnk_lz := frank(n_mcuentas_saldo_qntl)]





###n_mcuentas_saldo_por_com  ---- 
for (mes in meses_train_test)
{
  dataset_fe <- dataset_fe[ foto_mes == mes, n_mcuentas_saldo_por_com := mcuentas_saldo / mcomisiones]
}



#'## binarizar ---- 
#Hist
#ggplot( dataset_fe[foto_mes==202103 & Visa_status>=6], aes(x=clase_ternaria)) + 
#  geom_histogram(stat="count")




### relativizar a limite compra ----
# dataset_fe[ , mvr_Master_mlimitecompra:= Master_mlimitecompra / mv_mlimitecompra ]
# dataset_fe[ , mvr_Visa_mlimitecompra  := Visa_mlimitecompra / mv_mlimitecompra ]
# dataset_fe[ , mvr_msaldototal         := mv_msaldototal / mv_mlimitecompra ]
# dataset_fe[ , mvr_msaldopesos         := mv_msaldopesos / mv_mlimitecompra ]
# dataset_fe[ , mvr_msaldopesos2        := mv_msaldopesos / mv_msaldototal ]
# dataset_fe[ , mvr_msaldodolares       := mv_msaldodolares / mv_mlimitecompra ]
# dataset_fe[ , mvr_msaldodolares2      := mv_msaldodolares / mv_msaldototal ]
# dataset_fe[ , mvr_mconsumospesos      := mv_mconsumospesos / mv_mlimitecompra ]
# dataset_fe[ , mvr_mconsumosdolares    := mv_mconsumosdolares / mv_mlimitecompra ]
# dataset_fe[ , mvr_madelantopesos      := mv_madelantopesos / mv_mlimitecompra ]
# dataset_fe[ , mvr_madelantodolares    := mv_madelantodolares / mv_mlimitecompra ]
# dataset_fe[ , mvr_mpagado             := mv_mpagado / mv_mlimitecompra ]
# dataset_fe[ , mvr_mpagospesos         := mv_mpagospesos / mv_mlimitecompra ]
# dataset_fe[ , mvr_mpagosdolares       := mv_mpagosdolares / mv_mlimitecompra ]
# dataset_fe[ , mvr_mconsumototal       := mv_mconsumototal  / mv_mlimitecompra ]
# dataset_fe[ , mvr_mpagominimo         := mv_mpagominimo  / mv_mlimitecompra ]






## Normalizar ----
#creo un ctr_quarter que tenga en cuenta cuando los clientes hace 3 menos meses que estan
dataset_fe[  , ctrx_quarter_normalizado := ctrx_quarter ]
dataset_fe[ cliente_antiguedad==1 , n_ctrx_quarter_normalizado := ctrx_quarter * 5 ]
dataset_fe[ cliente_antiguedad==2 , n_ctrx_quarter_normalizado := ctrx_quarter * 2 ]
dataset_fe[ cliente_antiguedad==3 , n_ctrx_quarter_normalizado := ctrx_quarter * 1.2 ]

  #columna a quitar..
  columnas_a_quitar <- c(columnas_a_quitar,"ctrx_quarter")



## Crea Saldos: como ingresos - egresos ----
  col_aplicar_DF_antes <- c(colnames(dataset_fe))
  
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
dataset_fe[, n_tarjetas_saldo_total := ifelse(is.na(Visa_msaldototal),0,Visa_msaldototal) + ifelse(is.na(Master_msaldototal),0,Master_msaldototal)]
dataset_fe[, n_tarjetas_saldo_total4 :=0]



## Crea Totales: como suma de egresos o ingresos----
dataset_fe[,n_Total_Pagos := Visa_mpagado + Master_mpagado]

dataset_fe[,n_Saldo_antes_pagos := mcuentas_saldo + n_Total_Pagos]

dataset_fe[,n_Total_Ingresos := n_mpayroll + mtransferencias_recibidas + mforex_sell ]
dataset_fe[,n_Total_Egresos := mtransferencias_emitidas + mforex_buy - Visa_mpagospesos - Master_mpagospesos + mpagomiscuentas]








##Crea Unitarios -----
#monto unitarios de prestamos
dataset_fe[, n_munitario_prestamos_personales := mprestamos_personales / cprestamos_personales]
dataset_fe[, n_munitario_prestamos_personales_nonull := ifelse(is.na(n_munitario_prestamos_personales),0,n_munitario_prestamos_personales) ]

dataset_fe[, n_munitario_prestamos_prendarios := mprestamos_prendarios / cprestamos_prendarios]
dataset_fe[, n_munitario_prestamos_prendarios_nonull := ifelse(is.na(n_munitario_prestamos_prendarios),0,n_munitario_prestamos_prendarios) ]

dataset_fe[, n_munitario_prestamos_hipotecarios := mprestamos_hipotecarios / cprestamos_hipotecarios]
dataset_fe[, n_munitario_prestamos_hipotecarios_nonull := ifelse(is.na(n_munitario_prestamos_hipotecarios),0,n_munitario_prestamos_hipotecarios) ]

dataset_fe[, n_munitario_master_consumos := Master_mconsumototal / Master_cconsumos]
dataset_fe[, n_munitario_master_consumos_nonull := ifelse(is.na(n_munitario_master_consumos),0,n_munitario_master_consumos) ]

dataset_fe[, n_munitario_visa_consumos := Visa_mconsumototal / Visa_cconsumos]
dataset_fe[, n_munitario_visa_consumos_nonull := ifelse(is.na(n_munitario_visa_consumos),0,n_munitario_visa_consumos) ]


  #columnas a aplicar data drifting
  col_aplicar_DF <- c(col_aplicar_DF,setdiff(colnames(dataset_fe),col_aplicar_DF_antes))



## Crea booleanos ----
#minimos pagados? 
dataset_fe[, n_visa_minimo_pagado := ifelse(-Visa_mpagospesos >= Visa_mpagominimo,2,1)]
dataset_fe[, n_master_minimo_pagado := ifelse(-Master_mpagospesos >= Master_mpagominimo,2,1)]
dataset_fe[, n_tarjetas_minimos_pagado := ifelse(is.na(n_visa_minimo_pagado),1,n_visa_minimo_pagado) + ifelse(is.na(n_master_minimo_pagado),1,n_master_minimo_pagado)]

#tarjeta cerrada?
dataset_fe[, n_visa_status_bin := ifelse(Visa_status>=6,2,1)]
dataset_fe[, n_master_status_bin := ifelse(Master_status>=6,2,1)]
dataset_fe[, n_tarjetas_cierre_tot := ifelse((n_visa_status_bin==1 & n_master_status_bin==1) |
                                               (is.na(Visa_status) & n_master_status_bin==1) |
                                               (is.na(Master_status) & n_visa_status_bin==1) ,2,1)]
dataset_fe[, n_tarjetas_cierre_tot := ifelse(is.na(n_tarjetas_cierre_tot),0,n_tarjetas_cierre_tot)]


#Estacionalidades
for (i in c(1,2,3,4,5,6,7,8,9,10,11,12)) {
  dataset_fe[, paste0("n_es_mes_",i) := 0L ]
  dataset_fe[foto_mes%%100 == i, paste0("n_es_mes_",i) := 1L ]
}

#Pandemia
dataset_fe[, pandemia := 0L ]
dataset_fe[foto_mes >= 202003 & foto_mes<=202012, n_es_pandemia := 1L ]



## Crea nuevos features part2  ----


##Data drifting parte 2----

###frank ----


#columnas_frank <- c("mcuentas_saldo", "mprestamos_personales","mcomisiones","mtarjeta_visa_consumo")
# columnas_frank <- c("mcuentas_saldo", "mprestamos_personales","mcomisiones","mtarjeta_visa_consumo","mrentabilidad","mrentabilidad_annual","mcomisiones","mactivos_margen","mpasivos_margen","mcuenta_corriente_adicional","mcuenta_corriente","mcaja_ahorro","mcaja_ahorro_adicional","mcaja_ahorro_dolares","mautoservicio","mtarjeta_master_consumo","mprestamos_prendarios","mprestamos_hipotecarios","mplazo_fijo_dolares","mplazo_fijo_pesos","minversion1_pesos","minversion1_dolares","minversion2","mpayroll","mpayroll2","mcuenta_debitos_automaticos","mttarjeta_visa_debitos_automaticos","mttarjeta_master_debitos_automaticos","mpagodeservicios","mpagomiscuentas","mcajeros_propios_descuentos","mtarjeta_visa_descuentos","mtarjeta_master_descuentos","mcomisiones_mantenimiento","mcomisiones_otras","mforex_buy","mforex_sell","mtransferencias_recibidas","mtransferencias_emitidas","mextraccion_autoservicio","mcheques_depositados","mcheques_emitidos","mcheques_depositados_rechazados","mcheques_emitidos_rechazados","matm","matm_other",
#                     "Visa_mfinanciacion_limite","Visa_msaldototal","Visa_msaldopesos","Visa_msaldodolares","Visa_mconsumospesos","Visa_mconsumosdolares","Visa_mlimitecompra","Visa_madelantopesos","Visa_madelantodolares","Visa_mpagado","Visa_mpagospesos","Visa_mpagosdolares","Visa_mconsumototal","Visa_mpagominimo","Master_mfinanciacion_limite","Master_msaldototal","Master_msaldopesos","Master_msaldodolares","Master_mconsumospesos","Master_mconsumosdolares","Master_mlimitecompra","Master_madelantopesos","Master_madelantodolares","Master_mpagado","Master_mpagospesos","Master_mpagosdolares","Master_mconsumototal","Master_mpagominimo")
# columnas_frank <- c(columnas_frank, col_aplicar_DF)
# 

columnas_frank <- c("Master_fultimo_cierre","Visa_fultimo_cierre")
drift_rank_simple(columnas_frank)

# 


#combino MasterCard y Visa
dataset_fe[ , n_vm_fultimo_cierre_rnk_gz       := pmax( Master_fultimo_cierre_rank, Visa_fultimo_cierre_rank, na.rm = TRUE) ]


## Clase Binaria  ----
#creo la clase_binaria SI={ BAJA+1, BAJA+2 }    NO={ CONTINUA }
# for (mes in meses_train)
# {
#   dataset_fe[ foto_mes==mes, 
#               clase_binaria :=  ifelse( clase_ternaria=="CONTINUA", "NO", "SI" ) ]
# }
# 

## Rank

##kmeans
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

#valvula de seguridad para evitar valores infinitos
# ## FIX: paso los infinitos a NULOS-----
# infinitos      <- lapply(names(dataset_fe),function(.name) dataset_fe[ , sum(is.infinite(get(.name)))])
# infinitos_qty  <- sum( unlist( infinitos) )
# if( infinitos_qty > 0 )
# {
#   cat( "ATENCION, hay", infinitos_qty, "valores infinitos en tu dataset. Seran pasados a NA\n" )
#   dataset_fe[mapply(is.infinite, dataset_fe)] <- NA
# }


#'-------------------------------

timestamp <- format(Sys.time(), "%Y%m%d_%H%M%S")
archivo_featureEngineer=paste0(timestamp,"_part1_FeatEng")
archivo_columnas_a_quitar=paste0(timestamp,"_part1_columnas_a_quitar")

fwrite( data.table(columnas_a_quitar), #solo los campos para Kaggle
        file= paste0( dir_salidas,"/",archivo_columnas_a_quitar,".csv"),
        sep=  "," )


#checkpoint
crearCheckpoint(dir_salidas,archivo_featureEngineer)

#'------------------------------------------------------------------------------
# 5. Grabar dataset final en csv -----
#'-----------------------------------------------------------------------------

#'#creo la carpeta donde va el experimento
dir.create( paste0( "./exp/", PARAM$experimento, "/"), showWarnings = FALSE )
setwd(paste0( "./exp/", PARAM$experimento, "/"))   #Establezco el Working Directory DEL EXPERIMENTO

#grabo el dataset
fwrite( dataset_fe,
        "dataset.csv.gz",
        logical01= TRUE,
        sep= "," )



