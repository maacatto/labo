#Necesita para correr en Google Cloud
#  64 GB de memoria RAM
# 256 GB de espacio en el disco local
#   8 vCPU

# permite manejar el sampling_total y el undersampling de la clase mayoritaria

#limpio la memoria
rm( list=ls() )  #remove all objects
gc()             #garbage collection

require("data.table")



#Parametros del script
PARAM  <- list()
PARAM$experimento <- "TS9320"

PARAM$exp_input  <- "FE9250"

PARAM$future       <- c( 202109 )

PARAM$final_train  <- c( 201908,201909, 202010, 202011, 202012, 202101, 202102, 202103, 202104, 202105, 202106, 202107 )
#PARAM$final_train  <- c(  201905, 202105 ) #Mayos de años anteriores

#PARAM$train$training     <- c( 202101, 202102, 202103 )
#PARAM$train$training     <- c( 201903, 201904, 201905 )
PARAM$train$training     <- c( 201908, 201909, 202010, 202011,202012, 202101, 202102, 202103, 202104, 202105)
PARAM$train$validation   <- c( 202106 )
PARAM$train$testing      <- c( 202107 )

PARAM$train$sampling_total  <- 0.4  # 1.0 significa que NO se hace sampling total,  0.3 es quedarse con el 30% de TODOS los registros
PARAM$train$undersampling_mayoritaria  <- 0.2   # 1.0 significa NO undersampling ,  0.1  es quedarse con el 10% de los CONTINUA

#Atencion, las semillas deben ser distintas
PARAM$train$semilla_sampling  <- 102191
PARAM$train$semilla_under     <- 892237

#WALKBACKWARDS VALIDATION
PARAM$walkbackward$testing    <- c( 201903, 201907,201910,201911,201912,202001,202002,202003,202004,202005,202006,202007,202008,202009 )

# FIN Parametros del script




#------------------------------------------------------------------------------

options(error = function() { 
  traceback(20); 
  options(error = NULL); 
  stop("exiting after script error") 
})
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
#------------------------------------------------------------------------------
#Aqui empieza el programa

setwd( "~/buckets/b1/" )

dir_salidas="~/buckets/b1/exp/TP_final/"
dir.create( dir_salidas )


#cargo el dataset donde voy a entrenar
#esta en la carpeta del exp_input y siempre se llama  dataset.csv.gz
dataset_input  <- paste0( "./exp/", PARAM$exp_input, "/dataset.csv.gz" )
dataset  <- fread( dataset_input )


#creo la carpeta donde va el experimento
dir.create( paste0( "./exp/", PARAM$experimento, "/"), showWarnings = FALSE )
setwd(paste0( "./exp/", PARAM$experimento, "/"))   #Establezco el Working Directory DEL EXPERIMENTO


setorder( dataset, foto_mes, numero_de_cliente )

#grabo los datos del futuro
# aqui JAMAS se hace sampling
fwrite( dataset[ foto_mes %in% PARAM$future, ],
        file= "dataset_future.csv.gz",
        logical01= TRUE,
        sep= "," )

#grabo los datos donde voy a entrenar los Final Models
# aqui  JAMAS se hace sampling
fwrite( dataset[ foto_mes %in% PARAM$final_train, ],
        file= "dataset_train_final.csv.gz",
        logical01= TRUE,
        sep= "," )

###################################
#grabo los datos del walkforward validation
# aqui JAMAS se hace sampling
fwrite( dataset[ foto_mes %in% PARAM$walkbackward$testing, ],
        file= "dataset_walkbackwards.csv.gz",
        logical01= TRUE,
        sep= "," )
###################################

#grabo los datos donde voy a hacer la optimizacion de hiperparametros
set.seed( PARAM$train$semilla_sampling )
dataset[ foto_mes %in% PARAM$train$training , azar_sampling := runif( nrow(dataset[foto_mes %in% PARAM$train$training ]) ) ]


set.seed( PARAM$train$semilla_under )
dataset[ foto_mes %in% PARAM$train$training , azar_under := runif( nrow(dataset[foto_mes %in% PARAM$train$training ]) ) ]

dataset[  , fold_train := 0L ]
dataset[ foto_mes %in% PARAM$train$training & 
         ( azar_sampling <= PARAM$train$sampling_total ) &
         ( azar_under <= PARAM$train$undersampling_mayoritaria | clase_ternaria %in% c( "BAJA+1", "BAJA+2" ) )
         , fold_train := 1L ]

#Se valida SIN sampling de ningun tipo
dataset[  , fold_validate := 0L ]
dataset[ foto_mes %in% PARAM$train$validation, fold_validate := 1L ]

#Se testea SIN sampling de ningun tipo
dataset[  , fold_test := 0L ]
dataset[ foto_mes %in% PARAM$train$testing, fold_test := 1L ]


fwrite( dataset[ fold_train + fold_validate + fold_test >= 1 , ],
        file= "dataset_training.csv.gz",
        logical01= TRUE,
        sep= "," )





#'-------------------------------
timestamp <- format(Sys.time(), "%Y%m%d_%H%M%S")
archivo_nombre=paste0(timestamp,"_training_strategy_under")

#checkpoint
crearCheckpoint(dir_salidas,archivo_nombre)
