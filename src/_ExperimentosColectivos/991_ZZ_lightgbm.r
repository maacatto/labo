#Necesita para correr en Google Cloud
# 128 GB de memoria RAM
# 256 GB de espacio en el disco local
#   8 vCPU


#limpio la memoria
rm( list=ls() )  #remove all objects
gc()             #garbage collection

require("data.table")

require("lightgbm")

#Parametros del script
PARAM  <- list()
PARAM$experimento  <- "ZZ9410"
PARAM$exp_input  <- "HT9410"

PARAM$modelos  <- 3
# FIN Parametros del script
ksemilla  <- 807299


PARAM$semillas_azar  <- c( 807299, 962041, 705689, 909463, 637597 )
corte = 10500

dir_salidas="~/buckets/b1/exp/EC/"
dir.create( dir_salidas )


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
options(error = function() { 
  traceback(20); 
  options(error = NULL); 
  stop("exiting after script error") 
})
#------------------------------------------------------------------------------
vprob_optima  <- c()

fganancia_lgbm_meseta  <- function( probs, datos) 
{
  vlabels  <- get_field(datos, "label")
  
  tbl  <- as.data.table( list( "prob"= probs, 
                               "gan" = ifelse( vlabels==1 , 78000, -2000  ) ) )
  
  setorder( tbl, -prob )
  tbl[ , posicion := .I ]
  tbl[ , gan_acum :=  cumsum( gan ) ]
  
  gan  <-  tbl[ , max(gan_acum) ]
  
  pos  <- which.max(  tbl[ , gan_acum ] ) 
  vprob_optima  <<- c( vprob_optima, tbl[ pos, prob ] )
  
  rm( tbl )
  return( list( "name"= "ganancia", 
                "value"=  gan,
                "higher_better"= TRUE ) )
}
#------------------------------------------------------------------------------
#Aqui empieza el programa

base_dir <- "~/buckets/b1/"

#creo la carpeta donde va el experimento
dir.create( paste0( base_dir, "exp/", PARAM$experimento, "/"), showWarnings = FALSE )
setwd(paste0( base_dir, "exp/", PARAM$experimento, "/"))   #Establezco el Working Directory DEL EXPERIMENTO

#leo la salida de la optimizaciob bayesiana
arch_log  <- paste0( base_dir, "exp/", PARAM$exp_input, "/BO_log.txt" )
tb_log  <- fread( arch_log )
setorder( tb_log, -ganancia )

#leo el nombre del expermento de la Training Strategy
arch_TS  <- paste0( base_dir, "exp/", PARAM$exp_input, "/TrainingStrategy.txt" )
TS  <- readLines( arch_TS, warn=FALSE )

#leo el dataset donde voy a entrenar el modelo final
arch_dataset  <- paste0( base_dir, "exp/", TS, "/dataset_train_final.csv.gz" )
dataset  <- fread( arch_dataset )

#leo el dataset donde voy a aplicar el modelo final
arch_future  <- paste0( base_dir, "exp/", TS, "/dataset_future.csv.gz" )
dfuture <- fread( arch_future )


#defino la clase binaria
dataset[ , clase01 := ifelse( clase_ternaria %in% c("BAJA+1","BAJA+2"), 1, 0 )  ]

campos_buenos  <- setdiff( colnames(dataset), c( "clase_ternaria", "clase01") )


#genero un modelo para cada uno de las modelos_qty MEJORES iteraciones de la Bayesian Optimization
for( i in  1:PARAM$modelos )
{
  vector_ganancia <- c()
  #####################################
  for (seed in PARAM$semillas_azar) {
    
    parametros  <- as.list( copy( tb_log[ i ] ) )
    iteracion_bayesiana  <- parametros$iteracion_bayesiana
    
    arch_modelo  <- paste0( "modelo_" ,
                            sprintf( "%02d", i ),
                            "_",
                            sprintf( "%03d", iteracion_bayesiana ),
                            ".model" )
    
    
    #creo CADA VEZ el dataset de lightgbm
    dtrain  <- lgb.Dataset( data=    data.matrix( dataset[ , campos_buenos, with=FALSE] ),
                            label=   dataset[ , clase01],
                              weight=  dataset[ , ifelse( clase_ternaria %in% c("BAJA+2"), 1.0000001, 1.0)],
                            free_raw_data= FALSE
    )
    
    ganancia  <- parametros$ganancia
    
    #elimino los parametros que no son de lightgbm
    parametros$experimento  <- NULL
    parametros$cols         <- NULL
    parametros$rows         <- NULL
    parametros$fecha        <- NULL
    parametros$prob_corte   <- NULL
    parametros$estimulos    <- NULL
    parametros$ganancia     <- NULL
    parametros$iteracion_bayesiana  <- NULL
    
    #Utilizo la semilla definida en este script
    #parametros$seed  <- ksemilla
    #genero el modelo entrenando en los datos finales
    set.seed( seed )
    #AGREGADO
    parametros$seed <- seed
    modelo_final  <- lightgbm( data= dtrain,
                               param=  parametros,
                               verbose= -100 )
    
    # #grabo el modelo, achivo .model
    # lgb.save( modelo_final,
    #           file= arch_modelo )
    # 
    #creo y grabo la importancia de variables
    tb_importancia  <- as.data.table( lgb.importance( modelo_final ) )
    fwrite( tb_importancia,
            file= paste0( "impo_", 
                          sprintf( "%02d", i ),
                          "_",
                          sprintf( "%03d", iteracion_bayesiana ),
                          ".txt" ),
            sep= "\t" )
    
    
    #genero la prediccion, Scoring
    prediccion  <- predict( modelo_final,
                            data.matrix( dfuture[ , campos_buenos, with=FALSE ] ) )
    
    tb_prediccion  <- dfuture[  , list( numero_de_cliente, foto_mes,clase_ternaria ) ]
    tb_prediccion[ , prob := prediccion ]
    
    
    #GANANCIA
    setorder( tb_prediccion, -prob )
<<<<<<< HEAD
    
    tb_prediccion[  , Predicted := 0L ]
    tb_prediccion[ 1:corte, Predicted := 1L ]
    
    ganancia_test  <- tb_prediccion[ Predicted == 1L, 
                                     sum( ifelse(clase_ternaria=="BAJA+2", 78000, -2000 ) )]
=======
>>>>>>> 982b1f94b563a90fd5174cac92a3cc153cc92134
    
    tb_prediccion[  , Predicted := 0L ]
    tb_prediccion[ 1:corte, Predicted := 1L ]
    
    ganancia_test  <- tb_prediccion[ Predicted == 1L, 
                                     sum( ifelse(clase_ternaria=="BAJA+2", 78000, -2000 ) )]
    
    
      
    vector_ganancia <- c(vector_ganancia, ganancia_test)
    
    #borro y limpio la memoria para la vuelta siguiente del for
    rm( tb_prediccion )
    rm( tb_importancia )
    rm( modelo_final)
    rm( parametros )
    rm( dtrain )
    gc()
  } 
  
  ganancia_test_promedio <- mean(vector_ganancia)
  
  
  nom_pred  <- paste0( "pred_",
                       sprintf( "%02d", i ),
                       "_",
                       sprintf( "%03d", iteracion_bayesiana),
                       ".csv"  )
  
  #Grabo ganancia
  timestamp <- format(Sys.time(), "%Y%m%d_%H%M%S")
  fwrite( list(vector_ganancia),
          file= paste0(dir_salidas,timestamp,nom_pred),
          sep= "\t" )
  
  
  
  
}
#----------------------------------------

timestamp <- format(Sys.time(), "%Y%m%d_%H%M%S")
archivo_nombre=paste0(timestamp,"_lightgbm")

#checkpoint
crearCheckpoint(dir_salidas,archivo_nombre)

#Copio BO_log.txt a carpeta salidas
fwrite( tb_log,
        file= paste0(dir_salidas,timestamp,"_BO_log.txt"),
        sep= "\t" )
