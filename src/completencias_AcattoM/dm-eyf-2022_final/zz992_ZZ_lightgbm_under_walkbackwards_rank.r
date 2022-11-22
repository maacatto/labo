#Necesita para correr en Google Cloud
# 128 GB de memoria RAM
# 256 GB de espacio en el disco local
#   8 vCPU

# ZZ final que necesita de UNDERSAMPLING

#limpio la memoria
rm( list=ls() )  #remove all objects
gc()             #garbage collection

require("data.table")

require("lightgbm")

#Parametros del script
PARAM  <- list()
PARAM$experimento  <- "ZZ9420_V2"
PARAM$exp_input  <- "HT9420"

PARAM$modelos  <- 3
# FIN Parametros del script

ksemilla  <- 102191

dir_salidas="~/buckets/b1/exp/TP_final/"
dir.create( dir_salidas )

#WalkBalkward validation
PARAM$semillas_azar  <- c( 807299, 962041, 705689, 909463, 637597 )
corte = 10500

#Ensamble
PARAM$use_rank_final <- TRUE #Usar rank en vez de probabilidad para generar salida Kaggle

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

Ganancia_WalkBackward <- function(dfuture, corte = 10500) {
  vector_ganancia <- c()
  #####################################
  for (seed in PARAM$semillas_azar[1]) {
    
    #Utilizo la semilla definida en este script
    #parametros$seed  <- ksemilla
    #genero el modelo entrenando en los datos finales
    set.seed( seed )
    
    
    #genero la prediccion, Scoring
    prediccion  <- predict( modelo_final,
                            data.matrix( dfuture[ , campos_buenos, with=FALSE ] ) )
    
    tb_prediccion  <- dfuture[  , list( numero_de_cliente, foto_mes,clase_ternaria ) ]
    tb_prediccion[ , prob := prediccion ]
    
    #GANANCIA
    setorder( tb_prediccion, -prob )
    
    tb_prediccion[  , Predicted := 0L ]
    tb_prediccion[ 1:corte, Predicted := 1L ]
    
    ganancia_test  <- tb_prediccion[ Predicted == 1L, 
                                     sum( ifelse(clase_ternaria=="BAJA+2", 78000, -2000 ) )]
    
    vector_ganancia <- c(vector_ganancia, ganancia_test)
  } 
  
  ganancia_test_promedio <- mean(vector_ganancia)
  
  
  nom_pred  <- paste0( "_walkbackwards_",
                       sprintf( "%02d", i ),
                       "_",
                       sprintf( "%03d", iteracion_bayesiana),
                       ".csv"  )
  
  #Grabo ganancia
  timestamp <- format(Sys.time(), "%Y%m%d_%H%M%S")
  fwrite( list(vector_ganancia),
          file= paste0(dir_salidas,timestamp,nom_pred),
          sep= "\t" )
  
  return (ganancia_test_promedio)
}
#----------------------------------------




#------------------------------------------------------------------------------
#Aqui empieza el programa

base_dir <- "~/buckets/b1/"

#----------------------------------------------------
timestamp <- format(Sys.time(), "%Y%m%d_%H%M%S")


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

#######
#cargo el dataset para walkbackward validation
arch_walkbackward  <- paste0( base_dir, "exp/", TS, "/dataset_walkbackwards.csv.gz" )
dtest <- fread( arch_walkbackward )
#######



#defino la clase binaria
dataset[ , clase01 := ifelse( clase_ternaria %in% c("BAJA+1","BAJA+2"), 1, 0 )  ]

campos_buenos  <- setdiff( colnames(dataset), c( "clase_ternaria", "clase01") )


#genero un modelo para cada uno de las modelos_qty MEJORES iteraciones de la Bayesian Optimization
for( i in  1:PARAM$modelos )
{
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
  
  if( ! ("leaf_size_log" %in% names(parametros) ) )  stop( "El Hyperparameter Tuning debe tener en BO_log.txt  el pseudo hiperparametro  lead_size_log.\n" )
  if( ! ("coverage" %in% names(parametros) ) ) stop( "El Hyperparameter Tuning debe tener en BO_log.txt  el pseudo hiperparametro  coverage.\n" )
  
  #Primero defino el tamaño de las hojas
  parametros$min_data_in_leaf  <- pmax( 1,  round( nrow(dtrain) / ( 2.0 ^ parametros$leaf_size_log ))  )
  #Luego la cantidad de hojas en funcion del valor anterior, el coverage, y la cantidad de registros
  parametros$num_leaves  <-  pmin( 131072, pmax( 2,  round( parametros$coverage * nrow( dtrain ) / parametros$min_data_in_leaf ) ) )
  cat( "min_data_in_leaf:", parametros$min_data_in_leaf,  ",  num_leaves:", parametros$num_leaves, "\n" )
  
  #ya no me hacen falta
  parametros$leaf_size_log  <- NULL
  parametros$coverage  <- NULL
  
  probabilidad_ensemble  <- rep( 0, nrow(dfuture) )
  rank_ensemble  <- rep( 0, nrow(dfuture) )
  
  tb_ensembles  <-  copy( dfuture[ , list( numero_de_cliente ) ] )
  
  for (seed in PARAM$semillas_azar) {
    
    #Utilizo la semilla definida en este script
    parametros$seed  <- seed
    
    #genero el modelo entrenando en los datos finales
    set.seed( parametros$seed )
    modelo_final  <- lightgbm( data= dtrain,
                               param=  parametros,
                               verbose= -100 )
    
    
    
    #creo y grabo la importancia de variables
    # tb_importancia  <- as.data.table( lgb.importance( modelo_final ) )
    # fwrite( tb_importancia,
    #         file= paste0( dir_salidas,timestamp,"_impo_",
    #                       sprintf( "%02d", i ),
    #                       "_",
    #                       sprintf( "%03d", iteracion_bayesiana ),
    #                       ".txt" ),
    #         sep= "\t" )
    
    #ENSAMBLE de distintas semillas
    ##################################### 
    #inicializo en CERO el vector de las probabilidades en dfuture
    #Aqui es donde voy acumulando, sumando, las probabilidades
    
    #genero la prediccion, Scoring
    prediccion  <- predict( modelo_final,
                            data.matrix( dfuture[ , campos_buenos, with=FALSE ] ) )
    
    #ENSAMBLE de distintas semillas
    tb_ensembles[  ,  paste0( "semilla_", seed) :=  prediccion ]
    
    #voy acumulando la probabilidad
    probabilidad_ensemble  <- probabilidad_ensemble +  prediccion
    
    ###################
    #hago el rank de las probabilidades (Copyright Tomas Delvechio)
    tb_ensembles[  ,  paste0( "semilla_", seed,"_rank") :=  frank(-get(paste0( "semilla_", seed)), ties.method = "random") ]
    
    rank_ensemble <- rank_ensemble + tb_ensembles[,get(paste0( "semilla_", seed,"_rank"))]
    ###################
    
  }
  
  #grabo el modelo, achivo .model
  lgb.save( modelo_final,
            file= arch_modelo )
  
  #fue sumando las probabilidades, ahora hago el promedio
  probabilidad_ensemble  <- probabilidad_ensemble / length(PARAM$semillas_azar)
  rank_ensemble_final <- frank(-rank_ensemble, ties.method = "random")
  
  #asigngo el promedio y grabo
  tb_ensembles[  , prob_promedio := probabilidad_ensemble ]
  tb_ensembles[  , rank_promedio := rank_ensemble_final ]
  
  nom_ensemble  <- paste0( dir_salidas,timestamp,"_ensemble_",
                           sprintf( "%02d", i ),
                           "_",
                           sprintf( "%03d", iteracion_bayesiana),
                           ".csv"  )
  fwrite( tb_ensembles,
          file= nom_ensemble,
          sep="\t" )
  
  #cambia nombre para seguir utilizando script original
  prediccion <- probabilidad_ensemble
  ##################################### 
  
  
  tb_prediccion  <- dfuture[  , list( numero_de_cliente, foto_mes ) ]
  tb_prediccion[ , prob := prediccion ]
  tb_prediccion[ , rank := rank_ensemble_final ]
  
  nom_pred  <- paste0( dir_salidas,timestamp,"_pred_",
                       sprintf( "%02d", i ),
                       "_",
                       sprintf( "%03d", iteracion_bayesiana),
                       ".csv"  )
  
  fwrite( tb_prediccion,
          file= nom_pred,
          sep= "\t" )
  
  
  #genero los archivos para Kaggle
  cortes  <- seq( from=  9000,
                  to=   15000,
                  by=     500 )
  ###########################
  if (PARAM$use_rank_final) {
    # Generamos predicción individual
    setorder(tb_prediccion, rank)
  } else {
    # Generamos predicción individual
    setorder(tb_prediccion, -prob)
  }
  ##########################
  #setorder( tb_prediccion, -prob )
  
  for( corte in cortes )
  {
    tb_prediccion[  , Predicted := 0L ]
    tb_prediccion[ 1:corte, Predicted := 1L ]
    
    nom_submit  <- paste0( dir_salidas,timestamp, 
                           "_",
                           sprintf( "%02d", i ),
                           "_",
                           sprintf( "%03d", iteracion_bayesiana ),
                           "_",
                           sprintf( "%05d", corte ),
                           ".csv" )
    
    fwrite(  tb_prediccion[ , list( numero_de_cliente, Predicted ) ],
             file= nom_submit,
             sep= "," )
    
  }
  
  #----------------------------------------
  #WALK BACKWARD validation
  #----------------------------------------
  Ganancia_WalkBackward(dtest,corte)
  #-----------------------------------------
  
  #borro y limpio la memoria para la vuelta siguiente del for
  rm( tb_prediccion )
  #  rm( tb_importancia )
  rm( modelo_final)
  rm( parametros )
  rm( dtrain )
  gc()
}








archivo_nombre=paste0(timestamp,"_lightgbm_under")

#checkpoint
crearCheckpoint(dir_salidas,archivo_nombre)

#Copio BO_log.txt a carpeta salidas
fwrite( tb_log,
        file= paste0(dir_salidas,timestamp,"_BO_log.txt"),
        sep= "\t" )

