#Necesita para correr en Google Cloud
# 128 GB de memoria RAM
# 256 GB de espacio en el disco local
#   8 vCPU

# ZZ final que necesita de UNDERSAMPLING
#' @importFrom xgboost xgb.train xgb.DMatrix
#' @importFrom lightgbm lightgbm
#' @importFrom catboost catboost.train catboost.load_pool catboost.predict
#' @importFrom stats glm binomial
#' @importFrom progress progress_bar

#limpio la memoria
rm( list=ls() )  #remove all objects
gc()             #garbage collection

require("data.table")

require("lightgbm")

#Parametros del script
PARAM  <- list()
PARAM$experimento  <- "ZZ9420"
PARAM$exp_input  <- "HT9420"

PARAM$modelos  <- 3
# FIN Parametros del script

ksemilla  <- 102191

dir_salidas="~/buckets/b1/exp/TP_final/"
dir.create( dir_salidas )

#STACKING validation
PARAM$semillas_azar  <- c( 807299, 962041, 705689, 909463, 637597 )

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

Ganancia_WalkBackward <- function(dfuture, cortes = c(10500) ) {
  # vector_ganancia <- c()
  xx <- list()
  nom_pred  <- paste0( "_walkbackwards_",
                       sprintf( "%02d", i ),
                       "_",
                       sprintf( "%03d", iteracion_bayesiana)
  )
  
  #####################################
  xx$seed <- seed
  xx$modelo <- i
  xx$iteracion <- iteracion_bayesiana
  
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
  
  for (corte in cortes) {
    tb_prediccion[  , Predicted := 0L ]
    tb_prediccion[ 1:corte, Predicted := 1L ]
    
    xx[[paste0(corte)]]  <- tb_prediccion[ Predicted == 1L, 
                                           sum( ifelse(clase_ternaria=="BAJA+2", 78000, -2000 ) )]
  }
  
  #Graba cortes y ganancia
  exp_log(xx, arch= paste0(dir_salidas,timestamp,nom_pred,"_cortes.csv") )
  
  # vector_ganancia <- c(vector_ganancia, xx$`2500`)
  # 
  # ganancia_test_promedio <- mean(vector_ganancia)
  # 
  # #Grabo ganancia
  # fwrite( list(vector_ganancia),
  #         file= paste0(dir_salidas,timestamp,nom_pred,".csv"),
  #         sep= "\t" )
  # 
  return (xx$`2500`)
}
#----------------------------------------
#graba a un archivo los componentes de lista
#para el primer registro, escribe antes los titulos

exp_log  <- function( reg, arch=NA, folder="./exp/", ext=".txt", verbose=TRUE )
{
  archivo  <- arch
  if( is.na(arch) )  archivo  <- paste0(  folder, substitute( reg), ext )
  
  if( !file.exists( archivo ) )  #Escribo los titulos
  {
    linea  <- paste0( "fecha\t", 
                      paste( list.names(reg), collapse="\t" ), "\n" )
    
    cat( linea, file=archivo )
  }
  
  linea  <- paste0( format(Sys.time(), "%Y%m%d %H%M%S"),  "\t",     #la fecha y hora
                    gsub( ", ", "\t", toString( reg ) ),  "\n" )
  
  cat( linea, file=archivo, append=TRUE )  #grabo al archivo
  
  if( verbose )  cat( linea )   #imprimo por pantalla
}

#----------------------------------------
#'#####################################################
#Stacking
#stack_3lgbm_glm <- function(x, y, params, nfolds = 5L, seed = 42, verbose = TRUE) {
stack_3lgbm_glm <- function(dttrain, dtvalid, params1,params2,params3, semillas_azar , verbose = TRUE) {
  x <- dttrain
  
  set.seed(semillas_azar[1])
  nfolds = length(semillas_azar)
  
  nrow_x <- nrow(x)
  
  model_lgb1 <- vector("list", nfolds)
  model_lgb2 <- vector("list", nfolds)
  model_lgb3 <- vector("list", nfolds)
  
  x_glm <- matrix(NA, nrow = nrow_x, ncol = 3L)
  colnames(x_glm) <- c("lgb1", "lgb2", "lgb3")
  
  # lightgbm 1
  
  x_lgb <- as.matrix(x)
  
  for (i in 1L:nfolds) {
    dtrain  <- lgb.Dataset( data=    data.matrix( x_lgb[ , campos_buenos, with=FALSE] ),
                            label=   x_lgb[ , clase01],
                            weight=  x_lgb[ , ifelse( clase_ternaria %in% c("BAJA+2"), 1.0000001, 1.0)],
                            free_raw_data= FALSE
    )
    params1$seed = semillas_azar[i]
    
    fit <- lightgbm(
      data = dtrain,
      params = params1,
      verbose = -1
    )
    
    model_lgb1[[i]] <- fit
    x_glm[index_lgb1 == i, "lgb1"] <- predict(fit, data.matrix( dtvalid ))
    
    
    #borro y limpio la memoria para la vuelta siguiente del for
    rm( dtrain )
    gc()
  }
  
  # lightgbm 2
  
  x_lgb <- as.matrix(x)
  
  for (i in 1L:nfolds) {
    dtrain  <- lgb.Dataset( data=    data.matrix( x_lgb[ , campos_buenos, with=FALSE] ),
                            label=   x_lgb[ , clase01],
                            weight=  x_lgb[ , ifelse( clase_ternaria %in% c("BAJA+2"), 1.0000001, 1.0)],
                            free_raw_data= FALSE
    )
    params2$seed = semillas_azar[i]
    fit <- lightgbm(
      data = dtrain,
      params = params2,
      verbose = -1
    )
    
    model_lgb1[[i]] <- fit
    x_glm[index_lgb2 == i, "lgb2"] <- predict(fit, data.matrix( dtvalid ))

    #borro y limpio la memoria para la vuelta siguiente del for
    rm( dtrain )
    gc()
  }
  
  # lightgbm 3
  
  x_lgb <- as.matrix(x)
  
  for (i in 1L:nfolds) {
    dtrain  <- lgb.Dataset( data=    data.matrix( x_lgb[ , campos_buenos, with=FALSE] ),
                            label=   x_lgb[ , clase01],
                            weight=  x_lgb[ , ifelse( clase_ternaria %in% c("BAJA+2"), 1.0000001, 1.0)],
                            free_raw_data= FALSE
    )
    params3$seed = semillas_azar[i]
    fit <- lightgbm(
      data = dtrain,
      params = params3,
      verbose = -1
    )
    
    model_lgb1[[i]] <- fit
    x_glm[index_lgb3 == i, "lgb3"] <- predict(fit, data.matrix( dtvalid ))
 
    #borro y limpio la memoria para la vuelta siguiente del for
    rm( dtrain )
    gc()
  }

  # logistic regression
  df <- as.data.frame(cbind(dtrain[,"clase01"], x_glm))
  names(df)[1] <- "y"
  model_glm <- glm(y ~ ., data = df, family = binomial())
  
  lst <- list(
    "model_lgb1" = model_lgb1,
    "model_lgb2" = model_lgb2,
    "model_lgb3" = model_lgb3,
    "model_glm" = model_glm
  )
  class(lst) <- "stack_nlgbm_glm"
  return( lst )
}

#predict.stack_nlgbm_glm : Predección final
predict.stack_nlgbm_glm <- function(object, dttest, ...) {
  nrow_newx <- nrow(dttest)
  nfolds <- length(object$model_lgb1)
  
  pred_lgb1 <- matrix(NA, nrow = nrow_newx, ncol = nfolds)
  pred_lgb2 <- matrix(NA, nrow = nrow_newx, ncol = nfolds)
  pred_lgb3 <- matrix(NA, nrow = nrow_newx, ncol = nfolds)
  
  newx_lgb1 <- as.matrix(dttest)
  for (i in 1L:nfolds) pred_lgb1[, i] <- predict(object$model_lgb1[[i]], newx_lgb1)
  
  newx_lgb2 <- as.matrix(dttest)
  for (i in 1L:nfolds) pred_lgb2[, i] <- predict(object$model_lgb2[[i]], newx_lgb2)
  
  newx_lgb3 <- as.matrix(dttest)
  for (i in 1L:nfolds) pred_lgb3[, i] <- predict(object$model_lgb3[[i]], newx_lgb3)
  
  newx_glm <- data.frame(
    "lgb1" = rowMeans(pred_lgb1),
    "lgb2" = rowMeans(pred_lgb2),
    "lgb3" = rowMeans(pred_lgb3)
  )
  
  pred_prob <- unname(predict(object$model_glm, newx_glm, type = "response"))
  # pred_resp <- ifelse(pred_prob > threshold, classes[1], classes[2])
  # 
  # lista <- list("prob" = pred_prob, "resp" = pred_resp)
  return(pred_prob)
}

#'############################################

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
arch_stacking  <- paste0( base_dir, "exp/", TS, "/dataset_walkbackwards.csv.gz" )
dvalid <- fread( arch_stacking )

#defino la clase binaria
dvalid[ , clase01 := ifelse( clase_ternaria %in% c("BAJA+1","BAJA+2"), 1, 0 )  ]
#######


#defino la clase binaria
dataset[ , clase01 := ifelse( clase_ternaria %in% c("BAJA+1","BAJA+2"), 1, 0 )  ]

campos_buenos  <- setdiff( colnames(dataset), c( "clase_ternaria", "clase01") )

#NUEVO
ls_parametros <- list()


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
  parametros$min_data_in_leaf  <- pmax( 1,  round( nrow(dataset) / ( 2.0 ^ parametros$leaf_size_log ))  )
  #Luego la cantidad de hojas en funcion del valor anterior, el coverage, y la cantidad de registros
  parametros$num_leaves  <-  pmin( 131072, pmax( 2,  round( parametros$coverage * nrow( dataset ) / parametros$min_data_in_leaf ) ) )
  cat( "min_data_in_leaf:", parametros$min_data_in_leaf,  ",  num_leaves:", parametros$num_leaves, "\n" )
  
  #ya no me hacen falta
  parametros$leaf_size_log  <- NULL
  parametros$coverage  <- NULL
  

  ls_parametros <- append(ls_parametros,list(parametros))
  
  
  #borro y limpio la memoria para la vuelta siguiente del for
#   rm( tb_prediccion )
#   rm( tb_importancia )
#   rm( modelo_final)
#   rm( parametros )
# #  rm( dtrain )
#   gc()
}





#----------------------------------------
#STACKING
#----------------------------------------
#modelos <- stack_3lgbm_glm(dataset,dvalid,tb_parametros[1,"params"],tb_parametros[2,"params"],tb_parametros[3,"params"],PARAM$semillas_azar )

#stack_3lgbm_glm <- function(dttrain, dvalid, params1,params2,params3, semillas_azar , verbose = TRUE) {
  
  dtvalid <- dvalid[ , campos_buenos, with=FALSE]
  x <- dataset
  params1 <- ls_parametros[[1]]
  params2 <- ls_parametros[[2]]
  params3 <- ls_parametros[[3]]
  semillas_azar <- PARAM$semillas_azar
  
  
  
  
  
  set.seed(semillas_azar[1])
  nfolds = length(semillas_azar)
  
  nrow_x <- nrow(x)
  
  model_lgb1 <- vector("list", nfolds)
  model_lgb2 <- vector("list", nfolds)
  model_lgb3 <- vector("list", nfolds)
  
  x_glm <- matrix(NA, nrow = nrow(dtvalid), ncol = 3L)
  colnames(x_glm) <- c("lgb1", "lgb2", "lgb3")
  
  # lightgbm 1
  
  x_lgb <- x
  
  for (i in 1L:nfolds) {
    dtrain  <- lgb.Dataset( data=    data.matrix( x_lgb[ , campos_buenos, with=FALSE] ),
                            label=   x_lgb[ , clase01],
                            weight=  x_lgb[ , ifelse( clase_ternaria %in% c("BAJA+2"), 1.0000001, 1.0)],
                            free_raw_data= FALSE
    )
    params1$seed = semillas_azar[i]
    
    fit <- lightgbm(
      data = dtrain,
      param = params1,
      verbose = -1
    )
    
    model_lgb1[[i]] <- fit
    x_glm[, "lgb1"] <- predict(fit, data.matrix( dtvalid ))
    
    
    #borro y limpio la memoria para la vuelta siguiente del for
    rm( dtrain )
    gc()
  }
  
  # lightgbm 2
  
  x_lgb <- x
  
  for (i in 1L:nfolds) {
    dtrain  <- lgb.Dataset( data=    data.matrix( x_lgb[ , campos_buenos, with=FALSE] ),
                            label=   x_lgb[ , clase01],
                            weight=  x_lgb[ , ifelse( clase_ternaria %in% c("BAJA+2"), 1.0000001, 1.0)],
                            free_raw_data= FALSE
    )
    params2$seed = semillas_azar[i]
    fit <- lightgbm(
      data = dtrain,
      param = params2,
      verbose = -1
    )
    
    model_lgb2[[i]] <- fit
    x_glm[, "lgb2"] <- predict(fit, data.matrix( dtvalid ))
    
    #borro y limpio la memoria para la vuelta siguiente del for
    rm( dtrain )
    gc()
  }
  
  # lightgbm 3
  
  x_lgb <- x
  
  for (i in 1L:nfolds) {
    dtrain  <- lgb.Dataset( data=    data.matrix( x_lgb[ , campos_buenos, with=FALSE] ),
                            label=   x_lgb[ , clase01],
                            weight=  x_lgb[ , ifelse( clase_ternaria %in% c("BAJA+2"), 1.0000001, 1.0)],
                            free_raw_data= FALSE
    )
    params3$seed = semillas_azar[i]
    fit <- lightgbm(
      data = dtrain,
      param = params3,
      verbose = -1
    )
    
    model_lgb3[[i]] <- fit
    x_glm[, "lgb3"] <- predict(fit, data.matrix( dtvalid ))
    
    #borro y limpio la memoria para la vuelta siguiente del for
    rm( dtrain )
    gc()
  }
  

  # logistic regression
  df <- as.data.frame(cbind(dvalid[,"clase01"], x_glm))
  names(df)[1] <- "y"
  model_glm <- glm(y ~ ., data = df, family = binomial())
  
  lst <- list(
    "model_lgb1" = model_lgb1,
    "model_lgb2" = model_lgb2,
    "model_lgb3" = model_lgb3,
    "model_glm" = model_glm
  )
  class(lst) <- "stack_nlgbm_glm"
  modelos <-  lst

  #GRABAR o LEER el stacking
  #save(modelos, file = paste0("Stacking_modelos.RData"))
  modelos <- fread(paste0("Stacking_modelos.RData"))
##################################################




prediccion <- predict.stack_nlgbm_glm(modelos, dfuture[ , campos_buenos, with=FALSE ] )
                        
tb_prediccion  <- dfuture[  , list( numero_de_cliente, foto_mes ) ]
tb_prediccion[ , prob := prediccion ]


#Grabo probabilidades
nom_pred  <- paste0( dir_salidas,timestamp,"_pred_",
                     sprintf( "%02d", i ),
                     "_",
                     sprintf( "%03d", iteracion_bayesiana),
                     ".csv"  )

fwrite( tb_prediccion,
        file= nom_pred,
        sep= "\t" )



#genero los archivos para Kaggle
cortes  <- seq( from=  1000,
                to=   7000,
                by=     500 )


setorder( tb_prediccion, -prob )

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
  
  



archivo_nombre=paste0(timestamp,"_lightgbm_under")

#checkpoint
crearCheckpoint(dir_salidas,archivo_nombre)

#Copio BO_log.txt a carpeta salidas
fwrite( tb_log,
        file= paste0(dir_salidas,timestamp,"_BO_log.txt"),
        sep= "\t" )

