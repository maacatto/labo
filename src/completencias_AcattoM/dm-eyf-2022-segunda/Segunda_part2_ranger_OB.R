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
#setwd("G:\\My Drive\\Facultad\\Maestria DM\\Expecializacion\\DMEyF\\Carpetas")  #Establezco el Working Directory
setwd("~/buckets/b1/")   #Establezco el Working Directory

dir_salidas="./exp/TP"
dir.create( dir_salidas )

#'------------------------------------------------------------------------------
# 1. Lectura de datos ----
#'------------------------------------------------------------------------------

## Dataset de entrada <----- ----
archivo_featureEngineer <- "20220928_152726_part1_FeatEng.csv"


# cargo el dataset
#dataset_fe <- fread("./datasets/competencia1_2022.csv")
#dataset_fe  <- fread("./datasets/competencia2_2022.csv.gz", stringsAsFactors= TRUE)
dataset_fe  <- fread(paste0( dir_salidas,"/",archivo_featureEngineer), stringsAsFactors= TRUE)



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

## Optimizacion Bayesiana ----
#fold_test  tiene el numero de fold que voy a usar para testear, entreno en el resto de los folds
#param tiene los hiperparametros del arbol

#graba a un archivo los componentes de lista
#para el primer registro, escribe antes los titulos

loguear  <- function( reg, arch=NA, folder="./work/", ext=".txt", verbose=TRUE )
{
  archivo  <- arch
  if( is.na(arch) )  archivo  <- paste0(  folder, substitute( reg ), ext )
  
  if( !file.exists( archivo ) )  #Escribo los titulos
  {
    linea  <- paste0( "fecha\t", 
                      paste( list.names(reg), collapse="\t" ), "\n" )
    
    cat( linea, file= archivo )
  }
  
  linea  <- paste0( format(Sys.time(), "%Y%m%d %H%M%S"),  "\t",     #la fecha y hora
                    gsub( ", ", "\t", toString( reg ) ),  "\n" )
  
  cat( linea, file= archivo, append= TRUE )  #grabo al archivo
  
  if( verbose )  cat( linea )   #imprimo por pantalla
}

#particionar agrega una columna llamada fold a un dataset que consiste en una particion estratificada segun agrupa
# particionar( data=dataset, division=c(70,30), agrupa=clase_ternaria, seed=semilla)   crea una particion 70, 30 
# particionar( data=dataset, division=c(1,1,1,1,1), agrupa=clase_ternaria, seed=semilla)   divide el dataset en 5 particiones

particionar  <- function( data, division, agrupa="", campo="fold", start=1, seed=NA )
{
  if( !is.na( seed)  )   set.seed( seed )
  
  bloque  <- unlist( mapply(  function(x,y) { rep( y, x ) }, division, seq( from=start, length.out=length(division) )  ) )
  
  data[ , (campo) :=  sample( rep( bloque, ceiling(.N/length(bloque))) )[1:.N],
        by= agrupa ]
}


ranger_Simple  <- function( fold_test, pdata, param )
{
  #genero el modelo
  
  set.seed(ksemilla_azar)
  
  modelo  <- ranger( formula= "clase_binaria ~ .",
                     data=  pdata[ fold!= fold_test], 
                     probability=   TRUE,  #para que devuelva las probabilidades
                     num.trees=     param$num.trees,
                     mtry=          param$mtry,
                     min.node.size= param$min.node.size,
                     max.depth=     param$max.depth
  )
  
  prediccion  <- predict( modelo, pdata[ fold == fold_test] )
  
  ganancia_testing  <- pdata[ fold==fold_test,
                              sum( (prediccion$predictions[ ,"POS" ] > 1/40) *
                                     ifelse( clase_binaria=="POS", 78000, -2000)  ) ]
  
  return( ganancia_testing )
}


ranger_CrossValidation  <- function( data, param, pcampos_buenos, qfolds, pagrupa, semilla )
{
  divi  <- rep( 1, qfolds )
  particionar( data, divi, seed=semilla, agrupa=pagrupa )
  
  ganancias  <- mcmapply( ranger_Simple, 
                          seq(qfolds), # 1 2 3 4 5  
                          MoreArgs= list( data, param), 
                          SIMPLIFY= FALSE,
                          mc.cores= 1 )   #dejar esto en  1, porque ranger ya corre en paralelo
  
  data[ , fold := NULL ]   #elimino el campo fold
  
  #devuelvo la ganancia promedio normalizada
  ganancia_promedio  <- mean( unlist( ganancias ) )
  ganancia_promedio_normalizada  <- ganancia_promedio * qfolds
  
  return( ganancia_promedio_normalizada )
}

#esta funcion solo puede recibir los parametros que se estan optimizando
#el resto de los parametros se pasan como variables globales

EstimarGanancia_ranger  <- function( x )
{
  GLOBAL_iteracion  <<- GLOBAL_iteracion + 1
  
  xval_folds  <- 5   # 5-fold cross validation
  
  ganancia  <- ranger_CrossValidation( dataset, 
                                       param= x,
                                       qfolds= xval_folds, 
                                       pagrupa= "clase_binaria",
                                       semilla= ksemilla_azar )
  
  #logueo 
  xx  <- x
  xx$xval_folds  <-  xval_folds
  xx$ganancia  <- ganancia
  xx$iteracion  <- GLOBAL_iteracion
  loguear( xx, arch= klog )
  
  return( ganancia )
}








#'------------------------------------------------------------------------------
# 5. OB -----
#'-----------------------------------------------------------------------------
# Creo dataset y columnas para la funcion
dataset  <- dataset_fe
dataset  <- dataset[ foto_mes==202103 ]
columnas <- "clase_binaria ~ . "



## (opcional) Defino la  Optimizacion Bayesiana -----
kBO_iter  <- 100   #cantidad de iteraciones de la Optimizacion Bayesiana


#Estructura que define los hiperparámetros y sus rangos
hs  <- makeParamSet(
          makeIntegerParam("num.trees" ,        lower=  100L, upper= 2500L),  #la letra L al final significa ENTERO
          makeIntegerParam("max.depth",         lower=    1L, upper=   30L),  # 0 significa profundidad infinita
          makeIntegerParam("min.node.size" ,    lower=    1L, upper=  500L),
          makeIntegerParam("mtry" ,             lower=    2L, upper=   50L))

ksemilla_azar  <- seeds[1]  #Aqui poner la propia semilla seeds[0]   #cambiar por la primer semilla

archivo_log  <- paste0( dir_salidas,"/", "20220911_034709", "OB.txt")
archivo_BO   <- paste0( dir_salidas,"/", "20220911_034709", "OB.RDATA") 

#leo si ya existe el log, para retomar en caso que se se corte el programa
GLOBAL_iteracion  <- 0



#OptBayesiana(dataset,columnas,GLOBAL_iteracion,archivo_log,archivo_BO)
#en estos archivos quedan los resultados
kbayesiana  <- archivo_log
klog        <- archivo_BO

#leo si ya existe el log, para retomar en caso que se se corte el programa
GLOBAL_iteracion  <- 0   #inicializo la variable global

#si ya existe el archivo log, traigo hasta donde llegue
if( file.exists(klog) )
{
  tabla_log  <- fread( klog )
  GLOBAL_iteracion  <- nrow( tabla_log )
}



#paso a trabajar con clase binaria POS={BAJA+2}   NEG={BAJA+1, CONTINUA}
dataset[ , clase_binaria := as.factor(ifelse( clase_ternaria=="BAJA+2", "POS", "NEG" )) ]
dataset[ , clase_ternaria := NULL ]  #elimino la clase_ternaria, ya no la necesito


#imputo los nulos, ya que ranger no acepta nulos
#Leo Breiman, ¿por que le temias a los nulos?
dataset  <- na.roughfix( dataset )



#Aqui comienza la configuracion de la Bayesian Optimization

configureMlr( show.learner.output = FALSE)

funcion_optimizar  <- EstimarGanancia_ranger

#configuro la busqueda bayesiana,  los hiperparametros que se van a optimizar
#por favor, no desesperarse por lo complejo
obj.fun  <- makeSingleObjectiveFunction(
  fn=       funcion_optimizar,
  minimize= FALSE,   #estoy Maximizando la ganancia
  noisy=    TRUE,
  par.set=  hs,
  has.simple.signature = FALSE
)

ctrl  <- makeMBOControl( save.on.disk.at.time= 120,  save.file.path= kbayesiana)
ctrl  <- setMBOControlTermination(ctrl, iters= kBO_iter )
ctrl  <- setMBOControlInfill(ctrl, crit= makeMBOInfillCritEI())

surr.km  <-  makeLearner("regr.km", predict.type= "se", covtype= "matern3_2", control= list(trace= TRUE))

#inicio la optimizacion bayesiana
if(!file.exists(kbayesiana)) {
  run  <- mbo(obj.fun, learner = surr.km, control = ctrl)
} else  run  <- mboContinue( kbayesiana )   #retomo en caso que ya exista

#'------------------------------------------------------------------------------
# 6. Grabar dataset final en csv -----
#'-----------------------------------------------------------------------------
archivo_OB=format(Sys.time(), "%Y%m%d_%H%M%S_")

fwrite( dataset_fe, #solo los campos para Kaggle
        file= paste0( dir_salidas,"/",archivo_OB,  "part2_ranger_OB", ".csv"),
        sep=  "," )
