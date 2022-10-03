# para correr el Google Cloud
#   8 vCPU
#  64 GB memoria RAM
# 256 GB espacio en disco

# son varios archivos, subirlos INTELIGENTEMENTE a Kaggle

#limpio la memoria
rm( list=ls() )  #remove all objects
gc()             #garbage collection

require("data.table")
require("lightgbm")

require("readODS")

#Aqui empieza el programa
setwd("G:\\My Drive\\Facultad\\Maestria DM\\Expecializacion\\DMEyF\\Carpetas")  #Establezco el Working Directory

dir_salidas="./exp/TP/"
dir.create( dir_salidas )


#'------------------------------------------------------------------------------
# 0. Funciones Auxiliares ----
#'------------------------------------------------------------------------------

crearCheckpoint <- function(path,filename) 
{
  require(rstudioapi)
  file.copy(rstudioapi::getSourceEditorContext()$path,
            to = file.path(path,
                           paste0(filename, "_antes.R")))
  documentSave()
  file.copy(rstudioapi::getSourceEditorContext()$path,
            to = file.path(path,
                           paste0(filename, ".R")))
}



#'------------------------------------------------------------------------------
# 1. Lectura de datos ----
#'------------------------------------------------------------------------------
## Dataset de entrada <----- ----
#archivo_featureEngineer <- "20220929_162111"
#archivo_featureEngineer <- "20221001_185830"
#archivo_featureEngineer <- "20221002_010212"
#archivo_featureEngineer <- "20221002_020526"
# archivo_featureEngineer <- "20221002_023834"
#archivo_featureEngineer <- "20221002_225454"
archivo_featureEngineer <- "20221002_230635"



#defino archivo input
archivo_input <- paste0( dir_salidas,"/",archivo_featureEngineer,"_part1_FeatEng.csv")
#cargo el dataset donde voy a entrenar
dataset  <- fread(archivo_input, stringsAsFactors= TRUE)

#cargo columnas a quitar definidas en el feature engineering
archivo_columnas_a_quitar <- paste0( dir_salidas,"/",archivo_featureEngineer,"_part1_columnas_a_quitar.csv")
columnas_a_quitar  <- c(fread(archivo_columnas_a_quitar ))$columnas_a_quitar

#'------------------------------------------------------------------------------
# 2. ENTRENAMIENTO ----
#'------------------------------------------------------------------------------
## Hyperparametros <-----  ----

#defino los parametros de la corrida, en una lista, la variable global  PARAM
#  muy pronto esto se leera desde un archivo formato .yaml
PARAM <- list()
PARAM$experimento  <- "LightGBM"

#PARAM$input$dataset       <- "./datasets/competencia2_2022.csv.gz"
PARAM$input$dataset       <-  archivo_input

PARAM$input$training      <- c( 202103 )
PARAM$input$future        <- c( 202105 )

# PARAM$finalmodel$max_bin           <-     31
# PARAM$finalmodel$learning_rate     <-      0.0280015981   #0.0142501265
# PARAM$finalmodel$num_iterations    <-    328  #615
# PARAM$finalmodel$num_leaves        <-   1015  #784
# PARAM$finalmodel$min_data_in_leaf  <-   5542  #5628
# PARAM$finalmodel$feature_fraction  <-      0.7832319551  #0.8382482539

#resultado de OB: ganancia mas comun (26600000)
# PARAM$finalmodel$max_bin           <-     31
# PARAM$finalmodel$learning_rate     <-      0.005085075
# PARAM$finalmodel$num_iterations    <-    2042
# PARAM$finalmodel$num_leaves        <-   47
# PARAM$finalmodel$min_data_in_leaf  <-   3108
# PARAM$finalmodel$feature_fraction  <-      0.770306605


#resultado de OB: la maxima ganacia (27360000)
# PARAM$finalmodel$max_bin           <-     31
# PARAM$finalmodel$learning_rate     <-      0.0337574788761941 
# PARAM$finalmodel$num_iterations    <-    352
# PARAM$finalmodel$num_leaves        <-   1012
# PARAM$finalmodel$min_data_in_leaf  <-   1380
# PARAM$finalmodel$feature_fraction  <-      0.245552123089927

#resultado de OB con feature eng (0929): 27570000 kaggle:19.82824 (8500 envios)
# PARAM$finalmodel$max_bin           <-     31
# PARAM$finalmodel$learning_rate     <-      0.0512213178454599
# PARAM$finalmodel$num_iterations    <-    144
# PARAM$finalmodel$num_leaves        <-   345
# PARAM$finalmodel$min_data_in_leaf  <-   2838
# PARAM$finalmodel$feature_fraction  <-      0.209201307170903

#resultado de OB con feature eng (0929): 27380000 kaggle:18.24022 (10500 envios)
# PARAM$finalmodel$max_bin           <-     31
# PARAM$finalmodel$learning_rate     <-      0.00558132405921785
# PARAM$finalmodel$num_iterations    <-    1210
# PARAM$finalmodel$num_leaves        <-   394
# PARAM$finalmodel$min_data_in_leaf  <-   621
# PARAM$finalmodel$feature_fraction  <-      0.200421467179802

#resultado de OB con feature eng (0929): 26910000 kaggle:17.83221 (9500 envios)
# PARAM$finalmodel$max_bin           <-     31
# PARAM$finalmodel$learning_rate     <-      0.00743226892518381
# PARAM$finalmodel$num_iterations    <-    1549
# PARAM$finalmodel$num_leaves        <-   112
# PARAM$finalmodel$min_data_in_leaf  <-   4244
# PARAM$finalmodel$feature_fraction  <-      0.200359186

#resultado de OB con feature eng (0929): 26620000 kaggle:18.84023 (10500 envios)
# PARAM$finalmodel$max_bin           <-     31
# PARAM$finalmodel$learning_rate     <-      0.0498724122610133
# PARAM$finalmodel$num_iterations    <-    176
# PARAM$finalmodel$num_leaves        <-   18
# PARAM$finalmodel$min_data_in_leaf  <-   3123
# PARAM$finalmodel$feature_fraction  <-      0.231419395517015

#resultado de OB con feature eng (0929):  27060000 kaggle:17.51621 (8500 envios)
# PARAM$finalmodel$max_bin           <-     31
# PARAM$finalmodel$learning_rate     <-       0.023843 
# PARAM$finalmodel$num_iterations    <-    589
# PARAM$finalmodel$num_leaves        <-   111
# PARAM$finalmodel$min_data_in_leaf  <-   3
# PARAM$finalmodel$feature_fraction  <-      0.502869432


#resultado de OB con feature eng (0929):  27470000 kaggle:19.13623 (9000 envios)
# PARAM$finalmodel$max_bin           <-     31
# PARAM$finalmodel$learning_rate     <-       0.00603474605109611 
# PARAM$finalmodel$num_iterations    <-    461
# PARAM$finalmodel$num_leaves        <-   752
# PARAM$finalmodel$min_data_in_leaf  <-   596
# PARAM$finalmodel$feature_fraction  <-      0.20111254


#resultado de OB con feature eng (0929):  27790000 kaggle:19.40423 (8500 envios)
# PARAM$finalmodel$max_bin           <-     31
# PARAM$finalmodel$learning_rate     <-       0.00610304458065043
# PARAM$finalmodel$num_iterations    <-    1266  #1266
# PARAM$finalmodel$num_leaves        <-   278
# PARAM$finalmodel$min_data_in_leaf  <-   4
# PARAM$finalmodel$feature_fraction  <-      0.200550442672245

#resultado de OB con feature eng (0929):  27104000 kaggle:19.26423 (8500 envios)
# PARAM$finalmodel$max_bin           <-     31
# PARAM$finalmodel$learning_rate     <-       0.00525615530368289 
# PARAM$finalmodel$num_iterations    <-    2183
# PARAM$finalmodel$num_leaves        <-   81
# PARAM$finalmodel$min_data_in_leaf  <-   96
# PARAM$finalmodel$feature_fraction  <-      0.468217739201665

#resultado de OB con feature eng (0929):  27150000 kaggle:19.60823 (9000 envios)
PARAM$finalmodel$max_bin           <-     31
PARAM$finalmodel$learning_rate     <-       0.00895073371986516
PARAM$finalmodel$num_iterations    <-    1157
PARAM$finalmodel$num_leaves        <-   18
PARAM$finalmodel$min_data_in_leaf  <-   2
PARAM$finalmodel$feature_fraction  <-      0.46710057742027

PARAM$finalmodel$semilla           <- 807299

#'------------------------------------------------------------------------------
#'------------------------------------------------------------------------------



#'--------------------------------------

#paso la clase a binaria que tome valores {0,1}  enteros
#set trabaja con la clase  POS = { BAJA+1, BAJA+2 } 
#esta estrategia es MUY importante
dataset[ , clase01 := ifelse( clase_ternaria %in%  c("BAJA+2","BAJA+1"), 1L, 0L) ]

#'--------------------------------------
## Columnas <---- -----
#los campos que se van a utilizar
campos_buenos  <- setdiff( colnames(dataset),c( "clase_ternaria","clase01","clase_binaria",columnas_a_quitar) )

#'--------------------------------------


#establezco donde entreno
dataset[ , train  := 0L ]
dataset[ foto_mes %in% PARAM$input$training, train  := 1L ]

#'--------------------------------------
#creo las carpetas donde van los resultados
#creo la carpeta donde va el experimento
# HT  representa  Hiperparameter Tuning
#dir.create( "./exp/",  showWarnings = FALSE ) 
#dir.create( paste0("./exp/", PARAM$experimento, "/" ), showWarnings = FALSE )
dir.create( paste0(dir_salidas, PARAM$experimento),  showWarnings = FALSE ) 
#setwd( paste0(dir_salidas, PARAM$experimento, "/" ) )   #Establezco el Working Directory DEL EXPERIMENTO
timestamp=format(Sys.time(), "%Y%m%d_%H%M%S")


#dejo los datos en el formato que necesita LightGBM
dtrain  <- lgb.Dataset( data= data.matrix(  dataset[ train==1L, campos_buenos, with=FALSE]),
                        label= dataset[ train==1L, clase01] )

#genero el modelo
#estos hiperparametros  salieron de una laaarga Optmizacion Bayesiana
modelo  <- lgb.train( data= dtrain,
                      param= list( objective=          "binary",
                                   max_bin=            PARAM$finalmodel$max_bin,
                                   learning_rate=      PARAM$finalmodel$learning_rate,
                                   num_iterations=     PARAM$finalmodel$num_iterations,
                                   num_leaves=         PARAM$finalmodel$num_leaves,
                                   min_data_in_leaf=   PARAM$finalmodel$min_data_in_leaf,
                                   feature_fraction=   PARAM$finalmodel$feature_fraction,
                                   seed=               PARAM$finalmodel$semilla,
                                   feature_pre_filter= "True" #AGREGUE 0929
                                  )
                    )

#'--------------------------------------
#ahora imprimo la importancia de variables
tb_importancia  <-  as.data.table( lgb.importance(modelo) ) 
archivo_importancia  <- paste0(dir_salidas, PARAM$experimento, "/" , timestamp ,"_impo.txt" )

fwrite( tb_importancia, 
        file= archivo_importancia, 
        sep= "\t" )

#'------------------------------------------------------------------------------
# 6. Predicción para Kaggle ----
#'------------------------------------------------------------------------------

#aplico el modelo a los datos sin clase
dapply  <- dataset[ foto_mes== PARAM$input$future ]

#aplico el modelo a los datos nuevos
prediccion  <- predict( modelo, 
                        data.matrix( dapply[, campos_buenos, with=FALSE ])                                 )

#genero la tabla de entrega
tb_entrega  <-  dapply[ , list( numero_de_cliente, foto_mes ) ]
tb_entrega[  , prob := prediccion ]

#grabo las probabilidad del modelo
#fwrite( tb_entrega,
#        file= paste0(dir_salidas, PARAM$experimento, "/" , timestamp ,"_prediccion.txt" ),
#        sep= "\t" )

#ordeno por probabilidad descendente
setorder( tb_entrega, -prob )


#genero archivos con los  "envios" mejores
#deben subirse "inteligentemente" a Kaggle para no malgastar submits
cortes <- seq( 7000, 10500, by=500 )
for( envios  in  cortes )
{
  tb_entrega[  , Predicted := 0L ]
  tb_entrega[ 1:envios, Predicted := 1L ]

  fwrite( tb_entrega[ , list(numero_de_cliente, Predicted)], 
          file= paste0(dir_salidas, PARAM$experimento, "/" , timestamp ,"_", PARAM$experimento, "_", envios, ".csv" ),
          sep= "," )
}

#'--------------------------------------

#checkpoint
crearCheckpoint(paste0(dir_salidas, PARAM$experimento), timestamp)

# quit( save= "no" )

# fwrite( list(campos_buenos), 
#         file= paste0(dir_salidas, PARAM$experimento, "/" , timestamp ,"_", PARAM$experimento, "_", envios, "_campos.csv" ),
#         sep= "," )