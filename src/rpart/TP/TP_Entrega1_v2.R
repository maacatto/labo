#'------------------------------------------------------------------------------
# INICIO ----
#'------------------------------------------------------------------------------

#Arbol elemental con libreria  rpart
#Debe tener instaladas las librerias  data.table  ,  rpart  y  rpart.plot

#cargo las librerias que necesito
require("data.table")
require("rpart")
require("rpart.plot")


seeds <- c( 807299, 962041, 705689, 909463, 637597 )

# ---- 1. Lectura de datos ----
#Aqui se debe poner la carpeta de la materia de SU computadora local
setwd("G:\\My Drive\\Facultad\\Maestria DM\\Expecializacion\\DMEyF\\Carpetas")  #Establezco el Working Directory

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
#'------------------------------------------------------------------------------




#'------------------------------------------------------------------------------
# 3. FEATURE ENGINEERING ----
#'-----------------------------------------------------------------------------

dataset_fe <- fread("./datasets/competencia1_2022.csv")

## Agrega columnas con expresiones logicas surgidas de arbol pequeño (BAJA+2)  ----
# ctrx_quarter < 14 & mcuentas_saldo < −1256.1 & cprestamos_personales >=2 & Visa_status <8 & mcuenta_corriente < −1.1319e+6
# ctrx_quarter < 14 & mcuentas_saldo >= −1256.1 & mcaja_ahorro < 2601.1 & Visa_fechaalta >= 4539 & mcaja_ahorro >= 2544.3
dataset_fe$expresion1 <- ifelse(dataset_fe$ctrx_quarter < 14 & dataset_fe$mcuentas_saldo < -1256.1 & dataset_fe$cprestamos_personales >= 2 & dataset_fe$Visa_status < 8 & dataset_fe$mcuenta_corriente < -1131900.0 , 1, 0)
dataset_fe[,sum(expresion1)]
## Data Drifting  ----
#-mcomisiones_mantenimiento -Visa_mpagado?



## Clase Binaria  ----
#creo la clase_binaria SI={ BAJA+1, BAJA+2 }    NO={ CONTINUA }
dataset[ foto_mes==202101, 
         clase_binaria :=  ifelse( clase_ternaria=="CONTINUA", "NO", "SI" ) ]







#'------------------------------------------------------------------------------
# 4. Entrenamiento ----
#'------------------------------------------------------------------------------
dtrain  <- dataset_fe[ foto_mes==202101 ]  #defino donde voy a entrenar
dapply  <- dataset_fe[ foto_mes==202103 ]  #defino donde voy a aplicar el modelo
 
#genero el modelo,  aqui se construye el arbol
modelo  <- rpart(formula=   "clase_binaria ~ . -clase_ternaria",
                 data=      dtrain,  #los datos donde voy a entrenar
                 xval=      0,
                 cp=        -0.076525636,   #esto significa no limitar la complejidad de los splits
                 minsplit=  113,     #minima cantidad de registros para que se haga el split
                 minbucket= 53,     #tamaño minimo de una hoja
                 maxdepth=  12 )    #profundidad maxima del arbol

modelo$variable.importance

#grafico el arbol
prp(modelo, extra=101, digits=5, branch=1, type=4, varlen=0, faclen=0)



#'------------------------------------------------------------------------------
# ---- 5. Predicción para Kaggle ----
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


dir.create( "./exp/" )
dir.create( "./exp/KA4120" )


for( corte  in  c( 7500, 8000, 8500, 9000, 9500, 10000, 10500, 11000 ) )
{
  #le envio a los  corte  mejores,  de mayor probabilidad de prob_SI
  dfinal[ , Predicted := 0L ]
  dfinal[ 1:corte , Predicted := 1L ]
  
  
  fwrite( dfinal[ , list(numero_de_cliente, Predicted) ], #solo los campos para Kaggle
          file= paste0( "./exp/KA4120/KA4120_005_",  corte, ".csv"),
          sep=  "," )
}