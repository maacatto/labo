#Arbol elemental con libreria  rpart
#Debe tener instaladas las librerias  data.table  ,  rpart  y  rpart.plot

#cargo las librerias que necesito
require("data.table")
require("rpart")
require("rpart.plot")

#Aqui se debe poner la carpeta de la materia de SU computadora local
setwd("G:\\My Drive\\Facultad\\Maestria DM\\Expecializacion\\DMEyF\\Carpetas")  #Establezco el Working Directory

#cargo el dataset
#dataset  <- fread("./datasets/competencia1_2022.csv")

#--------------------------------------------------------------------------------
dataset_fe <- fread("./datasets/competencia1_2022.csv")
#### FEATURE ENGINEERING
###### Agrega columnas con expresiones logicas surgidas de arbol pequeño (BAJA+2)
# ctrx_quarter < 14 & mcuentas_saldo < −1256.1 & cprestamos_personales >=2 & Visa_status <8 & mcuenta_corriente < −1.1319e+6
# ctrx_quarter < 14 & mcuentas_saldo >= −1256.1 & mcaja_ahorro < 2601.1 & Visa_fechaalta >= 4539 & mcaja_ahorro >= 2544.3
dataset_fe$expresion1 <- ifelse(dataset_fe$ctrx_quarter < 14 & dataset_fe$mcuentas_saldo < -1256.1 & dataset_fe$cprestamos_personales >= 2 & dataset_fe$Visa_status < 8 & dataset_fe$mcuenta_corriente < -1131900.0 , 1, 0)
dataset_fe[,sum(expresion1)]
####### Data Drifting
#-mcomisiones_mantenimiento -Visa_mpagado?



###### Clase Binaria
#creo la clase_binaria SI={ BAJA+1, BAJA+2 }    NO={ CONTINUA }
dataset[ foto_mes==202101, 
         clase_binaria :=  ifelse( clase_ternaria=="CONTINUA", "NO", "SI" ) ]
#--------------------------------------------------------------------------------
dtrain  <- dataset_fe[ foto_mes==202101 ]  #defino donde voy a entrenar
dapply  <- dataset_fe[ foto_mes==202103 ]  #defino donde voy a aplicar el modelo

#genero el modelo,  aqui se construye el arbol
modelo  <- rpart(formula=   "clase_ternaria ~ .",  #quiero predecir clase_ternaria a partir de el resto de las variables
                 data=      dtrain,  #los datos donde voy a entrenar
                 xval=      0,
                 cp=        -0.076525636,   #esto significa no limitar la complejidad de los splits
                 minsplit=  113,     #minima cantidad de registros para que se haga el split
                 minbucket= 53,     #tamaño minimo de una hoja
                 maxdepth=  12 )    #profundidad maxima del arbol

 modelo$variable.importance

#grafico el arbol
prp(modelo, extra=101, digits=5, branch=1, type=4, varlen=0, faclen=0)


#aplico el modelo a los datos nuevos
prediccion  <- predict( object= modelo,
                        newdata= dapply,
                        type = "prob")

#prediccion es una matriz con TRES columnas, llamadas "BAJA+1", "BAJA+2"  y "CONTINUA"
#cada columna es el vector de probabilidades 

#agrego a dapply una columna nueva que es la probabilidad de BAJA+2
dapply[ , prob_baja2 := prediccion[, "BAJA+2"] ]

#solo le envio estimulo a los registros con probabilidad de BAJA+2 mayor  a  1/40
dapply[ , Predicted := as.numeric( prob_baja2 > 1/40 ) ]

#genero el archivo para Kaggle
#primero creo la carpeta donde va el experimento
dir.create( "./exp/" )
dir.create( "./exp/KAz241" )

fwrite( dapply[ , list(numero_de_cliente, Predicted) ], #solo los campos para Kaggle
        file= "./exp/KAz241/K101_001.csv",
        sep=  "," )
