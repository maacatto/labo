#Arbol elemental con libreria  rpart
#Debe tener instaladas las librerias  data.table  ,  rpart  y  rpart.plot

#cargo las librerias que necesito
require("data.table")
require("rpart")
require("rpart.plot")

#Aqui se debe poner la carpeta de la materia de SU computadora local
setwd("G:\\My Drive\\Facultad\\Maestria DM\\Expecializacion\\DMEyF\\Carpetas")  #Establezco el Working Directory

#cargo el dataset
dataset  <- fread("./datasets/competencia1_2022.csv")


#AGREGADO 0901
#creo la clase_binaria SI={ BAJA+1, BAJA+2 }    NO={ CONTINUA }
dataset[ foto_mes==202101, 
         clase_binaria :=  ifelse( clase_ternaria=="CONTINUA", "NO", "SI" ) ]
dataset[ foto_mes==202101, 
         clase_mes :=  "enero" ]
dataset[ foto_mes==202103, 
         clase_mes :=  "marzo" ]


dtrain  <- dataset #defino donde voy a entrenar
dapply  <- dataset[ foto_mes==202103 ]  #defino donde voy a aplicar el modelo

#genero el modelo,  aqui se construye el arbol
modelo  <- rpart(formula=   "clase_mes ~ .  -clase_ternaria -clase_binaria -foto_mes",  #quiero predecir clase_ternaria a partir de el resto de las variables
                 data=      dataset,  #los datos donde voy a entrenar
                 xval=      0,
                 cp=        -0.076525636,   #esto significa no limitar la complejidad de los splits
                 minsplit=  113,     #minima cantidad de registros para que se haga el split
                 minbucket= 53,     #tamaño minimo de una hoja
                 maxdepth=  12 )    #profundidad maxima del arbol

install.packages("rattle")
library(rattle)
fancyRpartPlot(modelo)
#grafico el arbol
prp(modelo, extra=101, digits=5, branch=.3, type=3, varlen=0, faclen=0,compress=FALSE,ycompress=FALSE,tweak=.8)

modelo$variable.importance


# Conclusion:
# mas importante: 