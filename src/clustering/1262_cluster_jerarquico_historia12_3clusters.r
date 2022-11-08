#este script necesita para correr en Google Cloud
# RAM     16 GB
# vCPU     4
# disco  256 GB


#cluster jerárquico  utilizando "la distancia de Random Forest"
#adios a las fantasias de k-means y las distancias métricas, cuanto tiempo perdido ...
#corre muy lento porque la libreria RandomForest es del Jurasico y no es multithreading

#limpio la memoria
rm( list=ls() )  #remove all objects
gc()             #garbage collection

require("data.table")
require("randomForest")
require("ranger")

#Parametros del script
PARAM <- list()
PARAM$experimento  <- "CLU1262"
# FIN Parametros del script

set.seed("42")

#------------------------------------------------------------------------------
#------------------------------------------------------------------------------

#setwd("~/buckets/b1/")
setwd("G:\\My Drive\\Facultad\\Maestria DM\\Expecializacion\\DMEyF\\Carpetas")  #Establezco el Working Directory


#leo el dataset original
# pero podria leer cualquiera que tenga Feature Engineering
dataset <- fread( "c:/users/user/downloads/competencia3_2022.csv.gz")

#me quedo SOLO con los BAJA+2
dataset  <- dataset[ foto_mes>=202006  & foto_mes<=202105, ] 

#creo la carpeta donde va el experimento
dir.create( paste0( "./exp/", PARAM$experimento, "/"), showWarnings = FALSE )
setwd(paste0( "./exp/", PARAM$experimento, "/"))   #Establezco el Working Directory DEL EXPERIMENTO

#me quedo SOLO con los BAJA+2
dataset_b2  <- dataset[  clase_ternaria =="BAJA+2"  & foto_mes>=202006  & foto_mes<=202105, ] 

#armo el dataset de los 12 meses antes de la muerte de los registros que analizo
dataset12  <- copy( dataset[  numero_de_cliente %in%  dataset_b2[ , unique(numero_de_cliente)]  ]  )

#asigno para cada registro cuantos meses faltan para morir
setorderv( dataset12, c("numero_de_cliente", "foto_mes"), c(1,-1) )
dataset12[  , pos := seq(.N) , numero_de_cliente ]

#me quedo solo con los 12 meses antes de morir
dataset12  <- dataset12[  pos <= 12 , ]
gc()


col_buenas  <- setdiff(colnames(dataset) ,c("clase_ternaria") )

#quito los nulos para que se pueda ejecutar randomForest,  Dios que algoritmo prehistorico ...
dataset_b2  <- na.roughfix( dataset_b2[,..col_buenas] )


#los campos que arbitrariamente decido considerar para el clustering
#por supuesto, se pueden cambiar
campos_buenos  <- c( "ctrx_quarter", "cpayroll_trx", "mcaja_ahorro", "mtarjeta_visa_consumo", "ctarjeta_visa_transacciones",
                     "mcuentas_saldo", "mrentabilidad_annual", "mprestamos_personales", "mactivos_margen", "mpayroll",
                     "Visa_mpagominimo", "Master_fechaalta", "cliente_edad", "chomebanking_transacciones", "Visa_msaldopesos",
                     "Visa_Fvencimiento", "mrentabilidad", "Visa_msaldototal", "Master_Fvencimiento", "mcuenta_corriente",
                     "Visa_mpagospesos", "Visa_fechaalta", "mcomisiones_mantenimiento", "Visa_mfinanciacion_limite",
                     "mtransferencias_recibidas", "cliente_antiguedad", "Visa_mconsumospesos", "Master_mfinanciacion_limite",
                     "mcaja_ahorro_dolares", "cproductos", "mcomisiones_otras", "thomebanking", "mcuenta_debitos_automaticos",
                     "mcomisiones", "Visa_cconsumos", "ccomisiones_otras", "Master_status", "mtransferencias_emitidas",
                     "mpagomiscuentas")



#Ahora, a esperar mucho con este algoritmo del pasado que NO correr en paralelo, patetico
modelo  <- randomForest( x= dataset_b2[  , campos_buenos, with=FALSE ], 
                         y= NULL, 
                         ntree= 1000, #se puede aumentar a 10000
                         proximity= TRUE, 
                         oob.prox=  TRUE )


#genero los clusters jerarquicos
hclust.rf  <- hclust( as.dist ( 1.0 - modelo$proximity),  #distancia = 1.0 - proximidad
                      method= "ward.D2" )



#imprimo un pdf con la forma del cluster jerarquico
pdf( "cluster_jerarquico.pdf" )
plot( hclust.rf )


#genero 7 clusters
h <- 20
distintos <- 0

#while(  h>0  &  !( distintos >=6 & distintos <=7 ) )
while(  h>0  &  !( distintos >=3 & distintos <=4 ) )
{
  h <- h - 1 
  rf.cluster  <- cutree( hclust.rf, h)
  
  dataset_b2[  , cluster2 := NULL ]
  dataset_b2[  , cluster2 := rf.cluster ]

  distintos  <- nrow( dataset_b2[  , .N,  cluster2 ] )
  cat( distintos, " " )
  
  plot( rf.cluster )
}

dev.off()


#en  dataset,  la columna  cluster2  tiene el numero de cluster
#sacar estadicas por cluster

dataset_b2[  , .N,  cluster2 ]  #tamaño de los clusters

#grabo el dataset en el bucket, luego debe bajarse a la PC y analizarse
fwrite( dataset_b2,
        file= "cluster_de_bajas_3clusters.txt",
        sep= "\t" )


#ahora a mano veo los centroides de los 7 clusters
#esto hay que hacerlo para cada variable,
#  y ver cuales son las que mas diferencian a los clusters
#esta parte conviene hacerla desde la PC local, sobre  cluster_de_bajas.txt

dataset_b2[  , mean(ctrx_quarter),  cluster2 ]  #media de la variable  ctrx_quarter
dataset_b2[  , mean(mtarjeta_visa_consumo),  cluster2 ]
dataset_b2[  , mean(mcuentas_saldo),  cluster2 ]
dataset_b2[  , mean(chomebanking_transacciones),  cluster2 ]

#---------------------------------------
library("ggpubr")
library("ggplot2")                     
library("GGally")
#Boxplot
pdf( "boxplot_columnas.pdf" )
for (i in campos_buenos){
  fig1 <- ggplot(dataset_b2, aes(x = cluster2, group = cluster2, y=get(i),  color = cluster2)) +
    geom_boxplot() +
    theme_bw() +
    coord_cartesian(ylim = c( (dataset_b2[  , quantile(get(i),0.25)] - 2 * dataset_b2[  , IQR(get(i))]), dataset_b2[  , quantile(get(i),0.75)] + 2 * dataset_b2[  , IQR(get(i))] )) +
    ggtitle(i)
  print(fig1)
}
dev.off()


#Grabar lista de medias a archivo
all <- list()
for (i in campos_buenos) {
  all <- rbind(all, list(i, t(dataset_b2[  , mean(get(i)),  cluster2 ][,2]) ,t(dataset_b2[  , mean(get(i))])  ) )
}
fwrite( data.table(all), 
        file= "cluster_summary.txt",
        sep= "," )

#quito los nulos para que se pueda ejecutar randomForest,  Dios que algoritmo prehistorico ...
dataset  <- na.roughfix( dataset[,..col_buenas] )
#Grabar lista de medias a archivo
all <- list()
for (i in campos_buenos) {
  all <- rbind(all, list(i, t(dataset[  , mean(get(i)) ]) ,t(dataset[  , mean(get(i))])  ) )
}
fwrite( data.table(all), 
        file= "dataset_summary.txt",
        sep= "," )

#---------------------------------------


#Finalmente grabo el archivo para  Juan Pablo Cadaveira
#agrego a dataset12 el cluster2  y lo grabo

dataset12[ dataset_b2,
           on= "numero_de_cliente",
           cluster2 := i.cluster2 ]

fwrite( dataset12, 
        file= "cluster_de_bajas_12meses.txt",
        sep= "\t" )
