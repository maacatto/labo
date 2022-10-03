#limpio la memoria
rm( list=ls() )  #remove all objects
gc()             #garbage collection

require("data.table")
require("rpart")

path <- "labo/src/completencias_AcattoM/dm-eyf-2022-segunda"
#------------------------------------------------------------------------------

graficar_campo  <- function( campo, campo_clase, valores_clase )
{

  #quito de grafico las colas del 5% de las densidades
  qA  <- quantile(  dataset[ foto_mes==202103 , get(campo) ] , prob= c(0.05, 0.95), na.rm=TRUE )
  qB  <- quantile(  dataset[ foto_mes==202105 , get(campo) ] , prob= c(0.05, 0.95), na.rm=TRUE )

  xxmin  <- pmin( qA[[1]], qB[[1]] )
  xxmax  <- pmax( qA[[2]], qB[[2]] )

  densidad_A  <- density( dataset[ foto_mes==202103 & get(campo_clase) %in% valores_clase, get(campo) ],
                          kernel="gaussian", na.rm=TRUE )

  densidad_B  <- density( dataset[ foto_mes==202105 & get(campo_clase) %in% valores_clase, get(campo) ],
                          kernel="gaussian", na.rm=TRUE )

  plot( densidad_A,
        col="blue",
        xlim= c( xxmin, xxmax ),
        ylim= c( 0, pmax( max(densidad_A$y), max(densidad_B$y) ) ),
        main= paste0( campo, ",   ", campo_clase, " in ",  paste( valores_clase,collapse=",")) 
      )

  lines(densidad_B, col="red", lty=2)
  
  legend(  "topright",  
           legend=c("202003", "202005"),
           col=c("blue", "red"), lty=c(1,2))

}
#------------------------------------------------------------------------------
#Aqui comienza el programa
setwd("G:\\My Drive\\Facultad\\Maestria DM\\Expecializacion\\DMEyF\\Carpetas")  #Establezco el Working Directory

#cargo el dataset donde voy a entrenar
dataset  <- fread("./datasets/competencia2_2022.csv.gz")

dataset  <- dataset[  foto_mes %in% c( 202103, 202105 ) ]


#---Feature Engineer--------------------------------------
library(tidyverse)
descr <- dataset[ foto_mes == 202103] %>%   summarise(Minimo = min(mcuentas_saldo), Q1 = quantile(mcuentas_saldo, probs = 0.25), Mediana = quantile(mcuentas_saldo, probs = 0.5), 
            Promedio = round(mean(mcuentas_saldo),0), Q3 = quantile(mcuentas_saldo, probs = 0.75),  Maximo = max(mcuentas_saldo), Desvio = round(sd(mcuentas_saldo),0))

dataset <- dataset[ foto_mes == 202103 & mcuentas_saldo < descr$Q1 , mcuentas_saldo_qntl := 1]
dataset <- dataset[ foto_mes == 202103 & mcuentas_saldo >= descr$Q1 & mcuentas_saldo < descr$Mediana  , mcuentas_saldo_qntl := 2]
dataset <- dataset[ foto_mes == 202103 & mcuentas_saldo >= descr$Mediana & mcuentas_saldo < descr$Q3  , mcuentas_saldo_qntl := 3]
dataset <- dataset[ foto_mes == 202103 & mcuentas_saldo >= descr$Q3 , mcuentas_saldo_qntl := 4]
dataset <- dataset[ foto_mes == 202103, mcuentas_saldo_scl := mcuentas_saldo - mean(mcuentas_saldo)]

descr <- dataset[ foto_mes == 202105] %>%   summarise(Minimo = min(mcuentas_saldo), Q1 = quantile(mcuentas_saldo, probs = 0.25), Mediana = quantile(mcuentas_saldo, probs = 0.5), 
                                                      Promedio = round(mean(mcuentas_saldo),0), Q3 = quantile(mcuentas_saldo, probs = 0.75),  Maximo = max(mcuentas_saldo), Desvio = round(sd(mcuentas_saldo),0))
descr
dataset <- dataset[ foto_mes == 202105 & mcuentas_saldo < descr$Q1 , mcuentas_saldo_qntl := 1]
dataset <- dataset[ foto_mes == 202105 & mcuentas_saldo >= descr$Q1 & mcuentas_saldo < descr$Mediana  , mcuentas_saldo_qntl := 2]
dataset <- dataset[ foto_mes == 202105 & mcuentas_saldo >= descr$Mediana & mcuentas_saldo < descr$Q3  , mcuentas_saldo_qntl := 3]
dataset <- dataset[ foto_mes == 202105 & mcuentas_saldo >= descr$Q3 , mcuentas_saldo_qntl := 4]
dataset <- dataset[ foto_mes == 202105, mcuentas_saldo_scl := mcuentas_saldo - mean(mcuentas_saldo)]


#frank

columnas_frank <- c("mcuentas_saldo")
for (campo in columnas_frank)
{
  nrows <- length(dataset[ foto_mes == 202103,get(campo)])
  dataset <- dataset[ foto_mes == 202103, paste0("n_", campo,"_rnk") := (frank(get(campo)) -1) / (nrows-1)]
  dataset <- dataset[ foto_mes == 202105, paste0("n_", campo,"_rnk") := (frank(get(campo)) -1) / (nrows-1)]
  
  nrows <- length(dataset[ foto_mes == 202103 & get(campo) >=0,get(campo)])
  dataset <- dataset[ foto_mes == 202103 & get(campo) >=0, paste0("n_", campo,"_rnk_gz") := (frank(get(campo)) -1) / (nrows-1)]
  nrows <- length(dataset[ foto_mes == 202103 & get(campo) <0,get(campo)])
  dataset <- dataset[ foto_mes == 202103 & get(campo) <0, paste0("n_", campo,"_rnk_lz") := (frank(-get(campo)) -1) / (nrows-1)]

  nrows <- length(dataset[ foto_mes == 202105 & get(campo) >=0,get(campo)])
  dataset <- dataset[ foto_mes == 202105 & get(campo) >=0, paste0("n_", campo,"_rnk_gz") := (frank(get(campo)) -1) / (nrows-1)]
  nrows <- length(dataset[ foto_mes == 202105 & get(campo) <0,get(campo)])
  dataset <- dataset[ foto_mes == 202105 & get(campo) <0, paste0("n_", campo,"_rnk_lz") := (frank(-get(campo)) -1) / (nrows-1)]
  
  
  dataset <- dataset[ foto_mes == 202103 , paste0("n_", campo,"_rnk") :=  rowSums( cbind( get(paste0("n_", campo,"_rnk_gz")),   - get(paste0("n_", campo,"_rnk_lz"))) , na.rm=TRUE )  ]
  dataset <- dataset[ foto_mes == 202105 , paste0("n_", campo,"_rnk") :=  rowSums( cbind( get(paste0("n_", campo,"_rnk_gz")),   - get(paste0("n_", campo,"_rnk_lz"))) , na.rm=TRUE )  ]
  
  
}
# dataset <- dataset[ foto_mes == 202103 & mcuentas_saldo >=0 , mcuentas_saldo_rnk_gz := (frank(mcuentas_saldo) -1) / (nrows-1)]
# dataset <- dataset[ foto_mes == 202103 & mcuentas_saldo <0 , mcuentas_saldo_rnk_lz := (frank(mcuentas_saldo) -1) / (nrows-1)]
  # dataset <- dataset[ foto_mes == 202103 & mcuentas_saldo >=0 , mcuentas_saldo_log_rnk_gz := (frank(log(mcuentas_saldo)) -1) / (nrows-1)]
  # dataset <- dataset[ foto_mes == 202103 & mcuentas_saldo <0 , mcuentas_saldo_log_rnk_lz := (frank(log(-mcuentas_saldo)) -1) / (nrows-1)]
  # dataset <- dataset[ foto_mes == 202103 & mcuentas_saldo >=0 , mcuentas_saldo_log_gz := log(mcuentas_saldo+0.1)]
  # dataset <- dataset[ foto_mes == 202103 & mcuentas_saldo <0 , mcuentas_saldo_log_lz := log(-mcuentas_saldo+0.1)]

# dataset <- dataset[ foto_mes == 202105, mcuentas_saldo_rnk := frank(mcuentas_saldo_qntl)]
# dataset <- dataset[ foto_mes == 202105 & mcuentas_saldo >=0 , mcuentas_saldo_rnk_gz := frank(mcuentas_saldo_qntl)]
# dataset <- dataset[ foto_mes == 202105 & mcuentas_saldo <0 , mcuentas_saldo_rnk_lz := frank(mcuentas_saldo_qntl)]

dataset <- dataset[ foto_mes == 202103, mprestamos_personales_rnk := frank(mprestamos_personales)]
dataset <- dataset[ foto_mes == 202103, mprestamos_personales_scl := scale(mprestamos_personales)]
dataset <- dataset[ foto_mes == 202105, mprestamos_personales_rnk := frank(mprestamos_personales)]
dataset <- dataset[ foto_mes == 202105, mprestamos_personales_scl := scale(mprestamos_personales)]

dataset <- dataset[ foto_mes == 202103, mcomisiones_scl := scale(mcomisiones)]
dataset <- dataset[ foto_mes == 202105, mcomisiones_scl := scale(mcomisiones)]

dataset <- dataset[ foto_mes == 202103, mtarjeta_visa_consumo_scl := scale(mtarjeta_visa_consumo)]
dataset <- dataset[ foto_mes == 202105, mtarjeta_visa_consumo_scl := scale(mtarjeta_visa_consumo)]

dataset <- dataset[ foto_mes == 202103, mcuentas_saldo_por_com := mcuentas_saldo / mcomisiones]
dataset <- dataset[ foto_mes == 202105, mcuentas_saldo_por_com := mcuentas_saldo / mcomisiones]

dataset <- dataset[ foto_mes==202103, mcuentas_saldo_infla := mcuentas_saldo]
dataset <- dataset[ foto_mes==202105, mcuentas_saldo_infla := mcuentas_saldo / 1.033 / 1.047 ]

#------------------------------------------



#creo la clase_binaria SI={ BAJA+1, BAJA+2 }    NO={ CONTINUA }
dataset[ foto_mes==202103, 
         clase_binaria :=  ifelse( clase_ternaria=="CONTINUA", "NO", "SI" ) ]
#completo clase_ternaria para ver el grafico de data drifting
dataset[ foto_mes==202105, 
         clase_ternaria :=  "CONTINUA"]

# Entreno el modelo
# utilizo los mejores hiperparametros encontrados en una Bayesian Optimizationcon 5-fold Cross Validation
modelo  <- rpart(formula=   "clase_binaria ~ . -clase_ternaria -mcuentas_saldo -mprestamos_personales",
                 data=      dataset[ foto_mes==202103 ],  #los datos donde voy a entrenar
                 xval=         0,
                  cp=          -0.54,#  -0.89
                 minsplit=  400,   # 621
                 minbucket=  200,   # 309
                 maxdepth=     20 )  #  12


campos_modelo  <- names( modelo$variable.importance )
campos_buenos  <- c( campos_modelo,  setdiff( colnames(dataset), campos_modelo ) )
campos_buenos  <-  setdiff(  campos_buenos,  c( "foto_mes","clase_ternaria","clase_binaria" ) )


dir.create( "./labo/src/completencias_AcattoM/dm-eyf-2022-segunda/",  showWarnings = FALSE ) 
dir.create( "./labo/src/completencias_AcattoM/dm-eyf-2022-segunda/datadrifting/", showWarnings = FALSE )
setwd("./labo/src/completencias_AcattoM/dm-eyf-2022-segunda/datadrifting/")



pdf("densidades_01_03.pdf")

for( campo in  campos_buenos)
{
  cat( campo, "  " )
  
  graficar_campo( campo, "clase_ternaria", c( "BAJA+1", "BAJA+2", "CONTINUA" ) )
#  graficar_campo( campo, "clase_ternaria", c( "BAJA+1", "BAJA+2" ) )
#  graficar_campo( campo, "clase_ternaria", c( "BAJA+2" ) )
#  graficar_campo( campo, "clase_ternaria", c( "BAJA+1" ) )
#  graficar_campo( campo, "clase_ternaria", c( "CONTINUA" ) )
}

dev.off()

campo

#Grabar columnas en archivo
columnas_buenas <- c("foto_mes"	, "mcuentas_saldo", 	"n_mcuentas_saldo_rnk",	"n_mcuentas_saldo_rnk_gz",	"n_mcuentas_saldo_rnk_lz")

fwrite( dataset[, ..columnas_buenas], #solo los campos para Kaggle
        file= paste0( "dataset.csv"),
        sep=  "," )


#Hist
hist(log2(dataset[foto_mes==202103 & mcuentas_saldo < 0 ,- mcuentas_saldo]))
hist(log2(dataset[foto_mes==202103  ,n_mcuentas_saldo_rnk]))
     
     