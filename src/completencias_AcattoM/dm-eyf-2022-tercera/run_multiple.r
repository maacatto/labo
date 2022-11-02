dir_salidas="~/buckets/b1/exp/TP/"
dir.create( dir_salidas )

crearCheckpoint2 <- function(filesource, path,filename_target) 
{
  require(rstudioapi)
  documentSave()
  file.copy(filesource,
            to = file.path(path,
                           paste0(filename_target, ".R")))
}

ejecutarScript <- function(script,filename_target)
{
  #'-------------------------------
  #script = "~/labo/src/completencias_AcattoM/dm-eyf-2022-tercera/915N_FeatEng_cloud.R"
  
  source(script,echo=TRUE)
  
  #checkpoint
  crearCheckpoint2(script, dir_salidas,paste0(format(Sys.time(), "%Y%m%d_%H%M%S"),filename_target))
  #'-------------------------------
  
}


#ejecutarScript("~/labo/src/completencias_AcattoM/dm-eyf-2022-tercera/915N_FeatEng_cloud.R","FeatEng_cloud")

ejecutarScript("~/labo/src/completencias_AcattoM/dm-eyf-2022-tercera/925_FE_historia_cloud.r","FE_historia_cloud")

ejecutarScript("~/labo/src/completencias_AcattoM/dm-eyf-2022-tercera/932_training_strategy_under.r","training_strategy_under")

ejecutarScript("~/labo/src/completencias_AcattoM/dm-eyf-2022-tercera/942_HT_lightgbm_under.r","HT_lightgbm_under")
crearCheckpoint2("~/labo/src/completencias_AcattoM/dm-eyf-2022-tercera/942_HT_lightgbm_under.r", dir_salidas,paste0(format(Sys.time(), "%Y%m%d_%H%M%S"),"HT_lightgbm_under"))

ejecutarScript("~/labo/src/completencias_AcattoM/dm-eyf-2022-tercera/992_ZZ_lightgbm_under.r","ZZ_lightgbm_under")