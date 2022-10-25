tabulador="	"
logfile="./run_multiple_log.txt"

bitacora () {
  local fecha=$(date +"%Y%m%d %H%M%S")

  echo "$fecha""$tabulador""$1"  >>  "$logfile"
}

bitacora "Start"
bitacora "925_FE_historia_cloud.r --------------------------------------- "
Rscript ~/labo/src/completencias_AcattoM/dm-eyf-2022-tercera/925_FE_historia_cloud.r >>  "$logfile"


bitacora "932_training_strategy_under.r --------------------------------------- "
Rscript ~/labo/src/completencias_AcattoM/dm-eyf-2022-tercera/932_training_strategy_under.r >>  "$logfile"


bitacora "942_HT_lightgbm_under.r --------------------------------------- "
Rscript ~/labo/src/completencias_AcattoM/dm-eyf-2022-tercera/942_HT_lightgbm_under.r >>  "$logfile"



bitacora "992_ZZ_lightgbm_under.r --------------------------------------- "
Rscript ~/labo/src/completencias_AcattoM/dm-eyf-2022-tercera/992_ZZ_lightgbm_under.r >>  "$logfile"


