#!/usr/bin/env bash

# Descargar la capa del IDECanarias
if [[ ! -f shp/municipios.zip ]];then

  wget -P shp/ \
    "https://opendata.sitcan.es/upload/unidades-administrativas/gobcan_unidades-administrativas_municipios.zip" \
    -O shp/municipios.zip
  "> Descarga la capa de los municipio de GRAPHCAN exitosa"
  unzip shp/municipios.zip -d shp/

elif [[ -f shp/municipios.zip ]];then

  "> El archivo zip de municipios ya está descargado"

else

  echo "ERROR"

fi