#!/usr/bin/env bash
export LC_ALL=es_MX.UTF-8
export LANG=es_MX.UTF-8

# Casos
Rscript casos/inicio_sintomas_por_fecha.r
if [ $? -ne 0 ]; then
    echo "Error"
    exit 1
fi

# Clinicos
Rscript clinicos/clinicos_db_completa.r
if [ $? -ne 0 ]; then
    echo "Error"
    exit 1
fi

# Vacunas
Rscript vacunas/mortalidad_edad.r
if [ $? -ne 0 ]; then
    echo "Error"
    exit 1
fi


