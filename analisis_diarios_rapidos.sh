#!/usr/bin/env bash

# Casos
Rscript casos/inicio_sintomas_por_fecha.r
if [ $? -ne 0 ]; then
    echo "Error"
    exit 1
fi

# Clinicos
Rscript clinicos/riesgos_relativos.r
if [ $? -ne 0 ]; then
    echo "Error"
    exit 1
fi
Rscript clinicos/tiempo_sintomas_muerte.r
if [ $? -ne 0 ]; then
    echo "Error"
    exit 1
fi
Rscript clinicos/tiempos_deteccion.r
if [ $? -ne 0 ]; then
    echo "Error"
    exit 1
fi

Rscript estados/estados_casos100mil.r
if [ $? -ne 0 ]; then
    echo "Error"
    exit 1
fi
Rscript estados/estados_muertes_recientes.r
if [ $? -ne 0 ]; then
    echo "Error"
    exit 1
fi

Rscript muertes/letalidad_mexico.r
if [ $? -ne 0 ]; then
    echo "Error"
    exit 1
fi

Rscript muertes/muertes_fecha.r
if [ $? -ne 0 ]; then
    echo "Error"
    exit 1
fi

Rscript mundo/confirmados_muertes_por_dia.r
if [ $? -ne 0 ]; then
    echo "Error"
    exit 1
fi

Rscript municipios/muertes_recientes_municipios.r
if [ $? -ne 0 ]; then
    echo "Error"
    exit 1
fi
Rscript municipios/n_municipios.r
if [ $? -ne 0 ]; then
    echo "Error"
    exit 1
fi
Rscript municipios/curva_sintomas_zm.r
if [ $? -ne 0 ]; then
    echo "Error"
    exit 1
fi
Rscript municipios/incidencia_mortalidad_letalidad_municipio.r
if [ $? -ne 0 ]; then
    echo "Error"
    exit 1
fi

Rscript socioeconomicos/coneval_ind.r
if [ $? -ne 0 ]; then
    echo "Error"
    exit 1
fi
Rscript socioeconomicos/pob_indigena.r
if [ $? -ne 0 ]; then
    echo "Error"
    exit 1
fi