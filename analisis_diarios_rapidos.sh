#!/usr/bin/env bash

Rscript casos/inicio_sintomas_por_fecha.r
if [ $? -ne 0 ]; then
    echo "Error"
    exit 1
fi

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
