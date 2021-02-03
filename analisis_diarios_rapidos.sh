#!/usr/bin/env bash
export LC_ALL=es_MX.UTF-8
export LANG=es_MX.UTF-8

# Casos
Rscript casos/inicio_sintomas_por_fecha.r
if [ $? -ne 0 ]; then
    echo "Error"
    exit 1
fi

Rscript casos/sir.r
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

Rscript clinicos/tiempos_deteccion.r
if [ $? -ne 0 ]; then
    echo "Error"
    exit 1
fi

# Estados
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

Rscript estados/curvas_casos_estados.r
if [ $? -ne 0 ]; then
    echo "Error"
    exit 1
fi

# Muertes
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

Rscript muertes/muertes_nacionales_tipomun.r
if [ $? -ne 0 ]; then
    echo "Error"
    exit 1
fi

# Mundo
Rscript mundo/confirmados_muertes_por_dia_por_pob.r
if [ $? -ne 0 ]; then
    echo "Error"
    exit 1
fi

# Municipios
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

# Socioeconomicos
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
Rscript socioeconomicos/casos_marginacion_municipal.r
if [ $? -ne 0 ]; then
   echo "Error"
   exit 1
fi
