#!/usr/bin/env bash
export LC_ALL=es_MX.UTF-8
export LANG=es_MX.UTF-8

# Casos
# Rscript casos/inicio_sintomas_por_fecha.r
Rscript casos/sir.r
if [ $? -ne 0 ]; then
    echo "Error"
    exit 1
fi

# Clinicos
# Rscript clinicos/clinicos_db_completa.r
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
Rscript mundo/muertes_mundo.r
if [ $? -ne 0 ]; then
    echo "Error"
    exit 1
fi

# Socioeconomicos
Rscript socioeconomicos/socioeconomicos.r
if [ $? -ne 0 ]; then
   echo "Error"
   exit 1
fi
Rscript socioeconomicos/casos_marginacion_municipal.r
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
Rscript municipios/mapa_mortalidad.r
if [ $? -ne 0 ]; then
    echo "Error"
    exit 1
fi
