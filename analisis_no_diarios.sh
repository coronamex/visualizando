export LC_ALL=es_MX.UTF-8
export LANG=es_MX.UTF-8

Rscript estados/r_efectiva.r
if [ $? -ne 0 ]; then
    echo "Error"
    exit 1
fi
