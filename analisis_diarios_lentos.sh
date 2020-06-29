#!/usr/bin/env bash
#jupyter nbconvert --to script --execute --ExecutePreprocessor.timeout=-1 estados/rt.live.ipynb
papermill estados/rt.live.ipynb estados/papermill.rt.live.ipynb
if [ $? -ne 0 ]; then
    echo "Error"
    exit 1
fi
Rscript estados/r_efectiva.r
if [ $? -ne 0 ]; then
    echo "Error"
    exit 1
fi
