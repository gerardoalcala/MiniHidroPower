#!/bin/bash
# Be defensive
set -eu
# Copy output to file for reference.
exec > >(tee -i log-Rt$1-Ri$2-`date +'%Y%m%d-%H%M%S'`.txt)
exec 2>&1

# Rt define el radio de la turbina
# Ri define el radio del intake
# 500 1000 1500 2000 2500

Rt=$1
Ri=$2

#STEPS=( 14 11 9 15 13 6 12 8 10 7 5 4 3 16 2 1 17 18 19 20 )
#STEPS=( 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19 20 )
STEPS=( 8 7 15 11 14 12 9 13 10 6 5 16 4 17 19 3 18 20 2 1 )
#STEPS=( 13 )

export DATOSDIR=$HOME/datos_Rt${Rt}_Ri${Ri}
if [[ ! -d $DATOSDIR ]]; then
	mkdir $DATOSDIR
        script="MHidroPower.slurm.q"
        STEP=116718
        #STEP=100
    # Define como se mandan los bloques de puntos de forma manual
    Rt=`printf %04d $Rt`
    Ri=`printf %04d $Ri`
    for i in ${STEPS[@]}; do
        FROM=$((1+(i-1)*STEP))
        TO=$((STEP+(i-1)*STEP))

        FROM=`printf %07d $FROM`
        TO=`printf %07d $TO`

     	echo $Rt $Ri $FROM $TO
     	sed -i \
            -e "s/--job-name=.*/--job-name=mini.hydro.Rt${Rt}.Ri${Ri}.${FROM}.${TO}/" \
            -e "s/^ni1=.*/ni1=$FROM/" \
            -e "s/^nf1=.*/nf1=$TO/" \
            -e "s/^Rt=.*/Rt=$Rt/" \
            -e "s/^Ri=.*/Ri=$Ri/" \
            -e "s/^N=.*/N=$i/" ${script}
     	sbatch ${script}
    done
else
	echo "Directorio $DATOSDIR ya existe"
	exit 1
fi
#for f in datos_Rt100_Ri100/*; do cat $f >> final.txt; done
