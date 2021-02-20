#!/bin/bash --login
#SBATCH --job-name=mini.hydro.Rt2500.Ri1000.0000001.0116718
#SBATCH --output=%x.out.%J
#SBATCH --error=%x.err.%J
#SBATCH --partition=C1Mitad1
#SBATCH --ntasks-per-node=12
#SBATCH --ntasks=12
#SBATCH --mem=0
#SBATCH --time=240:00:00
#SBATCH --exclude=c-1-17

################################################
NNODES=$SLURM_NNODES
NCPUS=$SLURM_NTASKS
PPN=$SLURM_NTASKS_PER_NODE

echo Running on host `hostname`
echo Time is `date`
echo Directory is `pwd`
echo SLURM job ID is $SLURM_JOBID
echo This jobs runs on the following machine: `echo $SLURM_JOB_NODELIST | uniq`

echo Number of Processing Elements is $NCPUS
echo Number of mpiprocs per node is $PPN
################################################

env

#script="MHidroPrueba.R"
script="MHidroPower.R"
#script="1-MiniHidroPowerSinMascaras4.R"

INPUTDIR=$HOME/git/MiniHidroPower
WDPATH=/home/$USER/scratch/${SLURM_JOB_NAME}.${SLURM_JOBID}
mkdir -p $WDPATH
cd $WDPATH
cp $INPUTDIR/$script $WDPATH
cp $INPUTDIR/A-RasterRio.tif $WDPATH
cp $INPUTDIR/RasRioTotal.tif $WDPATH
cp $INPUTDIR/B-RasterDEM.tif $WDPATH
cp $INPUTDIR/D-Edificaciones.tif $WDPATH
cp $INPUTDIR/RasMaskTotal.tif $WDPATH
cp -r $INPUTDIR/VectorFiles $WDPATH

Rt=2500
Ri=1000
ni1=0000001
nf1=0116718
#sed -i -e "s/^ni1<-.*/ni1<-$ni1/" -e "s/^nf1<-.*/nf1<-$nf1/" ${script}
sed -i \
    -e "s/^ni1<-.*/ni1<-$ni1/" \
    -e "s/^nf1<-.*/nf1<-$nf1/" \
    -e "s/^  radIntake.*<-.*/  radIntake <- ${Ri}/" \
    -e "s/^  radTurbina.*<-.*/  radTurbina <- ${Rt}/" \
    ${script}

time Rscript ${script}

N=1
touch DatosMHP.csv
cp DatosMHP.csv $DATOSDIR/DatosMHP_$N.csv
