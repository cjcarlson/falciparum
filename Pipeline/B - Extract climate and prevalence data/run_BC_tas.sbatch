#!/bin/bash
indir=/home/romaric/Research/MalariaAttribution

bc_file="B00 - QQ Biascorrect.py"

# this is type of bias corrections, either ecdf or gauss
#bctype=gauss
bctype=ecdf

#defining variables to BC
filevar=tas

obsfile=${indir}/2-Inputs/observation/${filevar}_cru_ts4.06_190101-201401.nc
obsvar=${filevar}
histvar=${filevar}
simvar=${filevar}
detrend=true
isprecip=false


indhist=${indir}/2-Inputs/historical
outdhist=${indir}/4-Outputs/historical
hst1=${filevar}_Amon_ACCESS-CM2_historical_r1i1p1f1_gn_190101-201412
hst2=${filevar}_Amon_ACCESS-ESM1-5_historical_r1i1p1f1_gn_190101-201412
hst3=${filevar}_Amon_BCC-CSM2-MR_historical_r1i1p1f1_gn_190101-201412
hst4=${filevar}_Amon_CanESM5_historical_r1i1p1f1_gn_190101-201412
hst5=${filevar}_Amon_FGOALS-g3_historical_r1i1p1f1_gn_190101-201412
hst6=${filevar}_Amon_GFDL-ESM4_historical_r1i1p1f1_gr1_190101-201412
hst7=${filevar}_Amon_IPSL-CM6A-LR_historical_r1i1p1f1_gr_190101-201412
hst8=${filevar}_Amon_MIROC6_historical_r1i1p1f1_gn_190101-201412
hst9=${filevar}_Amon_MRI-ESM2-0_historical_r1i1p1f1_gn_190101-201412
hst10=${filevar}_Amon_NorESM2-LM_historical_r1i1p1f1_gn_190101-201412

for ii in {1..10}; do
   hst=hst${ii}
   histfile=${indhist}/${!hst}_lonlat.nc
   simfile=${indhist}/${!hst}_lonlat.nc
   outfile=${outdhist}/${!hst}_BC_lonlat.nc
   python "${bc_file}" ${obsfile} ${obsvar} ${histfile} ${histvar} ${simfile} ${simvar} ${outfile} ${bctype} ${detrend} ${isprecip}
done


indhnat=${indir}/2-Inputs/hist-nat
outdhnat=${indir}/4-Outputs/hist-nat
hnt1=${filevar}_Amon_ACCESS-CM2_hist-nat_r1i1p1f1_gn_190101-201412
hnt2=${filevar}_Amon_ACCESS-ESM1-5_hist-nat_r1i1p1f1_gn_190101-201412
hnt3=${filevar}_Amon_BCC-CSM2-MR_hist-nat_r1i1p1f1_gn_190101-201412
hnt4=${filevar}_Amon_CanESM5_hist-nat_r1i1p1f1_gn_190101-201412
hnt5=${filevar}_Amon_FGOALS-g3_hist-nat_r1i1p1f1_gn_190101-201412
hnt6=${filevar}_Amon_GFDL-ESM4_hist-nat_r1i1p1f1_gr1_190101-201412
hnt7=${filevar}_Amon_IPSL-CM6A-LR_hist-nat_r1i1p1f1_gr_190101-201412
hnt8=${filevar}_Amon_MIROC6_hist-nat_r1i1p1f1_gn_190101-201412
hnt9=${filevar}_Amon_MRI-ESM2-0_hist-nat_r1i1p1f1_gn_190101-201412
hnt10=${filevar}_Amon_NorESM2-LM_hist-nat_r1i1p1f1_gn_190101-201412

for ii in {1..10}; do
   hst=hst${ii}
   nat=hnt${ii}
   histfile=${indhist}/${!hst}_lonlat.nc
   simfile=${indhnat}/${!nat}_lonlat.nc
   outfile=${outdhnat}/${!nat}_BC_lonlat.nc
   python "${bc_file}" ${obsfile} ${obsvar} ${histfile} ${histvar} ${simfile} ${simvar} ${outfile} ${bctype} ${detrend} ${isprecip}
done


indssp1=${indir}/2-Inputs/ssp126
outdssp1=${indir}/4-Outputs/ssp126
sp11=${filevar}_Amon_ACCESS-CM2_ssp126_r1i1p1f1_gn_201501-210012
sp12=${filevar}_Amon_ACCESS-ESM1-5_ssp126_r1i1p1f1_gn_201501-210012
sp13=${filevar}_Amon_BCC-CSM2-MR_ssp126_r1i1p1f1_gn_201501-210012
sp14=${filevar}_Amon_CanESM5_ssp126_r1i1p1f1_gn_201501-210012
sp15=${filevar}_Amon_FGOALS-g3_ssp126_r1i1p1f1_gn_201501-210012
sp16=${filevar}_Amon_GFDL-ESM4_ssp126_r1i1p1f1_gr1_201501-210012
sp17=${filevar}_Amon_IPSL-CM6A-LR_ssp126_r1i1p1f1_gr_201501-210012
sp18=${filevar}_Amon_MIROC6_ssp126_r1i1p1f1_gn_201501-210012
sp19=${filevar}_Amon_MRI-ESM2-0_ssp126_r1i1p1f1_gn_201501-210012
sp110=${filevar}_Amon_NorESM2-LM_ssp126_r1i1p1f1_gn_201501-210012

for ii in {1..10}; do
   hst=hst${ii}
   ss1=sp1${ii}
   histfile=${indhist}/${!hst}_lonlat.nc
   simfile=${indssp1}/${!ss1}_lonlat.nc
   outfile=${outdssp1}/${!ss1}_BC_lonlat.nc
   python "${bc_file}" ${obsfile} ${obsvar} ${histfile} ${histvar} ${simfile} ${simvar} ${outfile} ${bctype} ${detrend} ${isprecip}
done


indssp2=${indir}/2-Inputs/ssp245
outdssp2=${indir}/4-Outputs/ssp245
sp21=${filevar}_Amon_ACCESS-CM2_ssp245_r1i1p1f1_gn_201501-210012
sp22=${filevar}_Amon_ACCESS-ESM1-5_ssp245_r1i1p1f1_gn_201501-210012
sp23=${filevar}_Amon_BCC-CSM2-MR_ssp245_r1i1p1f1_gn_201501-210012
sp24=${filevar}_Amon_CanESM5_ssp245_r1i1p1f1_gn_201501-210012
sp25=${filevar}_Amon_FGOALS-g3_ssp245_r1i1p1f1_gn_201501-210012
sp26=${filevar}_Amon_GFDL-ESM4_ssp245_r1i1p1f1_gr1_201501-210012
sp27=${filevar}_Amon_IPSL-CM6A-LR_ssp245_r1i1p1f1_gr_201501-210012
sp28=${filevar}_Amon_MIROC6_ssp245_r1i1p1f1_gn_201501-210012
sp29=${filevar}_Amon_MRI-ESM2-0_ssp245_r1i1p1f1_gn_201501-210012
sp210=${filevar}_Amon_NorESM2-LM_ssp245_r1i1p1f1_gn_201501-210012

for ii in {1..10}; do
   hst=hst${ii}
   ss2=sp2${ii}
   histfile=${indhist}/${!hst}_lonlat.nc
   simfile=${indssp2}/${!ss2}_lonlat.nc
   outfile=${outdssp2}/${!ss2}_BC_lonlat.nc
   python "${bc_file}" ${obsfile} ${obsvar} ${histfile} ${histvar} ${simfile} ${simvar} ${outfile} ${bctype} ${detrend} ${isprecip}
done


indssp5=${indir}/2-Inputs/ssp585
outdssp5=${indir}/4-Outputs/ssp585
sp51=${filevar}_Amon_ACCESS-CM2_ssp585_r1i1p1f1_gn_201501-210012
sp52=${filevar}_Amon_ACCESS-ESM1-5_ssp585_r1i1p1f1_gn_201501-210012
sp53=${filevar}_Amon_BCC-CSM2-MR_ssp585_r1i1p1f1_gn_201501-210012
sp54=${filevar}_Amon_CanESM5_ssp585_r1i1p1f1_gn_201501-210012
sp55=${filevar}_Amon_FGOALS-g3_ssp585_r1i1p1f1_gn_201501-210012
sp56=${filevar}_Amon_GFDL-ESM4_ssp585_r1i1p1f1_gr1_201501-210012
sp57=${filevar}_Amon_IPSL-CM6A-LR_ssp585_r1i1p1f1_gr_201501-210012
sp58=${filevar}_Amon_MIROC6_ssp585_r1i1p1f1_gn_201501-210012
sp59=${filevar}_Amon_MRI-ESM2-0_ssp585_r1i1p1f1_gn_201501-210012
sp510=${filevar}_Amon_NorESM2-LM_ssp585_r1i1p1f1_gn_201501-210012

for ii in {1..10}; do
   hst=hst${ii}
   ss5=sp5${ii}
   histfile=${indhist}/${!hst}_lonlat.nc
   simfile=${indssp5}/${!ss5}_lonlat.nc
   outfile=${outdssp5}/${!ss5}_BC_lonlat.nc
   python "${bc_file}" ${obsfile} ${obsvar} ${histfile} ${histvar} ${simfile} ${simvar} ${outfile} ${bctype} ${detrend} ${isprecip}
done

exit 0

