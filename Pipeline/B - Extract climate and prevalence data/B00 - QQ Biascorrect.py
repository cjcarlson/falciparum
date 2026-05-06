import numpy as np
import pandas as pd
from netCDF4 import Dataset, num2date
from datetime import datetime
from statsmodels.distributions.empirical_distribution import ECDF
import os, sys
import scipy.stats.stats as stats
from matplotlib import pyplot as plt
import statsmodels.api as sm


#defining files
#all have to be on the same grid
#


# all variables defined in the script
#obsfile="/terra/users/acdi/rodoulami/malariaattribution/2-Inputs/observation/pr_cru_ts4.06_190101-201401.nc"
#obsvar="pr"
#histfile="/terra/users/acdi/rodoulami/malariaattribution/2-Inputs/historical/pr_Amon_ACCESS-CM2_historical_r1i1p1f1_gn_190101-201412_lonlat.nc"
#histvar="pr"
#simfile="/terra/users/acdi/rodoulami/malariaattribution/2-Inputs/historical/pr_Amon_ACCESS-CM2_hist-nat_r1i1p1f1_gn_190101-201412_lonlat.nc"
#simvar="pr"
#outfile="/terra/users/acdi/rodoulami/malariaattribution/4-Outputs/historical/pr_Amon_ACCESS-CM2_historical_r1i1p1f1_gn_190101-201412_bc.nc"
#bctype="norm" #two options  norm and empirical


#or all variables read from the command line
# you need to uncomment the lines below
#if len(sys.argv)!=9:
#    print("use: python qqbiascorrect.py obsfile obsvar histfile histvar simfile simvar outfile bctype")
#    print("needs 8 arguments, got", str(len(sys.argv)-1), "exiting...")
#    sys.exit()
    
obsfile=sys.argv[1]
obsvar=sys.argv[2]
histfile=sys.argv[3]
histvar=sys.argv[4]
simfile=sys.argv[5]
simvar=sys.argv[6]
outfile=sys.argv[7]
bctype=sys.argv[8]
detrend=sys.argv[9]
isprecip=sys.argv[10]

if isprecip=="true":
    isprecip=True
else:
    isprecip=False
if detrend=="true":
    detrend=True
else:
    detrend=False
#


def qqbc(_sim,_hist,_obs, _type="ecdf", _detrend=True, _isprecip=True):
    # corrects _sim to _obs based on q-q mapping of _hist on _obs
    #_hist and _sim are from the same model, _obs are observations
    #_hist and _obs have to have identical dimensions
    #_sim may have different length of time series
    if _detrend:
        _Y=np.copy(_sim)
        _x=range(len(_Y))
        _X = sm.add_constant(np.array([_x]).T)
        _result=sm.OLS(_Y,_X, missing="drop").fit()
        _trendsim=_result.predict()
        _simraw=np.copy(_sim)
        _sim=_sim-_trendsim
        
        _Y=np.copy(_hist)
        _x=range(len(_Y))
        _X = sm.add_constant(np.array([_x]).T)
        _result=sm.OLS(_Y,_X, missing="drop").fit()
        _trendhist=_result.predict()
        _histraw=np.copy(_hist)
        _hist=_hist-_trendhist
        
        _Y=np.copy(_obs)
        _x=range(len(_Y))
        _X = sm.add_constant(np.array([_x]).T)
        _result=sm.OLS(_Y,_X, missing="drop").fit()
        _trendobs=_result.predict()
        _obsraw=np.copy(_obs)
        _obs=_obs-_trendobs
        
    if _type=="ecdf":
        ecdf=ECDF(_hist) #empirical density function for historical data
        _q=ecdf(_sim) #finding quantiles of _sim in that distribution
        _xbc=np.quantile(_obs, _q) #finding values from obs bccorresponding to quantiles
    elif _type=="gauss":
        _histpars=stats.distributions.norm.fit(_hist) #fitting normal distribution to historical data
        if _histpars[1]==0:
            #when standard deviation is 0, i.e. all vales are identical
            #cannot do much then, assume that it's the 50th quantile
            _q=np.repeat(0.5, len(_sim))
        else:
            _q=stats.distributions.norm.cdf(_sim,*_histpars) #finding quantiles of simulations from that distribution
        _obspars=stats.distributions.norm.fit(_obs) #fitting normal distribution to observations
        if _obspars[1]==0:
            #when standard deviation is 0, i.e. all vales are identical
            #cannot do much then, assume that the bias-corrected value is the mean of observations
            _xbc=np.repeat(_obspars[0], len(_q))
        else:
            _q[_q==1]=0.9999
            _xbc=stats.distributions.norm.ppf(_q, *_obspars) #finding values from the observations' distribution that correspond to simulation quantiles
    _output=np.copy(_xbc) #making array to put bc data values in
    if _detrend:
        #need to add trend back. But trend needs to be adjusted. precip is adjusted through ratio, temp - by difference
        if _isprecip==True:
            if np.nanmean(_histraw)>0:
                _trendadj=np.nanmean(_obsraw)/np.nanmean(_histraw)
            else:
                _trendadj=np.nanmean(_obsraw)/np.nanmean(_simraw)
            if _trendadj>1:
                _trendadj=1
            _output=_output+(_trendsim*_trendadj)
        else:
            _trendadj=np.nanmean(_obsraw)-np.nanmean(_histraw)
            _output=_output+(_trendsim+_trendadj)
        
    if _isprecip:
        _output[_output<0]=0 #making sure no negative values, should not be used if bias correcting temperature
    return _output



print("reading data")
#reading all data
obsncdata=Dataset(obsfile)
obsdata=obsncdata.variables[obsvar][:]
obslon=obsncdata.variables['lon'][:]
obslat=obsncdata.variables['lat'][:]
time=obsncdata.variables['time']
obsdates=num2date(time[:], time.units, time.calendar)
print("loaded obs data", obsdata.shape)

histncdata=Dataset(histfile)
histdata=histncdata.variables[histvar][:]
histlon=histncdata.variables['lon'][:]
histlat=histncdata.variables['lat'][:]
time=histncdata.variables['time']
histdates=num2date(time[:], time.units, time.calendar)
print("loaded historical data", histdata.shape)

simncdata=Dataset(simfile)
simdata=simncdata.variables[simvar][:]
simlon=simncdata.variables['lon'][:]
simlat=simncdata.variables['lat'][:]
simtime=simncdata.variables['time']
simdates=num2date(time[:], time.units, time.calendar)
print("loaded simulation data", simdata.shape)

#reconstucting dates, this is needed for selection of overlapping periods, and dates have to be 
#reconstructed because there might be differences in days of the month, or hours between obs and hist data
#also, the cftime object that comes from day2mon is not easily read by pandas (which I use for dates manipulation)
obsdates=pd.date_range(datetime(obsdates[0].year,obsdates[0].month,1,0,0,0), periods=obsdata.shape[0], freq="MS")
histdates=pd.date_range(datetime(histdates[0].year,histdates[0].month,1,0,0,0), periods=histdata.shape[0], freq="MS")
simdates=pd.date_range(datetime(simdates[0].year,simdates[0].month,1,0,0,0), periods=simdata.shape[0], freq="MS")

#finding overlap in dates between observation and historical data - reference period
selobs=np.where(np.in1d(obsdates,histdates))[0]
selhist=np.where(np.in1d(histdates,obsdates))[0]

# selecting (overlapping) obs and historical data for the reference period
histref=histdata[selhist,:,:]
obsref=obsdata[selobs,:,:]
refdates=obsdates[selobs]
print("refernce period data", histref.shape, obsref.shape)

#actual bias correction
simbc=np.copy(simdata) #array to store data
#iterating through months, lats and lons
print("starting bias correction...")
for m in range(12):
    selref=refdates.month==m+1 #m+1 because need to convert from pythonic to calendar month
    selsim=simdates.month==m+1
    for i in range(len(histlat)):
        for j in range(len(histlon)):
                if not obsdata.mask[0,i,j]:
                    simbc[selsim,i,j]=qqbc(simdata[selsim,i,j],histref[selref,i,j],obsref[selref,i,j], bctype, detrend, isprecip)
                    
print("done")

simbcmasked = np.ma.array(simbc, mask = np.tile(obsdata[0,:,:].mask, (simbc.shape[0],1)).reshape(simbc.shape))

print("writing output file", outfile)
#removing output file, otherwise Dataset crashes
if os.path.exists(outfile):
    os.remove(outfile)
    
#saving data into netcdf file
# this copies all variables and arguments from sim file
with Dataset(outfile, "w", format="NETCDF4") as outnc:
    # copy global attributes all at once via dictionary
    outnc.setncatts(simncdata.__dict__)
    # copy dimensions
    for name, dimension in simncdata.dimensions.items():
        outnc.createDimension(
            name, (len(dimension) if not dimension.isunlimited() else None))
    # copy all file data 
    for name, variable in simncdata.variables.items():
        x = outnc.createVariable(name, variable.datatype, variable.dimensions)
        # copy variable attributes all at once via dictionary
        outnc[name].setncatts(simncdata[name].__dict__)
        if name == simvar:
            # variable that is being corrected
            outnc[name][:] = simbcmasked        
        else:
            #other variables
            outnc[name][:] = simncdata[name][:]
print("finished")
