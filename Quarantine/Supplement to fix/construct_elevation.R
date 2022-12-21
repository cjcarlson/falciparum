####################################################
# This script computes min, mean, and max elevation across all ADM1s 
# in the main sample in Carlson et al.
##################################################
rm(list = ls())

user = "Tamma" #"Colin"
if (user == "Colin") {
  wd = 'C:/Users/cjcar/Dropbox/MalariaAttribution'
  repo = 'C:/Users/cjcar/Documents/Github/falciparum'
} else if (user == "Tamma") {
  wd ='/Users/tammacarleton/Dropbox/MalariaAttribution'
  repo = '/Users/tammacarleton/Dropbox/Works_in_progress/git_repos/falciparum'
} else {
  wd = NA
  print('Script not configured for this user!')
}

setwd(wd)

# source functions from previous script
source(file.path(repo,'code/R_utils.R'))
source(file.path(repo,'code/utils_plotting.R'))

# packages
library(doParallel)
 library(rgdal)
library(sf)
library(raster)
library(elevatr)

# Number of Cores
no_cores = 8

# Filename for shapefile 
fn<- file.path(wd,"Data", "AfricaADM1.shp")


####################################################

# shapefile of ADM1s 
shpFile = readOGR(fn)

registerDoParallel(no_cores)

IDs = shpFile$OBJECTID

# need to chunk this out because it's breaking and we can't identify which adm is the problem
# This vector indexes each 10k obs in the 1e6 set of lat-lons
kk <- rep(1:10, each=length(shpFile)/10)

# Divide into 100 pieces = 1e6/10k
for(k in 1:10) {
  
  sampIDs = IDs[kk==k]
  sampk <- subset(shpFile, OBJECTID %in% sampIDs)
  
elev = foreach (i = 1:length(sampk),
                .combine = rbind,
                .packages = c("raster","elevatr")) %dopar% 
  {
    
      #Create 
      adm = subset(sampk, OBJECTID==sampIDs[[i]])
      
      # Throwing this in here to stop the weird "different resolution" error from the AWS API
      outmean <- tryCatch({
        #mybbx = bbox(adm)
        #DEM = get_aws_terrain(mybbx, z = 8, prj = as.character(recs@proj4string))
        DEM = get_elev_raster(adm, z = 8, prj = as.character(adm@proj4string))
        #DEM = projectRaster(from = DEM, crs = crs(recs))
        
        # extract elevation for all overlapping grid cells
        evals = extract(x = DEM, y = adm)[[1]]
        
        # min, max, mean elevation 
        min = min(evals, na.rm=T)
        mn = mean(evals, na.rm=T)
        max = max(evals, na.rm=T)
          
        
      }, error=function(x){ 
        message(paste0("----- Different resolution error, skipping observation ", i, "--------")) 
        message("Here's the original error message:")
        message(x)
        return(NaN)}
      )
      
      out = cbind(as.numeric(adm$OBJECTID), min, mn, max)
      
      # Print so we can track progress
      if(round(i/10) == i/10) {
        print(paste0("-------- Done with sample ", i, " of ", length(sampk), " ---------"))
      }
      
      rm(adm,DEM)
      return(out)
    }

  #rbind all the sample obs together
  elev = data.frame(elev, stringsAsFactors = F)
  colnames(elev) = c("OBJECTID","elevmin", "elevmn", "elevmax")
  
  # Save Rdata of labels
  fn = file.path(wd,"Data", "elevation", paste0("iteration_", k ,"_elevation_ADM1.RData"))
  print(paste0("------- Saving iteration ", k, " of 10 ---------"))
  print(fn)
  save(file = fn, list = c("elev"))
  print("---------- Saved ------------")
}

stopImplicitCluster()

print('--------- Done extracting elevation in 10 subsamples ----------')

########################################################
# SAVE CSV OF LABELS
########################################################

savefn = file.path(wd, "Data", "elevation", "elevation_extracted_all_ADM1.csv")
fileend <- paste0("_elevation_ADM1.RData")

# Loop over and load all 10 iterations already saved
print(" -------- Merging and saving full dataset ----------")

library(data.table)

RLoad = function(fn) {
  return(get(load(fn)))
}

fl = list.files(file.path(wd, "Data", "elevation"), full.names = T)
fl = fl[grepl(x = fl, pattern = fileend)]
print(fl)
df <- do.call(rbind,lapply(fl,RLoad))
dt = data.table(df)
colnames(dt) = c("OBJECTID","elevmin", "elevmn", "elevmax")

write.csv(x = dt, file = savefn)

print(" -------- FULL DATASET SAVED ----------")

