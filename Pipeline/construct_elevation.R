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

elev = foreach (i = 1:length(shpFile),
                .combine = rbind,
                .packages = c("raster","elevatr")) %dopar% 
  {
    
      #Create 
      adm = subset(shpFile, OBJECTID==IDs[[i]])
      
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
        print(paste0("-------- Done with sample ", i, " of ", length(shpFile), " ---------"))
      }
      
      rm(adm,DEM)
      return(out)
    }
    
stopImplicitCluster()

elev = data.frame(elev, stringsAsFactors = F)
colnames(elev) = c("OBJECTID","elevmin", "elevmn", "elevmax")

print('--------- Done extracting elevation ----------')

########################################################
# SAVE CSV OF LABELS
########################################################

outfn = file.path(wd, "Data", "extracted_elevation_ADM1.csv")

write.csv(x = elev, file = outfn)

print('--------- Saved csv of labels ----------')
