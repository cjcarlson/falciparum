


source(here::here("Pipeline", "A - Utility functions", "A00 - Configuration.R"))

#### Climate data
# load data based on CRU version:
if (CRUversion=="4.03") {
  data_fp <- file.path(datadir, 'Data', 'CRU-Reextraction-Aug2022.csv')
} else if (CRUversion=="4.06") {
  data_fp <- file.path(datadir, 'Data', 'CRU-Reextraction-July2023-CRU4.06.csv')
}  else {
  print('CRU version not supported! Use 4.03 or 4.06.')
}

dominant_method <- file.path(
  datadir, 
  "Data", 
  paste0('dominant_diagnostic_method_summary.csv')
) |> 
  readr::read_csv(show_col_types = FALSE)


data <- readr::read_csv(data_fp, show_col_types = FALSE) |> 
  dplyr::left_join(dominant_method, by = join_by(OBJECTID, month, year))

#### Spatial data
spatial <- read.csv(file.path(datadir, 'Dataframe backups', 'shapefile-backup.csv'))
countrydf <- unique(spatial[,c('OBJECTID','NAME_0')])
data$country <- countrydf$NAME_0[sapply(data$OBJECTID, function(x){which(countrydf$OBJECTID==x)})]
data$country <- countrydf$NAME_0[sapply(data$OBJECTID, function(x){which(countrydf$OBJECTID==x)})]
iso = data %>% group_by(country, month, year) %>% summarize_all(mean, na.rm=T)
data_iso <- iso[complete.cases(iso),]

#### Dates & times
data$yearnum <- data$year
data$year <- factor(data$year)
data %>% unite("monthyr", month:year, sep=' ', remove=FALSE) %>% 
  mutate(monthyr = as.Date(as.yearmon(monthyr))) %>% 
  mutate(monthyr = as.numeric(ymd(monthyr)-ymd("1900-01-01"))) -> data.reset

### Store complete records only
complete <- data.reset[complete.cases(data.reset),]

########################################################################
# Define Global Burden of Disease regions 
########################################################################

gbod <- sf::read_sf(file.path(datadir, "Data", "OriginalGBD", "WorldRegions.shp"))
# head(gbod@data)

gboddf = as.data.frame(gbod)
gboddf = gboddf %>% dplyr::select("ISO", "NAME_0", "Region", "SmllRgn")
gboddf = gboddf %>% 
  group_by(ISO, NAME_0) %>% 
  summarize(Region = first(Region), SmllRgn = first(SmllRgn)) # note that the small regions are homogenous within country
colnames(gboddf) = c("ISO", "country", "region", "smllrgn")
gboddf$country = as.character(gboddf$country)

# clean to merge
gboddf$country = ifelse(gboddf$country=="Cote D'Ivoire", "Côte d'Ivoire", gboddf$country)

complete$country = as.character(complete$country)
complete = left_join(complete, gboddf, by = "country")
complete$country = as.factor(complete$country)

rm(gbod, gboddf)

########################################################################
# Define covariates:
# a) Drought/flood events
# b) Malaria intervention periods
########################################################################

##### Define flood/drought variables - need to pass the climate data separately from the merged dataset with the outcome
##### variable because we want to define climate over the whole period
complete = computePrcpExtremes(dfclimate = data.reset, dfoutcome = complete, pctdrought = 0.10, pctflood = 0.90, yearcutoff = NA)
complete = complete %>% arrange(OBJECTID, monthyr)
if (CRUversion=="4.03") {
  complete %>% 
    dplyr::select(OBJECTID, ppt_pctile0.1, ppt_pctile0.9) %>% 
    distinct() %>% 
    write_csv(file.path(repo, "Climate", "PrecipKey.csv"))
} else if (CRUversion=="4.06") {
  complete %>% 
    dplyr::select(OBJECTID, ppt_pctile0.1, ppt_pctile0.9) %>% 
    distinct() %>% 
    write_csv(file.path(repo, "Climate", "PrecipKey_CRU-TS4-06.csv"))
}  else {
  print('CRU version not supported! Use 4.03 or 4.06.')
}

# include: contemporaneous temp, then distributed lag in flood and drought
floodvars = paste(colnames(complete)[grep("flood", colnames(complete))], collapse = " + ")
droughtvars = paste(colnames(complete)[grep("drought", colnames(complete))], collapse = " + ")

# need this for country specific quadratic trends
complete$monthyr2 = complete$monthyr^2

# define key intervention periods
complete$intervention = ifelse(complete$yearnum>=1955 & complete$yearnum<=1969, 1, 0)
complete$intervention[complete$yearnum>=2000 & complete$yearnum<=2015] = 2
complete$intervention = as.factor(complete$intervention)

# classes: important for ensuring felm is treating these correctly
complete$month = as.factor(complete$month)
complete$year = as.factor(complete$year)

complete$dominant_METHOD = as.factor(complete$dominant_METHOD)

unique(complete$dominant_METHOD)
