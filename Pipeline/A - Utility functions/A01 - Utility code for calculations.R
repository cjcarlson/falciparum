
print("Loading A01 - Utility code for calculations.R")

## Functions needed throughout falciparum/ repo

swirl <- function(input.raster) {
  input.raster <- flip(t(input.raster),direction='y')
  extent(input.raster) <- c(-180,180,-90,90)
  input.raster = crop(input.raster, c(-30,60,-37,40))
  return(input.raster)
}


r0t <- function(T, na.rm=TRUE) {
  suppressWarnings(if(is.na(T)) return(NA))
  if(T<=14) {return(0)}
  a = 0.000203*T*(T-11.7)*((42.3-T)^0.5)
  bc = -0.54*T*T + 25.2*T - 206
  p = -0.000828*T*T + 0.0367*T + 0.522 # e^-mu
  mu = -1*log(p)
  PDR = 0.000111*T*(T-14.7)*((34.4-T)^0.5) # 1/EIP
  pEA = -0.00924*T*T + 0.453*T - 4.77
  MDR = 0.000111*T*(T-14.7)*((34-T)^0.5) # 1/tauEA
  EFD = -0.153*T*T + 8.61*T - 97.7
  
  R0 = (((a^2)*bc*(p^(1/PDR))*EFD*pEA*MDR)/(mu^3))^(1/2)
  if(is.nan(R0)){return(0)}
  return(R0/87.13333) # that's the max
}


computePrcpExtremes = function(dfclimate, dfoutcome, pctdrought, pctflood, yearcutoff=NA) {
  # NOTE: for percentiles: enter 0.90 for 90th, 0.10 for 10th.
  # NOTE: dfclimate is the dataframe where your climate data live (may be more comprehensive in space and/or time than your outcome variable)
  # dfoutcome is the dataframe where your outcome merged to climate data live
  
  # initialize
  data = dfclimate
  data = data %>% arrange(OBJECTID, monthyr)
  
  # year cutoff, if not null
  if(is.na(yearcutoff)) { yearcutoff = max(data$yearnum) }
  
  # flood definition
  data %>% 
    filter(yearnum <= yearcutoff ) %>% 
    group_by(OBJECTID) %>% 
    summarize(ppt.90 = quantile(na.omit(ppt), pctflood)) -> ppt.90
  data <- left_join(data, ppt.90)
  data$flood <- as.numeric(data$ppt >= data$ppt.90)
  colnames(data)[dim(data)[2]-1] = paste0("ppt_pctile", pctflood)
  
  # flood lags
  data %>% group_by(OBJECTID) %>% 
    mutate(flood.lag = lag(flood, order_by = monthyr),
           flood.lag2 = lag(flood, order_by = monthyr, n=2),
           flood.lag3 = lag(flood, order_by = monthyr, n=3)) -> data
  
  # drought definition
  data %>% 
    filter(yearnum <= yearcutoff ) %>% 
    group_by(OBJECTID) %>% 
    summarize(ppt.10 = quantile(na.omit(ppt), pctdrought)) -> ppt.10
  
  data <- left_join(data, ppt.10)
  data$drought <- as.numeric(data$ppt <= data$ppt.10)
  colnames(data)[dim(data)[2]-1] = paste0("ppt_pctile", pctdrought)
  
  # drought lags
  data %>% group_by(OBJECTID) %>% 
    mutate(drought.lag = lag(drought, order_by = monthyr),
           drought.lag2 = lag(drought, order_by = monthyr, n=2),
           drought.lag3 = lag(drought, order_by = monthyr, n=3)) -> data
  
  # merge into outcome dataframe 
  tokeep = c("OBJECTID", "monthyr", "month", "year")
  data = data %>% dplyr::select(tokeep, contains("flood"), contains("ppt_pctile"), contains("drought"))
  dfoutcome <- left_join(dfoutcome, data, by=c("OBJECTID", "monthyr", "month", "year"))
  
  # return
  return(dfoutcome)
  
}
