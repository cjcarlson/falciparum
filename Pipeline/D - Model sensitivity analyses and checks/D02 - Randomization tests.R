############################################################
# This script conducts temporal randomization test
# as a placebo check on the main regression specification.
############################################################

############################################################
# Set up
############################################################

rm(list = ls())

if (CRUversion=="4.03") {
  resdir = file.path(datadir, "Results")
} else if (CRUversion=="4.06") {
  resdir = file.path(datadir, "Results_CRU-TS4-06")
} else {
  print('CRU version not supported! Use 4.03 or 4.06.')
}

# source functions for easy plotting and estimation
# source functions for easy plotting and estimation
source(here::here("Pipeline", "A - Utility functions", "A00 - Configuration.R"))
source(here::here("Pipeline", "A - Utility functions", "A01 - Utility code for calculations.R"))
source(here::here("Pipeline", "A - Utility functions", "A02 - Utility code for plotting.R"))

# packages
library(lfe)
library(tidyverse)
library(zoo)
library(lubridate)
library(dplyr)
library(data.table)
library(doParallel)

########################################################################
# Data clean up
########################################################################

#### Call external script for data cleaning
source(here::here("Pipeline", "A - Utility functions", "A03 - Prep data for estimation.R"))

# Main specification: cXt2intrXm (country specific quadratic trends, intervention dummies, GBOD region by month FE)
myform = as.formula(
  paste0(
    "PfPR2 ~ temp + temp2 + ", floodvars, " + ", droughtvars, 
    " + I(intervention) + country:monthyr + country:monthyr2 | OBJECTID + as.factor(smllrgn):month | 0 | OBJECTID"
  )
)

########################################################################
# Randomly reassign weather within each panel unit
########################################################################

# drop if there is only one observation per ADM1 (these are dropped in regression anyway)
scram = complete %>% group_by(OBJECTID) %>%
  mutate(length=length(!is.na(PfPR2))) %>%
  filter(length>1)

# Which variables do you want to store?
pattern = c("temp", "flood", "drought")

# Run simulation S times
S = 1000

# Set number of cores to parallelize over:
n_cores = 4
registerDoParallel(n_cores)
set.seed(7812)

placebo_out=foreach(i=1:S,
                    .combine = rbind) %dopar%
  {
    
    # create scrambled climate
    scramS = scram %>% group_by(OBJECTID) %>% 
      mutate(temp = sample(temp),
             drought = sample(drought),
             flood = sample(flood))
    
    # obtain lags and squared term of scrambled climate
    scramS = scramS %>% 
      mutate(temp2 = temp^2,
             flood.lag=lag(flood, order_by=monthyr),
             flood.lag2 = lag(flood, order_by = monthyr, n=2),
             flood.lag3 = lag(flood, order_by = monthyr, n=3),
             drought.lag = lag(drought, order_by = monthyr),
             drought.lag2 = lag(drought, order_by = monthyr, n=2),
             drought.lag3 = lag(drought, order_by = monthyr, n=3))
    
    # run regression
    mod = felm(data=scramS, formula=myform)
    
    # store coefficients, their p values, and the result of an F test on the effect at 35C
    Xvars = rownames(mod$coefficients)[grepl(paste(pattern, collapse="|"), x = rownames(mod$coefficients))]
    Xcoefs = as.matrix(mod$coefficients[rownames(mod$coefficients) %in% Xvars])
    Xpvals = as.matrix(summary(mod)$coefficients[rownames(mod$coefficients) %in% Xvars,4])
    
    out = cbind(t(Xcoefs), t(Xpvals))
    
    # Print so we can track progress
    print(paste0("-------- Done with sample ", i, " of ", S, " ---------"))
    
    return(out)
  }


placebo_out=data.frame(placebo_out, stringsAsFactors = F)

# get column names right
mod = felm(data=complete, formula=myform)
Xvars = rownames(mod$coefficients)[grepl(paste(pattern, collapse="|"), x = rownames(mod$coefficients))]
pnames = list()
for(x in Xvars){
  pnames[[x]] = paste0(x, "_p")
}
colnames(placebo_out) = c(Xvars, unlist(pnames))
stopImplicitCluster()

# Save
fn <- "scramble_time_placebo.rds"
dir.create(file.path(resdir, "Models", "randomization_tests"), showWarnings = FALSE)
saveRDS(placebo_out, file = file.path(resdir, "Models", "randomization_tests", fn))

print("-------- Saved Rds of coefficients and p-values from randomization test with scrambled climate data --------")

########################################################################
# Show p-values from randomization versus main model
########################################################################

placebo = readRDS(file.path(resdir, "Models", "randomization_tests", "scramble_time_placebo.rds"))

# main model
mainmod = felm(data=complete, formula = myform)
Xvars = rownames(mainmod$coefficients)[grepl(paste(pattern, collapse="|"), x = rownames(mainmod$coefficients))]
Xcoefs = as.matrix(mainmod$coefficients[rownames(mainmod$coefficients) %in% Xvars])
Xpvals = as.matrix(summary(mainmod)$coefficients[rownames(mainmod$coefficients) %in% Xvars,4])
main = as.data.frame(cbind(t(Xcoefs), t(Xpvals)))
pnames = list()
for(x in Xvars){
  pnames[[x]] = paste0(x, "_p")
}
colnames(main) = c(Xvars, unlist(pnames))
main$vline = 1
vline = melt(data.table(main), id.vars = "vline")
vline = vline = vline[,2:3]
colnames(vline) = c("variable", "value_main")

# matrix of graphs: histogram of coefficients from placebo, compared to coef in true (vertical line in red) 
placebo = placebo %>% mutate(sim = row_number())
toplot = melt(data.table(placebo), id.vars = "sim")
toplot = toplot %>% mutate(pval = (grepl("_p",variable)))

# merge in main mod
toplot = toplot %>% left_join(vline, by=c("variable"))

p = ggplot(data = toplot[toplot$pval==FALSE & toplot$variable!="temp2",], aes(x=value)) +
  geom_histogram() + geom_vline(aes(xintercept = value_main), colour="red") +
  facet_wrap(~variable) +
  theme_bw()
p
dir.create(file.path(resdir, "Figures", "Diagnostics", "Randomization_tests"), showWarnings = FALSE)
ggsave(file.path(resdir, "Figures", "Diagnostics", "Randomization_tests", "coefficients_time_randomiz.pdf"), plot = p, width = 7, height = 9)

p2 = ggplot(data = toplot[toplot$pval==FALSE & toplot$variable=="temp2",], aes(x=value)) +
  geom_histogram() + geom_vline(aes(xintercept = value_main), colour="red") +
  theme_bw()
p2
ggsave(file.path(resdir, "Figures", "Diagnostics", "Randomization_tests", "coefficients_time_randomiz_TEMP2.pdf"), plot = p2, width = 7, height = 9)

# matrix of graphs: histogram of pvals from placebo, compared to pval in true (vertical line in red)
p3 = ggplot(data = toplot[toplot$pval==TRUE,], aes(x=value)) +
  geom_histogram() + geom_vline(aes(xintercept = value_main), colour="red") +
  facet_wrap(~variable) +
  theme_bw()
p3
ggsave(file.path(resdir, "Figures", "Diagnostics", "Randomization_tests", "pVals_time_randomiz.pdf"), plot = p3, width = 7, height = 9)
