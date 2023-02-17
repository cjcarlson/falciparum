
# lags of total precip (up to three)
# first divide by 10 to make the units cm 

complete <- complete %>% group_by(OBJECTID) %>% 
  mutate(pptcm = ppt/10 ) %>%
  mutate(ppt.lag = lag(pptcm, order_by = monthyr),
         ppt.lag2 = lag(pptcm, order_by = monthyr, n=2),
         ppt.lag3 = lag(pptcm, order_by = monthyr, n=3)) 

pptvars = paste(colnames(complete)[grep("ppt.lag", colnames(complete))], collapse = " + ")

# Formulas: all fixed effects (main spec = cXt2intrXm)
cym = as.formula(paste0("PfPR2 ~ temp + temp2 + pptcm + ", pptvars, "| OBJECTID + year + month | 0 | OBJECTID"))
cXt2m = as.formula(paste0("PfPR2 ~ temp + temp2 + pptcm + ", pptvars, " + country:monthyr + country:monthyr2 | OBJECTID  + month | 0 | OBJECTID"))
cXt2cXm = as.formula(paste0("PfPR2 ~ temp + temp2 + pptcm + ", pptvars, " + country:monthyr + country:monthyr2 | OBJECTID + country:month | 0 | OBJECTID"))
cXt2intm = as.formula(paste0("PfPR2 ~ temp + temp2 + pptcm + ", pptvars, " + country:monthyr + country:monthyr2 | OBJECTID  + intervention + month | 0 | OBJECTID"))
cXt2intrXm = as.formula(paste0("PfPR2 ~ temp + temp2 + pptcm + ", pptvars,  " + I(intervention) + country:monthyr + country:monthyr2 | OBJECTID  + as.factor(smllrgn):month | 0 | OBJECTID"))
cXt2intcXm = as.formula(paste0("PfPR2 ~ temp + temp2 + pptcm + ", pptvars,  " + I(intervention) + country:monthyr + country:monthyr2 | OBJECTID  + country:month | 0 | OBJECTID"))
rXyrXm = as.formula(paste0("PfPR2 ~ temp + temp2 + pptcm + ", pptvars,  "| OBJECTID + as.factor(smllrgn):month + as.factor(smllrgn):year | 0 | OBJECTID"))
rXycXm = as.formula(paste0("PfPR2 ~ temp + temp2 + pptcm + ", pptvars,  "| OBJECTID + country:month + as.factor(smllrgn):year | 0 | OBJECTID"))
rXyrXmcXt = as.formula(paste0("PfPR2 ~ temp + temp2 + pptcm + ", pptvars,  " + country:monthyr | OBJECTID + as.factor(smllrgn):month + as.factor(smllrgn):year | 0 | OBJECTID"))
myforms = c(cym, cXt2m, cXt2cXm, cXt2intm, cXt2intrXm, cXt2intcXm, rXyrXm, rXycXm, rXyrXmcXt) 
mycollabs = c("cnty + yr + mo FEs.", "cnty trd, mo FEs.", "cnty trd, cnty-mo FEs.", "cnty trd, int + mo FEs.", "cnty trd, int + rgn-mo FEs.", "cnty trd, int + cnty-mo FEs.", "rgn-yr + rgn-mo FEs.", "rgn-yr + cnty-mo FEs.", "rgn-yr+rgn-mo FEs., cnty trd")

# Run all models
modellist = list()
i=0
for (m in myforms) {
  i=i+1
  modellist[[i]] = felm(data = complete, formula = m)
}

# Plot precip lags for each model
figList = list()
for(m in 1:length(modellist)) {
  figList[[m]] =  plotLinearLags(modellist[[m]], "ppt", cluster = T, laglength = 3, xLab="Monthly lag", "Coefficient", title = mycollabs[[m]], yLim = c(-.2,.2))
}

p = plot_grid(figList[[1]], figList[[2]], figList[[3]], 
              figList[[4]], figList[[5]], figList[[6]],
              figList[[7]], figList[[8]], figList[[9]], nrow=3)
p

ggsave(file.path(wd, "Results", "Figures", "Diagnostics", "Drought_flood_defn", "total_precip_results.pdf"), plot = p, width = 7, height = 7)

