
library(reshape)
contdf <- cont@data
contdf <- contdf[,-c(2:15)]
contdf <- melt(contdf, id='OBJECTID')

dffix <- separate(contdf,  variable, into=c('month','year','var'), sep='\\.')
dffix <- cast(dffix, OBJECTID+month+year ~ var)
dffix$year <- as.numeric(dffix$year)
#dffix$timecont <- dffix$year + ((sapply(dffix$month, function(x){which(month==x)}))-1)/12

write.csv(dffix, 'dffix-backup.csv')

complete <- dffix[complete.cases(dffix),]
#write.csv(complete,'backup.csv')

countrydf <- unique(cont@data[,c('OBJECTID','NAME_0')])

complete$country <- countrydf$NAME_0[sapply(complete$OBJECTID, function(x){which(countrydf$OBJECTID==x)})]

# YEAR CUTOFF
#complete <- complete[complete$year < 2000,]

#install.packages('lfe')
library(lfe)

model0 <- felm(PfPR2 ~ R0 | 0, data = complete)
model1 <- felm(PfPR2 ~ R0 | OBJECTID | 0 | OBJECTID, data = complete)
model2 <- felm(PfPR2 ~ R0 | OBJECTID + year | 0 | OBJECTID, data = complete)
model3 <- felm(PfPR2 ~ R0 | OBJECTID + country:year | 0 | OBJECTID, data = complete)
model4 <- felm(PfPR2 ~ R0 | OBJECTID + country:year + month | 0 | OBJECTID, data = complete)


model1b <- felm(PfPR2 ~ R0 | country | 0 | OBJECTID, data = complete)
model2b <- felm(PfPR2 ~ R0 | country + year | 0 | OBJECTID, data = complete)
model3b <- felm(PfPR2 ~ R0 | country + country:year | 0 | OBJECTID, data = complete)

summary(model0)
summary(model1)
summary(model2)
summary(model3)
summary(model4)
summary(model1b)
summary(model2b)
summary(model3b)

getfe(model2b,se=TRUE)  -> yearly; yearly[yearly$fe=='year',] -> yearly
yearly$idx <- as.numeric(as.character(yearly$idx))
yearly %>% ggplot(aes(x=idx, y=effect)) + geom_line() + 
  geom_ribbon(aes(ymin=effect-se, ymax=effect+se), fill='light blue', alpha=0.35) + 
  theme_bw() + xlab('Year') + ylab('Fixed effect')



########

full0 <- felm(PfPR2 ~ temp + ppt | 
                OBJECTID + country:year | 0 | OBJECTID, data = complete)
full1 <- felm(PfPR2 ~ temp + temp2 + ppt + ppt2 | 
                OBJECTID + country:year | 0 | OBJECTID, data = complete)
full2 <- felm(PfPR2 ~ temp + temp2 + ppt + ppt2 | 
                OBJECTID + country:year + month | 0 | OBJECTID, data = complete)

summary(full0)
summary(full1)
summary(full2)


tempfit <- function(t, t2) {full2$coefficients['temp',]*t + full2$coefficients['temp2',]*t2}
x<-seq(10, 40, by=0.2); y<-sapply(x, tempfit)
tempdf <- data.frame(temp=x,response=y)
ggplot(tempdf,aes(temp,response)) + geom_line() + 
  ylab('Prevalence predicted') + xlab('Temperature') + theme_classic()
