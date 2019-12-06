
library(cowplot)
library(ggplot2)
library(ggthemr)
library(lfe)
library(reshape)
library(tidyverse)

contdf <- cont@data
contdf <- contdf[,-c(2:15)]
contdf <- melt(contdf, id='OBJECTID')

dffix <- separate(contdf,  variable, into=c('month','year','var'), sep='\\.')
dffix <- cast(dffix, OBJECTID+month+year ~ var)
dffix$year <- as.numeric(dffix$year)
write.csv(dffix, 'dffix-backup.csv')

complete <- dffix[complete.cases(dffix),]

countrydf <- unique(cont@data[,c('OBJECTID','NAME_0')])
complete$country <- countrydf$NAME_0[sapply(complete$OBJECTID, function(x){which(countrydf$OBJECTID==x)})]

############################

full.model <- felm(PfPR2 ~ temp + temp2 + ppt + ppt2 | 
                OBJECTID + country:year + month | 0 | OBJECTID, data = complete)

summary(full.model)









#####################


complete$Pf3 <- complete$PfPR2/100

x<-seq(10, 40, by=0.2); y<-sapply(x, r0t)
tempdf <- data.frame(temp=x,response=y)

tempfit2 <- function(t) {full.model$coefficients['temp',]*t + full.model$coefficients['temp2',]*t*t}
x<-seq(10, 40, by=0.2); y<-sapply(x, tempfit2)
tempdf2 <- data.frame(temp=x,response=y)

#ggthemr("fresh")
theme_set(theme_classic())  # not run gg


g1 <- ggplot(complete, aes(temp, Pf3)) + #geom_point(col='light grey') + 
  geom_smooth(col='turquoise3', lwd=1.2, fill='paleturquoise2', alpha=0.5) + ylab('P. falciparum prevalence') + 
  xlab('Temperature') + theme_classic2()  + 
  theme(text = element_text(size=13)) + 
  labs(title = 'Observed prevalence model', subtitle='African dataset (1900-2010)'); g1

g2 <- ggplot(tempdf2,aes(temp,response)) + geom_line(col='turquoise3', lwd=1.2) + 
  ylab('Prevalence predicted') + xlab('Temperature') + 
  theme(text = element_text(size=13)) + theme_classic2()  + 
  labs(title='Full econometric model', subtitle='At ADM1 level (1900-2010)')

g3 <- ggplot(tempdf,aes(temp,response)) + geom_line(col='turquoise3', lwd=1.3) + 
  ylab('Estimated R0') + xlab('Temperature')  + 
  theme(text = element_text(size=13)) + theme_classic2()  + 
  labs(title = 'Predicted R0', subtitle='Based on laboratory experiments') 

g4 <- ggplot(complete, aes(R0, Pf3)) + # geom_point(col='light grey') + 
  geom_smooth() + ylab('Observed prevalence of P. falciparum') + 
  xlab('Estimated scaled R0 based on temp.') + theme_classic()  + 
  theme(text = element_text(size=13)) + theme_classic2()  + 
  labs(title = 'Observed prevalence model', subtitle='Using R0 estimator') 

plot_grid(g1, g2, g3, g4)

plot_grid(g3, g1, g2, nrow=1)
##################

complete$predR0 <- r0t(complete$temp)

ggthemr('pale')
ggplot(data=complete, aes(x=temp), fill=NA) + 
  geom_line(aes(y=predR0, color='Predicted'), lwd=1.1) + 
  geom_smooth(aes(y=Pf3*3, color='Observed'), lwd=1.1, fill='light grey') + 
  xlim(15,35) + ylab('R0 predicted ') + xlab('Temperature') + 
  theme(text = element_text(size=15),
        axis.title.y = element_text(margin = margin(t = 0, r = 20, b = 0, l = 20)),
        axis.title.y.right = element_text(margin = margin(t = 0, r = 20, b = 0, l = 20)),
        axis.title.x = element_text(margin = margin(t = 20, r= 0, b = 20, l = 0)),
        plot.title = element_text(margin = margin(t = 20, r= 0, b = 0, l = 0),
                                  size = 20, hjust = 0.5),
        legend.position='top',
        legend.key = element_rect(colour = NA, fill = 'white'))+ 
  labs(title='Observed vs. theoretical temperature sensitivity', subtitle='')  +
  scale_y_continuous(sec.axis = sec_axis(~./3, name = "Observed prevalence (1900-2010)")) +
  scale_color_discrete(name="") + guides(color=guide_legend(override.aes=list(fill=NA)))











ggplot() + #theme_classic() + 
  geom_line(data=tempdf, aes(temp,response, fill='Theoretical'), lwd=1.3, color = "grey50")  +
  geom_smooth(data=complete, aes(temp, Pf3*3, fill='Observed'), color = 'dark blue') + xlim(15,35) + 
  ylab('R0 predicted ') + xlab('Temperature') + 
  theme(text = element_text(size=15),
        axis.title.y = element_text(margin = margin(t = 0, r = 20, b = 0, l = 20)),
        axis.title.y.right = element_text(margin = margin(t = 0, r = 20, b = 0, l = 20)),
        axis.title.x = element_text(margin = margin(t = 20, r= 0, b = 20, l = 0)),
        plot.title = element_text(margin = margin(t = 20, r= 0, b = 0, l = 0),
                                  size = 20, hjust = 0.5),
        legend.position='top') + 
  labs(title='Observed vs. theoretical temperature sensitivity', subtitle='')  +
  scale_y_continuous(sec.axis = sec_axis(~./3, name = "Observed prevalence (1900-2010)")) 
  
  #ylab('Estimated R0') + xlab('Temperature')  + 
  #theme(text = element_text(size=13)) + 
  #labs(title = 'Predicted R0', subtitle='Based on laboratory experiments') 








###################################################################
#getfe(full.model,se=TRUE)  -> effects
#effects[effects$fe=='month',] -> monthly
#monthly$idx <- as.numeric(as.character(monthly$idx))
#monthly %>% ggplot(aes(x=idx, y=effect)) + geom_line() + 
#  geom_ribbon(aes(ymin=effect-se, ymax=effect+se), fill='light blue', alpha=0.35) + 
#  theme_bw() + xlab('Month') + ylab('Fixed effect')



