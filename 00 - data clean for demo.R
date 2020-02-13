demo <- complete[,c('country','OBJECTID','month','year','PfPR2','ppt','ppt2','temp','temp2')]
head(demo)

colnames(demo) <- c('country','ADM1','month','year',
                    'PfPR','ppt','ppt2','temp','temp2')
s