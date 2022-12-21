
library(magrittr)
library(tidyverse)
library(lubridate)

setwd('C:/Users/cjcar/Dropbox/MalariaAttribution')

######################## LOAD REAL DATA

#### Read in the data backup
data <- read.csv('./Dataframe backups/formatted-backup.csv')
#### STANDARD FILTERING & AUGMENTING
spatial <- read.csv('./Dataframe backups/shapefile-backup.csv')
countrydf <- unique(spatial[,c('OBJECTID','NAME_0')])
data$country <- countrydf$NAME_0[sapply(data$OBJECTID, function(x){which(countrydf$OBJECTID==x)})]
data$country <- countrydf$NAME_0[sapply(data$OBJECTID, function(x){which(countrydf$OBJECTID==x)})]

######################## LOAD MODEL OUTPUTS

coef <- readRDS("./Results/Models/coefficients_cXt2intrXm.rds")
coef.t1 <- coef[1,]
coef.t2 <- coef[2,]

all.files <- list.files('./ClimateCSVs', full.names = TRUE)
nat.files <- list.files('./ClimateCSVs', pattern="nat.csv", full.names = TRUE)
hist.files <- all.files[!(all.files %in% nat.files)]

read_plus <- function(flnm) {
  read_csv(flnm) %>% 
    mutate(run = flnm)
}

nat.files %>% 
  map_df(~read_plus(.)) %>% 
  mutate(run = gsub('./ClimateCSVs/','', run)) %>%
  mutate(run = gsub('-nat.csv','', run)) %>% 
  mutate(scenario = 'nat') -> 
  natdf

hist.files %>% 
  map_df(~read_plus(.)) %>% 
  mutate(run = gsub('./ClimateCSVs/','', run)) %>%
  mutate(run = gsub('.csv','', run)) %>% 
  mutate(scenario = 'hist') -> 
  histdf

all <- bind_rows(natdf, histdf)

all %<>% mutate(Pf.temp = (temp*coef.t1 + temp2*coef.t2))

# Add country and region info

read_csv('./Dataframe backups/shapefile-backup.csv') %>%
  select(OBJECTID, NAME_0) %>%
  unique() %>% 
  dplyr::rename(Country = NAME_0) -> 
  countrydf

all %<>% left_join(countrydf)

# Plot Kenya temp effects

all %>% filter(Country == 'Kenya') %>%
  select(year, Pf.temp, run, scenario) %>%
  group_by(year, run, scenario) %>% 
  summarize(Pf.temp = mean(Pf.temp, na.rm = TRUE)) %>% 
  ggplot(aes(x = year, y = Pf.temp)) + 
  theme_bw() + 
  geom_smooth(aes(color = scenario, group = scenario)) + 
  geom_line(aes(color = scenario,
                group = interaction(run,scenario)),
            alpha = 0.3)

models <- all 

######

data %<>% mutate(year = as.numeric(as.character(year)))

all.data <- left_join(models, data, by = c('OBJECTID', 'month', 'year'))

all.data %>% unite("my", month:year, sep=' ') %>%
  mutate(date = decimal_date(parse_date_time(my, "%m %Y"))) -> all.data
  
# 1. Pull out those mean scenario trajectories

all.data %>% filter(Country == 'Kenya') %>%
  select(date, Pf.temp, run, scenario) %>%
  mutate(year = floor(date)) %>% 
  group_by(year, run, scenario) %>% 
  summarize(Pf.temp.mean = mean(Pf.temp, na.rm = TRUE)) -> kenya.scenario 

# 2. Pull out the points

all.data %>% filter(Country == "Kenya") %>%
  select(date, PfPR2) %>% unique() %>% na.omit() -> kenya.true

# 3. Plot a scatterplot of the data, and then add the lines on top?

g1 <- ggplot(kenya.true, aes(x = date, y = PfPR2)) + geom_point(col = 'grey') + 
         geom_smooth(col = 'black') + theme_bw() + 
         xlab(NULL) + ylab("P. falciparum prevalence")

g2 <- ggplot(kenya.scenario, aes(x = year, y = Pf.temp.mean)) + 
         theme_bw() + xlab("Year") + ylab("Partial effect of temperature") + 
         geom_smooth(aes(color = scenario, group = scenario)) #+ 
         #geom_line(aes(color = scenario,
         #       group = interaction(run,scenario)),
         #   alpha = 0.3)


library(patchwork)
g1/g2            
