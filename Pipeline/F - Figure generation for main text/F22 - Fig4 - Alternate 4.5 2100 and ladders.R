
###########################################################################
###########################################################################
###########################################################################
###########################################################################
###########################################################################
###########################################################################
###########################################################################


library(tidyverse); library(magrittr); library(ggplot2); library(data.table)
library(sf)
library(lubridate)
library(rgdal)
library(patchwork)
library(multiscales)
library(vroom)

iter.df <- vroom("~/Github/falciparum/TempFiles/Fig4BigRCP45.csv")

iter.df %>% pivot_wider(names_from = year, values_from = value) %>% 
  mutate(diff = (`2100` - `2015`)) %>%
  select(-c(`2100`,`2050`,`2015`,'RCP')) -> slices.runs

slices.runs %>% ungroup %>% group_by(OBJECTID) %>%
  summarize(mean.diff = mean(diff), runs.diff = sum(diff > 0), 
            lower.diff = quantile(diff, 0.05, na.rm = TRUE), 
            upper.diff = quantile(diff, 0.95, na.rm = TRUE)) ->
  slice.map2

### ADD THE MAP

slice.map2 %<>% mutate(OBJECTID = factor(OBJECTID))
cont <- readOGR('C:/Users/cjcar/Dropbox/MalariaAttribution/Data/AfricaADM1.shp')
cont@data <- left_join(cont@data, slice.map2)

sfcont <- st_as_sf(cont)
sfcont %<>% mutate(moe = 1 - abs(sfcont$runs.diff-5000)/5000)

colors <- scales::colour_ramp(
  colors = c(red = "#AC202F", purple = "#740280", blue = "#2265A3")
)((0:7)/7)

colors <- rev(colors)

library(multiscales)

ggplot(sfcont) + 
  geom_sf(aes(fill = zip(mean.diff, moe)), color = "gray30", size = 0.05) +
  scale_x_continuous(limits = c(-17,52), expand = c(0, 0)) +
  scale_y_continuous(limits = c(-36, 38), expand = c(0, 0)) +
  coord_sf(datum = NA) +
  bivariate_scale("fill",
                  pal_vsup(values = colors, max_desat = 0.8, pow_desat = 0.2, max_light = 0.7, pow_light = 1),
                  name = c("Prevalence (%)", "sign uncertainty"),
                  limits = list(c(-3, 3), c(0, 1)),
                  breaks = list(c(-3, -1.5, 0, 1.5, 3), c(0, 0.25, 0.5, 0.75, 1)),
                  labels = list(waiver(), scales::percent),
                  guide = "colourfan") +
  theme_void() +
  theme(
    legend.position = c(0.18,0.3),
    legend.key.size = grid::unit(0.8, "cm"),
    legend.title.align = 0.5,
    plot.margin = margin(0, 0, 0, 0)
  ) -> map.rcp45.2100


###########################################################################
###########################################################################
###########################################################################
###########################################################################
###########################################################################
###########################################################################
###########################################################################


library(tidyverse); library(magrittr); library(ggplot2); library(data.table)
library(vroom)

data.to.graph <- vroom("~/Github/falciparum/TempFiles/Fig4Regionals.csv")

data.to.graph %>% 
  mutate(Region = recode(Region, !!!c('Sub-Saharan Africa (Central)' = 'Central Africa',
                                      'Sub-Saharan Africa (East)' = 'East Africa',
                                      'Sub-Saharan Africa (Southern)' = 'Southern Africa',
                                      'Sub-Saharan Africa (West)' = 'West Africa'))) %>%
  
  ### Start plotting in 2016 because it's the first full year with lags incorporated right.
  filter(year > 2015) %>% 
  
  
  ############ radioactive code!! BE CAREFUL!! DO NOT LEAVE IN FUTURE VERSIONS WITHOUT LOOKING CLOSELY
  ############ this is a way of hard coding the CI's to still plot thanks to how ggplot does CI's
  ############ this is for plotting purposes ONLY and text stats give full CI's
  mutate(lower = pmax(lower, -4.1), upper = pmin(upper, 2.1)) %>%
  
  ggplot(aes(x = year, group = RCP, color = RCP, fill = RCP)) + 
  geom_line(aes(y=median), lwd = 1.25) + 
  geom_ribbon(aes(ymin=lower, ymax=upper, fill = RCP), color = NA, alpha = 0.1) + 
  scale_color_manual(values = c("#4d5f8e", "#C582B2", "#325756"), 
                     labels = c('Future climate (SSP1-RCP2.6)', 'Future climate (SSP2-RCP4.5)', 'Future climate (SSP5-RCP8.5)'),
                     name = '') + 
  scale_fill_manual(values = c("#4d5f8e", "#C582B2", "#325756"), 
                    labels = c('Future climate (SSP1-RCP2.6)', 'Future climate (SSP2-RCP4.5)', 'Future climate (SSP5-RCP8.5)'),
                    name = '') + 
  theme_classic() +
  theme(plot.title = element_text(size = 18)) + 
  xlab(NULL) + 
  ylab("Prevalence (%)") + 
  geom_hline(aes(yintercept = 0), lty = 2, lwd = 0.5) + 
  facet_wrap(Region ~ ., ncol = 4) + 
  theme(legend.position = 'bottom') + 
  theme(plot.title = element_text(size = 20)) -> bottom


###########################################################################
###########################################################################
###########################################################################
###########################################################################
###########################################################################
###########################################################################
###########################################################################


# Get the stuff 

elev <- read_csv("C:/Users/cjcar/Dropbox/MalariaAttribution/Data/elevation/elevation_extracted_all_ADM1.csv")

cont <- read_sf('C:/Users/cjcar/Dropbox/MalariaAttribution/Data/AfricaADM1.shp')
latlon <- cont %>% 
  mutate(lon = map_dbl(geometry, ~st_point_on_surface(.x)[[1]]),
         lat = map_dbl(geometry, ~st_point_on_surface(.x)[[2]]))
latlon %>%
  as.data.frame() %>%
  select(OBJECTID, lat) %>%
  mutate(OBJECTID = as.numeric(OBJECTID)) -> lat

temp <- read_csv("C:/Users/cjcar/Dropbox/MalariaAttribution/Data/CRU-Reextraction-Aug2022.csv")
temp %>% 
  filter(year %in% c(1901:1930)) %>%
  group_by(OBJECTID) %>% 
  summarize(t = mean(temp, na.rm = TRUE)) -> tmean

slice.map2 %>%
  left_join(elev %>% select(OBJECTID, elevmn) %>% mutate(OBJECTID = as.factor(OBJECTID))) %>%
  left_join(lat %>% mutate(OBJECTID = as.factor(OBJECTID))) %>%
  left_join(tmean %>% mutate(OBJECTID = as.factor(OBJECTID))) -> df

# Generate a nice little significance color scheme
df %>%
  mutate(sign = as.numeric(lower.diff > 0) + -1*as.numeric(upper.diff < 0)) %>%
  mutate(sign = factor(sign)) -> df

df %>%
  na.omit() %>%
  ggplot(aes(x = mean.diff, y = elevmn, color = sign)) + 
  geom_point() + 
  geom_errorbar(aes(xmin = lower.diff, xmax = upper.diff), alpha = 0.5) + 
  geom_vline(xintercept = 0, linetype = 'dashed') + 
  theme_classic() + 
  xlab("Prevalence (%)") + 
  ylab("Elevation (m)") + 
  theme(axis.title.x = element_text(margin = margin(t = 20, b = 10)),
        axis.title.y = element_text(margin = margin(r = 20, l = 10)), 
        legend.position = 'n',
        plot.margin = margin(0,0,10,0)) + 
  scale_color_manual(values = c("#2265A3","grey80","#AC202F")) -> g1

df %>%
  na.omit() %>%
  ggplot(aes(x = mean.diff, y = lat, color = sign)) + 
  geom_point() + 
  geom_errorbar(aes(xmin = lower.diff, xmax = upper.diff), alpha = 0.5) + 
  geom_vline(xintercept = 0, linetype = 'dashed') + 
  theme_classic() + 
  xlab("Prevalence (%)") + 
  ylab("Latitude") + 
  theme(axis.title.x = element_text(margin = margin(t = 20, b = 10)),
        axis.title.y = element_text(margin = margin(r = 20, l = 10)), 
        legend.position = 'n',
        plot.margin = margin(0,0,0,0)) + 
  scale_color_manual(values = c("#2265A3","grey80","#AC202F")) -> g2

df %>%
  na.omit() %>%
  ggplot(aes(x = mean.diff, y = t, color = sign)) + 
  geom_point() + 
  geom_errorbar(aes(xmin = lower.diff, xmax = upper.diff), alpha = 0.5) + 
  geom_vline(xintercept = 0, linetype = 'dashed') + 
  theme_classic() + 
  xlab("Prevalence (%)") + 
  ylab("Mean temperature (1901-1930)") + 
  theme(axis.title.x = element_text(margin = margin(t = 20, b = 10)),
        axis.title.y = element_text(margin = margin(r = 20, l = 10)), 
        legend.position = 'n',
        plot.margin = margin(0,0,0,0)) + 
  scale_color_manual(values = c("#2265A3","grey80","#AC202F")) -> g3



###########################################################################
###########################################################################
###########################################################################
###########################################################################
###########################################################################
###########################################################################
###########################################################################


layout <- "
AAAAAAAAABBCC
AAAAAAAAABBCC
AAAAAAAAABBCC
AAAAAAAAABBCC
AAAAAAAAABBCC
AAAAAAAAABBCC
AAAAAAAAABBCC
AAAAAAAAABBCC
AAAAAAAAABBCC
AAAAAAAAABBCC
AAAAAAAAABBCC
AAAAAAAAABBCC
AAAAAAAAABBCC
AAAAAAAAABBCC
AAAAAAAAABBCC
AAAAAAAAABBCC
AAAAAAAAABBCC
AAAAAAAAABBCC
AAAAAAAAABBCC
AAAAAAAAABBCC
AAAAAAAAABBCC
AAAAAAAAABBCC
AAAAAAAAABBCC
AAAAAAAAABBCC
AAAAAAAAABBCC
AAAAAAAAABBCC
AAAAAAAAABBCC
AAAAAAAAABBCC
AAAAAAAAABBCC
AAAAAAAAABBCC
AAAAAAAAABBCC
AAAAAAAAABBCC
AAAAAAAAABBCC
AAAAAAAAABBCC
AAAAAAAAABBCC
AAAAAAAAABBCC
AAAAAAAAABBCC
AAAAAAAAABBCC
AAAAAAAAABBCC
AAAAAAAAABBCC
AAAAAAAAABBCC
AAAAAAAAABBCC
AAAAAAAAABBCC
AAAAAAAAABBCC
AAAAAAAAABBCC
AAAAAAAAABBCC
AAAAAAAAABBCC
AAAAAAAAABBCC
AAAAAAAAABBCC
AAAAAAAAABBCC
AAAAAAAAABBCC
AAAAAAAAABBCC
AAAAAAAAABBCC
AAAAAAAAABBCC
AAAAAAAAABBCC
AAAAAAAAABBCC
AAAAAAAAABBCC
AAAAAAAAABBCC
AAAAAAAAABBCC
AAAAAAAAABBCC
AAAAAAAAABBCC
AAAAAAAAABBCC
AAAAAAAAABBCC
AAAAAAAAABBCC
AAAAAAAAABBCC
AAAAAAAAABBCC
AAAAAAAAABBCC
AAAAAAAAABBCC
AAAAAAAAABBCC
AAAAAAAAABBCC
AAAAAAAAABBCC
AAAAAAAAABBCC
AAAAAAAAABBCC
AAAAAAAAABBCC
AAAAAAAAABBCC
AAAAAAAAABBCC
AAAAAAAAABBCC
AAAAAAAAABBCC
AAAAAAAAABBCC
AAAAAAAAABBCC
AAAAAAAAABBCC
AAAAAAAAABBCC
AAAAAAAAABBCC
AAAAAAAAABBCC
AAAAAAAAABBCC
AAAAAAAAABBCC
AAAAAAAAABBCC
AAAAAAAAABBCC
AAAAAAAAABBCC
AAAAAAAAABBCC
AAAAAAAAABBCC
AAAAAAAAABBCC
AAAAAAAAABBCC
AAAAAAAAABBCC
AAAAAAAAABBCC
AAAAAAAAABBCC
AAAAAAAAABBCC
AAAAAAAAABBCC
AAAAAAAAABBCC
AAAAAAAAABBCC
AAAAAAAAABBCC
AAAAAAAAABBCC
AAAAAAAAABBCC
AAAAAAAAABBCC
AAAAAAAAABBCC
AAAAAAAAABBCC
AAAAAAAAABBCC
AAAAAAAAABBCC
AAAAAAAAABBCC
AAAAAAAAABBCC
AAAAAAAAABBCC
AAAAAAAAABBCC
AAAAAAAAABBCC
AAAAAAAAABBCC
AAAAAAAAABBCC
AAAAAAAAABBCC
AAAAAAAAABBCC
AAAAAAAAABBCC
AAAAAAAAABBCC
AAAAAAAAABBCC
AAAAAAAAABBCC
AAAAAAAAABBCC
AAAAAAAAABBCC
AAAAAAAAABBCC
AAAAAAAAABBCC
AAAAAAAAABBCC
AAAAAAAAABBCC
AAAAAAAAABBCC
AAAAAAAAABBCC
AAAAAAAAABBCC
AAAAAAAAABBCC
AAAAAAAAABBCC
AAAAAAAAABBCC
AAAAAAAAABBCC
AAAAAAAAABBCC
AAAAAAAAABBCC
AAAAAAAAABBCC
AAAAAAAAABBCC
AAAAAAAAABBCC
AAAAAAAAABBCC
AAAAAAAAABBCC
AAAAAAAAABBCC
AAAAAAAAABBCC
AAAAAAAAABBCC
AAAAAAAAABBCC
AAAAAAAAABBCC
AAAAAAAAABBCC
AAAAAAAAABBCC
AAAAAAAAABBCC
AAAAAAAAABBCC
AAAAAAAAABBCC
AAAAAAAAA####
DDDDDDDDDDDDD
DDDDDDDDDDDDD
DDDDDDDDDDDDD
DDDDDDDDDDDDD
DDDDDDDDDDDDD
DDDDDDDDDDDDD
DDDDDDDDDDDDD
DDDDDDDDDDDDD
DDDDDDDDDDDDD
DDDDDDDDDDDDD
DDDDDDDDDDDDD
DDDDDDDDDDDDD
DDDDDDDDDDDDD
DDDDDDDDDDDDD
DDDDDDDDDDDDD
DDDDDDDDDDDDD
DDDDDDDDDDDDD
DDDDDDDDDDDDD
DDDDDDDDDDDDD
DDDDDDDDDDDDD
DDDDDDDDDDDDD
DDDDDDDDDDDDD
DDDDDDDDDDDDD
DDDDDDDDDDDDD
DDDDDDDDDDDDD
DDDDDDDDDDDDD
DDDDDDDDDDDDD
DDDDDDDDDDDDD
DDDDDDDDDDDDD
DDDDDDDDDDDDD
DDDDDDDDDDDDD
DDDDDDDDDDDDD
DDDDDDDDDDDDD
DDDDDDDDDDDDD
DDDDDDDDDDDDD
DDDDDDDDDDDDD
DDDDDDDDDDDDD
DDDDDDDDDDDDD
DDDDDDDDDDDDD
DDDDDDDDDDDDD
DDDDDDDDDDDDD
DDDDDDDDDDDDD
DDDDDDDDDDDDD
DDDDDDDDDDDDD
DDDDDDDDDDDDD
DDDDDDDDDDDDD
DDDDDDDDDDDDD
DDDDDDDDDDDDD
DDDDDDDDDDDDD
DDDDDDDDDDDDD
DDDDDDDDDDDDD
DDDDDDDDDDDDD
DDDDDDDDDDDDD
DDDDDDDDDDDDD
DDDDDDDDDDDDD
DDDDDDDDDDDDD
DDDDDDDDDDDDD
DDDDDDDDDDDDD
DDDDDDDDDDDDD
DDDDDDDDDDDDD
DDDDDDDDDDDDD
DDDDDDDDDDDDD
DDDDDDDDDDDDD
DDDDDDDDDDDDD
DDDDDDDDDDDDD
DDDDDDDDDDDDD
DDDDDDDDDDDDD
DDDDDDDDDDDDD
DDDDDDDDDDDDD
DDDDDDDDDDDDD
DDDDDDDDDDDDD
DDDDDDDDDDDDD
DDDDDDDDDDDDD
DDDDDDDDDDDDD
DDDDDDDDDDDDD
DDDDDDDDDDDDD
DDDDDDDDDDDDD
DDDDDDDDDDDDD
DDDDDDDDDDDDD
DDDDDDDDDDDDD
"


map.rcp45.2100 + g3 + g1 + bottom +
  plot_layout(design = layout) +
  plot_annotation(tag_levels = 'A') &
  theme(plot.tag = element_text(size = 23))
