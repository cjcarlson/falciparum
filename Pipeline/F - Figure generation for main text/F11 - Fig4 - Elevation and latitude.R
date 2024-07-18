
elev <- file.path(datadir, "Data", "elevation", "elevation_extracted_all_ADM1.csv") |> 
  readr::read_csv(show_col_types = FALSE)

cont <- file.path(datadir, "Data", "AfricaADM1.shp") |>
  read_sf()
latlon <- cont %>% 
  mutate(lon = map_dbl(geometry, ~st_point_on_surface(.x)[[1]]),
         lat = map_dbl(geometry, ~st_point_on_surface(.x)[[2]]))
latlon %>%
  as.data.frame() %>%
  select(OBJECTID, lat) %>%
  mutate(OBJECTID = as.numeric(OBJECTID)) -> 
  lat

temp <- file.path(datadir, "Data", "CRU-Reextraction-Aug2022.csv") |> 
  readr::read_csv(show_col_types = FALSE)
temp %>% 
  filter(year %in% c(1901:1930)) %>%
  group_by(OBJECTID) %>% 
  summarize(t = mean(temp, na.rm = TRUE)) -> tmean

slice.map2 %>%
  left_join(elev %>% select(OBJECTID, elevmn) %>% mutate(OBJECTID = as.factor(OBJECTID))) %>%
  left_join(lat %>% mutate(OBJECTID = as.factor(OBJECTID))) %>%
  left_join(tmean %>% mutate(OBJECTID = as.factor(OBJECTID))) -> 
  df

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
  scale_color_manual(values = c("#2265A3","grey80","#AC202F")) ->
  g1

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
  scale_color_manual(values = c("#2265A3","grey80","#AC202F")) -> 
  g2

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
  scale_color_manual(values = c("#2265A3","grey80","#AC202F")) -> 
  g3