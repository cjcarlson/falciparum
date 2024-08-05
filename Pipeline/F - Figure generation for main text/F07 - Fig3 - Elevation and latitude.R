
iter.df <- here::here("TempFiles", "Fig3Big.feather") |> 
  arrow::read_feather() |>
  dplyr::mutate(model = stringr::str_replace_all(model,'BCC-CSM2-MR','BCC-CSM2')) 

iter.df |> 
  filter(year == 2014) ->
  slices

slices |> 
  pivot_wider(names_from = scenario, values_from = Pred) |> 
  mutate(diff = (historical - `hist-nat`)) |>
  select(-c(historical, `hist-nat`, year)) -> 
  slices.runs

slices.runs |> 
  group_by(OBJECTID) |>
  summarize(
    mean.diff = mean(diff, na.rm = TRUE), 
    lower.diff.90 = quantile(diff, 0.05, na.rm = TRUE), 
    upper.diff.90 = quantile(diff, 0.95, na.rm = TRUE),
    lower.diff.95 = quantile(diff, 0.025, na.rm = TRUE), 
    upper.diff.95 = quantile(diff, 0.975, na.rm = TRUE)
  ) ->
  slice.map1

###########################
###########################
###########################
###########################

# Get the stuff 
elev <- file.path(datadir, "Data", "elevation", "elevation_extracted_all_ADM1.csv") |> 
  readr::read_csv(show_col_types = FALSE) |> 
  dplyr::select(OBJECTID, elevmn)

cont <- file.path(datadir, 'Data', 'AfricaADM1.shp') |> 
  sf::read_sf()

latlon <- cont |> 
  mutate(lon = map_dbl(geometry, ~st_point_on_surface(.x)[[1]]),
         lat = map_dbl(geometry, ~st_point_on_surface(.x)[[2]]))
latlon |>
  as.data.frame() |>
  select(OBJECTID, lat) |>
  mutate(OBJECTID = as.numeric(OBJECTID)) -> lat

temp <- file.path(datadir, "Data", "CRU-Reextraction-Aug2022.csv") |> 
  readr::read_csv(show_col_types = FALSE) 
temp |> 
  filter(year %in% c(1901:1930)) |>
  group_by(OBJECTID) |> 
  summarize(t = mean(temp, na.rm = TRUE)) -> 
  tmean

###########################
###########################
###########################
###########################
###########################
###########################

slice.map1 |>
  left_join(elev) |>
  left_join(lat) |>
  left_join(tmean) -> 
  df

# Generate a nice little significance color scheme
df |>
  mutate(
    sign = as.numeric(lower.diff.90 > 0) + -1*as.numeric(upper.diff.90 < 0),
    sign = factor(sign)
  ) ->
  df

# After creating your df dataframe, split it into two based on significance
df_non_sig <- df %>% filter(sign == 0)
df_sig <- df %>% filter(sign != 0)

#### This version orders the colors such that the grey lines are plotted first
#### then the red and blue lines are plotted on top.

g1 <- ggplot() +
  geom_errorbar(
    data = df_non_sig, aes(x = mean.diff, y = elevmn, xmin = lower.diff.95, xmax = upper.diff.95), 
    color = "grey80", alpha = 0.3, size = 0.5) +
  geom_errorbar(
    data = df_non_sig, aes(x = mean.diff, y = elevmn, xmin = lower.diff.90, xmax = upper.diff.90), 
    color = "grey80", alpha = 0.5, size = 0.7) +
  geom_point(
    data = df_non_sig, aes(x = mean.diff, y = elevmn), 
    color = "grey80") +
  geom_errorbar(
    data = df_sig, aes(x = mean.diff, y = elevmn, xmin = lower.diff.95, xmax = upper.diff.95, color = sign), 
    alpha = 0.3, size = 0.5) +
  geom_errorbar(
    data = df_sig, aes(x = mean.diff, y = elevmn, xmin = lower.diff.90, xmax = upper.diff.90, color = sign), 
    alpha = 0.5, size = 0.7) +
  geom_point(data = df_sig, aes(x = mean.diff, y = elevmn, color = sign)) +
  geom_vline(xintercept = 0, linetype = 'dashed') + 
  theme_classic() + 
  xlab("Prevalence (%)") + 
  ylab("Elevation (m)") + 
  theme(axis.title.x = element_text(margin = margin(t = 20, b = 10)),
        axis.title.y = element_text(margin = margin(r = 20, l = 10)), 
        legend.position = 'n',
        plot.margin = margin(0,0,10,0)) +
  scale_color_manual(values = c("-1" = "#2265A3", "1" = "#AC202F"))



g2 <- ggplot() +
  geom_errorbar(
    data = df_non_sig, aes(x = mean.diff, y = lat, xmin = lower.diff.95, xmax = upper.diff.95), 
    color = "grey80", alpha = 0.3, size = 0.5) +
  geom_errorbar(
    data = df_non_sig, aes(x = mean.diff, y = lat,  xmin = lower.diff.90, xmax = upper.diff.90), 
    color = "grey80", alpha = 0.5, size = 0.7) +
  geom_point(
    data = df_non_sig, aes(x = mean.diff, y = lat), 
    color = "grey80") +
  geom_errorbar(
    data = df_sig, aes(x = mean.diff, y = lat, xmin = lower.diff.95, xmax = upper.diff.95, color = sign), 
    alpha = 0.3, size = 0.5) +
  geom_errorbar(
    data = df_sig, aes(x = mean.diff, y = lat, xmin = lower.diff.90, xmax = upper.diff.90, color = sign), 
    alpha = 0.5, size = 0.7) +
  geom_point(data = df_sig, aes(x = mean.diff, y = lat, color = sign)) +
  geom_vline(xintercept = 0, linetype = 'dashed') + 
  theme_classic() + 
  xlab("Prevalence (%)") + 
  ylab("Latitude") + 
  theme(axis.title.x = element_text(margin = margin(t = 20, b = 10)),
        axis.title.y = element_text(margin = margin(r = 20, l = 10)), 
        legend.position = 'n',
        plot.margin = margin(0,0,0,0)) +
  scale_color_manual(values = c("-1" = "#2265A3", "1" = "#AC202F"))


g3 <- ggplot() +
  geom_errorbar(
    data = df_non_sig, aes(x = mean.diff, y = t, xmin = lower.diff.95, xmax = upper.diff.95), 
    color = "grey80", alpha = 0.3, size = 0.5) +
  geom_errorbar(
    data = df_non_sig, aes(x = mean.diff, y = t,  xmin = lower.diff.90, xmax = upper.diff.90), 
    color = "grey80", alpha = 0.5, size = 0.7) +
  geom_point(
    data = df_non_sig, aes(x = mean.diff, y = t), 
    color = "grey80") +
  geom_errorbar(
    data = df_sig, aes(x = mean.diff, y = t,  xmin = lower.diff.95, xmax = upper.diff.95, color = sign), 
    alpha = 0.3, size = 0.5) +
  geom_errorbar(
    data = df_sig, aes(x = mean.diff, y = t, xmin = lower.diff.90, xmax = upper.diff.90, color = sign), 
    alpha = 0.5, size = 0.7) +
  geom_point(data = df_sig, aes(x = mean.diff, y = t, color = sign)) +
  geom_vline(xintercept = 0, linetype = 'dashed') + 
  theme_classic() + 
  xlab("Prevalence (%)") + 
  ylab("Mean temperature (1901-1930)") + 
  theme(axis.title.x = element_text(margin = margin(t = 20, b = 10)),
        axis.title.y = element_text(margin = margin(r = 20, l = 10)), 
        legend.position = 'n',
        plot.margin = margin(0,0,0,0)) +
  scale_color_manual(values = c("-1" = "#2265A3", "1" = "#AC202F"))



#### This commented version of the plots produces the same result as the above
#### with the exception that the colors are not as nice. This means there is no 
#### order to the colors, unlike the final version which has the colors ordered
#### such that grey lines are plotted first, and red and blue on top

# clrs <- c("-1" = "#2265A3", "0" = "grey80", "1" = "#AC202F")
#   
# df |>
#   na.omit() |>
#   ggplot(aes(x = mean.diff, y = elevmn, color = sign)) + 
#   geom_point() + 
#   geom_errorbar(aes(xmin = lower.diff.95, xmax = upper.diff.95), alpha = 0.3, linewidth = 0.5) +  # 95% CI
#   geom_errorbar(aes(xmin = lower.diff.90, xmax = upper.diff.90), alpha = 0.5, linewidth = 0.7) +  # 90% CI
#   # geom_errorbar(aes(xmin = lower.diff.90, xmax = upper.diff.90), alpha = 0.5) +
#   geom_vline(xintercept = 0, linetype = 'dashed') + 
#   theme_classic() + 
#   xlab("Prevalence (%)") + 
#   ylab("Elevation (m)") + 
#   theme(axis.title.x = element_text(margin = margin(t = 20, b = 10)),
#         axis.title.y = element_text(margin = margin(r = 20, l = 10)), 
#         legend.position = 'n',
#         plot.margin = margin(0,0,10,0)) + 
#   scale_color_manual(values = clrs) -> 
#   g1
# 
# df |>
#   na.omit() |>
#   ggplot(aes(x = mean.diff, y = lat, color = sign)) + 
#   geom_point() + 
#   geom_errorbar(aes(xmin = lower.diff.95, xmax = upper.diff.95), alpha = 0.3, linewidth = 0.3) +  # 95% CI
#   geom_errorbar(aes(xmin = lower.diff.90, xmax = upper.diff.90), alpha = 0.5, linewidth = 0.5) +  # 90% CI
#   # geom_errorbar(aes(xmin = lower.diff.90, xmax = upper.diff.90), alpha = 0.5) + 
#   geom_vline(xintercept = 0, linetype = 'dashed') + 
#   theme_classic() + 
#   xlab("Prevalence (%)") + 
#   ylab("Latitude") + 
#   theme(axis.title.x = element_text(margin = margin(t = 20, b = 10)),
#         axis.title.y = element_text(margin = margin(r = 20, l = 10)), 
#         legend.position = 'n',
#         plot.margin = margin(0,0,0,0)) + 
#   scale_color_manual(values = clrs) -> 
#   g2
# 
# df |>
#   na.omit() |>
#   ggplot(aes(x = mean.diff, y = t, color = sign)) + 
#   geom_point() + 
#   geom_errorbar(aes(xmin = lower.diff.95, xmax = upper.diff.95), alpha = 0.3, linewidth = 0.5) +  # 95% CI
#   geom_errorbar(aes(xmin = lower.diff.90, xmax = upper.diff.90), alpha = 0.5, linewidth = 0.7) +  # 90% CI
#   # geom_errorbar(aes(xmin = lower.diff.90, xmax = upper.diff.90), alpha = 0.5) + 
#   geom_vline(xintercept = 0, linetype = 'dashed') + 
#   theme_classic() + 
#   xlab("Prevalence (%)") + 
#   ylab("Mean temperature (1901-1930)") + 
#   theme(axis.title.x = element_text(margin = margin(t = 20, b = 10)),
#         axis.title.y = element_text(margin = margin(r = 20, l = 10)), 
#         legend.position = 'n',
#         plot.margin = margin(0,0,0,0)) + 
#   scale_color_manual(values = clrs) ->
#   g3



























