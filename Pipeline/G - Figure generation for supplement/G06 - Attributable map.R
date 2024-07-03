
library(sf)
library(here)
library(tidyverse)
library(patchwork)
library(colorspace)
library(multiscales)

###########################################################################.
###########################################################################.
###########################################################################.
###########################################################################.
###########################################################################.
###########################################################################.
###########################################################################.

source(here::here("Pipeline", "A - Utility functions", "A00 - Configuration.R"))

cont <- sf::read_sf(file.path(datadir, 'Data', 'AfricaADM1.shp'))|> 
  dplyr::mutate(OBJECTID = as.numeric(OBJECTID))

iter.df <- here::here("TempFiles", "Fig3Big.feather")|> 
  arrow::read_feather() |> 
  dplyr::filter(year == 2014) |> 
  tidyr::pivot_wider(names_from = scenario, values_from = Pred) |> 
  dplyr::mutate(diff = (historical - `hist-nat`)) |>
  dplyr::group_by(OBJECTID) |>
  dplyr::summarize(
    mean.diff = mean(diff, na.rm = TRUE), 
    lower.diff = quantile(diff, 0.05, na.rm = TRUE),
    upper.diff = quantile(diff, 0.95, na.rm = TRUE)
  ) 

cont <- dplyr::left_join(cont, iter.df)

ggplot(cont) + 
  geom_sf(aes(fill = mean.diff), color = "gray30", size = 0.05) +
  coord_sf(datum = NA, xlim = c(-17.5, 52), ylim = c(-35.5, 37.5)) + 
  scale_fill_continuous_divergingx(palette = "Geyser", na.value = "white") +
  labs(fill = "Change (%)") + 
  theme_void() + 
  theme(
    legend.position = "inside",
    legend.position.inside = c(0.15, 0.25)
  ) ->
  g1

cont |>
  mutate(sign = as.numeric(lower.diff > 0) + -1*as.numeric(upper.diff < 0)) |>
  mutate(sign = factor(sign)) -> cont

ggplot(cont) + 
  geom_sf(aes(fill = sign), color = "gray30", size = 0.05) +
  coord_sf(datum = NA, xlim = c(-17.5, 52), ylim = c(-35.5, 37.5)) + 
  scale_fill_manual(
    values = c("#00AFBB","grey80","#fa5340"),
    labels = c('Decline','Insignificant','Increase'),
    na.value = "white", na.translate = F) +
  labs(fill = "Significance") + 
  theme_void() + 
  theme(
    legend.position = "inside",
    legend.position.inside =   c(0.15, 0.25)
  )-> 
  g2 

g1 + g2 

### Supplemental Figure 2

cont |>
  mutate(sign = as.numeric(lower.diff > 0) + as.numeric(upper.diff < 0)) |>
  mutate(sign = replace_na(sign, 0)) |>
  arrange(-sign) |>
  mutate(sign = as.factor(sign)) -> 
  cont

cont |> 
  select(sign) |>
  filter(sign == 1) |>
  st_make_valid() |>
  st_union() |>
  st_make_valid() |>
  st_union() -> 
  top

supp_2 <- ggplot(cont) +
  geom_sf(aes(fill = mean.diff), color = 'grey70', size = 0.05) +
  coord_sf(datum = NA,
           xlim = c(-17.5, 52),
           ylim = c(-35.5, 37.5)) +
  theme_void() +
  scale_fill_continuous_divergingx(palette = "Geyser", na.value = "white") +
  labs(fill = "Prevalence (%)")+
  geom_sf(data = top, color = 'black', size = 0.25, fill = NA) +
  theme(
    legend.position = "inside",
    legend.position.inside = c(0.25, 0.35)
  ) 

ggplot2::ggsave(
  filename = "FigureS2_new.pdf",
  plot = supp_2,
  device = cairo_pdf,
  path = here::here("Figures"),
  width = 9.53,
  height = 10.07,
  units = "in",
  dpi = 1200
)
