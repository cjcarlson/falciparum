
library(sf)
library(lubridate)
library(magrittr)
# library(rgdal)
library(tidyverse)
library(patchwork)
library(data.table)
library(tictoc)

###########################################################################
###########################################################################
###########################################################################
###########################################################################
###########################################################################
###########################################################################
###########################################################################

# setwd("D:/MalariaAfrica/HistoricalTempFiles")

# setDTthreads(1L)
# meta <- fread("RowMetadata.csv", select = c("year", "scenario", "model"))


# mode <- "historical"
mode <- "future"

source(here::here("Pipeline", "A - Utility functions", "A00 - Configuration.R"))
source(here::here(pipeline_A_dir, "A01 - Utility code for calculations.R"))

if (mode == "historical") {
  temp_files <- "HistoricalTempFiles"
  save_fn <- "Fig2Hist.csv"
  yr_filter <- c(1900:1930)
} else {
  temp_files <- "FutureTempFiles"
  save_fn <- "Fig2Future.csv"
  yr_filter <- c(2015:2019)
}
temp_dir <- file.path(datadir, "IterationFiles", temp_files)

meta <- file.path(temp_dir, "RowMetadata.feather") |> 
  arrow::read_feather(col_select = c(year, scenario, model)) 

tictoc::tic()
for (i in 1:1000) { 
  tictoc::tic()
  # i <- 2
  iter <- file.path(temp_dir, paste0("iter_", i, ".feather")) |> 
    arrow::read_feather(col_select = "Pred") |> 
    data.table::as.data.table() |> 
    dplyr::bind_cols(meta)
  iter <-  iter[,list(Pred = mean(Pred, na.rm = TRUE)), by = 'scenario,model,year']
  iter$run <- i
  if(i==1) {iter.df <- iter} else {iter.df <- dplyr::bind_rows(iter.df, iter)}
  tictoc::toc()
} 
tictoc::toc()

iter.df %<>% as_tibble()

iter.df |>
  filter(year %in% yr_filter) |># This is actually 1901 to 1930 in practice
  group_by(model, run, scenario) %>%
  summarize(BetaMean = mean(Pred, na.rm = TRUE)) |>
  right_join(iter.df) |>
  mutate(Pred = (Pred-BetaMean)) %>%
  select(-BetaMean) -> df

df %>%
  group_by(scenario, year) %>%
  summarize(median = median(Pred, na.rm = TRUE),
            upper = quantile(Pred, 0.95, na.rm = TRUE),
            lower = quantile(Pred, 0.05, na.rm = TRUE)) -> hist.to.graph

write_csv(hist.to.graph, here::here("TempFiles", save_fn))
# rm(meta)



hist.to.graph <- readr::read_csv( here::here("TempFiles", save_fn))

# hist.to.graph %>%
#   # mutate(scenario = factor(scenario, levels = c('hist-nat', 'historical'))) %>%
#   mutate(scenario = factor(scenario, levels = scenarios)) %>%
#   ggplot(aes(x = year, group = scenario, color = scenario, fill = scenario)) +
#   geom_line(aes(y=median), lwd = 1.25) +
#   geom_ribbon(aes(ymin=lower, ymax=upper, fill = scenario), color = NA, alpha = 0.1) +
#   theme_classic() +
#   geom_hline(aes(yintercept = 0), lty = 2, lwd = 0.5) +
#   xlab(NULL) +
#   ylab("Predicted change in prevalence (%)") +
#   
#   # scale_color_manual(values = c("grey50", "#287DAB"),
#   #                    labels = c('Historical counterfactual', 'Historical climate'),
#   #                    name = '') +
#   # scale_fill_manual(values = c("grey50", "#287DAB"),
#   #                   labels = c('Historical counterfactual', 'Historical climate'),
#   #                   name = '') +
#   
#   
#   scale_color_manual(values = c("#4d5f8e", "#C582B2", "#325756"),
#                      labels = c('Future climate (RCP 2.6)', 'Future climate (RCP 4.5)', 'Future climate (RCP 8.5)'),
#                      name = '') +
#   scale_fill_manual(values = c("#4d5f8e", "#C582B2", "#325756"),
#                     labels = c('Future climate (RCP 2.6)', 'Future climate (RCP 4.5)', 'Future climate (RCP 8.5)'),
#                     name = '') +
#   theme(legend.position = 'bottom')

###########################################################################
###########################################################################
###########################################################################
###########################################################################
###########################################################################
###########################################################################
###########################################################################

# setwd("D:/MalariaAfrica/FutureTempFiles")
# meta <- fread("RowMetadata.csv", select = c("year", "run"))
# 
# for (i in 1:1000) { 
#   iter <- fread(paste(paste("iter", i, sep=""), ".csv", sep = ""), select = "Pred")
#   iter <- bind_cols(meta, iter)
#   iter <-  iter[,list(Pred = mean(Pred, na.rm = TRUE)), by = 'run,year']
#   if(i==1) {iter.df <- iter} else {iter.df <- bind_cols(iter.df, iter$Pred)}
# } 
# 
# iter.df %<>% as_tibble()
# iter.df %>%
#   pivot_longer(cols = -c(run, year), names_to = c("Pred")) %>%
#   tidyr::extract(run, 
#                  into = c('model','RCP'),
#                  regex = "(ACCESS-CSM2|ACCESS-ESM1|BCC-CSM2-MR|CanESM5|FGOALS-g3|GFDL-ESM4|IPSL-CM6A-LR|MIROC6|MRI-ESM2-0|NorESM2-LM)-(rcp26|rcp45|rcp85)",
#                  remove = FALSE) -> iter.df2
# 
# iter.df2 |>
#   filter(year %in% c(2015:2019)) %>%
#   group_by(model, Pred, RCP) %>%
#   summarize(BetaMean = mean(value, na.rm = TRUE)) |>
#   right_join(iter.df2) |>
#   mutate(value = (value-BetaMean)) %>%
#   select(-BetaMean) -> df
# 
# df %>%
#   group_by(RCP, year) %>%
#   summarize(median = median(value),
#             upper = quantile(value, 0.95),
#             lower = quantile(value, 0.05)) -> future.to.graph
# 
# write_csv(future.to.graph, "~/Github/falciparum/TempFiles/Fig2Future.csv")
# rm(meta)

# future.to.graph %>%
#   mutate(scenario = factor(RCP, levels = c('rcp26', 'rp45', 'rcp85'))) %>%
#   ggplot(aes(x = year, group = scenario, color = scenario, fill = scenario)) +
#   geom_line(aes(y=median), lwd = 1.25) +
#   geom_ribbon(aes(ymin=lower, ymax=upper, fill = scenario), color = NA, alpha = 0.1) +
#   theme_classic() +
#   geom_hline(aes(yintercept = 0), lty = 2, lwd = 0.5) +
#   xlab(NULL) +
#   ylab("Predicted change in prevalence (%)") +
#   scale_color_manual(values = c("#4d5f8e", "#C582B2", "#325756"), 
#                      labels = c('Future climate (RCP 2.6)', 'Future climate (RCP 4.5)', 'Future climate (RCP 8.5)'),
#                      name = '') + 
#   scale_fill_manual(values = c("#4d5f8e", "#C582B2", "#325756"), 
#                     labels = c('Future climate (RCP 2.6)', 'Future climate (RCP 4.5)', 'Future climate (RCP 8.5)'),
#                     name = '') + 
#   theme(legend.position = 'bottom')
