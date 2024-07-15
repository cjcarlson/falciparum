# 
# library(tidyverse)
# library(arrow)
# library(here)
# library(data.table)
# 
# iter.df <- here::here("TempFiles", "SuppFutureBig.feather") |>
#   arrow::read_feather()
# 
# iter.df %>%
#   filter(year %in% c(2015:2020)) %>%
#   group_by(model, scenario, iter) %>%
#   summarize(BetaMean = mean(Pred, na.rm = TRUE)) ->
#   bm
# 
# iter.df %>%
#   left_join(bm) %>%
#   mutate(Pred = (Pred-BetaMean)) %>%
#   select(-BetaMean) ->
#   df
# 
# df %>%
#   filter(year %in% c(2048:2052)) %>%
#   group_by(iter, model, scenario) %>%
#   summarize(Pred = mean(Pred)) %>%
#   ungroup() %>%
#   group_by(scenario) %>%
#   summarize(
#     mean = mean(Pred),
#     lower = quantile(Pred, 0.025),
#     upper = quantile(Pred, 0.975))
# 
# df %>%
#   filter(year %in% c(2096:2100)) %>%
#   group_by(iter, model, scenario) %>%
#   summarize(Pred = mean(Pred)) %>%
#   ungroup() %>%
#   group_by(scenario) %>%
#   summarize(
#     mean = mean(Pred),
#     lower = quantile(Pred, 0.025),
#     upper = quantile(Pred, 0.975))


