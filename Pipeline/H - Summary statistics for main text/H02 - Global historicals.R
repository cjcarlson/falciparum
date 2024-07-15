# 
# library(tidyverse)
# # library(data.table)
# 
# source(here::here("Pipeline", "A - Utility functions", "A00 - Configuration.R"))
# 
# iter.df <- here::here("TempFiles", "SuppHistoricalBig.feather")  |> 
#   arrow::read_feather() 
# 
# # iter.df <- read_delim("~/Github/falciparum/TempFiles/SuppHistoricalBig.csv", delim='\t')
# 
# iter.df |>
#   # mutate(model = str_replace_all(model,'./Historical/','')) |>
#   mutate(model = str_replace_all(model,'BCC-CSM2-MR','BCC-CSM2')) -> 
#   iter.df
# 
# iter.df |> 
#   filter(year %in% c(1901:1930)) |>
#   group_by(model, scenario, iter) |>
#   summarize(BetaMean = mean(Pred, na.rm = TRUE)) -> 
#   bm
# 
# iter.df |> 
#   left_join(bm) |> 
#   mutate(Pred = (Pred-BetaMean)) |>
#   select(-BetaMean) -> 
#   df
# 
# df |> 
#   filter(year %in% c(2010:2014)) |>
#   select(model, iter, year, Pred, scenario) |> 
#   pivot_wider(names_from = scenario, values_from = Pred) -> 
#   df2
# 
# df2 |>
#   group_by(model, iter) |>
#   summarize(`hist-nat` = mean(`hist-nat`), historical = mean(historical)) -> 
#   df2
#   
# print(mean(df2$historical - df2$`hist-nat`))
# print(100000*mean(df2$historical - df2$`hist-nat`)/100)
# 
# print(quantile((df2$historical - df2$`hist-nat`), 0.025))
# print(quantile((df2$historical - df2$`hist-nat`), 0.975))
# table((df2$historical - df2$`hist-nat`) > 0) |>
#   prop.table() |> 
#   print()

