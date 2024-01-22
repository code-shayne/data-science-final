library(tidyverse)
power_plant = read_csv("California_Power_Plants.csv")
library(dataMaid)
makeDataReport(power_plant, output = "html", replace = TRUE)
library(DataExplorer)
create_report(power_plant)
power_plant <- power_plant |>
  mutate(year = year(StartDate)) |> 
  arrange(year)
power_plant |> 
  group_by(year, PriEnergySource) |> 
  summarize(count = sum(active_num))
