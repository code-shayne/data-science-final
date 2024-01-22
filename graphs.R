library(tidyverse)
library(ggthemes)
library(janitor)
library(hrbrthemes)
library(dplyr)
library(tidyr)
library(viridis)

#read in and pre-process file
power_plant = read_csv("California_Power_Plants.csv")
power_plant <- power_plant |> 
  janitor::clean_names()
power_plant <- power_plant |>
  mutate(year = year(start_date)) |> 
  arrange(year)

#animated bar plot
p <- ggplot(power_plant, aes(x = pri_energy_source, y = capacity_latest)) + 
  geom_bar(stat = "identity")

anim <- p + 
  transition_states(Species,
                    transition_length = 2,
                    state_length = 1)

animate(
  anim + enter_fade() + exit_fly(y_loc = 1),
  renderer = av_renderer()
)






























