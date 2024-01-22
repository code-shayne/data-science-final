library(tidyverse)
library(ggthemes)
library(janitor)
library(hrbrthemes)
library(dplyr)
library(tidyr)
library(viridis)
library(gganimate)
library(av)

#read in and pre-process file
power_plant = read_csv("California_Power_Plants.csv")
power_plant <- power_plant |> 
  janitor::clean_names()
power_plant <- power_plant |>
  mutate(year = year(start_date)) |> 
  arrange(year)
power_plant <- power_plant |> 
  mutate(active = if_else(retired_plant == 0, "Yes", "No"),
         active_num = if_else(retired_plant == 0, 1, 0))
county_count <- power_plant |> 
  group_by(year, county)|> 
  summarize(num_plants = sum(active_num))
active_plants <- power_plant |> 
  filter(retired_plant == 0)

#animated bar plot
#p <- ggplot(county_count, aes(x = county, y = num_plants)) + 
 # geom_bar(stat = "identity")

p <- ggplot(power_plant, aes(x="", y=pri_energy_source, fill=pri_energy_source)) +
  geom_bar(stat="identity", width=1) +
  coord_polar("y", start=0)
anim <- p + 
  transition_states(year,
                    transition_length = 2,
                    state_length = 1)

animate(
  anim + enter_fade() + exit_fly(y_loc = 1),
  renderer = av_renderer()
)































