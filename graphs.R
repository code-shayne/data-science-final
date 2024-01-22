library(tidyverse)
library(janitor)
library(ggthemes)
library(ggridges)
library(viridis)
library(hrbrthemes)

power_plant <- read_csv("California_Power_Plants.csv")
power_plant <- power_plant |> 
  janitor::clean_names()

company_plants <- power_plant |> 
  filter(operator_company_id != "Not Available")
company_plants

ggplot(company_plants, aes(x = `capacity_latest`, y = `county`, fill = ..x..)) +
  geom_density_ridges_gradient(scale = 3, rel_min_height = 0.01) +
  scale_fill_viridis(name = "Temp. [F]", option = "C") +
  labs(title = 'Temperatures in Lincoln NE in 2016') +
  theme_ipsum() +
  theme(
    legend.position="none",
    panel.spacing = unit(0.1, "lines"),
    strip.text.x = element_text(size = 8)
  )

library(ggthemes)
library(janitor)
library(hrbrthemes)
library(dplyr)
library(tidyr)
library(viridis)
library(gganimate)
library(av)
library(animation)

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

#p <- ggplot(power_plant, aes(x="", y=pri_energy_source, fill=pri_energy_source)) +
#  geom_bar(stat="identity", width=1) +
 # coord_polar("y", start=0)

i = 0
saveGIF({
  for (i in unique(power_plant$year)) {
    p = ggplot(power_plant[power_plant$year==i,], aes(x="", y=pri_energy_source, fill=pri_energy_source, frame=year))+
      geom_bar(width = 1, stat = "identity") + 
      facet_grid(~year) +
      coord_polar("y", start=0) 
    print(p)
  }
}, movie.name="piechart1.gif")

#anim <- p + 
 # transition_states(year,
 #                   transition_length = 2,
  #                  state_length = 1)

#animate(
 # anim + enter_fade() + exit_fly(y_loc = 1),
  #renderer = av_renderer()
#)































>>>>>>> a4a62bdf183fc17d6ec749c25144874b86bad894
