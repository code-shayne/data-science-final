library(tidyverse)
<<<<<<< HEAD
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
=======
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


































>>>>>>> a4a62bdf183fc17d6ec749c25144874b86bad894
