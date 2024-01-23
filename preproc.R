library(tidyverse)
power_plant = read_csv("California_Power_Plants.csv")
library(dataMaid)
makeDataReport(power_plant, output = "html", replace = TRUE)
library(DataExplorer)
create_report(power_plant)
power_plant <- power_plant |>
  mutate(year = year(StartDate)) |> 
  arrange(year)
count <- power_plant |> 
  group_by(year, PriEnergySource) |> 
  summarize(count = sum(active_num))
library(animation)
p <- ggplot(count, aes(x="", y=count, fill=PriEnergySource)) +
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

county_count <- power_plant |> 
  group_by(year, county)|> 
  summarize(num_plants = sum(active_num))