library(tidyverse)
library(janitor)
library(ggthemes)
library(gganimate)
library(animation)

#read in and pre-process file
power_plant = read_csv("California_Power_Plants.csv")
power_plant <- power_plant |> 
  janitor::clean_names() 
power_plant <- power_plant |> 
  mutate(year = year(start_date), 
         active = if_else(retired_plant == 0, "Yes", "No"),
         active_num = if_else(retired_plant == 0, 1, 0))|> 
  filter(operator_company_id != "Not Available" & retired_plant == 0)|>
  arrange(year)

source_count <- power_plant |> 
  group_by(county, pri_energy_source)|> 
  summarize(num_plants = sum(active_num))

#animated pie chart
saveGIF({
  for (i in unique(power_plant$year)) {
    p <- ggplot(power_plant[power_plant$year==i,], aes(x="", y=pri_energy_source, fill=pri_energy_source, frame=year))+
      geom_bar(width = 1, stat = "identity") + 
      facet_grid(~year) +
      coord_polar("y", start=0) +
      labs(
        x = "",
        y = "",
        color = "Primary Energy Source",
        title = "Energy Source Distribution of New Power Plants"
      )
    print(p)
  }
}, movie.name="power_plant_pie.gif")

#bar chart with color stacks
p <- ggplot(power_plant, aes(x = year, y = capacity_latest)) +
  geom_bar(stat = "identity", aes(fill = power_plant$pri_energy_source)) +
  labs(
    x = "Year",
    y = "Capacity",
    title = "Total Capacity of New California Power Plants",
    fill = "Primary Energy Source"
  )
p

#horizontal lollipop plot
p <- ggplot(source_count, aes(x = county, y = num_plants)) +
  geom_segment(aes(x = county, xend = county, y = 0, yend = num_plants), color = "purple") +
  geom_point(color = "purple4", size = 2.5, alpha = 0.7, shape = 18) +
  theme_light() +
  coord_flip() +
  theme(
    panel.grid.major.y = element_blank(),
    panel.border = element_blank(),
    axis.ticks.y = element_blank()
  ) +
  labs(
    x = "County",
    y = "Number of Plants",
    title = "Power Plants in California Counties"
  )
p






















