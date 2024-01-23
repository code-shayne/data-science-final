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




























