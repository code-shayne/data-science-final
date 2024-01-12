library(tidyverse)
library(sf)
library(terra)
library(spData)
library(spDataLarge)
library(tmap)
library(tmaptools)
library(tigris)
library(leaflet) 
library(readxl)
library(ggmap)
power_plant = read_csv("California_Power_Plants.csv")
us_geo <- tigris::states(class = "sf")
class(us_geo)
Salary4Helpers <- 
  read_excel("OES_Report.xlsx",
             col_types = c("text", "numeric"), 
             skip = 4)
BlsWage_ToJoin <- Salary4Helpers %>% 
  rename(Area = "Area Name") %>% 
  rename(wages = "Annual mean wage(2)") %>% 
  mutate(State = gsub("\\(\\d{7}\\)", "", Area)) %>% 
  filter(wages != "NA_character_") %>% 
  select(State, wages)
HelperShapeObject <- left_join(us_geo, BlsWage_ToJoin, by = c("NAME" = "State"))
qtm(HelperShapeObject, fill = "wages")
contiguous_states <- HelperShapeObject %>% 
  filter(REGION != 9) %>% 
  shift_geometry()
tm_shape(contiguous_states) + tm_borders()
states <- map_data("state")
ca_df <- subset(states, region == "california")
counties <- map_data("county")
ca_county <- subset(counties, region == "california")
ca_base <- ggplot(data = ca_df, mapping = aes(x = long, y = lat, group = group)) + 
  coord_fixed(1.3) + 
  geom_polygon(color = "black", fill = "gray")

api <- 'AIzaSyCIIr_a0MnPo8Vn-3uGVR32EoJ3ufMY9sc'
register_google(key = api)
plants <- make_bbox(lon = power_plant$X , lat = power_plant$Y, f = 0.1)
sq_map <- get_map(location = plants, maptype = "roadmap", source = "google", api_key = api)
ditch_the_axes <- theme(
  axis.text = element_blank(),
  axis.line = element_blank(),
  axis.ticks = element_blank(),
  panel.border = element_blank(),
  panel.grid = element_blank(),
  axis.title = element_blank()
)
ggmap(sq_map) + geom_point(data = power_plant, mapping = aes(x = X, y = Y, color = Retired_Plant)) +
  ditch_the_axes + labs(title = "Power Plants in California")
#color by if retired or not, size by capacity range (make capacity ranges)




ca_base + geom_polygon(data = ca_county, fill = NA, color = "white")+
  geom_polygon(color = "black", fill = NA)
+ geom_point(data=power_plant, mapping = aes(x=X , y=Y))


