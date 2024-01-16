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
#pre processing
power_plant = read_csv("California_Power_Plants.csv")
power_plant <- power_plant |> 
 mutate(Active = if_else(Retired_Plant == 0, "Yes", "No"),
        active_num = if_else(Retired_Plant == 0, 1, 0)) |>
  rename(Capacity = Capacity_Latest)

#failed tmap attempt
us_geo <- tigris::states(class = "sf")
class(us_geo)
Salary4Helpers <- 
  read_excel("OES_Report.xlsx",
             col_types = c("text", "numeric"), 
             skip = 4)
BlsWage_ToJoin <- Salary4Helpers |>  
  rename(Area = "Area Name")  |>  
  rename(wages = "Annual mean wage(2)") |>  
  mutate(State = gsub("\\(\\d{7}\\)", "", Area)) |> 
  filter(wages != "NA_character_") |>  
  select(State, wages)
HelperShapeObject <- left_join(us_geo, BlsWage_ToJoin, by = c("NAME" = "State"))
qtm(HelperShapeObject, fill = "wages")
contiguous_states <- HelperShapeObject|> 
  filter(REGION != 9) |> 
  shift_geometry()
tm_shape(contiguous_states) + tm_borders()

#satellite CA map
#only needed this on home computer
install.packages("devtools")
devtools::install_github("dkahle/ggmap")
library(ggmap)
library(devtools)
#start from here prob
api <- 'AIzaSyCIIr_a0MnPo8Vn-3uGVR32EoJ3ufMY9sc'
register_google(key = api)
plants <- make_bbox(lon = power_plant$X , lat = power_plant$Y, f = 0.1)
sq_map <- get_map(location = plants, maptype = "terrain", source = "google", api_key = api, zoom = 6)
ditch_the_axes <- theme(
  axis.text = element_blank(),
  axis.line = element_blank(),
  axis.ticks = element_blank(),
  panel.border = element_blank(),
  panel.grid = element_blank(),
  axis.title = element_blank()
)
ggmap(sq_map) + geom_point(data = power_plant, mapping = aes(x = X, y = Y, color = Active, size = Capacity)) +
  ditch_the_axes + labs(title = "Power Plants in California", 
                        caption = "A map showing the locations of California power plants. Active
                        plants are in blue and inactive are grey.The power capacity is
                        represented by the size of the points, ranging from <1 to >5000.")+
  theme(plot.title=element_text(face="bold",hjust=.5,vjust=.8,colour="Black",size=20))+
  theme(plot.caption=element_text(vjust=4,colour="grey10",size=9))+
  scale_color_manual(values=c("gray40", "#0a06d6"))+
  theme(panel.border = element_rect(color = "black", 
                                    fill = NA, 
                                    size = 1))
#color by if retired or not, size by capacity range

#CA state polygon map
states <- map_data("state")
ca_df <- subset(states, region == "california")
counties <- map_data("county")
ca_county <- subset(counties, region == "california")
ca_base <- ggplot(data = ca_df, mapping = aes(x = long, y = lat, group = group)) + 
  coord_fixed(1.3) + 
  geom_polygon(color = "black", fill = "gray")
county_count <- power_plant |> 
  group_by(County)|> 
  summarize(num_plants = sum(active_num))
county_count

ca_base + geom_polygon(data = ca_county, fill = NA, color = "white")+
  geom_polygon(color = "black", fill = NA)+
  geom_polygon(data = county_count, aes(fill = num_plants), color = "white") +
  geom_polygon(color = "black", fill = NA) +
  theme_bw() +
  ditch_the_axes

+ geom_point(data=power_plant, mapping = aes(x=X , y=Y))


