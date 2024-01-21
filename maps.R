#the actual maps only use a few pf these but I 
#imported a lot of libraries to try different 
#things initially
library(tidyverse)
library(sf)
library(terra)
library(spData)
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
#couldn't get it down to only California 

#satellite CA map
#only needed this on my home computer
install.packages("devtools")
devtools::install_github("dkahle/ggmap")
library(ggmap)
library(devtools)
#starts from here
api <- 'AIzaSyCIIr_a0MnPo8Vn-3uGVR32EoJ3ufMY9sc'
register_google(key = api) #to access the Google basemap you need an account and api key
plants <- make_bbox(lon = power_plant$X , lat = power_plant$Y, f = 0.1)
sq_map <- get_map(location = plants, maptype = "terrain", source = "google", api_key = api, zoom = 6)
ditch_the_axes <- theme( #gets rid of axes and labels
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
  theme(plot.background = element_rect(fill = "snow2"))+
  theme(panel.border = element_rect(color = "black", 
                                    fill = NA, 
                                    size = 1))
#colored by if retired or not, size by capacity range

#CA state polygon map w/ counties
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
ca_county <- read_csv("counties.csv") #added num_plants data to ca_county in excel

library(RColorBrewer)
myPalette = colorRampPalette(brewer.pal(n=4, "Blues")) #attempt to color the background that I didn't use

ca_base + geom_polygon(data = ca_county, fill = NA, color = "white")+
  geom_polygon(color = "black", fill = NA)+
  geom_polygon(data = ca_county, aes(fill = num_plants), color = "white") +
  geom_polygon(color = "black", fill = NA) +
  theme_bw() +
  ditch_the_axes+
  labs(title = "Number of Power Plants in Each County", fill = "Number of Plants")+
  theme(plot.title=element_text(face="bold",hjust=.5,vjust=.8,colour="Black",size=20))+
  theme(panel.background = element_rect(fill = "white"))+
  scale_fill_continuous(low = "honeydew2", high = "forestgreen")+
  annotate("text", x = -121, y = 33.6, label = "Los Angeles County has the most", size=3, colour="forestgreen")+
  annotate("text", x = -121, y = 33.35, label = "power plants with 208 in total", size=3, colour="forestgreen")+
  annotate("segment", x = -118.9, xend = -118, y = 33.7, yend = 34.3, colour = "forestgreen")+
  annotate("text", x = -117.5, y = 39.6, label = "Both Del Norte and Alpine", size=3, colour="forestgreen")+
  annotate("text", x = -117.5, y = 39.35, label = "County have 0 power plants", size=3, colour="forestgreen")+
  annotate("segment", x = -119.9, xend = -119, y = 38.7, yend = 39.2, colour = "forestgreen")+
  annotate("segment", x = -123.8, xend = -119.15, y = 41.7, yend = 39.6, colour = "forestgreen")



