library(tidyverse)
library(sf)
library(spdep)
library(caret)
library(ckanr)
library(FNN)
library(grid)
library(gridExtra)
library(ggcorrplot)
library(kableExtra)
library(jtools)     # for regression model plots
library(ggstance) # to support jtools plots
library(osmdata)
library(knitr)
library(tidycensus)
library(scales)

root.dir = "C:/Users/zoeny/OneDrive - PennO365/GrSchool/Year1/MUSA 508 Public Policy Analytics/MUSA508/MUSA_Midterm"
source("https://raw.githubusercontent.com/urbanSpatial/Public-Policy-Analytics-Landing/master/functions.r")

palette5 <- c("#25CB10", "#5AB60C", "#8FA108",   "#C48C04", "#FA7800")

boulder.sf <- 
  st_read(file.path(root.dir,"/studentData.geojson"), ) %>% 
  st_set_crs('ESRI:102254') %>% st_make_valid() %>% 
  mutate(Age = 2021 - builtYear) 

county_boundary <- 
  st_read("https://opendata.arcgis.com/datasets/964b8f3b3dbe401bb28d49ac93d29dc4_0.geojson") %>%
  st_transform(st_crs(boulder.sf))

tracts <-
  st_read("https://raw.githubusercontent.com/zoenyoo/MUSA_Midterm/main/Modeling_Data/SHP%20data/BoulderCounty_tracts.geojson") %>% 
  st_transform(st_crs(boulder.sf))

boulder.sf <- st_join(boulder.sf, tracts) %>% 
  select(c(1:56))

floodplain <-
  st_read("https://opendata.arcgis.com/datasets/f2b0b0d290894d3faafc6a5c7964445a_0.geojson") %>% 
  st_transform(st_crs(boulder.sf))

playgrounds.sf <- 
  st_read("https://opendata.arcgis.com/datasets/b89ea27bc3cd492682503f03df1a9fb9_0.geojson") %>%
  st_as_sf(coords=playgrounds.sf$geometry, crs = 4326) %>% 
  st_transform(st_crs(boulder.sf)) 


schools <- osmdata_sf(getbb("Boulder County", base_url="https://nominatim.openstreetmap.org") %>% 
                             opq() %>% 
                             add_osm_feature("amenity", "school"))
schools <- schools$osm_points
schoolPoints <- st_as_sf(schools,coords=schools$geometry, crs = 4326) %>% 
  st_transform(st_crs(boulder.sf)) 

water <- osmdata_sf(getbb("Boulder County", base_url="https://nominatim.openstreetmap.org") %>% 
                        opq() %>% 
                        add_osm_feature("natural", "water")) 
water <- water$osm_polygons 
water.sf <- st_as_sf(water,coords=water$geometry, crs = 4326) %>% 
  st_transform(st_crs(boulder.sf)) %>% 
  drop_na(name)

ggplot() +
  geom_sf(data=county_boundary)+
  geom_sf(data=tracts, alpha=.4)+
  geom_sf(data=playgrounds.sf, color="green")+
  geom_sf(data=water.sf)+
  geom_sf(data = boulder.sf, aes(colour = q5(price)), 
          show.legend = "point", size = 2)+
  mapTheme()

boulder.sf$school.Buffer =
  st_buffer(boulder.sf, 660) %>% 
  aggregate(mutate(schoolPoints, counter = 1),., sum) %>%
  pull(counter)

x <- lengths(st_intersects(st_buffer(boulder.sf, 660), schoolPoints))
mutate(boulder.sf, school.Buffer=x)
poly$pt_count <- length(st_intersects(poly, pts))
pointsBuffer$city_count <- count$name

