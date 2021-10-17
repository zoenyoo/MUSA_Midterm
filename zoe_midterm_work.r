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

root.dir = "https://raw.githubusercontent.com/urbanSpatial/Public-Policy-Analytics-Landing/master/DATA/"
source("https://raw.githubusercontent.com/urbanSpatial/Public-Policy-Analytics-Landing/master/functions.r")
palette5 <- c("#25CB10", "#5AB60C", "#8FA108",   "#C48C04", "#FA7800")
palette_rank5 <- c("#ffffb2", "#fecc5c", "#fd8d3c",   "#f03b20", "#bd0026")

boulder.sf <- 
  st_read("studentData.geojson", ) %>% 
  st_set_crs('ESRI:102254') %>% st_make_valid() %>% 
  mutate(Age = 2021 - builtYear,
         PricePerSF = price/TotalFinishedSF) 
boulder.sf[2638,]$price <- 315000 #mistake input as 31500000, checked on zillow

county_boundary <- 
  st_read("https://opendata.arcgis.com/datasets/964b8f3b3dbe401bb28d49ac93d29dc4_0.geojson") %>%
  st_transform(st_crs(boulder.sf))

tracts <-
  st_read("https://raw.githubusercontent.com/zoenyoo/MUSA_Midterm/main/Modeling_Data/SHP%20data/BoulderCounty_tracts.geojson") %>% 
  st_transform(st_crs(boulder.sf))

floodplain <-
  st_read("https://opendata.arcgis.com/datasets/f2b0b0d290894d3faafc6a5c7964445a_0.geojson") %>% 
  st_transform(st_crs(boulder.sf))

playgrounds.sf <- 
  st_read("https://opendata.arcgis.com/datasets/b89ea27bc3cd492682503f03df1a9fb9_0.geojson") %>%
  st_as_sf(coords=playgrounds.sf$geometry, crs = 4326) %>% 
  st_transform(st_crs(boulder.sf)) 

parcels <- 
  st_read("https://opendata.arcgis.com/datasets/89ae49d4ddf246388ee5f5e952aa84db_0.geojson") %>% 
  st_transform(st_crs(boulder.sf))

boulder.sf <- st_join(boulder.sf, tracts) 
boulder.sf <- st_join(boulder.sf, parcels) %>% 
  select(!c(51:54, 56:58, 61:65, 67:60)) 

#available_features() #amenity, shop, natural, highway, building


schools <- osmdata_sf(getbb("Boulder County", base_url="https://nominatim.openstreetmap.org") %>% 
                        opq() %>% 
                        add_osm_feature("amenity", "school"))$osm_points
schools.sf <- st_as_sf(schools, coords=schools$geometry, crs = 4326) %>% 
  st_geometry(schools.sf$geometry) %>%
  st_transform(st_crs(boulder.sf))  %>%
  st_sf() %>%
  cbind(., schools$name) %>%
  rename(NAME = schools.name)

parks <- osmdata_sf(getbb("Boulder County", base_url="https://nominatim.openstreetmap.org") %>% opq() %>%
                      add_osm_feature(key = 'leisure', value = "park"))
parks.sf <- st_geometry(parks$osm_polygons) %>%
  st_transform(st_crs(boulder.sf)) %>%
  st_sf() %>%
  cbind(., parks$osm_polygons$name) %>%
  rename(NAME = parks.osm_polygons.name)

water <- osmdata_sf(getbb("Boulder County", base_url="https://nominatim.openstreetmap.org") %>% 
                        opq() %>% 
                        add_osm_feature("natural", "water"))$osm_polygons
water.sf <- st_as_sf(water,coords=water$geometry, crs = 4326) %>% 
  st_transform(st_crs(boulder.sf)) %>% 
  drop_na(name)

highways <- osmdata_sf(getbb("Boulder County", base_url="https://nominatim.openstreetmap.org") %>% 
                      opq() %>% 
                      add_osm_feature("highway"))$osm_lines
highways.sf <- st_as_sf(highways,coords=highways$geometry, crs = 4326) %>% 
  st_transform(st_crs(boulder.sf)) %>% 
  drop_na(name)

trailheads.sf <-
  st_read("https://opendata.arcgis.com/datasets/3a950053bbef46c6a3c2abe3aceee3de_0.geojson") %>%
  st_transform(st_crs(boulder.sf))

ggplot() +
  geom_sf(data=county_boundary, color="white")+
  geom_sf(data=tracts, alpha=.4)+
  geom_sf(data = boulder.sf, aes(color = q5(price)), 
          show.legend = "point", size = 0.3)+
  scale_color_brewer(type=seq, palette = "YlOrRd")+
  mapTheme()

boulder.sf <- 
  mutate(boulder.sf, water.Buffer=lengths(st_intersects(st_buffer(boulder.sf, 660), water.sf))) %>% 
  mutate(boulder.sf, playground.Buffer=lengths(st_intersects(st_buffer(boulder.sf, 660), playgrounds.sf))) %>% 
  mutate(boulder.sf, school.Buffer=lengths(st_intersects(st_buffer(boulder.sf, 660), schools.sf))) %>% 
  mutate(boulder.sf, parks.Buffer=lengths(st_intersects(st_buffer(boulder.sf, 660), parks.sf)))

st_drop_geometry(boulder.sf) %>% 
  dplyr::select(price, TotalFinishedSF, Age, playground.Buffer, SHAPEarea, school.Buffer) %>%
  filter(price <= 10000000, Age < 500, SHAPEarea < 4000000) %>%
  gather(Variable, Value, -price) %>% 
  ggplot(aes(Value, price)) +
  geom_point(size = .5) + geom_smooth(method = "lm", se=F, colour = "#FA7800") +
  facet_wrap(~Variable, ncol = 3, scales = "free") +
  labs(title = "Price as a function of continuous variables") +
  plotTheme()

numericVars <- 
  select_if(st_drop_geometry(boulder.sf), is.numeric) %>% 
  dplyr::select(-MUSA_ID, -toPredict, -builtYear, -Stories, -UnitCount) %>% 
  na.omit() 

# only for continuous variables
ggcorrplot(  #correlation plot
  round(cor(numericVars), 1), 
  p.mat = cor_pmat(numericVars),
  show.diag = TRUE,
  colors = c("#25CB10", "white", "#FA7800"),
  type="lower",
  insig = "blank") +  
  labs(title = "Correlation across numeric variables") 

reg1 <- lm(price ~ ., data = st_drop_geometry(boulder.sf) %>% 
             dplyr::select(price, Age, TotalFinishedSF, designCodeDscr, 
                           bsmtType, qualityCodeDscr, nbrRoomsNobath, nbrFullBaths, 
                           mainfloorSF, AcDscr, playground.Buffer, NAME.y,
                           parks.Buffer))

summary(reg1)
plot_summs(reg1)

inTrain <- createDataPartition(
  y = paste(boulder.sf$NAME.y, boulder.sf$bsmtType, 
            boulder.sf$qualityCode, boulder.sf$AcDscr), 
  p = .60, list = FALSE)
boulder.training <- boulder.sf[inTrain,] 
boulder.test <- boulder.sf[-inTrain,]  

reg.training <- 
  lm(price ~ ., data = as.data.frame(boulder.training) %>% 
       dplyr::select(price, Age, TotalFinishedSF, designCodeDscr, 
                     bsmtType, qualityCode, nbrRoomsNobath, nbrFullBaths, 
                     mainfloorSF, AcDscr, playground.Buffer))

boulder.test <-
  boulder.test %>%
  mutate(Regression = "Baseline Regression",
         price.Predict = predict(reg.training, boulder.test),
         price.Error = price.Predict - price,
         price.AbsError = abs(price.Predict - price),
         price.APE = (abs(price.Predict - price)) / price.Predict)%>%
  filter(price < 10000000) 

coords <- st_coordinates(boulder.sf) 
neighborList <- knn2nb(knearneigh(coords, 5))
spatialWeights <- nb2listw(neighborList, style="W")
boulder.sf <- mutate(boulder.sf, lagPrice=lag.listw(spatialWeights, boulder.sf$price, zero.policy=TRUE, NAOK = TRUE)) 
coords.test <-  st_coordinates(boulder.test) 
neighborList.test <- knn2nb(knearneigh(coords.test, 5))
spatialWeights.test <- nb2listw(neighborList.test, style="W")

boulder.test %>% 
  mutate(lagPriceError = lag.listw(spatialWeights.test, price.Error)) %>%
  ggplot(aes(lagPriceError, price.Error))

summary(boulder.test)

left_join(
  st_drop_geometry(boulder.test) %>%
    group_by(NAME.y) %>%
    summarize(meanPrice = mean(price, na.rm = T)),
  mutate(boulder.test, predict.fe = 
           predict(lm(price ~ NAME.y, data = boulder.test), 
                   boulder.test)) %>%
    st_drop_geometry %>%
    group_by(NAME.y) %>%
    summarize(meanPrediction = mean(predict.fe))) %>%
  kable() %>% kable_styling()

reg.nhood <- lm(price ~ ., data = as.data.frame(boulder.training) %>% 
                  dplyr::select(NAME.y, price, Age, TotalFinishedSF, designCodeDscr, 
                                bsmtType, qualityCodeDscr, nbrRoomsNobath, nbrFullBaths, 
                                mainfloorSF, AcDscr, playground.Buffer))

boulder.test.nhood <-
  boulder.test %>%
  mutate(Regression = "Neighborhood Effects",
         price.Predict = predict(reg.nhood, boulder.test),
         price.Error = price.Predict - price,
         price.AbsError = abs(price.Predict - price),
         price.APE = (abs(price.Predict - price)) / price.Predict)%>%
  filter(price < 10000000)

bothRegressions <- 
  rbind(
    dplyr::select(boulder.test, starts_with("price"), Regression, NAME.y) %>%
      mutate(lagPriceError = lag.listw(spatialWeights.test, price.Error)),
    dplyr::select(boulder.test, starts_with("price"), Regression, NAME.y) %>%
      mutate(lagPriceError = lag.listw(spatialWeights.test, price.Error)))  
