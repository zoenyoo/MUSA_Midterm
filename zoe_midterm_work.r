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
boulder.sf <- st_join(boulder.sf, parcels)


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

boulder.sf <- 
  mutate(boulder.sf, water.Buffer=lengths(st_intersects(st_buffer(boulder.sf, 804), water.sf))) %>% 
  mutate(boulder.sf, playground.Buffer=lengths(st_intersects(st_buffer(boulder.sf, 804), playgrounds.sf))) %>% 
  mutate(boulder.sf, school.Buffer=lengths(st_intersects(st_buffer(boulder.sf, 804), schools.sf))) %>% 
  mutate(boulder.sf, parks.Buffer=lengths(st_intersects(st_buffer(boulder.sf, 804), parks.sf)))

ggplot() +
  geom_sf(data=county_boundary, color="white")+
  geom_sf(data=tracts, alpha=.4)+
  geom_sf(data = boulder.sf, aes(color = q5(price)), 
          show.legend = "point", size = 0.3)+
  scale_color_brewer(type=seq, palette = "YlOrRd")+
  mapTheme()

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
  dplyr::select(-MUSA_ID, -toPredict, -builtYear, -Stories, -UnitCount,
                -OBJECTID, -SHAPElen) %>% 
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
                           mainfloorSF, AcDscr, playground.Buffer, parks.Buffer))

summary(reg1)
plot_summs(reg1)

inTrain <- createDataPartition(
  y = paste(boulder.sf$NAME.y, boulder.sf$bsmtType, 
            boulder.sf$qualityCode, boulder.sf$AcDscr), 
  p = .75, list = FALSE)
boulder.training <- boulder.sf[inTrain,] 
boulder.test <- boulder.sf[-inTrain,]  

reg.training <- 
  lm(price ~ ., data = as.data.frame(boulder.training) %>% 
       dplyr::select(price, Age, TotalFinishedSF, designCodeDscr, 
                     bsmtType, qualityCodeDscr, nbrRoomsNobath, nbrFullBaths, 
                     mainfloorSF, AcDscr, playground.Buffer, parks.Buffer))

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
moranTest <- moran.mc(boulder.test$price.Error, 
                      spatialWeights.test, nsim = 999)
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
    dplyr::select(boulder.test.nhood, starts_with("price"), Regression, NAME.y) %>%
      mutate(lagPriceError = lag.listw(spatialWeights.test, price.Error)))  

st_drop_geometry(bothRegressions) %>%
  gather(Variable, Value, -Regression, -NAME.y) %>%
  filter(Variable == "price.AbsError" | Variable == "price.APE") %>%
  group_by(Regression, Variable) %>%
  summarize(meanValue = mean(Value, na.rm = T)) %>%
  spread(Variable, meanValue) %>%
  kable()

bothRegressions %>%
  dplyr::select(price.Predict, price, Regression) %>%
  ggplot(aes(price, price.Predict)) +
  geom_point() +
  stat_smooth(aes(price, price), 
              method = "lm", se = FALSE, size = 1, colour="#FA7800") + 
  stat_smooth(aes(price.Predict, price), 
              method = "lm", se = FALSE, size = 1, colour="#25CB10") +
  facet_wrap(~Regression) +
  labs(title="Predicted sale price as a function of observed price",
       subtitle="Orange line represents a perfect prediction; Green line represents prediction") +
  plotTheme()

st_drop_geometry(bothRegressions) %>%
  group_by(Regression, NAME.y) %>%
  summarize(mean.MAPE = mean(price.APE, na.rm = T)) %>%
  ungroup() %>% 
  left_join(tracts, by = "NAME") %>%
  st_sf() %>%
  ggplot() + 
  geom_sf(aes(fill = mean.MAPE)) +
  geom_sf(data = bothRegressions, colour = "black", size = .5) +
  facet_wrap(~Regression) +
  scale_fill_gradient(low = palette5[1], high = palette5[5],
                      name = "MAPE") +
  labs(title = "Mean test Set MAPE by Census Tract") +
  mapTheme()

tracts19 <- 
  get_acs(geography = "tract", variables = c("B01001_001E","B01001A_001E","B19013_001"), 
          year = 2019, state=08, county=013, geometry=T, output = "wide", survey = "acs5") %>%
  st_transform('ESRI:102254')  %>%
  rename(TotalPop = B01001_001E,
         NumberWhites = B01001A_001E,
         Median_Income = B19013_001E) %>%
  mutate(percentWhite = NumberWhites / TotalPop,
         raceContext = ifelse(percentWhite > .5, "Majority White", "Majority Non-White"),
         incomeContext = ifelse(Median_Income > 83019, "High Income", "Low Income")) #https://www.census.gov/quickfacts/bouldercountycolorado

grid.arrange(ncol = 2,
             ggplot() + geom_sf(data = na.omit(tracts19), aes(fill = raceContext)) +
               scale_fill_manual(values = c("#25CB10", "#FA7800"), name="Race Context") +
               labs(title = "Race Context") +
               mapTheme() + theme(legend.position="bottom"), 
             ggplot() + geom_sf(data = na.omit(tracts19), aes(fill = incomeContext)) +
               scale_fill_manual(values = c("#25CB10", "#FA7800"), name="Income Context") +
               labs(title = "Income Context") +
               mapTheme() + theme(legend.position="bottom"))

st_join(bothRegressions, tracts19) %>% 
  group_by(Regression, raceContext) %>%
  summarize(mean.MAPE = scales::percent(mean(SalePrice.APE, na.rm = T))) %>%
  st_drop_geometry() %>%
  spread(raceContext, mean.MAPE) %>%
  kable(caption = "Test set MAPE by neighborhood racial context")