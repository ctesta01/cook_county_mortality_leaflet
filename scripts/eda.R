source(here::here('scripts/dependencies.R'))
source(here::here('scripts/read_cook_cty_deaths.R'))
source(here::here('scripts/make_tract_mort_w_absms.R'))

options(tigris_use_cache=TRUE) 

cook_county_deaths <- read_cook_county_deaths()
cook_tracts <- get_cook_cty_census_tracts()

tract_deaths <- make_tract_mortality_counts(cook_county_deaths, cook_tracts)

df <- tract_deaths %>% add_absms()

cook_tracts_w_data <- cook_tracts %>% st_as_sf() %>% left_join(
  df, by = c('GEOID' = 'GEOID'))

map_feature <- function(df, feature = 'mort_per_100k') {
  
  bins <- unique(c(0, quantile(df[[feature]], seq(0,1,0.2), na.rm=T), Inf))
  pal <- colorBin("YlOrRd", domain = df[[feature]], bins = bins)
  
  df %>% 
    leaflet() %>% 
    addTiles() %>% 
    addPolygons(
      weight = 0,
      fillOpacity = 0.75,
      color = pal(df[[feature]]),
      label = round(df[[feature]], 2)
    )
}


map_feature(cook_tracts_w_data, 'mort_per_100k')
map_feature(cook_tracts_w_data, 'pct_black')
map_feature(cook_tracts_w_data, 'pct_poc')
map_feature(cook_tracts_w_data, 'pct_hispanic')
map_feature(cook_tracts_w_data, 'ice_black_white')



# making one map w everything ---------------------------------------------

variables <- c('mort_per_100k', 
               'pct_poc',
               'pct_black',
               'pct_hispanic',
               'ice_black_white')
               
variable_pals <- lapply(variables, function(feature) {
  bins <- unique(c(0, quantile(cook_tracts_w_data[[feature]], seq(0,1,0.2), na.rm=T), Inf))
  pal <- colorBin("YlOrRd", domain = cook_tracts_w_data[[feature]], bins = bins)
  return(pal)
})

names(variable_pals) <- variables
  

rendered_leaflet <- cook_tracts_w_data %>% 
    leaflet() %>% 
    addTiles() 

for (feature in variables) {
  rendered_leaflet <- rendered_leaflet %>% 
      addPolygons(
        weight = 0,
        fillOpacity = 0.75,
        color = variable_pals[[feature]](cook_tracts_w_data[[feature]]),
        label = str_c(feature, ": ", round(cook_tracts_w_data[[feature]], 2)),
        group = feature
      )
  }

rendered_leaflet %<>%   addLayersControl(
  baseGroups = variables,
  options = layersControlOptions(collapsed = FALSE)
)

rendered_leaflet


library(randomForest)

model <- randomForest(
  mort_per_100k ~ pct_poc + pct_black + pct_hispanic + pct_asian + 
    pct_some_other_race + pct_two_or_more,
    cook_tracts_w_data %>% 
    select(mort_per_100k, starts_with('pct_')) %>% 
    na.omit())

library(vip)

vip(model)


ggplot(cook_tracts_w_data, aes(x = pct_poc, y = mort_per_100k, size = overall)) + 
  geom_point(alpha=0.5) + 
  geom_smooth(alpha=0.5) + 
  labs(size = 'census population size') + 
  scale_size_continuous(range = c(0.5, 3), trans = 'log', labels = scales::comma_format())
  

  