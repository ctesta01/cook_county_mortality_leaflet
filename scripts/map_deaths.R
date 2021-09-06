source(here::here('scripts/dependencies.R'))
source(here::here('scripts/read_cook_cty_deaths.R'))
source(here::here('scripts/make_tract_mort_w_absms.R'))

options(tigris_use_cache=TRUE) 

cook_county_deaths <- read_cook_county_deaths()

cook_tracts <- get_cook_cty_census_tracts()

tract_mort_rates <- make_tract_mort_w_absms_df(
  deaths = cook_county_deaths, 
  cook_tracts = cook_tracts)

saveRDS(tract_mort_rates, here::here("output/tract_mort_rates.rds"))

bins <- c(0, 100, 200, 500, 1000, 2000, 5000, Inf)
pal <- colorBin("YlOrRd", domain = tract_mort_rates$mort_per_100k, bins = bins)

tract_mort_rates %>% 
  leaflet() %>% 
  addTiles() %>% 
  addPolygons(
    weight = 0,
    fillOpacity = 0.75,
    color = ~pal(mort_per_100k),
    label = ~ str_c(
      'deaths per 100k: ',
      round(mort_per_100k),
      ', deaths: ',
      n,
      ', popsize: ',
      scales::comma_format()(estimate)
    )
  )

                              