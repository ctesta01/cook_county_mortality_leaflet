source(here::here('scripts/dependencies.R'))
source(here::here('scripts/read_cook_cty_deaths.R'))
source(here::here('scripts/make_tract_mort_w_absms.R'))

options(tigris_use_cache=TRUE) 

map_overall_tract_mortality_rates()
                              
