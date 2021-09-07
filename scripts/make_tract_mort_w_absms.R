
#' Get Cook County, IL Census Tracts with tigris
#' 
#' @examples 
#' 
#' # code for creating census tracts map in leaflet
#' cook_tracts %>% 
#'   leaflet() %>% 
#'   addTiles() %>% 
#'   addPolygons(
#'     weight = 2
#'     )
get_cook_cty_census_tracts <- function() {
  tigris::tracts("IL", county = "Cook", year = 2019)
}

#' Calculate Census Tract Mortality Counts
#' 
#' @examples 
#' # code for making simple map of death counts by census tract
#' tract_counts <- make_tract_mortality_counts(cook_county_deaths, cook_tracts)
#' 
#' bins <- c(0, 10, 20, 50, 100, 200, Inf)
#' pal <- colorBin("YlOrRd", domain = tract_counts$n, bins = bins)
#' 
#' cook_tracts %>% st_as_sf() %>% 
#'   left_join(tract_counts,
#'             by = c("GEOID" = 'tract')) %>% 
#'   leaflet() %>% 
#'   addTiles() %>% 
#'   addPolygons(
#'   weight = 0,
#'   fillOpacity = 0.75,
#'   color = ~pal(n),
#'   label = ~ str_c('deaths: ', n))
make_tract_mortality_counts <- function(deaths, cook_tracts) {
  
  # convert the deaths data into an sf object with the right coordinates
  # columns and the same coordinate system (crs) as the cook_tracts data
  df_sf <- deaths %>% 
  filter(! is.na(longitude), ! is.na(latitude)) %>% 
  st_as_sf(coords = c("longitude", "latitude"),
           crs = st_crs(cook_tracts))
  
  # Determine which cook county tract ID each deaths data row corresponds to --
  # and include it in df_sf as a tract. 
  # as.numeric() is used to get the index of the output of st_within since it comes
  # out as a geometry object.
  df_sf$tract <- as.numeric(st_within(df_sf, cook_tracts %>% st_as_sf()))
  
  # Use those indices to look up the GEOID (tract ID) of which tract each death 
  # record was coded to using st_within
  df_sf$tract <- cook_tracts$GEOID[df_sf$tract]
  
  # drop the geometry, group by census tract, and count deaths by tract
  tract_counts <- df_sf %>% 
    as.data.frame() %>% 
    select(-geometry) %>% 
    group_by(tract) %>% 
    count() 
  
  return(tract_counts)
  
}

#' Get Census Tract Sex, Race, Age Stratified Population Estimates
get_tract_sex_race_age_stratified_popsizes <- function() {
  
  acs_vars <- tidycensus::load_variables(2019, dataset = 'acs5')
  
  race_chars <-
    c(
      white = 'A',
      black = 'B',
      asian = 'D',
      some_other_race = 'F',
      two_or_more_races = 'G',
      hispanic_or_latino = 'I'
    )

  sex_race_age_vars <-
    paste0(rep(paste0('B01001', race_chars, '_0'), each = 31),
           str_pad(
             1:31,
             width = 2,
             side = 'left',
             pad = '0'
           ))
  
  sex_race_age_vars %<>% c(.,
                           paste0('B01001_0', str_pad(1:49, width = 2, side = 'left', pad = '0')))
  
  # split the label
  acs_vars %<>%
    separate(label,
             into = c('estimate', 'total', 'gender', 'age', 'subgroup'),
             sep = '!!')
  
  # clean label values (remove extra :s and leading/trailing spaces)
  acs_vars %<>% mutate_at(.vars = vars(estimate, total, gender, age, subgroup),
                          ~ gsub(":", "", .) %>% stringr::str_trim())
  
  # select only what we need
  acs_vars %<>% select(name, total, gender, age, concept)
  
  # get sex, race (incl. hispanic or latino), and age stratified population
  # estimates
  tract_popsize <-
    tidycensus::get_acs(
      geography = 'tract',
      state = 'IL',
      county = 'Cook',
      year = 2019,
      geometry = T,
      variables = sex_race_age_vars,
      output = 'tidy'
    )
  
  tract_popsize %<>% left_join(acs_vars, by = c('variable' = 'name'))
  
  return(tract_popsize)
}


#' Add Area-Based Socioeconomic Measures
#' @examples 
#' tract_deaths <- make_tract_mortality_counts(cook_county_deaths, cook_tracts)
#' df <- tract_counts %>% add_absms()
#' View(df)
add_absms <- function(tract_deaths) {
  
  tract_popsize <- get_tract_sex_race_age_stratified_popsizes()
  
  race_chars <-
    c(
      white = 'A',
      black = 'B',
      asian = 'D',
      some_other_race = 'F',
      two_or_more_races = 'G',
      hispanic_or_latino = 'I',
      overall = ''
    )
  
  # code race/ethnicity for each variable
  tract_popsize %<>% rowwise() %>% mutate(
    race_char = str_extract(substr(variable, 2, 10), "[A-Z]"),
    race_char = ifelse(is.na(race_char), "", race_char),
    race_char_idx = which(race_char == race_chars),
    race = names(race_chars)[race_char_idx]
    ) %>%
    select(-c(race_char, race_char_idx))
  
  # pivot to wide format for population totals by race
  tract_popsize_totals <- tract_popsize %>% 
    as.data.frame() %>% 
    filter(is.na(age) & is.na(gender)) %>% 
    select(GEOID, race, estimate) %>% 
    pivot_wider(
      id_cols = GEOID,
      names_from = race,
      values_from = estimate) 
  
  # add in census tract features (pct race, ice)
  tract_features <- tract_popsize_totals %>% 
    mutate(
      pct_black = black / overall,
      pct_poc = 1-white / overall,
      pct_hispanic = hispanic_or_latino / overall,
      pct_asian = asian / overall,
      pct_two_or_more = two_or_more_races / overall,
      pct_some_other_race = some_other_race / overall,
      ice_black_white = (black - white) / (white + black),
      ice_hispanic_white = (hispanic_or_latino - white) / (white + hispanic_or_latino)
    )
  
  # join the death counts into the total tract population size (i.e. the 
  # gender == NA, age == NA population) and add a mortality rate per 100k column
  tract_mort_rates <- tract_features %>%
    left_join(tract_deaths, by = c('GEOID' = 'tract')) %>% 
    mutate(mort_per_100k = n / overall * 1e5) 
    
  return(tract_mort_rates)
}

#' Make Tract Level Estimates of Mortality per 100k with Area Based Socioeconomic Measures
#' 
make_tract_mort_w_absms_df <- function(deaths, cook_tracts) {
  
  tract_deaths <- make_tract_mortality_counts(cook_county_deaths, cook_tracts)
  df <- tract_deaths %>% add_absms()
  
  return(df)
}



#' Map Overall Tract Mortality Rates 
map_overall_tract_mortality_rates <- function() {
  
 cook_county_deaths <- read_cook_county_deaths()
 cook_tracts <- get_cook_cty_census_tracts()
 
 tract_mort_rates <- make_tract_mort_w_absms_df(
   deaths = cook_county_deaths, 
   cook_tracts = cook_tracts)
 
 # to update stored output/tract_mort_rates.rds file, run:
 #> saveRDS(tract_mort_rates, here::here("output/tract_mort_rates.rds"))
 
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
 
}
