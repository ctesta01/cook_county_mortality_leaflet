library(here)
library(tidyverse)
library(tidycensus)
library(leaflet)
library(tigris)

df <- readr::read_csv(here("Medical_Examiner_Case_Archive.csv"))

colnames(df)

cts <- tidycensus::get_acs(
  year = 2019,
  county = 'Cook',
  state = 'IL',
  geography = 'tract',
  geometry = T,
  tidy = T,
  variables = c()
)

cook_tracts <- tigris::tracts("IL", county = "Cook", year = 2019)

cook_tracts %>% 
  leaflet() %>% 
  addTiles() %>% 
  addPolygons(
    weight = 2
    )

library(sf)

df_sf <- df %>% 
  filter(! is.na(longitude), ! is.na(latitude)) %>% 
  st_as_sf(coords = c("longitude", "latitude"),
           crs = st_crs(cook_tracts))

df_sf$tract <- as.numeric(st_within(df_sf, cook_tracts %>% st_as_sf()))

df_sf$tract <- cook_tracts$GEOID[df_sf$tract]

tract_counts <- df_sf %>% 
  as.data.frame() %>% 
  select(-geometry) %>% 
  group_by(tract) %>% 
  count() 

bins <- c(0, 10, 20, 50, 100, 200, Inf)
pal <- colorBin("YlOrRd", domain = tract_counts$n, bins = bins)

cook_tracts %>% st_as_sf() %>% 
  left_join(tract_counts,
            by = c("GEOID" = 'tract')) %>% 
  leaflet() %>% 
  addTiles() %>% 
  addPolygons(
    weight = 2,
    color = ~pal(n),
    )




# get tidycensus population sizes
# -------------------------------------------------------------------------

acs_vars <- tidycensus::load_variables(2019, dataset = 'acs5')

race_chars <-
  c(
    white = 'A',
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


head(tract_popsize)
  
tract_mort_rates <- tract_popsize %>% filter(is.na(gender), is.na(age), concept == 'SEX BY AGE') %>%
  left_join(tract_counts, by = c('GEOID' = 'tract')) %>% 
  mutate(mort_per_100k = n / estimate * 1e5) 
  
tract_mort_rates %>% 
                        
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

                              