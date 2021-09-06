
library(tidyverse)
library(vroom)
library(magrittr)
library(hms)

df <- vroom::vroom("Medical_Examiner_Case_Archive.csv")

df %<>% separate(`Date of Death`, sep = " ", into = c('date_of_death', 'time_of_death', 'am_pm_death'), remove = F)

df %<>% mutate(
  date_of_death = lubridate::mdy(date_of_death),
  time_of_death = lubridate::mdy_hms(`Date of Death`),
  weekday_death = weekdays(date_of_death),
  time_of_death = hms::as_hms(time_of_death))
  
df %>% 
  mutate(weekday_death = factor(weekday_death, c("Monday", "Tuesday", "Wednesday", 'Thursday', 'Friday', 'Saturday', 'Sunday'))) %>% 
  ggplot(aes(x = time_of_death, fill = as.integer(lubridate::month(date_of_death)), group = lubridate::month(date_of_death))) + 
  geom_histogram(position = 'identity', alpha=0.6) + 
  geom_smooth() + 
  facet_grid(~weekday_death) + 
  theme(axis.text.x = element_text(angle = 90))
  


df %>% group_by(
  month_of_death = lubridate::month(date_of_death),
  weekday_death,
  hour = lubridate::hour(time_of_death)) %>% 
  summarize(
    count = n()) %>%
  ggplot(aes(x = hour, y = count, color = month_of_death, group = month_of_death)) + 
  geom_point() + 
  geom_line() + 
  facet_grid(~weekday_death) + 
  theme(axis.text.x = element_text(angle = 90)) + 
  scale_color_gradient2(low = 'darkblue', mid = 'lightpink', high = 'darkblue', midpoint = 3)

df %>% group_by(
  month_of_death = lubridate::month(date_of_death),
  weekday_death,
  hour = lubridate::hour(time_of_death)) %>% 
  summarize(
    count = n()) %>%
  ggplot(aes(x = hour, y = count, color = weekday_death, group = weekday_death, shape = weekday_death)) + 
  geom_point() + 
  geom_line() + 
  facet_grid(~month_of_death) + 
  theme(axis.text.x = element_text(angle = 90)) 
