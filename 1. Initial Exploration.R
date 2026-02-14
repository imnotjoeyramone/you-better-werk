#### Setup ####

library(tidyverse)
library(vroom)
library(lubridate)
library(janitor)
library(corrr)

measurements <- vroom("data/measurement_data.csv") %>%
  mutate(date = dmy(str_remove(date, ",.*$")))
workout <- vroom("data/workout_data.csv")  %>%
  mutate(date = dmy(str_remove(start_time, ",.*$")), .before = title)


# join measurements to workout data

weight <- measurements %>% 
  select(date_measured = date, body_weight = weight_kg) %>% 
  filter(!is.na(body_weight))

combined_data <- 
  workout %>% 
  cross_join(weight) %>% 
  mutate(date_diff = abs(as.numeric(date - date_measured))) %>% 
  group_by(date, title) %>% 
  slice_min(date_diff) %>% 
  ungroup()

# Pick an exercise

bench <- 
  combined_data %>% 
  subset(exercise_title == "Bench Press (Barbell)" & set_index != 0) %>% 
  select(date, exercise_title, weight_kg, reps, body_weight) %>% 
  group_by(date) %>% 
  slice_max(weight_kg, n = 1, with_ties = F) %>% # max weight set per date
  arrange(date)

bench %>% 
  ggplot(aes(x=date, y=weight_kg))+
  geom_line()+
  geom_smooth()

measurements %>% 
  filter(!is.na(weight_kg)) %>% 
  ggplot(aes(x=date, y=weight_kg))+
  geom_line()


