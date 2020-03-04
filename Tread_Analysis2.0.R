#### Analysis started 3/4/20, looking at the data again using Allison's suggestions###########

#### multivariate linear model using weight ###

# First we need to join the data set with presurvey data that has the weight

library(tidyverse)
lirary(janitor)
pre_survey_data <- read_csv("Pre Survey Data.csv") 

pre_survey_data_weight <- pre_survey_data %>% 
  clean_names(.) %>% 
  select(weight, name) %>%
  mutate_if(is.character, str_to_upper) %>% #make everything upper case
  drop_na() %>% 
  filter(name != "BRI WINKLER") %>% 
  filter(name != "THOMAS BUTERA") %>% 
  filter(name != "TIMMY HUYNH") %>% 
  filter(name != "CURTIS BAUMANN") %>%
  filter(name != "SHIVA HASSON") %>%
  filter(name != "GARY FOX") %>% 
  filter(name != "LINDA HUYNH")

name_ID_weight <- full_join(pre_survey_data_weight,clean_shoe_ID)  #join together name ID and weight in one data frame

tread_joined_weight <- full_join(name_ID_weight,full_data_joined_tread_norm) 





