########################################################
##Post Wear Test Survey Data Management 
## ... an attempt to import the data and make it into a usable form for eas[ier] analysis
## by Teresa Fukuda
## 
##########################################################
## Upload the data, tidy the answers, analyse into values that can give an idea of how many shoes a person goes through a year
## Every person goes through some number of shoes/year... trying to figure out the number


# Part I. Load packages and import data

library(tidyverse) # hello tidyverse
library(janitor) # load janitor to clean up dataframe
library(lubridate) # load lubridate to work with dates and times


post_data<- read_csv("Edited_headers_postform.csv") # second sheet of the Post-Survey Form (Responses) from Drive




# Part II. Tidy data

clean_post <- post_data %>% 
  mutate_if(is.character, str_to_upper) %>% 
  mutate(shoe_id_left=`Shoe ID Left (ex: L1)`) %>%
  mutate(shoe_id_right=`Shoe ID Right (ex: R1) (same as left ID!)`) %>% 
  mutate(shoe_id_left=gsub("-","",.$shoe_id_left)) %>% 
  mutate(shoe_id_right=gsub("-","",.$shoe_id_right)) %>%
  mutate(shoe_id_left=gsub(" ","",.$shoe_id_left)) %>% 
  mutate(shoe_id_right=gsub(" ","",.$shoe_id_right)) %>% 
  select(-c(`Shoe ID Left (ex: L1)`)) %>% 
  select(-c(`Shoe ID Right (ex: R1) (same as left ID!)`))

# Part III. Some data visualization

# How many shoes in rotation histogram
total_shoes_rotation_plot <- ggplot(clean_post, aes(x=total_current))+
  geom_histogram(stat="count")

total_shoes_rotation_plot

# How many shoes retired due to excess wear
excess_wear_plot <- ggplot(clean_post, aes(x=retired_from_wear))+
  geom_histogram(stat="count")

excess_wear_plot

# wear frequency plot
wear_frequency_plot <- ggplot(clean_post, aes(x=wear_frequency))+
  geom_histogram(stat="count")+
  theme(axis.text.x = element_text(angle = 90, hjust = 1))
wear_frequency_plot


# shoes in rotation vs wear frequency of the adidas shoes
rotation_frquency_plot <- ggplot(clean_post, aes(x=wear_frequency, y=total_current))+
  geom_point()+
  theme(axis.text.x = element_text(angle = 90, hjust = 1))
rotation_frquency_plot # not much relationship between number of shoes in rotation and how frequently they wore our shoes... 
