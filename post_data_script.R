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


#summarize continuous data

post_means <- clean_post %>% 
  summarize_all(funs(mean), na.rm=TRUE)

# Part III. Some data visualization

# How many shoes in rotation histogram
total_shoes_rotation_plot <- ggplot(clean_post, aes(x=total_current))+
  geom_histogram(stat="count")+
  theme_bw()+
  theme(axis.text.x = element_text(angle = 75, hjust=1), axis.text=element_text(size=12),
        axis.title=element_text(size=14,face="bold"))+
  labs(x = "Total Shoes in Rotation", y = "Frequency")

total_shoes_rotation_plot

# How many shoes retired due to excess wear
excess_wear_plot <- ggplot(clean_post, aes(x=retired_from_wear))+
  geom_histogram(stat="count")+
  theme_bw()+
  theme(axis.text.x = element_text(angle = 75, hjust=1),axis.text=element_text(size=12),
        axis.title=element_text(size=14,face="bold"))+
  labs(x = "Shoes Retired due to Excess Wear per Year", y = "Frequency")

excess_wear_plot

# How many shoes retired due to other reasons
ugly_wear_plot <- ggplot(clean_post, aes(x=retired_ugly))+
  geom_histogram(stat="count")+
  theme_bw()+
  theme(axis.text.x = element_text(angle = 75, hjust=1),axis.text=element_text(size=12),
        axis.title=element_text(size=14,face="bold"))+
  labs(x = "Shoes Retired due to Style/Aesthetics", y = "Frequency")

ugly_wear_plot

# wear frequency plot
wear_frequency_plot <- ggplot(clean_post, aes(x=wear_frequency))+
  geom_histogram(stat="count")+
  theme_bw()+
  theme(axis.text.x = element_text(angle = 75, hjust=1), axis.text=element_text(size=12),
        axis.title=element_text(size=14,face="bold"))+
  labs(x = "Frequency that the Study Shoes Were Worn", y = "Count")

wear_frequency_plot

# Performance of shoes (scale of 1 to 5)
perf_plot <- ggplot(clean_post, aes(x=performance_of_shoes, fill=wantback))+
  geom_histogram(stat="count")+
  theme_bw()+
  theme(axis.text.x = element_text(angle = 0, hjust=.5),axis.text=element_text(size=12),
        axis.title=element_text(size=14,face="bold"))+
  labs(x = "Shoe Performance Rating", y = "Frequency", fill="Want shoes back")

perf_plot


# shoes in rotation vs wear frequency of the adidas shoes
rotation_frquency_plot <- ggplot(clean_post, aes(x=wear_frequency, y=total_current))+
  geom_point()+
  theme(axis.text.x = element_text(angle = 90, hjust = 1))
rotation_frquency_plot # not much relationship between number of shoes in rotation and how frequently they wore our shoes... 
