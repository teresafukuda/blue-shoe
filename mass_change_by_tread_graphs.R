
######################################################################################
### Looking at if the rate of tread change shoe mass loss changes as someone wears the shoes more, this is using Andrew P's code


library(tidyverse) # hello tidyverse
library(janitor) # load janitor to clean up dataframe
library(ggthemes)

#join data together with step data

# Finding mean and median of number of steps to divide shoes into two groups to compare

median(tread_joined$miles, na.rm = TRUE)

#57.5 miles

mean(tread_mass_joined$miles, na.rm = TRUE)

#107.6568 miles

### Dividing the data into two sections: runners with steps greater than the mean, and runners with steps below the mean; doing the same by median

# By median

miles_lowerhalf_bymedian <- tread_mass_joined %>% 
  filter(miles <= 57.5) %>% 
  filter(miles != 0)

miles_upperhalf_bymedian <- tread_mass_joined %>% 
  filter(miles > 57.5) %>% 
  filter(miles != 0)

lowerhalf_mass_bymedian <- miles_lowerhalf_bymedian %>% 
  mutate(grams_per_mile=mass_change/miles)

upperhalf_mass_bymedian <- miles_upperhalf_bymedian %>% 
  mutate(grams_per_mile=mass_change/miles)

mean(lowerhalf_mass_bymedian$grams_per_mile, na.rm = TRUE)

### Gives -0.129777 grams per mile as the mean for runners below the median number of steps taken

mean(upperhalf_mass_bymedian$grams_per_mile, na.rm = TRUE)

### Gives -0.01005713 grams per mile as the mean for runners above the median number of steps taken

#### By mean number of steps ####

steps_lowerhalf_bymean <- tread_mass_joined %>% 
  filter(miles <= 107.6568) %>% 
  filter(miles != 0)

steps_upperhalf_bymean <- tread_mass_joined %>% 
  filter(miles > 107.6568) %>% 
  filter(miles != 0)

lowerhalf_mass_bymean <- steps_lowerhalf_bymean %>% 
  mutate(grams_per_mile=mass_change/miles)

upperhalf_mass_bymean <- steps_upperhalf_bymean %>% 
  mutate(grams_per_mile=mass_change/miles)

mean(lowerhalf_mass_bymean$grams_per_mile, na.rm = TRUE)

# Gives -0.1108309 as grams lost per mile for runners below mean number of steps

mean(upperhalf_mass_bymean$grams_per_mile, na.rm = TRUE)

# Gives -0.009905905 as grams lost per mile for runners above mean number of steps

##### It looks as if the number of grams lost per mile using tread depth measurement and coverting it to mass is still noticeably lower after the first half of wear in our test, whether you calculate it by mean or median! #####


####### Trying with quartiles ####

summary(tread_mass_joined$miles)

# First quartile is 35633, mean is 234,959, 3rd quartile is 381,600, max is 1205000

steps_1Q <- tread_mass_joined %>% 
  filter(`steps to miles` <=  22.0) %>% 
  filter(miles != 0)

steps_2Q <- tread_mass_joined %>% 
  filter(`steps to miles` >  22.0) %>% 
  filter(`steps to miles` <=  57.5) %>% 
  filter(miles != 0)

steps_3Q <- tread_mass_joined %>% 
  filter(`steps to miles` >  57.5) %>% 
  filter(`steps to miles` <= 161.8 ) %>% 
  filter(miles != 0)  

steps_4Q <- tread_mass_joined %>% 
  filter(`steps to miles` > 161.8 ) %>% 
  filter(miles != 0)

massloss_1Q <- steps_1Q %>% 
  mutate(grams_per_mile=mass_change/miles)

massloss_2Q <- steps_2Q %>% 
  mutate(grams_per_mile=mass_change/miles)

massloss_3Q <- steps_3Q %>% 
  mutate(grams_per_mile=mass_change/miles)

massloss_4Q <- steps_4Q %>% 
  mutate(grams_per_mile=mass_change/miles)

mean(massloss_1Q$grams_per_mile, na.rm = TRUE) #-0.1637461
mean(massloss_2Q$grams_per_mile, na.rm = TRUE) #-0.1090796
mean(massloss_3Q$grams_per_mile, na.rm = TRUE) #-0.009359978
mean(massloss_4Q$grams_per_mile, na.rm = TRUE) #-0.009663298

#Looking at top 10% and bottom 10%

# Top 10

# Order and select participants with top 10 in miles walked

top10treadmiles <- tread_mass_joined[with(tread_mass_joined,order(-miles)),]

top10treadmiles2 <- top10treadmiles[1:10,]

# Calculate mass change per mile

treadmassloss_top10 <- top10treadmiles2 %>% 
  mutate(grams_per_mile=mass_change/miles)

mean(treadmassloss_top10$grams_per_mile, na.rm = TRUE) # mean is -0.005271418

# Bottom 10

# Order and select participants with top 10 in miles walked. Remove participants that didn't walk at all

bottom10treadmiles <- tread_mass_joined %>% 
  filter(miles != 0) 

bottom10treadmiles2 <- bottom10treadmiles[with(bottom10treadmiles,order(steps)),] 

bottom10treadmiles3 <- bottom10treadmiles2[1:10,]

# Calculate mass change per mile

treadmassloss_bottom10 <- bottom10treadmiles3 %>% 
  mutate(grams_per_mile=mass_change/miles)

mean(treadmassloss_bottom10$grams_per_mile, na.rm = TRUE) # mean is -0.2153745