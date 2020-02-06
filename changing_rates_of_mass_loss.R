
########################################################################################

### Looking at if the rate of shoe mass loss changes as someone wears the shoes more ###

# Finding mean and median of number of steps to divide shoes into two groups to compare

median(full_data_joined$steps, na.rm = TRUE)

#gives 114,800 as median steps

mean(full_data_joined$steps, na.rm = TRUE)

#gives 234959 as mean steps

### Dividing the data into two sections: runners with steps greater than the mean, and runners with steps below the mean; doing the same by median

# By median

steps_lowerhalf_bymedian <- full_data_joined %>% 
  filter(steps <= 114800) %>% 
  filter(miles != 0)

steps_upperhalf_bymedian <- full_data_joined %>% 
  filter(steps > 114800) %>% 
  filter(miles != 0)

lowerhalf_mass_bymedian <- steps_lowerhalf_bymedian %>% 
  mutate(grams_per_mile=mass_change/miles)

upperhalf_mass_bymedian <- steps_upperhalf_bymedian %>% 
  mutate(grams_per_mile=mass_change/miles)

mean(lowerhalf_mass_bymedian$grams_per_mile, na.rm = TRUE)

### Gives -0.1057689 grams per mile as the mean for runners below the median number of steps taken

mean(upperhalf_mass_bymedian$grams_per_mile, na.rm = TRUE)

### Gives -0.01384143 grams per mile as the mean for runners above the median number of steps taken

#### By mean number of steps ####

steps_lowerhalf_bymean <- full_data_joined %>% 
  filter(steps <= 234959) %>% 
  filter(miles != 0)

steps_upperhalf_bymean <- full_data_joined %>% 
  filter(steps > 234959) %>% 
  filter(miles != 0)

lowerhalf_mass_bymean <- steps_lowerhalf_bymean %>% 
  mutate(grams_per_mile=mass_change/miles)

upperhalf_mass_bymean <- steps_upperhalf_bymean %>% 
  mutate(grams_per_mile=mass_change/miles)

mean(lowerhalf_mass_bymean$grams_per_mile, na.rm = TRUE)

# Gives -0.08387811 as grams lost per mile for runners below mean number of steps

mean(upperhalf_mass_bymean$grams_per_mile, na.rm = TRUE)

# Gives -0.0130713 as grams lost per mile for runners above mean number of steps

##### It looks as if the number of grams lost per mile is noticeably lower after the first half of wear in our test, whether you calculate it by mean or median! #####


####### Trying with quartiles ####

summary(full_data_joined$steps)

# First quartile is 35633, mean is 234,959, 3rd quartile is 381,600, max is 1205000

steps_1Q <- full_data_joined %>% 
  filter(steps <= 35633) %>% 
  filter(miles != 0)

steps_2Q <- full_data_joined %>% 
  filter(steps > 35633) %>% 
  filter(steps <= 234959) %>% 
  filter(miles != 0)

steps_3Q <- full_data_joined %>% 
  filter(steps > 234959) %>% 
  filter(steps <= 381600) %>% 
  filter(miles != 0)  

steps_4Q <- full_data_joined %>% 
  filter(steps > 381600) %>% 
  filter(miles != 0)

massloss_1Q <- steps_1Q %>% 
  mutate(grams_per_mile=mass_change/miles)

massloss_2Q <- steps_2Q %>% 
  mutate(grams_per_mile=mass_change/miles)

massloss_3Q <- steps_3Q %>% 
  mutate(grams_per_mile=mass_change/miles)

massloss_4Q <- steps_4Q %>% 
  mutate(grams_per_mile=mass_change/miles)

mean(massloss_1Q$grams_per_mile, na.rm = TRUE) #-0.1473715
mean(massloss_2Q$grams_per_mile, na.rm = TRUE) #-0.05501749
mean(massloss_3Q$grams_per_mile, na.rm = TRUE) #-0.01797436
mean(massloss_4Q$grams_per_mile, na.rm = TRUE) #-0.01043119
