
########################################################################################

### Looking at if the rate of shoe mass loss changes as someone wears the shoes more ###

# Finding mean and median of number of steps to divide shoes into two groups to compare

median(full_data_joined$steps, na.rm = TRUE)

#gives 114,800 as median steps

mean(full_data_joined$steps, na.rm = TRUE)

#gives 232706.7 as mean steps

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
  filter(steps <= 232706.7) %>% 
  filter(miles != 0)

steps_upperhalf_bymean <- full_data_joined %>% 
  filter(steps > 232706.7) %>% 
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