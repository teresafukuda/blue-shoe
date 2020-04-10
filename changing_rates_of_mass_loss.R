library(Rmisc)


########################################################################################

### Looking at if the rate of shoe mass loss changes as someone wears the shoes more ###

# Finding mean and median of number of steps to divide shoes into two groups to compare

median(mass_joined_fixed_noout$`miles to steps`, na.rm = TRUE)

#gives 115,000 as median steps

mean(mass_joined_fixed_noout$`miles to steps`, na.rm = TRUE)

#gives 228614.3 as mean steps

### Dividing the data into two sections: runners with steps greater than the mean, and runners with steps below the mean; doing the same by median

# By median

steps_lowerhalf_bymedian <- mass_joined_fixed_noout %>% 
  filter(steps <= 115000) %>% 
  filter(miles != 0)

steps_upperhalf_bymedian <- mass_joined_fixed_noout %>% 
  filter(steps > 115000) %>% 
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

steps_lowerhalf_bymean <- mass_joined_fixed_noout %>% 
  filter(steps <= 228614.3) %>% 
  filter(miles != 0)

steps_upperhalf_bymean <- mass_joined_fixed_noout %>% 
  filter(steps > 228614.3) %>% 
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

summary(mass_joined_fixed_noout$`miles to steps`)

# First quartile is 35633, mean is 234,959, 3rd quartile is 381,600, max is 1205000

steps_1Q <- full_data_joined %>% 
  filter(steps <= 49440) %>% 
  filter(miles != 0)

steps_2Q <- full_data_joined %>% 
  filter(steps > 49440) %>% 
  filter(steps <= 323580) %>% 
  filter(miles != 0)

steps_3Q <- full_data_joined %>% 
  filter(steps > 323580) %>% 
  filter(steps <= 1098480) %>% 
  filter(miles != 0)  

steps_4Q <- full_data_joined %>% 
  filter(steps > 1098480) %>% 
  filter(miles != 0)

massloss_1Q <- steps_1Q %>% 
  mutate(grams_per_mile=mass_change/miles)

# Finding mean and SE to add error bars

summary_1Q <- summarySE(massloss_1Q, measurevar="grams_per_mile")

#Standard error is 0.0472939

massloss_2Q <- steps_2Q %>% 
  mutate(grams_per_mile=mass_change/miles)

summary_2Q <- summarySE(massloss_2Q, measurevar="grams_per_mile")

#Standard error is 	0.01866876

massloss_3Q <- steps_3Q %>% 
  mutate(grams_per_mile=mass_change/miles)

summary_3Q <- summarySE(massloss_3Q, measurevar="grams_per_mile")

#Standard Error is 0.002177654

massloss_4Q <- steps_4Q %>% 
  mutate(grams_per_mile=mass_change/miles)

summary_4Q <- summarySE(massloss_4Q, measurevar="grams_per_mile")

#Standard Error is 0.00101758

mean(massloss_1Q$grams_per_mile, na.rm = TRUE) #-0.1194021
mean(massloss_2Q$grams_per_mile, na.rm = TRUE) #-0.05163573
mean(massloss_3Q$grams_per_mile, na.rm = TRUE) #-0.01106753
mean(massloss_4Q$grams_per_mile, na.rm = TRUE) #-0.006974372

#Looking at top 10% and bottom 10%

# Top 10

# Order and select participants with top 10 in miles walked

top10miles <- mass_joined_fixed_noout[with(mass_joined_fixed_noout,order(-steps)),]

top10miles2 <- top10miles[1:10,]

# Calculate mass change per mile

massloss_top10 <- top10miles2 %>% 
  mutate(grams_per_mile=mass_change/miles)

mean(massloss_top10$grams_per_mile, na.rm = TRUE) # mean is -0.01484602

summary_top10 <- summarySE(massloss_top10, measurevar="grams_per_mile", na.rm = TRUE) # standard error is 0.004427872

# Bottom 10

# Order and select participants with top 10 in miles walked. Remove participants that didn't walk at all

bottom10miles <- mass_joined_fixed_noout %>% 
  filter(miles != 0) 

bottom10miles2 <- bottom10miles[with(bottom10miles,order(steps)),] 

bottom10miles3 <- bottom10miles2[1:10,]

# Calculate mass change per mile

massloss_bottom10 <- bottom10miles3 %>% 
  mutate(grams_per_mile=mass_change/miles)

mean(massloss_bottom10$grams_per_mile, na.rm = TRUE) # mean is -0.298494

summary_bottom10 <- summarySE(massloss_bottom10, measurevar="grams_per_mile", na.rm = TRUE) # standard error is 0.08305663

#### Making a graph of this data ####

mass_change <- read_csv("rates_of_mass_loss.csv")

mass_change$interval <- factor(mass_change$interval, levels = mass_change$interval[order(mass_change$mass_change_per_mile)])

mass_change_plot <- ggplot(mass_change, aes(x=interval, y=mass_change_per_mile))+
  geom_point()+
  theme_bw()+
  theme(axis.text.x = element_text(angle = 75, hjust=1), 
        axis.text=element_text(size=12),
        axis.title=element_text(size=14,face="bold"),
        legend.text = element_text(size = 15),
        legend.title = element_text(size = 20))+
  labs(x = "Interval of Wear", y = "Average Mass Lost per Mile (g)")+
  geom_pointrange(aes(ymin=mass_change_per_mile-se, ymax=mass_change_per_mile+se))
mass_change_plot
