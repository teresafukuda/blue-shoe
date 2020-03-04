
library(tidyverse) # hello tidyverse
library(janitor) # load janitor to clean up dataframe
library(ggthemes)



tread_mass_average <- tread_joined %>% 
  group_by(shoe_ID) %>% 
  summarize(
    average_tread_mass=mean(tread_mass)
    ) 

#join step data with new data

full_data_joined_tread <- full_join(full_data_joined, tread_mass_average) 

#create a data frame that includes normalized mass loss

full_data_joined_tread_norm <- full_data_joined_tread %>% 
  mutate("tread_mass_lost_per_mile"= average_tread_mass/`steps to miles`) %>% 
  drop_na(mass_lost_per_mile)

full_data_joined_tread_norm



treadmass_miles_lm <- lm(average_tread_mass ~ `steps to miles`, data = full_data_joined_tread)
summary(treadmass_miles_lm) 
#Multiple R-squared:  0.1843,	Adjusted R-squared:  0.1736 
#F-statistic: 17.17 on 1 and 76 DF,  p-value: 8.772e-05

#add trendline to
treadmass_miles_lm_graph<- ggplot(full_data_joined_tread, aes(x=`steps to miles`, y= average_tread_mass))+
  geom_point()+
  ylim(-8,4)+
  xlim(NA, 600)+
  theme_bw()+
  labs(x = "Distance Travelled (Miles)", y = "Mass Change (g)")+
  geom_smooth(method = "lm", color = "indianred1")+ # add se = FALSE to remove error bar
  geom_hline(yintercept=c(0), color="blue")
treadmass_miles_lm_graph




######################## Updating mass change by tread NORMALIZED  vs variables ############################

# Wrangling to remove high ourliers, -Inf values, NA shoe types

# tread mass loss vs. rubber

treadmass_vs_rubber <- ggplot(full_data_joined_tread_norm, aes(x=rubber_type, y=mass_lost_per_mile))+
  geom_point()+
  theme_bw()+
  theme(axis.text.x = element_text(angle = 75, hjust=1))+
  labs(title = "Mass Lost Calc by tread", x = "Rubber Type", y = "Mass Lost per Mile (g)")

treadmass_vs_rubber

# tread mass loss vs. hardness

treadmass_vs_hardness <- ggplot(full_data_joined_tread_norm, aes(x=hardness, y=mass_lost_per_mile))+
  geom_point()+
  theme_bw()+
  theme(axis.text.x = element_text(angle = 75, hjust=1))+
  labs(title = "Mass Lost Calc by tread, hardness", x = "Hardness", y = "Mass Lost per Mile (g)")

treadmass_vs_hardness

# tread mass loss vs. geometry

treadmass_vs_geometry <- ggplot(full_data_joined_tread_norm, aes(x=geometry, y=mass_lost_per_mile))+
  geom_point()+
  theme_bw()+
  theme(axis.text.x = element_text(angle = 75, hjust=1))+
  labs(title = "Mass Lost Calc by tread, geometry", x = "Geometry", y = "Mass Lost per Mile (g)")

treadmass_vs_geometry


#tread mass normalized lost based on shoe model
treadmasslost_model <- ggplot(full_data_joined_tread_norm, aes(x = model, y = mass_lost_per_mile)) +
  geom_boxplot() +
  theme_bw() +
  ylim(-0.51,.4)+
  theme(axis.text.x = element_text(angle = 45, hjust=1, size=12))+
  theme(axis.text.y = element_text(size = 12))+
  theme(axis.title.y = element_text(size = 12))+
  labs(x = "", y = "Mass Change per Mile (g)")
  
  
treadmasslost_model




##################Calculating some averages for the slides########################
#mean

mean_average_tread_mass_change <- mean(full_data_joined_tread_norm$average_tread_mass, na.rm = TRUE)
mean_average_tread_mass_change
#-1.841239

mean_average_tread_mass_changenorm <- mean(full_data_joined_tread_norm$mass_lost_per_mile, na.rm = TRUE)
mean_average_tread_mass_changenorm
#-0.03541883


#############Calculating measurement error for tread depth measurements#############
tread_depth_measureerror <- tread_depth_raw_initial %>% 
  mutate("measurement_difference"= final_mm-initial_mm) %>% 
  group_by(shoe_ID,model,side,location) %>% 
  summarize(
      average_measurement_difference=mean(measurement_difference)
    ) %>% 
  drop_na()

abs(tread_depth_measureerror$average_measurement_difference) #abslute value of the differences 
#the mean of these abolute values is 0.238645833



##############Boxplot of mass_changed per mile###################

tread_mass_norm_boxplot <- ggplot(full_data_joined_tread_norm, aes(y=mass_lost_per_mile))+
  geom_boxplot()+
  ylim(-0.8,.4)+
  theme_bw()

tread_mass_norm_boxplot

measured_mass_norm_boxplot <- ggplot(full_data_joined, aes(y=measuremass_lost_per_mile))+
  geom_boxplot()+ 
  ylim(-0.8,.4)+
  theme_bw()

measured_mass_norm_boxplot



###### find the median and mean of average tread mass lost #######
median(full_data_joined_tread_norm$mass_lost_per_mile, na.rm = TRUE)
mean(full_data_joined_tread_norm$mass_lost_per_mile, na.rm = TRUE)



##########The script following this is is if we wanted to analyze by quartile###########


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

