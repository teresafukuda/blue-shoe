
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
  ylim(-0.51,.4)+
  theme(axis.text.x = element_text(angle = 45, hjust=1))+
  labs(x = "", y = "Mass Change per Mile (g)")
  
  scale_shape_manual(values=seq(0,15)) #need more shapes
  #scale_color_manual(name="Sex",
                     #values=c("dark green","light blue"))+

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
