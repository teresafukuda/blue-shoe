
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
  mutate("mass_lost_per_mile"= average_tread_mass/`steps to miles`) %>% 
  drop_na(mass_lost_per_mile)



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
  labs(x = "Distance Travelled (Miles)", y = "Mass Lost (g)")+
  geom_smooth(method = "lm", color = "indianred1")+ # add se = FALSE to remove error bar
  geom_hline(yintercept=c(0), color="blue")
treadmass_miles_lm_graph



######################## Updating mass change by tread  vs variables ############################

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


#tread mass lost based on shoe model
treadmasslost_model <- ggplot(full_data_joined_tread_norm, aes(x = model, y = mass_lost_per_mile)) +
  geom_point(aes(color = model,
                  pch = model)) +
  ylim(-0.51,.4)+
  theme_bw() +
  scale_shape_manual(values=seq(0,15)) #need more shapes
  #scale_color_manual(name="Sex",
                     #values=c("dark green","light blue"))+

treadmasslost_model
