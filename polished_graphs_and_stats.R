
#################################

# More stats, making modifications to original graphs, trying to get them ready for presentations

################################

### Packages ###

library(tidyverse) # hello tidyverse
library(janitor) # load janitor to clean up dataframe
library(lubridate) # load lubridate to work with dates and times
library(ggthemes) # ggplot themes
library(plm)# regression analysis package

################

### Grams lost versus steps taken: "grams_steps_scatter"

# Running a regression #

grams_steps_lm <- lm(mass_change ~ steps, data = full_data_joined)
summary(grams_steps_lm)

# P value is less than .01. Multiple R squared is .06152

# Adding trendline to graph #

grams_steps_scatter_lm <- ggplot(full_data_joined, aes(x=`steps to miles`, y=mass_change))+
  geom_point()+
  theme_bw()+
  labs(x = "Distance Travelled (miles)", y = "Mass Change (g)")+
  geom_smooth(method = "lm", color = "indianred1")+ # add se = FALSE to remove error bar
  geom_hline(yintercept=c(0), color="darkblue")+
  theme(axis.text=element_text(size=12),
        axis.title=element_text(size=14))

grams_steps_scatter_lm

#### Questions for group ####  
# should we add R squared and slope to the graph?  
# Should we remove error bar?
# Should we remove high outliers?

### Grams lost versus miles travelled: "grams_miles_scatter"

# Running a regression #

grams_miles_lm <- lm(mass_change ~ `steps to miles`, data = full_data_joined)
summary(grams_miles_lm)

# P value is less than .01. Multiple R squared is .06152

# Adding trendline to graph #

grams_steps_scatter_lm <- ggplot(full_data_joined, aes(x=`steps to miles`, y=mass_change))+
  geom_point()+
  theme_bw()+
  labs(x = "Distance Travelled (miles)", y = "Mass Change (g)")+
  geom_smooth(method = "lm", color = "indianred1")+ # add se = FALSE to remove error bar
  geom_hline(yintercept=c(0), color="darkblue")

grams_steps_scatter_lm


################## Graph of mass change vs. tread depth lost #################

tread_mass_joined <- full_join(tread_joined_details, step_calculations)

# Verify that I didn't take too many datapoints out

### Normalize tread depth by steps taken ###

tread <- tread_mass_joined %>% 
  mutate("mm_per_mile"= avg_depth_change/miles) %>% 
  mutate("weight_kg"= weight*0.453592) %>% 
  mutate("mm_per_miles_per_kg"=mm_per_mile/weight_kg) 

tread_calculations <- tread %>% ### Remove rows that prevent regression
  filter(steps != 0) 

mass_vs_tread <- ggplot(tread_calculations, aes(x=mm_per_mile, y=g_per_milesteps))+
  geom_point()+
  theme_bw()+
  scale_y_continuous(limits = c(-0.8, 0.2))+
  scale_x_continuous(limits = c(-0.12, 0.02))+
  geom_hline(yintercept=c(0), color="blue")+
  geom_vline(xintercept=c(0), color="blue")+
  labs(title = "Change in Mass vs. Change in Tread Depth (Normalized by Distance)", x = "Tread Change per Mile (mm)", y = "Change in Mass per Mile (g)")
  
mass_vs_tread

mass_tread_lm <- lm(mm_per_mile ~ g_per_milesteps, data = tread_calculations)
summary(mass_tread_lm)

# Ran a regression between mass lost and tread depth change, P > 0.001, R-squared = 0.1569. 

# mm_per_mile = -0.007202 + 0.048617*g_per_milesteps

tread_mass_lm <- lm(g_per_milesteps ~ mm_per_mile, data = tread_calculations)
summary(tread_mass_lm)



################### Graphs of tread depth change vs. different variables ################



# Tread depth vs rubber type

tread_change_vs_rubber <- ggplot(tread_calculations, aes(x=rubber_type, y=mm_per_mile))+
  geom_point()+
  theme_bw()+
  theme(axis.text.x = element_text(angle = 75, hjust=1))+
  labs(title = "Rate of Tread Depth Change by Rubber Type", x = "Rubber Type", y = "Tread Change per Mile (mm)")

tread_change_vs_rubber

# Tread depth vs hardness

tread_change_vs_hardness <- ggplot(tread_calculations, aes(x=hardness, y=mm_per_mile))+
  geom_point()+
  theme_bw()+
  theme(axis.text.x = element_text(angle = 75, hjust=1))+
  labs(title = "Rate of Tread Depth Change by Hardness", x = "Hardness", y = "Tread Change per Mile (mm)")

tread_change_vs_hardness

# Tread depth vs geometry

tread_change_vs_geometry <- ggplot(tread_calculations, aes(x=geometry, y=mm_per_mile))+
  geom_point()+
  theme_bw()+
  theme(axis.text.x = element_text(angle = 75, hjust=1))+
  labs(title = "Rate of Tread Depth Change by Geometry", x = "Geometry", y = "Tread Change per Mile (mm)")

tread_change_vs_geometry

### Tukey's HSD for tread depth vs. our variables ###

# First, ANOVA for each variable/relationship

# Rubber Type
tread_rubber_lm <- lm(mm_per_mile ~ rubber_type, data = tread_calculations)
tread_rubber_av <- aov(tread_rubber_lm)
summary(tread_rubber_av)

# P-value is 0.6, so no significant difference in means. No Tukey's HSD

# Hardness
tread_hardness_lm <- lm(mm_per_mile ~ hardness, data = tread_calculations)
tread_hardness_av <- aov(tread_hardness_lm)
summary(tread_hardness_av)

# P-value is 0.847, so no significant difference in means. No Tukey's HSD

# Geometry
tread_geometry_lm <- lm(mm_per_mile ~ geometry, data = tread_calculations)
tread_geometry_av <- aov(tread_geometry_lm)
summary(tread_geometry_av)

# P-value is 0.0383, so a significant difference in means. Tukey's HSD:

tread_geometry_tukey <- TukeyHSD(tread_geometry_av)
tread_geometry_tukey

# Only significant difference is between running and lifestyle (p = 0.038) 

### Tukey's HSD for mass change vs our variables ###

# First, remove rows that prevent regression from occuring (g_per_steps = 0, because this gives -Inf for g_per_milesteps)

step_calc_for_Tukey <- step_calculations %>% 
  filter(steps != 0)

# Then, ANOVA for each variable/relationship

# Rubber Type
mass_rubber_lm <- lm(g_per_milesteps ~ rubber_type, data = step_calc_for_Tukey)
mass_rubber_av <- aov(mass_rubber_lm)
summary(mass_rubber_av)

# P-value is 0.138, so no significant difference in means. No Tukey's HSD

# Hardness
mass_hardness_lm <- lm(g_per_milesteps ~ hardness, data = step_calc_for_Tukey)
mass_hardness_av <- aov(mass_hardness_lm)
summary(mass_hardness_av)

# P-value is 0.547, so no significant difference in means. No Tukey's HSD

# Geometry
mass_geometry_lm <- lm(g_per_milesteps ~ geometry, data = step_calc_for_Tukey)
mass_geometry_av <- aov(mass_geometry_lm)
summary(mass_geometry_av)

# P-value is 0.983, so no significant difference in means. No Tukey's HSD


######################### Making tread depth change by model graph #################

# Ultra Boost Stella #

tread_stella <- tread_joined_shoeID %>% 
  filter(model == "Ultra Boost Stella")

stella_error <- tread_error %>% 
  filter(shoe_id == "R38")

tread_scatter_stella <- tread_stella %>% 
  arrange(avg_depth_change) %>% 
  ungroup() %>% 
  mutate(shoe_ID=factor(shoe_ID, levels=shoe_ID)) %>% 
  ggplot(., aes(x=shoe_ID, y=avg_depth_change))+
  geom_point(aes(color="blue"))+
  theme_classic()+
  theme(axis.text.x = element_text(angle = 90, hjust = 1))+
  theme(legend.position = "none")+
  geom_point(data=stella_error, aes(x=shoe_ID, y=avg_depth_change))+
  geom_hline(yintercept=c(0), color="blue")+
  labs(title = "Tread Change, Ultra Boost Stella", y = "Average Tread Depth Change (mm)", x = "Shoe ID")

tread_scatter_stella

# Terrex Boat #

tread_terrex <- tread_joined_shoeID %>% 
  filter(model == "Adidas Terrex Boat")

terrex_error <- tread_error %>% 
  filter(model == "ADIDAS TERREX BOAT")

tread_scatter_terrex <- tread_terrex %>% 
  arrange(avg_depth_change) %>% 
  ungroup() %>% 
  mutate(shoe_ID=factor(shoe_ID, levels=shoe_ID)) %>% 
  ggplot(., aes(x=shoe_ID, y=avg_depth_change))+
  geom_point(aes(color="blue"))+
  theme_classic()+
  theme(axis.text.x = element_text(angle = 90, hjust = 1))+
  theme(legend.position = "none")+
  geom_point(data=terrex_error, aes(x=shoe_ID, y=avg_depth_change))+
  geom_hline(yintercept=c(0), color="blue")+
  labs(title = "Tread Change, Terrex Boat", y = "Average Tread Depth Change (mm)", x = "Shoe ID")

tread_scatter_terrex

# Ultratech

tread_ultratech <- tread_joined_shoeID %>% 
  filter(model == "Adidas Ultratech")

ultratech_error <- tread_error %>% 
  filter(model == "ADIDAS ULTRATECH")

tread_scatter_ultratech <- tread_ultratech %>% 
  arrange(avg_depth_change) %>% 
  ungroup() %>% 
  mutate(shoe_ID=factor(shoe_ID, levels=shoe_ID)) %>% 
  ggplot(., aes(x=shoe_ID, y=avg_depth_change))+
  geom_point(aes(color="blue"))+
  theme_classic()+
  theme(axis.text.x = element_text(angle = 90, hjust = 1))+
  theme(legend.position = "none")+
  geom_point(data=ultratech_error, aes(x=shoe_ID, y=avg_depth_change))+
  geom_hline(yintercept=c(0), color="blue")+
  labs(title = "Tread Change, Ultratech", y = "Average Tread Depth Change (mm)", x = "Shoe ID")

tread_scatter_ultratech

# Nizza(50A syn) white

tread_nizzawhite <- tread_joined_shoeID %>% 
  filter(model == "Adidas Nizza(50A syn) white")

nizzawhite_error <- tread_error %>% 
  filter(shoe_id == "L54")

tread_scatter_nizzawhite <- tread_nizzawhite %>% 
  arrange(avg_depth_change) %>% 
  ungroup() %>% 
  mutate(shoe_ID=factor(shoe_ID, levels=shoe_ID)) %>% 
  ggplot(., aes(x=shoe_ID, y=avg_depth_change))+
  geom_point(aes(color="blue"))+
  theme_classic()+
  theme(axis.text.x = element_text(angle = 90, hjust = 1))+
  theme(legend.position = "none")+
  geom_point(data=nizzawhite_error, aes(x=shoe_ID, y=avg_depth_change))+
  geom_hline(yintercept=c(0), color="blue")+
  labs(title = "Tread Change, Nizza 50A Syn White", y = "Average Tread Depth Change (mm)", x = "Shoe ID")

tread_scatter_nizzawhite

# Example

tread_change_scatter <- tread_joined_shoeID %>% 
  arrange(avg_depth_change) %>% 
  ungroup() %>% 
  mutate(shoe_ID=factor(shoe_ID, levels=shoe_ID)) %>% 
  ggplot(., aes(x=shoe_ID, y=avg_depth_change))+
  geom_point(aes(color="blue"))+
  theme_classic()+
  theme(axis.text.x = element_text(angle = 90, hjust = 1))+
  theme(legend.position = "none")+
  geom_point(data=tread_error, aes(x=shoe_ID, y=avg_depth_change))+
  geom_hline(yintercept=c(0), color="blue")

tread_change_scatter

######################## Updating mass change vs variables ############################

# Wrangling to remove high ourliers, -Inf values, NA shoe types

step_calc <- step_calculations %>% 
  filter(rubber_type == "NATURAL" | rubber_type == "SYNTHETIC") %>% 
  filter(g_per_milesteps < 0.5) %>% 
  filter(g_per_milesteps > -10)

# Mass loss vs Rubber Type

mass_vs_rubber <- ggplot(step_calc, aes(x=rubber_type, y=g_per_milesteps))+
  geom_point()+
  theme_bw()+
  theme(axis.text.x = element_text(angle = 75, hjust=1))+
  labs(title = "Rate of Mass Change by Rubber Type", x = "Rubber Type", y = "Mass Lost per Mile (g)")

mass_vs_rubber

# As a boxplot

mass_vs_rubber_box <- ggplot(step_calc, aes(x=rubber_type, y=g_per_milesteps))+
  geom_boxplot()+
  theme_bw()+
  theme(axis.text.x = element_text(angle = 75, hjust=1))+
  labs(title = "Rate of Mass Change by Rubber Type", x = "Rubber Type", y = "Mass Lost per Mile (g)")

mass_vs_rubber_box

# Mass loss vs. hardness

mass_vs_hardness <- ggplot(step_calc, aes(x=hardness, y=g_per_milesteps))+
  geom_point()+
  theme_bw()+
  theme(axis.text.x = element_text(angle = 75, hjust=1))+
  labs(title = "Rate of Mass Change by Hardness", x = "Hardness", y = "Mass Lost per Mile (g)")

mass_vs_hardness

# as a boxplot

mass_vs_hardness_box <- ggplot(step_calc, aes(x=hardness, y=g_per_milesteps))+
  geom_boxplot()+
  theme_bw()+
  theme(axis.text.x = element_text(angle = 75, hjust=1))+
  labs(title = "Rate of Mass Change by Hardness", x = "Hardness", y = "Mass Lost per Mile (g)")

mass_vs_hardness_box

# Mass vs geometry

mass_vs_geometry <- ggplot(step_calc, aes(x=geometry, y=g_per_milesteps))+
  geom_point()+
  theme_bw()+
  theme(axis.text.x = element_text(angle = 75, hjust=1))+
  labs(title = "Rate of Mass Change by Geometry", x = "Geometry", y = "Mass Lost per Mile (g)")

mass_vs_geometry

# As a boxplot

mass_vs_geometry_box <- ggplot(step_calc, aes(x=geometry, y=g_per_milesteps))+
  geom_boxplot()+
  theme_bw()+
  theme(axis.text.x = element_text(angle = 75, hjust=1))+
  labs(title = "Rate of Mass Change by Geometry", x = "Geometry", y = "Mass Lost per Mile (g)")

mass_vs_geometry_box

######### Revamping tread change vs model, now normalized by distance #############

## Nothing here has been modified yet ##

tread_joined_model <- tread_joined %>% 
  group_by(model) %>% 
  summarize("avg_depth_change"= mean(final_initial))

tread_model_scatter <- tread_joined_model %>% 
  arrange(avg_depth_change) %>% 
  mutate(model=factor(model, levels=model)) %>% 
  ggplot(., aes(x=model, y=avg_depth_change))+
  geom_point()+
  theme_bw()+
  theme(axis.text.x = element_text(angle = 75, hjust=1))

tread_model_scatter

###### Revamping depth change by sole location graph to be normalized by distance #######

depth_by_location <- full_join(step_calculations, tread_joined, by = "shoe_ID") %>% 
  mutate("treadchange_per_mile" = final_initial/`steps to miles`) %>% 
  filter(treadchange_per_mile > -1 ) %>% 
  filter(treadchange_per_mile < 1)

depth_by_location_plot <- depth_by_location %>% 
  group_by(location) %>% 
  drop_na() %>% 
  summarize("mean_depth_change_per_mile"=mean(treadchange_per_mile)) %>% 
  ungroup() %>% 
  mutate(location=fct_relevel(location, "heel", "outer_arch","ball","tip")) %>% 
  ggplot(.,aes(x=location, y=mean_depth_change_per_mile))+
  geom_point()+
  theme_bw()+
  labs(title = "Mean Depth Change by Sole Location (Normalized by Distance)", y = "Mean Depth Change per Mile (mm)", x = "Sole Location")+
  scale_y_continuous(limits = c(-0.02, 0), expand = c(0,0))

depth_by_location_plot

##### Finding average ###

step_calc2 <- step_calculations %>% 
  filter(g_per_milesteps > -1 | g_per_milesteps < 1)
 

###################### Creating boxplot of change in shoe mass #########################

tread_mass_joined <- full_join(tread_joined_details, step_calculations)

# Verify that I didn't take too many datapoints out

### Normalize tread depth by steps taken ###

tread <- tread_mass_joined %>% 
  mutate("mm_per_mile"= avg_depth_change/miles) %>% 
  mutate("weight_kg"= weight*0.453592) %>% 
  mutate("mm_per_miles_per_kg"=mm_per_mile/weight_kg) 

tread_calculations <- tread %>% ### Remove rows that prevent regression
  filter(steps != 0) 

mass_boxplot <- ggplot(tread_calculations, aes(y=g_per_milesteps))+
  geom_boxplot()+
  theme_bw()+
  scale_y_continuous(limits = c(-0.8, 0.2))+
  labs(title = "Change in Mass (Normalized by Distance)", y = "Change in Mass per Mile (g)")

mass_boxplot

mass_tread_lm <- lm(mm_per_mile ~ g_per_milesteps, data = tread_calculations)
summary(mass_tread_lm)

# Ran a regression between mass lost and tread depth change, P > 0.001, R-squared = 0.1569. 

# mm_per_mile = -0.007202 + 0.048617*g_per_milesteps

tread_mass_lm <- lm(g_per_milesteps ~ mm_per_mile, data = tread_calculations)
summary(tread_mass_lm)

####################### Making scatterplot of tread change vs mileage ##############

mm_steps_lm <- lm(avg_depth_change ~ `steps to miles`, data = tread_calculations)
summary(mm_steps_lm)

# P < .001. Multiple R squared is .195

# Adding trendline to graph #

mm_miles_scatter_lm <- ggplot(tread_calculations, aes(x=`steps to miles`, y=avg_depth_change))+
  geom_point()+
  theme_bw()+
  labs(x = "Distance Travelled (miles)", y = "Average Tread Depth Change (mm)")+
  geom_smooth(method = "lm", color = "indianred1")+ # add se = FALSE to remove error bar
  geom_hline(yintercept=c(0), color="black")+
  geom_hline(yintercept=c(0.1193), color="darkblue")+
  geom_hline(yintercept=c(-0.1193), color="darkblue")+
  theme(axis.text=element_text(size=12),
        axis.title=element_text(size=14))

mm_miles_scatter_lm

########################### Finding average mass lost per mile for entire population ####

mean_massloss_total <- step_calculations %>% 
  filter(g_per_milesteps > -1) %>% 
  filter(g_per_milesteps < 1)
  
mean(mean_massloss_total$g_per_milesteps, na.rm = TRUE)


