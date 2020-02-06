
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

grams_steps_scatter_lm <- ggplot(full_data_joined, aes(x=steps, y=mass_change))+
  geom_point()+
  theme_bw()+
  labs(x = "Steps Taken", y = "Mass Change (g)")+
  geom_smooth(method = "lm", color = "indianred1") # add se = FALSE to remove error bar

grams_steps_scatter_lm

#### Questions for group ####  
# should we add R squared and slope to the graph?  
# Should we remove error bar?
# Should we remove high outliers?


################## Graph of mass change vs. tread depth lost #################

tread_mass_joined <- full_join(tread_joined_details, full_data_joined) %>% 
  drop_na()

mass_vs_tread <- ggplot(tread_mass_joined, aes(x=avg_depth_change, y=mass_change))+
  geom_point()+
  theme_bw()

mass_vs_tread

# Ran a regression between mass lost and tread depth change, P value is absurdly high (.971). 

################### Graphs of tread depth change vs. different variables ################

### Normalize tread depth by steps taken ###

tread <- tread_mass_joined %>% 
  mutate("mm_per_mile"= avg_depth_change/miles) %>% 
  mutate("weight_kg"= weight*0.453592) %>% 
  mutate("mm_per_miles_per_kg"=mm_per_mile/weight_kg) 

tread_calculations <- tread %>% ### Remove rows that prevent regression
  filter(steps != 0) 

# Tread depth vs rubber type

tread_change_vs_rubber <- ggplot(tread_calculations, aes(x=rubber_type, y=mm_per_mile))+
  geom_point()+
  theme_bw()+
  theme(axis.text.x = element_text(angle = 75, hjust=1))

tread_change_vs_rubber

# Tread depth vs hardness

tread_change_vs_hardness <- ggplot(tread_calculations, aes(x=hardness, y=mm_per_mile))+
  geom_point()+
  theme_bw()+
  theme(axis.text.x = element_text(angle = 75, hjust=1))

tread_change_vs_hardness

# Tread depth vs geometry

tread_change_vs_geometry <- ggplot(tread_calculations, aes(x=geometry, y=mm_per_mile))+
  geom_point()+
  theme_bw()+
  theme(axis.text.x = element_text(angle = 75, hjust=1))

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

# Only significant difference is between running and lifestyle. 

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