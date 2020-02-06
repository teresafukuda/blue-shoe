
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

tread_calculations <- tread_mass_joined %>% 
  mutate("mm_per_mile"= avg_depth_change/miles) %>% 
  mutate("weight_kg"= weight*0.453592) %>% 
  mutate("mm_per_miles_per_kg"=mm_per_mile/weight_kg)

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