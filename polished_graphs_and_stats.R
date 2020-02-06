
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

# Adding trendline to graph #

grams_steps_scatter_lm <- ggplot(full_data_joined, aes(x=steps, y=mass_change))+
  geom_point()+
  theme_bw()+
  geom_smooth(method = "lm", color = "indianred1") # add se = FALSE to remove error bar

grams_steps_scatter_lm

#### Questions for group ####  
# should we add R squared and slope to the graph?  
# Should we remove error bar?


