######################################################################################
# Round 2 of Making Graphs for our Final Report #
# Andrew, Started 3/4/20 #
#####################################################################################

### Packages ###

library(tidyverse) # hello tidyverse
library(janitor) # load janitor to clean up dataframe
library(lubridate) # load lubridate to work with dates and times
library(ggthemes) # ggplot themes
library(plm)# regression analysis package

################

### Grams lost versus steps taken, forcing regression to 0 ###

# Running a regression #

grams_steps_lm0 <- lm(mass_change ~ 0 + steps, data = full_data_joined)
summary(grams_steps_lm0)

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