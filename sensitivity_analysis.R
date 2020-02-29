####################################################################
#
# Attempt at a sensitivity analysis for weights and for tread derived mass
#
# by Teresa Fukuda
# for the Future Footwear Project
#
#########################################################################

# vector of 549 randomly generated numbers

random_vector <- runif(549)


# this creates a new df with the errors randomly added or subtracted from the original tread_mass values
new_tread_mass <- tread_joined %>% 
  select(model, location, shoe_ID,tread_mass) %>% 
  ungroup() %>% 
  mutate("random"=random_vector) %>% 
  full_join(.,tread_error) %>% 
  drop_na() %>% 
  mutate("new_tread"= case_when(random >= 0.5~ tread_mass+avg_error,
                                TRUE ~ tread_mass - avg_error))

# find average tread_change per model based on the new values

summary_tread_error <- new_tread_mass %>% 
  group_by(model, location) %>% 
  summarize("avg_depth_change"=mean(new_tread))
         

# thinking through a loop now...

sensitivity_simluation_output <- matrix(0, nrow=36, ncol=500)

# for (i in (1:500)) {
#    sensitivity_simluation_output[[i]]<- summary_tread_error$avg_depth_change 
# }
