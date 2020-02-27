####################################################################
#
# Attempt at a sensitivity analysis for weights and for tread derived mass
#
# by Teresa Fukuda
# for the Future Footwear Project
#
#########################################################################

random_vector <- runif(549)

new_tread_mass <- tread_joined %>% 
  select(model, shoe_ID,tread_mass) %>% 
  ungroup() %>% 
  mutate("random"=random_vector) %>% 
  full_join(.,tread_error)
