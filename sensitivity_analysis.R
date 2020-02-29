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

############## TREAD MASS ###############################################

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
  group_by(model) %>% 
  summarize("avg_depth_change"=mean(new_tread))
         

# thinking through a loop now...

# create an empty df of zeros with 9 rows and 500 columns
sensitivity_simluation_output <- as.data.frame(matrix(0,nrow=9,ncol=500))

# for (i in (1:500)) {
#    sensitivity_simluation_output[[i]]<- summary_tread_error$avg_depth_change 
# }

# add the simulation into this data frame
sensitivity_simluation_output[[1]]<- as.data.frame(summary_tread_error$avg_depth_change)

########## HERE BEGINS THE MONTE CARLO SIMULATION FOR REAL ###################
# run monte carlo simluation 500 times for randomly + or - the error per model 
 for (i in (1:500)) {
   random_vector <- runif(549)
   
   new_tread_mass <- tread_joined %>% 
     select(model, location, shoe_ID,tread_mass) %>% 
     ungroup() %>% 
     mutate("random"=random_vector) %>% 
     full_join(.,tread_error) %>% 
     drop_na() %>% 
     mutate("new_tread"= case_when(random >= 0.5~ tread_mass+avg_error,
                                   TRUE ~ tread_mass - avg_error))
   
   summary_tread_error <- new_tread_mass %>% 
     group_by(model) %>% 
     summarize("avg_depth_change"=mean(new_tread))
   
   sensitivity_simluation_output[[i]]<- summary_tread_error$avg_depth_change 
 }

#output gives 500 columns, each is a simulation with each row being a different model, consistent with the model names in summary_tread_error

# create a df with the min, max, and mean values for each model tread_mass change based on the values calculated from the control shoes as our error (randomly + or -)
sensitivity_simluation_output_clean <- sensitivity_simluation_output %>%
  mutate ("model"=summary_tread_error$model) %>% 
  select(model,everything()) %>% 
  gather(simulation, tread_mass, V1:V500) %>% 
  group_by(model) %>% 
  summarize("min"=min(tread_mass),
            "max"=max(tread_mass),
            "mean"=mean(tread_mass))


########## SHOE MEASURED MASS #################################################

# first, need to create a DF with the control mass changes
# input full_data_joined, choose control shoes and average within each pair

control_mass <- full_data_joined %>% 
  filter(name=="CONTROL") %>% 
  select(name,model,mass_change) %>% 
  group_by(model) %>% 
  summarize("avg_error"= mean(mass_change))

# now do the same thing but with the mass data
sensitivity_mass_simluation_output <- as.data.frame(matrix(0,nrow=13,ncol=500))


for (i in (1:500)) {
  random_vector <- runif(133)
  
  new_shoe_mass <- full_data_joined %>% 
    select(model, shoe_ID, mass_change) %>% 
    mutate("random"=random_vector) %>% 
    full_join(.,control_mass) %>% 
    drop_na() %>% 
    mutate("new_mass"= case_when(random >= 0.5~ mass_change+avg_error,
                                  TRUE ~ mass_change - avg_error))
  
  summary_mass_error <- new_shoe_mass %>% 
    group_by(model) %>% 
    summarize("avg_mass_change"=mean(new_mass))
  
  sensitivity_mass_simluation_output[[i]]<- summary_mass_error$avg_mass_change 
}

#output gives 500 columns, each is a simulation with each row being a different model, consistent with the model names in summary_tread_error

# create a df with the min, max, and mean values for each model tread_mass change based on the values calculated from the control shoes as our error (randomly + or -)
sensitivity_mass_simluation_output_clean <- sensitivity_mass_simluation_output %>%
  mutate ("model"=summary_mass_error$model) %>% 
  select(model,everything()) %>% 
  gather(simulation, tread_mass, V1:V500) %>% 
  group_by(model) %>% 
  summarize("min"=min(tread_mass),
            "max"=max(tread_mass),
            "mean"=mean(tread_mass))

