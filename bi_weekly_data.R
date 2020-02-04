
########################################################
## Bi-Weekly Wear Test Data Management 
## ... an attempt to import the data and make it into a usable form for eas[ier] analysis
## by Teresa Fukuda
## 
##########################################################


#### full_data_joined dataframe shoes step data, mass change, and shoe details for each participant (most "final" df) ####


# Part I. Load packages and import data

library(tidyverse) # hello tidyverse
library(janitor) # load janitor to clean up dataframe
library(lubridate) # load lubridate to work with dates and times
library(ggthemes) # ggplot themes


# I made this so that you can download the data from the Google Drive as a csv and import directly into here, no edits

biweeklydata <- read_csv("Bi-Weekly Wear Test Form FINAL.csv") # edit file name to match the downloaded file, updated 1/6

shoe_id_table <- read_csv("ShoeID_data_forR - EDITED 1_30.csv") # table from google sheets of all participants and their shoe models and ID, updated 1/6

presurvey_data <- read_csv("Pre Survey Data - Sheet1.csv") #all the presurvey data, updated 12/3

mass_data <- read_csv("Shoe_mass_forR - Sheet1 (1).csv") #this has the shoe weight data for before and after testing in grams

shoe_deets <- read_csv("ShoeID_data_forR - Shoe Model Names HERE.csv") # sheet 2 in the google sheet has the shoe details about rubber, abrasion, etc.

washing_test <- read_csv("WashingTestError - Sheet1.csv") # washing test pre and post measurements

tread_depth_raw_initial <- read_csv("Shoe Depth Measurements - initial_control (1).csv")

tread_depth_raw_final <- read_csv("Shoe Depth Measurements - wear_shoes (2).csv")

# Part II. Clean up the biweekly data, presurvey data and summarize into totals by name of user

clean_biweekly <- biweeklydata  %>% 
  clean_names(.)  %>%
  mutate_if(is.character, str_to_upper) %>% # make all character labels uppercase
  filter(!is.na(how_many_steps_were_taken_in_the_test_shoes_during_the_past_two_weeks)) 

totals_biweekly <- clean_biweekly %>% 
  mutate("name"= `name_first_last`) %>% # This may need to be changed to match whatever is in the actual survey right now (if it was changed since this is confusing)
  mutate("steps"= `how_many_steps_were_taken_in_the_test_shoes_during_the_past_two_weeks`) %>% 
  mutate ("miles"= `how_many_miles`) %>% 
  mutate ("minutes"= `how_many_minutes_have_been_recorded_while_wearing_the_shoes`) %>% 
  select(name, steps, miles, minutes) %>% 
  group_by(name) %>% 
  summarize (steps= sum(steps),
             miles= sum(miles)) %>% #,
             #minutes=sum(minutes)) 
  mutate ("steps to miles" = steps/2000) %>% 
  mutate ("miles to steps" = miles*2000)# assuming 2000 steps on average per mile

clean_pre <- presurvey_data  %>%
  rename("name"= 'X1') %>% 
  clean_names(.) %>% 
  mutate ("gait" = question_11) %>%
  mutate ("exercise"= question_1) %>% 
  select(name,age, weight, gait, exercise) %>% 
  mutate_if(is.character, str_to_upper) # takes name, age, weight, and gait from pre-survey; makes data ALL CAPS

# summarize pre survey data- gives us the demographics of the sample that we chose
pre_summary <- clean_pre %>%
  na.omit() %>% 
  summarize(min_age=min(age),
            max_age=max(age),
            average_age=mean(age),
            sd_age=sd(age),
            min_weight=min(weight),
            max_weight=max(weight),
            average_weight=mean(weight),
            sd_weight=sd(weight))
  

  
# Part III. Clean up shoe ID data; keep participant, shoe, model 

clean_shoe_ID <- shoe_id_table %>% 
  clean_names(.) %>% 
  mutate (name=participant) %>% 
  mutate_if(is.character, str_to_upper) %>% 
  select(shoe_id,model,name) %>% 
  mutate(shoe_ID=gsub("-","",.$shoe_id)) %>%
  mutate(shoe_ID=gsub(" ","",.$shoe_ID)) %>%
  select (-c('shoe_id'))





# Part IV. Merge shoe ID data with each users reported miles/steps/minutes

wear_data_joined <- full_join(totals_biweekly,clean_shoe_ID)

pre_data_joined <- full_join(wear_data_joined,clean_pre)
#joins the step data, shoe ID, and pre survey data




# Part V. Clean up mass data and join with shoe traits (rubber type, abrasion rating, etc.)

# clean up shoe details (rubber type, hardness, abrasion, geometry)

clean_shoedeets <- shoe_deets %>% 
  select(Model, hardness, abrasion, rubber_type,geometry) %>% 
  clean_names(.) %>% 
  mutate_if(is.character, str_to_upper)

# make mass data tidy, then find average of pre and post mass, then find difference

clean_mass <- mass_data %>% 
  gather ("trial","mass",3:12) %>%
  mutate("prepost"= case_when(trial=="pre1"|trial=="pre2"|trial=="pre3"|trial=="pre4"|trial=="pre5" ~ "pre", TRUE~"post")) %>% 
  group_by(`Shoe ID`,`prepost`) %>% 
  mutate(mass= as.double(mass))  %>% 
  filter(mass!='#DIV/0!') %>% 
  summarize("average"=mean(mass)) %>%  # averages of pre and post data for each shoe ID
  spread(.,prepost, average) %>% # separate pre and post columns
  mutate("mass_change"= post-pre) %>% 
  mutate("shoe_ID"= `Shoe ID`) %>% 
  select (-c('Shoe ID'))





# Part VI. Add the post-wear measurement data and calculate the loss per mile, loss per step, normalize by body weight??

# join the wear data and the pre and post mass data

mass_data_joined <- full_join(pre_data_joined,clean_mass)

full_data_joined <- full_join(mass_data_joined, clean_shoedeets) %>%  #join all pre and post mass data, participant age/weight/name, shoe model/rubber/abrasion
  filter(!is.na(name)) %>% 
  filter(name!="0") %>% 
  select(-c('Shoe ID'))


step_calculations <- full_data_joined %>% 
  mutate("milesteps"= steps/2000) %>% 
  mutate("g_per_milesteps"= mass_change/milesteps) %>% 
  mutate("weight_kg"= weight*0.453592) %>% 
  mutate("g_per_milesteps_per_kg"=g_per_milesteps/weight_kg) %>% 
  mutate("g_per_step"=mass_change/steps) %>% 
  mutate("g_per_10kstep"=g_per_step*10000)

#compare steps to miles and choose "best"?





# Part VII. Visualize data loss per style, loss per rubber type, overall loss per mile, loss per mile per pound of force?
#age_vis <- ggplot(clean_pre, aes(x=age)) + 
#geom_histogram()

# steps per person - shows steps per person 
steps_per_person <- full_data_joined %>% 
  group_by(name) %>% 
  summarize("steps"=mean(steps)) %>% 
  arrange(steps) %>% 
  mutate(name=factor(name, levels=name)) %>% 
  ggplot(., aes(x=name, y=steps)) +
  geom_point()+
  theme_bw()+
  theme(axis.text.x = element_text(angle = 90, hjust = 1))+
  geom_hline(yintercept=c(250000), color="blue")



steps_per_person

steps_per_person_hist <- ggplot (full_data_joined, aes(x=steps)) +
  geom_histogram()+
  theme_bw()
  
steps_per_person_hist

### grams loss per shoe for each style###

#histogram of grams lost for all shoes - using total grams lost, not normalized by steps; fill= variables
grams_per_shoe_hist <- ggplot(step_calculations, aes(x=mass_change))+
  geom_histogram()+
  scale_x_continuous()+
  scale_y_continuous()+
  geom_vline(xintercept=c(0), color="blue")+
  theme_bw()

grams_per_shoe_hist


# #histogram of grams/km broken up by style-- not particularly useful because of so few data points per style
# per_shoe_style <- step_calculations %>% 
#   group_by(model) %>% 
#   ggplot(., aes(x=g_per_km))+
#   geom_histogram()+
#   facet_wrap(~model)
# per_shoe_style

#### grams loss per shoe for each rubber type across styles ###



### overall loss per km ####
#histogram of grams/km for all shoes
grams_per_shoe_mile <- ggplot(step_calculations, aes(x=g_per_milesteps))+
  geom_histogram()+
  theme_bw()


grams_per_shoe_mile

grams_per_shoe_mile_point <- ggplot(step_calculations, aes(x=rubber_type, y=g_per_milesteps))+
  geom_point()+
  theme_bw()+
  theme(axis.text.x = element_text(angle = 75, hjust=1))


grams_per_shoe_mile_point

### loss per km per kg body weight ####

#histogram of grams/km/kg weight for all shoes
grams_per_bodyweight <- ggplot(step_calculations, aes(x=g_per_milesteps_per_kg))+
  geom_histogram()+
  theme_bw()
grams_per_bodyweight

### loss per km per kg body weight by abrasion rating ###
grams_per_bodyweight_abrasion <- ggplot(step_calculations, aes(x=g_per_milesteps_per_kg))+
  geom_histogram()+
  facet_wrap(~abrasion)
grams_per_bodyweight_abrasion

### loss per km per kg body weight by hardness rating ###
grams_per_bodyweight_hardness <- ggplot(step_calculations, aes(x=g_per_milesteps_per_kg))+
  geom_histogram()+
  facet_wrap(~hardness)
grams_per_bodyweight_hardness


# grams versus steps

grams_steps_scatter <- ggplot(full_data_joined, aes(x=steps, y=mass_change))+
                                geom_point()+
                                theme_bw()

grams_steps_scatter

##### mass error ######
# gives change in measurements for the control shoes-- min value is the largest loss, max value is the largest gain, all taken per shoes model

mass_error<- full_data_joined %>% 
  filter(name=="CONTROL") %>% 
  group_by(model) %>% 
  summarize(min_value=min(mass_change),
            max_value=max(mass_change),
            avg=mean(mass_change))


#Part VIII. Statistical testing ?

# Hardness, abrasion, rubber type, geometry
# Rate of shoe abrasion? g/km/kg/time across all shoes to see if it's linear relationship (time v. abrasion rate)

# some summary information

summary_model <- full_data_joined %>%
  group_by(model) %>%
  summarize("count"= n())

# tests among each parameter? (within geometry, abrasion, hardness, rubber type)
# some sort of test across all parameters?
# comparing mass loss and change in tread depth




# Part IX. Finding Error in our measurements

# Using the washing test data from the 6 sample shoes to find the error in the calculations-- how much of the change of mass might be due to washing?

#creates a df with the averages of pre and post washing weights, and the difference (post-pre)
# washing_error <- washing_test %>% 
#   gather ("trial","mass",2:11) %>% 
#   mutate("prepost"= case_when(trial=="initial1"|trial=="initial2"|trial=="initial3"|trial=="initial4"|trial=="initial5" ~ "pre", TRUE~"post")) %>% 
#   group_by(shoe_ID,prepost) %>% 
#   summarize(average=mean(mass)) %>% 
#   spread(.,prepost, average) %>% # separate pre and post columns
#   mutate("change_post_to_pre"= post-pre)



# Using the preliminary mass data to find the expected range of manufacturer variation in mass (not sure if this is relevant at all)

mass_model_join <- clean_mass %>% 
  full_join(.,clean_shoe_ID) %>%
  mutate("control"=case_when(name=="CONTROL"~"control", TRUE~"worn")) %>% 
  filter(!is.na(pre)) %>% 
  group_by(model,control) %>% 
  summarize("change"=mean(post-pre))

endless_run_premass <- clean_mass %>% # trying to see what is going on with endless run mass measurements
  full_join(.,clean_shoe_ID) %>% 
  filter(!is.na(pre)) %>% 
  filter(model=="REEBOK ENDLESS RUN")

endless_run_premass_plot <- ggplot(endless_run_premass, aes(x=pre)) +
  geom_histogram()
endless_run_premass_plot #spread of mass measurements from 240 to 280 grams... yikes. 

#this gives us the measurement error from the balance/us
# spread of our pre mass measurements, can add variation in post measurements when/if we get those too
# premass_error gives a single value in grams, the average of the standard deviations of our mass measurements... not sure if this is valid
premass_error<- mass_data %>% 
  gather ("trial","mass",3:12) %>%
  mutate("prepost"= case_when(trial=="pre1"|trial=="pre2"|trial=="pre3"|trial=="pre4"|trial=="pre5" ~ "pre", TRUE~"post")) %>% 
  group_by(`Shoe ID`,`prepost`) %>% 
  mutate(mass= as.double(mass)) %>% 
  filter(prepost=="pre") %>% 
  summarize("average"=mean(mass),
            "sd"=sd(mass)) %>% 
  ungroup() %>% 
  summarize("avgsd"=mean(sd))
# don't think this is incredibly valuable, just interesting


# Part X. Tread Depth

# clean up tread depth measurements: find average of the three measurements, summarize by model and location

tread_initial <- tread_depth_raw_initial %>% 
  group_by(model, location) %>% 
  summarize("initial_depth"=mean(initial_mm
                             ))


tread_final <- tread_depth_raw_final %>% 
  select(shoe_ID, side, model, location, final_mm) %>% 
  mutate(final_mm_num= as.numeric(.$final_mm)) %>% 
  group_by(shoe_ID, side, location, model) %>% 
  summarize("final_depth"=mean(final_mm_num
  ))
  
# join pre and post tread data
tread_joined <- full_join(tread_initial, tread_final) %>% 
  mutate(final_initial= final_depth-initial_depth)

# group by shoe_ID, considering each ind shoe
tread_joined_shoeID <- tread_joined %>% 
  drop_na() %>% 
  group_by(shoe_ID, model) %>% 
  summarize("avg_depth_change"= mean(final_initial)) 

tread_joined_details<- tread_joined_shoeID %>% 
  clean_names(.) %>% 
  ungroup() %>% 
  mutate("shoe_ID"=shoe_id) %>% 
  mutate_if(is.character, str_to_upper) %>% 
  full_join(.,clean_shoedeets) %>% 
  drop_na()

# histogram of avg depth change for each shoe
tread_change_hist <- ggplot(tread_joined_details, aes(x=avg_depth_change))+
  geom_histogram()+
  theme_bw()+
  geom_vline(xintercept=c(0), color="blue")+
  theme_economist()+scale_colour_economist()

tread_change_hist

# scatter of avg depth change for each shoe, colored by shoe model
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

# histogram of avg depth change for each shoe model
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

#hist of avg depth change for each shoe model
tread_model_hist <- ggplot(tread_joined_shoeID, aes(x=avg_depth_change))+
  geom_histogram(aes(fill=model))

tread_model_hist

tread_error <- tread_joined_details %>%
  full_join(.,clean_shoe_ID) %>% 
  filter(name=="CONTROL") %>% 
  drop_na()
  
# tread_hist_final <- ggplot()

tread_by_location <- tread_joined %>% 
  group_by(location) %>% 
  drop_na() %>% 
  summarize("avg_depth_change"=mean(final_initial)) %>% 
  ungroup() %>% 
  mutate(location=fct_relevel(location, "heel", "outer_arch","ball","tip")) %>% 
  ggplot(.,aes(x=location, y=avg_depth_change))+
  geom_point()+
  theme_bw()

tread_by_location


########################################################################################

### Looking at if the rate of shoe mass loss changes as someone wears the shoes more ###

# Finding mean and median of number of steps to divide shoes into two groups to compare

median(full_data_joined$steps, na.rm = TRUE)

#gives 114,800 as median steps

mean(full_data_joined$steps, na.rm = TRUE)

#gives 232706.7 as mean steps

### Dividing the data into two sections: runners with steps greater than the mean, and runners with steps below the mean; doing the same by median

# By median

steps_lowerhalf_bymedian <- full_data_joined %>% 
  filter(steps <= 114800) %>% 
  filter(miles != 0)

steps_upperhalf_bymedian <- full_data_joined %>% 
  filter(steps > 114800) %>% 
  filter(miles != 0)

lowerhalf_mass_bymedian <- steps_lowerhalf_bymedian %>% 
  mutate(grams_per_mile=mass_change/miles)

upperhalf_mass_bymedian <- steps_upperhalf_bymedian %>% 
  mutate(grams_per_mile=mass_change/miles)

mean(lowerhalf_mass_bymedian$grams_per_mile, na.rm = TRUE)

### Gives -0.1057689 grams per mile as the mean for runners below the median number of steps taken

mean(upperhalf_mass_bymedian$grams_per_mile, na.rm = TRUE)

### Gives -0.01384143 grams per mile as the mean for runners above the median number of steps taken

#### By mean number of steps ####

steps_lowerhalf_bymean <- full_data_joined %>% 
  filter(steps <= 232706.7) %>% 
  filter(miles != 0)

steps_upperhalf_bymean <- full_data_joined %>% 
  filter(steps > 232706.7) %>% 
  filter(miles != 0)

lowerhalf_mass_bymean <- steps_lowerhalf_bymean %>% 
  mutate(grams_per_mile=mass_change/miles)

upperhalf_mass_bymean <- steps_upperhalf_bymean %>% 
  mutate(grams_per_mile=mass_change/miles)

mean(lowerhalf_mass_bymean$grams_per_mile, na.rm = TRUE)

# Gives -0.08387811 as grams lost per mile for runners below mean number of steps

mean(upperhalf_mass_bymean$grams_per_mile, na.rm = TRUE)

# Gives -0.0130713 as grams lost per mile for runners above mean number of steps

##### It looks as if the number of grams lost per mile is noticeably lower after the first half of wear in our test, whether you calculate it by mean or median! #####

