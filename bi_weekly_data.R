
########################################################
## Bi-Weekly Wear Test Data Management 
## ... an attempt to import the data and make it into a usable form for eas[ier] analysis
## by Teresa
## because I'm bored
##########################################################

# Part I. Load packages and import data

library(tidyverse) # hello tidyverse
library(janitor) # load janitor to clean up dataframe
library(lubridate) # load lubridate to work with dates and times

# I made this so that you can download the data from the Google Drive as a csv and import directly into here, no edits

biweeklydata <- read_csv("Bi-Weekly Wear Test Form Oct.csv") # edit file name to match the downloaded file (the one I used to test this was edited with a bunch of fake data)

shoe_id_table <- read_csv("Shoe ID table - will be revised - Sheet1.csv")

presurvey_data <- read_csv("Pre Survey Data - Sheet1.csv")

# Part II. Clean up the biweekly data, presurvey data and summarize into totals by name of user

clean_biweekly <- biweeklydata  %>% 
  clean_names(.)  %>%
  mutate_if(is.character, str_to_upper)

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
  mutate ("steps to miles" = steps/2250) %>% 
  mutate ("miles to steps" = miles*2250)#assuming 2250 steps on average per mile

clean_pre <- presurvey_data  %>%
  rename("name"= 'X1') %>% 
  clean_names(.) %>% 
  select(name,age, weight) %>% 
  mutate_if(is.character, str_to_upper)

# Part III. Clean up shoe ID data; keep participant, shoe, model 
# Would probably be useful here to also include more info about each shoe model, such as type of rubber, type of shoe (lifestyle, running, hybrid), etc.

clean_shoe_ID <- shoe_id_table %>% 
  clean_names(.) %>% 
  mutate (name=participant) %>% 
  select(shoe_id_left,shoe_id_right,model,name)

# Part IV. Merge shoe ID data with each users reported miles/steps/minutes

# right now the names columns are not quite aligned, for example JAMES is Fuller, James in one and James in the other

# wear_data_joined <- full_join(clean_biweekly,clean_shoe_ID)

pre_data_joined <- full_join(totals_biweekly,clean_pre) # this dataframe is a gross mess but it's a start!
