zero = 0

mass_joined_weight_fixed_nopos2 <- full_data_joined %>% #we need to use this dataframe because the tread data frame has values removed due to no Surface Area Data
  filter(mass_change < zero) 

mass_multi_lm2_new <- lm(formula = mass_change ~ 0 + weight + `steps to miles`, data = mass_joined_weight_fixed_nopos2) #linear model with weight as a contributing factor

summary(mass_multi_lm2_new)

# p-value << 0.001. R-squared now .5369

lmeq2_new = function(x){coef(mass_multi_lm2)[2]*x+coef(mass_multi_lm2_new)[1]} 

mass_multi_lm2_plot_new <- ggplot(mass_joined_weight_fixed_nopos2,aes(x=`steps to miles`,y=mass_change,color=weight))+
  geom_point() +
  stat_function(fun=lmeq2,geom="line")+
  geom_hline(yintercept=c(0), color="dark blue")+
  theme_bw() +
  scale_y_continuous(limits = c(-7, 2)) + 
  scale_x_continuous(limits = c(0, 600)) +
  labs(title = "Forced to Zero, NO positive Values", y = "Mass Change (g)", x = "Distance Travelled (Miles)")

mass_multi_lm2_plot2_new

########################################################################################

### Outliers using Cook's D ###

# Question for the team: Should this be based on rate of mass loss instead of total mass lost?

#####Find the outliers using Cook's D!#####
#graphs outliers but I can't figure out which observation is which
cooksdplot2 <- ols_plot_cooksd_chart(mass_multi_lm2_new) #nopos

#find outlier values, this just prints the outliers from a boxplot
average_mass_outliers2 <- boxplot(mass_joined_weight_fixed_nopos2$measuremass_lost_per_mile)
outvals2 = boxplot(mass_joined_weight_fixed_nopos2$measuremass_lost_per_mile)$out
outvals2 # the outliers in nopos are -0.1838642 -0.1874984 -0.7719500 -0.7860500 -0.2286885 -0.4416800 -0.1462051 -0.1722194 -0.1694932

### Make data frame with all Cook's D outliers removed

mass_joined_fixed_noout2 <- mass_joined_weight_fixed_nopos2 %>%
  filter(name != "CONTROL") %>% 
  filter(shoe_ID != "R18") %>% 
  filter(shoe_ID != "L18") %>% 
  filter(shoe_ID != "R24") %>% 
  filter(shoe_ID != "L24") %>% 
  filter(shoe_ID != "R31") %>% 
  filter(shoe_ID != "R25") %>% 
  filter(shoe_ID != "L70") %>% 
  filter(shoe_ID != "R70") 



### Fitting a model with outliers removed

mass_multi_lm3 <- lm(formula = mass_change ~ 0 + weight + `steps to miles`, data = mass_joined_fixed_noout2) #linear model with weight as a contributing factor

summary(mass_multi_lm3_new)
# p << .001, R-squared = 0.5988

lmeq3_new = function(x){coef(mass_multi_lm3)[2]*x+coef(mass_multi_lm3_new)[1]}

mass_multi_lm3_plot_new <- ggplot(mass_joined_fixed_noout2,aes(x=`steps to miles`,y=mass_change,color=weight))+
  geom_point() +
  stat_function(fun=lmeq3,geom="line")+
  geom_hline(yintercept=c(0), color="dark blue")+
  theme_bw() +
  theme(axis.text.x = element_text(angle = 75, hjust=1), 
        axis.text=element_text(size=12),
        axis.title=element_text(size=14,face="bold"),
        legend.text = element_text(size = 15),
        legend.title = element_text(size = 20))+
  scale_y_continuous(limits = c(-5, 2)) + 
  scale_x_continuous(limits = c(0, 600)) +
  labs(y = "Mass Change (g)", x = "Distance Travelled (Miles)", color='Weight (lbs)')

mass_multi_lm3_plot_new



####Averages####Mean Mass Loss Rate#######

mean(tread_joined_fixed_noout$tread_mass_lost_per_mile)
#-0.05547505 grams/mile

mean(mass_joined_fixed_noout2$measuremass_lost_per_mile)
#-0.03245783