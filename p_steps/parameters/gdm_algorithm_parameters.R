#Inclusion criteria for GDM algorithm

gestational_age_gdm<-140 #minimum length of pregnancies to be considered in the gdm algorithm

min_start_date_gdm<-"20050101"
max_end_date_gdm<-"20190301"
lookback_period_pregnancy<-3*365.25 #3 months of lookback before start of pregnancy(will be retrieved from the observation periods table)

max_follow_up_preg<-42*7 #42 weeks of follow up from start date of pregnancy(difference between start date of pregnancy and op_end_date from OBS PER table)
follow_up_after_delivery<-7 #op end date should be after or equal to delivery date + 7 days
min_age_preg_gdm<-15 #age at start pregnancy date
max_age_preg_gdm<-49 #age at start pregnancy date

first_trimester<-97 #from start date of pregnancy up to 97 days
second_trimester_min<-98 #starts at 98 day from LMP
second_trimester_max<-195 #ends at 195 days from LMP
third_trimester<-196 #starts at 196 day from LMP
