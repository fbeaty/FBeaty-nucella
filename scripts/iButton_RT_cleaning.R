#Going to try resolving the ibutton scripts source population by source population, starting with Kwakshua
#Essentially, I think that some of the iButton dates are off, which skews the esimated values and tides

#First load in the csvs and do preliminary cleaning----
DF_K1 <- data.frame()	
files <- list.files(path="data/iButton_RT/Cleaned/Combined/Kwak_1", pattern="*.csv", full.names=TRUE, recursive=FALSE)	

for (i in 1:length(files)) {	
  Kwak_1<- read.csv(as.character(files[i]),header = TRUE, sep = ",", stringsAsFactors = FALSE, skip = 0)	
  Kwak_1$Date <- as.Date(paste(Kwak_1$Date.Time), formate = "%Y-%m-%d")
  Kwak_1$button <- i
  Kwak_1 <- data.frame(Kwak_1)	
  DF_K1 <- rbind(Kwak_1,DF_K1) # add it to your list	
}	

#Now add a line that indicates it's Kwakshua
DF_K1<- DF_K1 %>% 
  mutate(SP = "Kwakshua") %>% 
  setNames(., c("DT" , "Time" , "Value" , "Temp" , "Date" , "Button", "SP"))

#Pruth dataset looks fine, no discrepancies across ibuttons - woot!
#Heron dataset looks fine, no discrepancies across ibuttons - woot!
#Cedar also looks good! Excellent

#delete any days when the ibuttons were recording in the lab. For Kwak remove first 6 rows, 06-04, 06-05, 
#Separate the date into date and time, add tidal height column
Kwak <- DF_K1 %>% 
  filter(Date != "2019-03-20") %>% 
  filter(Date != "2019-03-21") %>% 
  filter(Date != "2019-06-04") %>% 
  filter(Date != "2019-06-05") %>% 
  mutate(Time_2 = parse_time(Time, "%I:%M:%S %p")) %>% 
  mutate(Time = format(Time_2, format = "%H:%M:%S")) %>% 
  select(Date, Time, SP, Temp, Button) %>% 
  mutate(TideHeight = 1.30) %>% 
  mutate(date.time = ymd_hms(paste(Date, Time))) %>% 
  select(Date, Time, date.time, SP, Temp, TideHeight, Button)

#Now import and attach the tides data----
cal_tides <- read_csv("data/iButtons/08863_PruthBay_20190301.csv") %>% 
  mutate(Obs_date = dmy_hms(paste(Date, Time))) %>% 
  select(Obs_date, slev_m)

nan_tides <- read_csv("data/iButtons/07480_BoatHarbour_20190301.csv") %>% 
  mutate(Obs_date = dmy_hms(paste(Date, Time))) %>% 
  select(Obs_date, slev_m)

#Combine the datasets by the time column but the all_df reports time at different minute increments than the 
#exported csv, which reports on the 00 min. So, I rounded the minutes to the hour by replacing them with '00'
kwak_corr <- Kwak %>% 
  mutate(time_corr = format(date.time, format = "%H"),
         time_corr = paste0(time_corr, ":00:00"),
         time_corr = format(time_corr, format = "%H:%M:%S"),
         Obs_date = as.POSIXct(paste(Date, time_corr)),
         ID = row_number()) %>% 
  select(SP, Temp, TideHeight, Date, Time, date.time, Obs_date, Button, ID)

#The following Buttons are off by the following amounts:
#Button 3 and 7 are 11 hours ahead --> have to subtract 11 hours from the Time and Date columns
hrs <- hours(11)

kwak_corr <- kwak_corr %>% 
  mutate(Obs_date = if_else(Button == 7, Obs_date - hrs, 
                            if_else(Button == 3, Obs_date - hrs, Obs_date)),
         Date = if_else(Button == 7, as.Date(ymd_hms(Obs_date)), 
                        if_else(Button == 3, as.Date(ymd_hms(Obs_date)), Date))) %>% 
  filter(Button != 6 & Button != 2)

ggplot(cedar_corr, aes(Obs_date, Temp, fill = Button)) + 
  geom_point (aes(colour = Button)) +
  labs(x = "Date", y = "temperature (°C)") +
  ylim(5, 30) +
  theme_cowplot(16) + theme(legend.position = c(0.1, 0.75))

#Test whether the ibutton times line up with the other tides----
#Trial with a week in May when there were large tides (May 15-22)
may_select_k <- kwak_corr %>% 
  filter(Date > "2019-05-17" & Date < "2019-05-20") %>% 
  filter(Button > 0 & Button < 16)

ggplot(may_select_k, aes(Obs_date, Temp, fill = Button)) + 
  geom_point (aes(colour = Button)) +
  labs(x = "Date", y = "temperature (°C)") +
  ylim(5, 30) +
  theme_cowplot(16) + theme(legend.position = c(0.1, 0.75))

may_select_k_outlier <-  kwak_corr %>% 
  filter(Date > "2019-05-17" & Date < "2019-05-20") %>% 
  filter(Button == 3)

may_select_k_outlier <-  kwak_corr %>% 
  filter(Date > "2019-05-17" & Date < "2019-05-20") %>% 
  filter(Button == 9)

ggplot(may_select_k_outlier, aes(Obs_date, Temp, fill = Button)) + 
  geom_point (aes(colour = Button)) +
  labs(x = "Date", y = "temperature (°C)") +
  ylim(8, 30) +
  theme_cowplot(16) + theme(legend.position = c(0.1, 0.75))

#Outlier buttons: 7, 6, 3, 2
#7 seems like it's measuring low tide 11 hours later than the other ones
#6 is consistently measuring temperatures that are 3 degrees higher and different tidal cycles --> remove
#3 is similar to 7, measuring low tide x hours later than the other ones
#2 is similar to 6 --> remove

#9 low tide = 8:00 on 5-18
#9 low tide = 9:00 on 5-19 (there's also a high measurement at 8:00 on 5-19)
#7 low tide = 19:00 on 5-18
#7 low tide = 19:00 on 5-19
#6 low tide = 19:00 on 5-18
#6 low tide = 20:00 on 5-19 (also one at 19:00) so again, 11 hours difference

#SO: if we were to correct the Button #7, we'd need to set its Obs-date back by 11 hours
