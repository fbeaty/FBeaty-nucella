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
  #filter(Date != "2019-03-20") %>% 
  #filter(Date != "2019-03-21") %>% 
  #filter(Date != "2019-06-04") %>% 
  filter(Date != "2019-06-05") %>% 
  mutate(Time_2 = parse_time(Time, "%I:%M:%S %p")) %>% 
  mutate(Time = format(Time_2, format = "%H:%M:%S")) %>% 
  select(Date, Time, SP, Temp, Button) %>% 
  mutate(TideHeight = 1.20) %>% 
  mutate(date.time = ymd_hms(paste(Date, Time))) %>% 
  select(Date, Time, date.time, SP, Temp, TideHeight, Button)

#Combine the datasets by the time column but the all_df reports time at different minute increments than the 
#exported csv, which reports on the 00 min. So, I rounded the minutes to the hour by replacing them with '00'
kwak_corr <- Kwak %>% 
  mutate(time_corr = format(date.time, format = "%H"),
         time_corr = paste0(time_corr, ":00:00"),
         time_corr = format(time_corr, format = "%H:%M:%S"),
         Obs_date = as.POSIXct(paste(Date, time_corr)),
         ID = row_number()) %>% 
  select(SP, Temp, TideHeight, Date, Time, date.time, Obs_date, Button, ID)

#Adjust the iButtons with incorrect dates/hours----
#The following Buttons are off by the following amounts:
#Buttons 3 and 7 are 11 hours ahead --> have to subtract 11 hours from the Time and Date columns
#Buttons 2 and 6 were measuring in the second half but dates were in the first half so I added 77 days
#I adjusted the hours for 2, 6, 10, and 13 so that their high tides align
hrs <- hours(11)
hrs_2_6 <- hours(10)
hrs_13 <- hours(1)
days_2_6 <- days(77)

kwak_corr_1 <- kwak_corr %>% 
  mutate(Obs_date = if_else(Button == 7, Obs_date - hrs, 
                            if_else(Button == 3, Obs_date - hrs, Obs_date)),
         Date = if_else(Button == 7, as.Date(ymd_hms(Obs_date)), 
                        if_else(Button == 3, as.Date(ymd_hms(Obs_date)), Date))) 

#Now adjust the days for 2 and 6
kwak_corr_2 <- kwak_corr_1 %>% 
  mutate(Obs_date = if_else(Button == 2, Obs_date + days_2_6, 
                            if_else(Button == 6, Obs_date + days_2_6, Obs_date)),
         Date = if_else(Button == 2, as.Date(ymd_hms(Obs_date)), 
                        if_else(Button == 6, as.Date(ymd_hms(Obs_date)), Date)))

#Now adjust the hours for 13
kwak_corr_3 <- kwak_corr_2 %>% 
  mutate(Obs_date = if_else(Button == 2, Obs_date - hrs_2_6, 
                            if_else(Button == 6, Obs_date - hrs_2_6,
                                    if_else(Button == 13, Obs_date - hrs_13, Obs_date))),
         Date = if_else(Button == 2, as.Date(ymd_hms(Obs_date)), 
                        if_else(Button == 6, as.Date(ymd_hms(Obs_date)),
                                if_else(Button == 13, as.Date(ymd_hms(Obs_date)), Date)))) %>% 
  filter(Date > "2019-03-20" & Date < "2019-08-03")

ggplot(kwak_corr_3, aes(Obs_date, Temp, fill = Button)) + 
  geom_point (aes(colour = Button)) +
  labs(x = "Date", y = "temperature (°C)") +
  theme_cowplot(16) + theme(legend.position = c(0.1, 0.75))

#Test whether the ibutton times line up with the other tides----
#Trial with a week in May when there were large tides (May 15-22)
#Buttons 1 and 4 are identical
select_k <- kwak_corr_3 %>% 
  filter(Date > "2019-03-18" & Date < "2019-03-31") 

ggplot(select_k, aes(Obs_date, Temp, fill = Button)) + 
  geom_point (aes(colour = Button)) +
  labs(x = "Date", y = "temperature (°C)") +
  theme_cowplot(16) + theme(legend.position = c(0.1, 0.75))

#Go through each month and add 0 if iButton is under water and 1 if it's in the air based on temperature data ----
#Start with March
march <- kwak_corr_3 %>% 
  filter(Date > "2019-03-01" & Date < "2019-03-30")

ggplot(march, aes(Obs_date, Temp, fill = Button)) + 
  geom_point (aes(colour = Button)) +
  labs(x = "Date", y = "temperature (°C)") +
  geom_hline(yintercept = 9.8, linetype = "dashed", alpha = 0.8, col = "grey") +
  theme_cowplot(16) + theme(legend.position = c(0.1, 0.75))

march_tides <- march %>% 
  mutate(in.water = ifelse(Temp > 9.8, 1, 0))

march_remaining <- kwak_corr_3 %>% 
  filter(Date > "2019-03-29" & Date < "2019-04-01") %>% 
  mutate(in.water = 0)
  
march_tides <- rbind(march_tides, march_remaining)
  
#Now try with April
april <- kwak_corr_3 %>% 
  filter(Date > "2019-04-06" & Date < "2019-04-29")

ggplot(april, aes(Obs_date, Temp, fill = Button)) + 
  geom_point (aes(colour = Button)) +
  labs(x = "Date", y = "temperature (°C)") +
  geom_hline(yintercept = 10.3, linetype = "dashed", alpha = 0.8, col = "grey") +
  theme_cowplot(16) + theme(legend.position = c(0.1, 0.75))

april_tides <- april %>% 
  mutate(in.water = ifelse(Temp > 10.3, 1, 0))

#I removed these first few days because the tides data showed there were no low tides during this range
april_remaining <- kwak_corr_3 %>% 
  filter(Date > "2019-03-31" & Date < "2019-04-07") %>% 
  mutate(in.water = 0)

april_tides <- rbind(april_tides, april_remaining)

#Now add the final days of the month when water is gettin a bit warmer
april_remaining <- kwak_corr_3 %>% 
  filter(Date > "2019-04-28" & Date < "2019-05-01") %>% 
  mutate(in.water = 0)

april_tides <- rbind(april_tides, april_remaining)

#Now import and attach the tides data----
cal_tides <- read_csv("data/iButtons/08863_PruthBay_20190301.csv") %>% 
  mutate(Obs_date = dmy_hms(paste(Date, Time)),
         Date = as.Date(Date, "%d/%m/%Y"))

cal_tides <- as.data.frame(cal_tides)

#Because the tidal station is in a different region, the dates/times are slightly off our iButton data
#Add 3 days and 3 hours to the cal_tides data
cal_days <- days(3)
cal_hours <- hours(3)

cal_tides_1 <- cal_tides %>% 
  filter(Date > "2019-03-01" & Date < "2019-06-09") %>% 
  mutate(Obs_date = Obs_date + cal_days + cal_hours) %>% 
  select(Obs_date, slev_m)

cal_tides_2 <- cal_tides_1 %>% 
  filter(Obs_date > "2019-03-18" & Obs_date < "2019-03-31")

#First visualize the tides data by themselves and find the high and low tide times
ggplot(cal_tides_1, aes(Obs_date, slev_m)) + 
  geom_point() +
  labs(x = "Date", y = "sea level") +
  geom_hline(yintercept = 1.20, linetype = "dashed", alpha = 0.8, col = "grey") +
  theme_cowplot(16) + theme(legend.position = c(0.1, 0.75))

#Now match up whether the times line up with the iButtons times

#Now join them and assign whether the iButton is in the water (0) or air (1)
str(kwak_corr_3)
str(cal_tides_1)

#Because the date.time objects weren't merging properly, I'm converting both Obs_date columns into characters
kwak_corr_3$Obs_date <- as.character(kwak_corr_3$Obs_date)
cal_tides_1$Obs_date <- as.character(cal_tides_1$Obs_date)

kwak_tides <- merge(kwak_corr_3, cal_tides_1, by = "Obs_date") %>% 
  mutate(in.water = ifelse(slev_m > TideHeight, 0, 1))%>% 
  filter(Date > "2019-03-20" & Date < "2019-06-06")

#Now summarize air and water temp in Kwakshua----
air <- march_tides %>% 
  rbind(april_tides) %>% 
  filter(in.water == 1)

water <- march_tides %>% 
  rbind(april_tides) %>% 
  filter(in.water == 0)

## Summarize values by site & dates & add a column for region
sum_air <- air %>% 
  group_by(SP, Date) %>% 
  summarise(avgair=mean(Temp), maxair=max(Temp), sdair=sd(Temp), air90th=quantile(Temp, 0.90), 
            air99th=quantile(Temp, 0.99), air10th=quantile(Temp, 0.10)) %>% 
  ungroup()

sum_water <- water %>% 
  group_by(SP, Date) %>% 
  summarise(avgwater=mean(Temp), maxwater=max(Temp), sdwater=sd(Temp), water90th=quantile(Temp, 0.90), 
            water99th=quantile(Temp, 0.99), water10th=quantile(Temp, 0.10)) %>% 
  ungroup()

#Visualise air & water temps!
ggplot(sum_water, aes(Date, avgwater)) + 
  geom_point (colour = "skyblue") +
  labs(x = "Date", y = "avg daily temperature, water (°C)", color = "Source Population") +
  theme_cowplot(16) + theme(legend.position = c(0.1, 0.75))

ggplot(sum_air, aes(Date, avgair)) + 
  geom_point (colour = "skyblue") +
  labs(x = "Date", y = "avg daily temperature, water (°C)", color = "Source Population") +
  theme_cowplot(16) + theme(legend.position = c(0.1, 0.75))

air_90 <- ggplot(sum_air, aes(Date, air90th, fill = SP)) + 
  geom_line (linewidth = 0.7) +
  scale_colour_manual(values = "skyblue") +
  labs(x = "Date", y = "90th percentile temperature, air (°C)", color = "Source Population") +
  theme_cowplot(16)+ theme(legend.position = c(0.1, 0.75))

water_90 <- ggplot(sum_water, aes(Date, avgwater)) + 
  geom_line (linewidth = 0.7, colour = "skyblue") +
  labs(x = "Date", y = "90th percentile temperature, water (°C)", color = "Source Population") +
  theme_cowplot(16) + theme(legend.position = c(0.1, 0.75))

#What if we just visualized the mean hourly temps and then inserted grey bars to visualize high tide----
sum_hour <- kwak_corr_3 %>% 
  group_by(Date, Obs_date) %>% 
  summarise(avgtemp=mean(Temp), maxtemp=max(Temp), sdtemp=sd(Temp), temp90th=quantile(Temp, 0.90)) %>% 
  ungroup() 

ggplot(sum_hour, aes(Date, avgtemp)) + 
  geom_point (colour = "skyblue") +
  labs(x = "Date", y = "avg daily temperature, water (°C)", color = "Source Population") +
  theme_cowplot(16) + theme(legend.position = c(0.1, 0.75))
