#iButtons script, 
#Last updated by FB Sept 2022

#Load packages----
pkgs <- c("tidyverse", "lubridate", "car", "visreg", "cowplot", "survminer", "survival",
          "emmeans", "lme4", "RVAideMemoire")
lapply(pkgs, library, character.only = TRUE)
rm(pkgs)

#Upload & clean iButton data from data folder----
#Load & clean ibutton data from Kwak and Pruth
#Kwak first timeseries (init-mid) 
#The Kwak and Pruth data comes from the cleaned folders (I had to change the years for some of the ibuttons
#which were programmed to start at 2000). For Cedar and Heron the unaltered files are in the 'Original> Combined>Heron or Cedar folders.

DF_K1 <- data.frame()	
files <- list.files(path="data/iButton_RT/Cleaned/Combined/Kwak_1", pattern="*.csv", full.names=TRUE, recursive=FALSE)	

for (i in 1:length(files)) {	
  Kwak_1<- read.csv(as.character(files[i]),header = TRUE, sep = ",", stringsAsFactors = FALSE, skip = 0)	
  Kwak_1$Date <- as.Date(paste(Kwak_1$Date.Time), formate = "%Y-%m-%d")
  Kwak_1 <- data.frame(Kwak_1)	
  DF_K1 <- rbind(Kwak_1,DF_K1) # add it to your list	
}	

#Now add a line that indicates it's Kwakshua
DF_K1<- DF_K1 %>% 
  mutate(SP = "Kwakshua") %>% 
  setNames(., c("DT" , "Time" , "Value" , "Temp" , "Date" , "SP"))

#delete any days when the ibuttons were recording in the lab. For Kwak remove first 6 rows, 06-04, 06-05, 
DF_K1 <- DF_K1 %>% 
  filter(Date != "2019-03-20") %>% 
  filter(Date != "2019-03-21") %>% 
  filter(Date != "2019-06-04") %>% 
  filter(Date != "2019-06-05")

#Now do the same code with the Pruth data sets from the first time point
DF_P1 <- data.frame()	
files_P1 <- list.files(path="data/iButton_RT/Cleaned/Combined/Pruth_1", pattern="*.csv", full.names=TRUE, recursive=FALSE)	

for (i in 1:length(files_P1)) {	
  Pruth_1<- read.csv(as.character(files_P1[i]),header = TRUE, sep = ",", stringsAsFactors = FALSE, skip = 0)	
  Pruth_1$Date <- as.Date(paste(Pruth_1$Date.Time), formate = "%Y-%m-%d")
  Pruth_1 <- data.frame(Pruth_1)
  DF_P1 <- rbind(Pruth_1,DF_P1) # add it to your list	
}	

DF_P1<- DF_P1 %>% 
  mutate(SP = "Pruth") %>% 
  setNames(., c("DT" , "Time", "Value" , "Temp" , "Date" , "SP"))

#delete any days when the ibuttons were recording in the lab. For Pruth remove first 9 rows, 06-04, 06-05, 
DF_P1 <- DF_P1 %>% 
  filter(Date != "2019-03-20") %>% 
  filter(Date != "2019-03-21") %>% 
  filter(Date != "2019-06-04") %>% 
  filter(Date != "2019-06-05")

#Load & clean ibutton data from Heron and Cedar (at Calvert)

DF_H1 <- data.frame()	
files_H1 <- list.files(path="data/iButton_RT/Original/Combined/Heron", pattern="*.csv", full.names=TRUE, recursive=FALSE)	

for (i in 1:length(files_H1)) {	
  Heron_1<- read.csv(as.character(files_H1[i]),header = TRUE, sep = ",", stringsAsFactors = FALSE, skip = 14)	
  Heron_1$Date <- as.Date(paste(Heron_1$Date.Time), format = "%d/%m/%y")
  Heron_1 <- data.frame(Heron_1)
  DF_H1 <- rbind(Heron_1,DF_H1) # add it to your list	
}	

DF_H1<- DF_H1 %>% 
  mutate(SP = "Heron") %>% 
  setNames(., c("DT" , "Value" , "Temp" , "Date" , "SP"))

#delete any days when the ibuttons were recording in the lab. For Heron remove everything prior to 
#2019-04-10, 2019-06-17 (when I switched the sampling frequency) and anything after 2019-08-12 
dates_excl <- c("2019-04-04", "2019-04-05", "2019-04-06", "2019-04-07", "2019-04-08", "2019-04-09", 
                "2019-06-17", "2019-08-13", "2019-08-14", "2019-08-15", "2019-08-16", "2019-08-17", 
                "2019-08-18", "2019-08-19", "2019-08-20", "2019-08-21", "2019-08-22")

DF_H1 <- DF_H1 %>% 
  filter(!Date %in% dates_excl)

#Now with Cedar! 
DF_C1 <- data.frame()	

files_C1 <- list.files(path="data/iButton_RT/Original/Combined/Cedar", pattern="*.csv", full.names=TRUE, recursive=FALSE)	

for (i in 1:length(files_C1)) {	
  Cedar_1<- read.csv(as.character(files_C1[i]),header = TRUE, sep = ",", stringsAsFactors = FALSE, skip = 14)	
  Cedar_1$Date <- as.Date(paste(Cedar_1$Date.Time), format = "%d/%m/%y")
  Cedar_1 <- data.frame(Cedar_1)
  DF_C1 <- rbind(Cedar_1,DF_C1) # add it to your list	
}	

DF_C1<- DF_C1 %>% 
  mutate(SP = "Cedar") %>% 
  setNames(., c("DT" , "Value" , "Temp" , "Date" , "SP"))

DF_C1 <- DF_C1 %>% 
  filter(!Date %in% dates_excl)

#Merge & export the datasets
#Start with the Nanaimo datasets & split the DT column into separate date & time columns
nanaimo_df <- rbind(DF_C1, DF_H1) %>% 
  mutate(test = parse_date_time(DT, "%d/%m/%y %I:%M:%S %p")) %>% 
  mutate(Time = format(test, format = "%H:%M:%S")) %>% 
  select(Date, Time, SP, Temp)

#Now merge with the calvert datasets
calvert_df <- rbind(DF_K1, DF_P1) %>% 
  mutate(Time_2 = parse_time(Time, "%I:%M:%S %p")) %>% 
  mutate(Time = format(Time_2, format = "%H:%M:%S")) %>% 
  select(Date, Time, SP, Temp)

#Merge into one cdataframe and add tidal height column based on RT_2_V1_Aug_KM excel sheet
all_df <- rbind(nanaimo_df, calvert_df) %>% 
  mutate(TideHeight = ifelse(SP == "Cedar", 1.15, 
                             ifelse(SP == "Heron", 0.925,
                                    ifelse(SP == "Kwakshua", 1.30,
                                           ifelse(SP == "Pruth", 1.65, ""))))) %>% 
  mutate(date.time = ymd_hms(paste(Date, Time))) %>% 
  select(Date, Time, date.time, SP, Temp, TideHeight)

#Export to data folder
write.csv(all_df, "data/iButtons/all_cleaned.csv")

#Remove objects/variables not needed from hereonin----
#Remove unnecessary objects
rm(calvert_df, Cedar_1, DF_C1, DF_H1, DF_K1, DF_P1, Heron_1, Kwak_1, nanaimo_df, Pruth_1,
   dates_excl, files, files_C1, files_H1, files_P1, i)

#Manually assign 0 or 1 based on whether the marker is emmersed or not based on tide data from----
#I requested the tides data from DFO/CHS (see emails with Patrick McNeill on Jan 31-Feb 1 2022)
#The data came from this website: https://tides.gc.ca/tides/en/stations
#Chose Pruth Lagoon & Boat Harbour
#but I requested his to send me the csvs because otherwise you have to export by 7-day incremenets
#First I manually deleted the top few rows
cal_tides <- read_csv("data/iButtons/08863_PruthBay_20190301.csv") %>% 
  mutate(Obs_date = dmy_hms(paste(Date, Time))) %>% 
  select(Obs_date, slev_m)

nan_tides <- read_csv("data/iButtons/07480_BoatHarbour_20190301.csv") %>% 
  mutate(Obs_date = dmy_hms(paste(Date, Time))) %>% 
  select(Obs_date, slev_m)

#You  only need to run this line if you're starting the code here, otherwise just run the first section of the code
#all_df <- read_csv("data/iButtons/all_cleaned.csv")
#all_df <- all_df[, c(2:7)] %>% 
#  mutate(SP = as.factor(SP))

#Combine the datasets by the time column but the all_df reports time at different minute increments than the 
#exported csv, which reports on the 00 min. So, I rounded the minutes to the hour by replacing them with '00'

all_df_corr <- all_df %>% 
  mutate(time_corr = format(date.time, format = "%H")) %>% 
  mutate(time_corr = paste0(time_corr, ":00:00")) %>% 
  mutate(time_corr = format(time_corr, format = "%H:%M:%S")) %>% 
  mutate(Obs_date = as.POSIXct(paste(Date, time_corr))) %>% 
  select(SP, Temp, TideHeight, Date, Time, date.time, Obs_date)

#Merge the nan_tides & all_df_nan & assign whether ibutton was: underwater = 0, if slev_m > TideHeight OR emersed = 1, if slev_m <= TideHeight
#Also clip each regional dataset for the following date range:
#Calvert = 2019-03-21 - 2019-08-03
#Nanaimo = 2019-04-11 - 2019-08-12

all_df_corr_cal <- all_df_corr %>% 
  filter(SP == "Kwakshua" | SP == "Pruth")

all_df_cal_tides <- merge(all_df_corr_cal, cal_tides, by = "Obs_date") %>% 
  mutate(in.water = ifelse(slev_m > TideHeight, 0, 1))%>% 
  filter(Date > "2019-03-21" & Date < "2019-08-03")

all_df_corr_nan <- all_df_corr %>% 
  filter(SP == "Cedar" | SP == "Heron")

all_df_nan_tides <- merge(all_df_corr_nan, nan_tides, by = "Obs_date") %>% 
  mutate(in.water = ifelse(slev_m > TideHeight, 0, 1)) %>% 
  filter(Date > "2019-04-11" & Date < "2019-08-12")

#Combine into one dataset
all_tides <- rbind(all_df_cal_tides, all_df_nan_tides) %>% 
  select(Date, Time, Obs_date, SP, Temp, TideHeight, slev_m, in.water) %>% 
  mutate(SP = as.factor(SP))

#Export files to csv for further analysis
write.csv(all_tides, "data/iButtons/all_tides.csv")

#Remove objects that aren't needed going forward
rm(cal_tides, nan_tides, all_df_cal_tides, all_df_corr, all_df_corr_cal,
   all_df_corr_nan, all_df_nan_tides, all_df)

#Summarize iButton data ----
#The code in this section is informed by Alyssa's code in the scripts folder

air <- all_tides %>% 
  filter(in.water == 1)

water <- all_tides %>% 
  filter(in.water == 0)

## Summarize values by site & dates & add a column for region
sum_air <- air %>% 
  group_by(SP, Date) %>% 
  summarise(avgair=mean(Temp), maxair=max(Temp), sdair=sd(Temp), air90th=quantile(Temp, 0.90), 
            air99th=quantile(Temp, 0.99), air10th=quantile(Temp, 0.10)) %>% 
  mutate(region = ifelse(c(SP == "Kwakshua" | SP == "Pruth"), "Calvert", "Nanaimo"),
         region = as.factor(region)) %>% 
  ungroup()

sum_water <- water %>% 
  group_by(SP, Date) %>% 
  summarise(avgwater=mean(Temp), maxwater=max(Temp), sdwater=sd(Temp), water90th=quantile(Temp, 0.90), 
            water99th=quantile(Temp, 0.99), water10th=quantile(Temp, 0.10)) %>% 
  mutate(region = ifelse(c(SP == "Kwakshua" | SP == "Pruth"), "Calvert", "Nanaimo"),
         region = as.factor(region)) %>% 
  ungroup()

sum_both <- all_tides %>% 
  group_by(SP, Date) %>% 
  summarize(avgboth=mean(Temp), sdboth=sd(Temp), both90th=quantile(Temp, 0.90)) %>% 
  mutate(region = ifelse(c(SP == "Kwakshua" | SP == "Pruth"), "Calvert", "Nanaimo"),
         region = as.factor(region)) %>% 
  ungroup()

#Visualize temps----
water_90 <- ggplot(sum_water, aes(Date, water90th, fill = SP)) + 
  geom_line (aes(colour = SP), size = 0.7) +
  scale_colour_manual(values = c("coral", "coral3", "skyblue", "skyblue3")) +
  labs(x = "Date", y = "90th percentile temperature, water (°C)", color = "Source Population") +
  theme_cowplot(16) + theme(legend.position = c(0.1, 0.75))

air_90 <- ggplot(sum_air, aes(Date, air90th, fill = SP)) + 
  geom_line (aes(colour = SP), size = 0.7) +
  scale_colour_manual(values = c("coral", "coral3", "skyblue", "skyblue3")) +
  labs(x = "Date", y = "90th percentile temperature, air (°C)", color = "Source Population") +
  theme_cowplot(16)+ theme(legend.position = c(0.1, 0.75))

both_90 <- ggplot(sum_both, aes(Date, both90th, fill = SP)) + 
  geom_line (aes(colour = SP), size = 0.7) +
  geom_hline(yintercept = 12, linetype = "dashed", alpha = 0.8, col = "grey") +
  geom_hline(yintercept = 15, linetype = "dashed", alpha = 0.8, col = "grey") +
  geom_hline(yintercept = 19, linetype = "dashed", alpha = 0.8, col = "grey") +
  geom_hline(yintercept = 22, linetype = "dashed", alpha = 0.8, col = "grey") +
  scale_colour_manual(values = c("coral", "coral3", "skyblue", "skyblue3")) +
  labs(x = "Date", y = "90th percentile temperature (°C)", color = "Outplant site") +
  theme_cowplot(16)+ theme(legend.position = c(0.1, 0.75))

#Sites faceted
water_90_facet<- ggplot(data = sum_water, aes(Date, water90th, fill = SP)) + 
  geom_line(aes(colour = SP), size = 0.7) +
  scale_colour_manual(values = c("coral", "coral3", "skyblue", "skyblue3")) +
  theme_bw() + labs(x = "Date", y = "90th percentile temperature, water (°C)") +
  facet_grid(. ~ SP) +
  theme_cowplot(16)

ggsave(both_90, file = "plots/iButtons/Fig_3_both_90percentile.pdf", width = 10, height = 8, dpi = 300)

#Now create a new dataframe that's melted (long) and visualize both the mean and 90th percentil----
sum_long <- sum_both %>% 
  gather(metric, value, c(avgboth, both90th), factor_key = TRUE) %>% 
  mutate(metric = fct_relevel(metric, c("both90th", "avgboth")),
         region = ifelse(region == "Nanaimo", "Strait of Georgia", "Central Coast"),
         SP = fct_relevel(SP, c("Kwakshua", "Pruth", "Cedar", "Heron")))
levels(sum_long$metric) <- c("90th percentile", "Mean")

sum_long_facet<- ggplot(data = sum_long, aes(Date, value, colour = SP)) + 
  geom_line(aes(colour = SP, linetype = metric), size = 0.7) +
  scale_colour_manual(values = c("skyblue", "skyblue3", "coral", "coral3")) +
  geom_hline(yintercept = 12, linetype = "dashed", alpha = 0.8, col = "grey") +
  geom_hline(yintercept = 15, linetype = "dashed", alpha = 0.8, col = "grey") +
  geom_hline(yintercept = 19, linetype = "dashed", alpha = 0.8, col = "grey") +
  geom_hline(yintercept = 22, linetype = "dashed", alpha = 0.8, col = "grey") +
  labs(x = "Date", y = "Temperature (°C)", colour = "Outplant site", linetype = "Temperature metric") +
  facet_wrap(. ~ SP, ncol = 2) +
  theme_cowplot(16)+ 
  theme(legend.position = c(0.06, 0.9), legend.direction = "horizontal",
        strip.background = element_blank(), strip.text.x = element_blank())

ggsave(sum_long_facet, file = "plots/iButtons/Fig_3_90percentil_mean.pdf", width = 10, height = 8, dpi = 300)


#Test whether the 90th percentile is significantly different across regions over time----
#Use an ancova to test difference with time as the covariate (as per https://www.r-bloggers.com/2021/07/how-to-perform-ancova-in-r/)
#Load the packages required for this at each stage cause they seem to interact weirdly with one another

library('car')
region_ibutton_lm <- lmer(both90th ~ Date + region + (1|SP), data = sum_both)

Anova(region_ibutton_lm, type = "II")

ancova_model <- aov(both90th ~ Date + region, data = sum_both)
Anova(ancova_model, type="II")
visreg(ancova_model)
#Both region & Date are very significant, with temp increasing with time & differing between the 2 regions

#Test whether temp significantly differed across sites within each region
sum_both_cal <- sum_both %>% 
  filter(SP == "Kwakshua" | SP == "Pruth")

sum_both_nan <- sum_both %>% 
  filter(SP == "Cedar" | SP == "Heron")

ancova_cal <-aov(both90th ~ SP + Date, data = sum_both_cal)
Anova(ancova_cal, type="II")
visreg(ancova_cal)

ancova_nan <-aov(both90th ~ SP + Date, data = sum_both_nan)
Anova(ancova_nan, type="II")
visreg(ancova_nan)

#Calculate the mean difference between Nan & Cal during the summer (i.e. May - August)----
#Shorten dataset to Nanaimo start date and Calvert end date so you're comparing the same range of dates across regions:
#Start April 12th (i.e. 2019-04-12)
#Eng August second (i.e. 2019-08-02)
#THIS IS THE ONE YOU USED IN YOUR PAPER AS OF SEPT 21st
diff <- sum_both %>% 
  filter("2019-04-11" < Date & Date < "2019-08-02") %>% 
  mutate(month = month(Date)) %>% 
  group_by(region) %>% 
  summarize(mean_90th = mean(both90th), sd_90th = sd(both90th),
            mean_avg = mean(avgboth), sd_avg = sd(avgboth))

diff_2 <- sum_both %>% 
  mutate(month = month(Date)) %>% 
  filter(month == 4 | month == 5 | month == 6 | month == 7 | month == 8) %>% 
  group_by(region, month) %>% 
  summarize(mean_90th = mean(both90th), meanavg = mean(avgboth), sdavg = sd(avgboth)) %>% 
  arrange(month) %>% 
  ungroup() %>% 
  group_by(month) %>% 
  mutate(diff_90th = mean_90th - lead(mean_90th, default = first(mean_90th)),
         diff_avg = meanavg - lead(meanavg, default = first(meanavg)),
         diff_sd = sdavg - lead(sdavg, default = first(sdavg))) %>% 
  ungroup()

#Remove every even row from diff dataframe, then calculate the mean & sd of the 90th percentile diff
ind <- seq(1, nrow(diff), by=2)
diff_sum <- diff[ind, ] %>% 
  summarize(mean_90th_diff = mean(mean_90th), sd_90th_diff = sd(mean_90th),
            mean_avg_diff = mean(mean_avg), sd_avg_diff = sd(mean_avg))

#Calculate the average & sd for mean & 90th at each region
diff_sd <- sum_both %>% 
  mutate(month = month(Date)) %>% 
  filter(month == 4 | month == 5 | month == 6 | month == 7 | month == 8) %>% 
  group_by(region) %>% 
  summarize(mean_avg = mean(avgboth), sd_avg = sd(avgboth), mean_90th = mean(both90th), sd_90th = sd(both90th))

#Now calculate the difference for the Calvert & Nanaimo sites
diff_cal <- sum_both_cal %>% 
  mutate(month = month(Date)) %>% 
  filter(month == 4 | month == 5 | month == 6 | month == 7 | month == 8) %>% 
  group_by(SP, month) %>% 
  summarize(mean_90th = mean(both90th), meanavg = mean(avgboth)) %>% 
  arrange(month) %>% 
  ungroup() %>% 
  group_by(month) %>% 
  mutate(diff_90th = mean_90th - lead(mean_90th, default = first(mean_90th)),
         diff_avg = meanavg - lead(meanavg, default = first(meanavg))) %>% 
  ungroup()

ind <- seq(1, nrow(diff_cal), by=2)
diff_cal_sum <- diff_cal[ind, ] %>% 
  summarize(mean_90th_diff = mean(diff_90th), sd_90th_diff = sd(diff_90th),
            mean_avg_diff = mean(diff_avg), sd_avg_diff = sd(diff_avg))

diff_nan <- sum_both_nan %>% 
  mutate(month = month(Date)) %>% 
  filter(month == 4 | month == 5 | month == 6 | month == 7 | month == 8) %>% 
  group_by(SP, month) %>% 
  summarize(mean_90th = mean(both90th), meanavg = mean(avgboth)) %>% 
  arrange(month) %>% 
  ungroup() %>% 
  group_by(month) %>% 
  mutate(diff_90th = mean_90th - lead(mean_90th, default = first(mean_90th)),
         diff_avg = meanavg - lead(meanavg, default = first(meanavg))) %>% 
  ungroup()

#Remove every even row from diff dataframe, then calculate the mean & sd of the 90th percentile diff
ind <- seq(1, nrow(diff_nan), by=2)
diff_nan_sum <- diff_nan[ind, ] %>% 
  summarize(mean_90th_diff = mean(diff_90th), sd_90th_diff = sd(diff_90th),
            mean_avg_diff = mean(diff_avg), sd_avg_diff = sd(diff_avg))
#Remove all objects----
rm(nan_tides, cal_tides, sum_air, sum_water, water, air, all_df, all_tides, water_90, air_90, water_90_facet, 
   all_df_cal_tides, all_df_corr, all_df_corr_cal, all_df_corr_nan, my_theme)
