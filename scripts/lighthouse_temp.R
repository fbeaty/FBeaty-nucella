#Visualizing the long-term temperature data at Calvert & Nanaimo
#Data downloaded from https://open.canada.ca/data/en/dataset/719955f2-bf8e-44f7-bc26-6bd623e82884
#Last edited Sept 2022

#Load in necessary packages----
install.packages("janitor")
pkgs <- c("janitor", "dplyr", "tidyverse", "ggplot2", "zoo", "lubridate", "cowplot")
lapply(pkgs, library, character.only = TRUE)
rm(pkgs)

#Load csvs & summarize temp data----
#from Egg Island (closest lighthouse to Calvert) & Departure Bay (closest to Nanaimo sites)
egg <- read.csv("data/lighthouse/Egg_Island_-_Daily_Sea_Surface_Temperature_and_Salinity_1970-2021.csv")
departure <- read.csv("data/lighthouse/Departure_Bay_PBS_-_Daily_Sea_Surface_Temperature_and_Salinity_1914-2021.csv")

#Clean dataset and filter for 2012-2020 data
egg_1 <- egg[ , c(1,3)] %>% 
  row_to_names(row_number = 1) %>% 
  rename("date" = "DATE (YYYY-MM-DD)", "temp" = "TEMPERATURE ( C )") %>% 
  mutate(date = ymd(date),
         temp = as.numeric(temp),
         station = "Egg Island, Central Coast",
         year = year(date),
         month = month(date)) %>% 
  filter(date > "2012-01-01" & date < "2020-01-01", 
         temp != 999.9) 

departure_1 <- departure[ , c(1,3)] %>% 
  row_to_names(row_number = 1) %>% 
  rename("date" = "DATE (YYYY-MM-DD)", "temp" = "TEMPERATURE ( C )") %>% 
  mutate(date = ymd(date),
         temp = as.numeric(temp),
         station = "Departure Bay, Strait of Georgia",
         year = year(date),
         month = month(date)) %>% 
  filter(date > "2012-01-01" & date < "2020-01-01", 
         temp != 999.9) 

#Combine datasets & summarize temps
lighthouse <- rbind(egg_1, departure_1) %>% 
  mutate(station = as.factor(station),
         year = as.factor(year),
         month = as.factor(month)) %>% 
  group_by(station, year, month) %>% 
  summarize(avgtemp = mean(temp), maxtemp = max(temp), sdtemp = sd(temp), temp90th = quantile(temp, 0.90)) %>% 
  ungroup() %>% 
  mutate(date = as.yearmon(paste(year, month), "%Y %m"))

lighthouse_annual <- rbind(egg_1, departure_1) %>% 
  mutate(station = as.factor(station),
         year = as.factor(year),
         month = as.factor(month)) %>% 
  group_by(station, year) %>% 
  summarize(annltemp = mean(temp), annl_90th = quantile(temp, 0.90)) %>% 
  ungroup()

#Visualize the data----
both <- ggplot(lighthouse, aes(date, temp90th, group = station)) + 
  geom_line(aes(colour = station), size = 0.7) +
  scale_colour_manual(values = c("coral", "skyblue")) +
  labs(x = "Year", y = "90th percentile SST (°C)") +
  theme_cowplot(16) + theme(legend.position = "top", legend.justification = "right")

ggsave(both, file = "plots/lighthouse/both_lighthouse_stations.pdf", height = 5, width = 9, dpi = 300)

#Calculate the mean difference between Dep & Egg during the summer (i.e. May - September)----
diff <- lighthouse %>% 
  filter(month == 6 | month == 7 | month == 8 | month == 9) %>% 
  group_by(station, date) %>% 
  summarize(mean_90th = mean(temp90th)) %>% 
  arrange(date) %>% 
  ungroup() %>% 
  group_by(date) %>% 
  mutate(diff = mean_90th - lead(mean_90th, default = first(mean_90th))) %>% 
  ungroup()

#Remove every even row from diff dataframe, then calculate the mean & sd of the 90th percentile diff
ind <- seq(1, nrow(diff), by=2)
diff_sum <- diff[ind, ] %>% 
  summarize(mean_90th_diff = mean(diff), sd_90th_diff = sd(diff))

#Calculate the average variability in each region dueing the summer (May-Sept)
var_nan <- departure_1 %>% 
  mutate(station = as.factor(station),
         year = as.factor(year),
         month = as.factor(month),
         date = as.yearmon(paste(year, month), "%Y %m")) %>% 
  summarize(mean_month = mean(temp), sd_month = sd(temp))

var_cal <- egg_1 %>% 
  mutate(station = as.factor(station),
         year = as.factor(year),
         month = as.factor(month),
         date = as.yearmon(paste(year, month), "%Y %m")) %>% 
  summarize(mean_month = mean(temp), sd_month = sd(temp))

meanvar <- var_nan %>% 
  rbind(var_cal)
#Visualize temps from May - Sept
months <- c(5:9)

summer <- lighthouse %>% 
  filter(month %in% months)

ggplot(summer, aes(date, avgtemp, group = station)) + 
  geom_line(aes(colour = station), size = 0.7) +
  scale_colour_manual(values = c("coral", "skyblue")) +
  labs(x = "Month", y = "90th percentile SST (°C)") +
  theme_cowplot(16) + theme(legend.position = "top", legend.justification = "right")

#Calculate the number of days where temp > 15 in Nanaimo & Calvert
fifteen_nan <- departure_1 %>% 
  filter(month == 6 | month == 7 | month == 8 | month == 9) %>% 
  filter(temp > 15)
nineteen_nan <- departure_1 %>% 
  filter(month == 6 | month == 7 | month == 8 | month == 9) %>% 
  filter(temp > 19)
summer_nan <- departure_1 %>% 
  filter(month == 6 | month == 7 | month == 8 | month == 9)

#780/2901 = 27% of the time (annual)
#780/974 = 75% of the time during the summer temps exceed 15
#136/974 = 14% of the time temps exceed 19 in the summer

fifteen_cal <- egg_1 %>% 
  filter(month == 6 | month == 7 | month == 8 | month == 9) %>% 
  filter(temp > 15)
nineteen_cal <- egg_1 %>% 
  filter(month == 6 | month == 7 | month == 8 | month == 9) %>% 
  filter(temp > 19)
summer_cal <- egg_1 %>% 
  filter(month == 6 | month == 7 | month == 8 | month == 9)

#94/2398 = 4.0 % of the time in the year
#94/902 = 10% of the time during the summer
#temps never exceeded 19 in the summer

plot(fifteen_nan$date, fifteen_nan$temp)

