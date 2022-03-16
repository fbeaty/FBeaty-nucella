#This script is to analyze the mesocosm ibuttons
#Based in part upon Alyssa's code in scripts folder
#Last updated Jan 2022

#Load packages----
pkgs <- c("viridis", "dplyr", "lattice", "readr", "MuMIn", "tidyr", "lubridate", "ggplot2", "purrr", "tidyverse", "stringr", "ggpubr")
lapply(pkgs, library, character.only = TRUE)
rm(pkgs)

#Load in csv files from CH 2 analysis folder ----
DF_T1 <- data.frame()	

files <- list.files(path="/Users/fionabeaty/Dropbox/Fiona/School/Chapter 2 - Local Adaptation/Ch2 Data Analysis/iButton_meso/Tank1", pattern="*.csv", full.names=TRUE, recursive=FALSE)	

for (i in 1:length(files)) {	
  T1 <- read.csv(as.character(files[i]),header = TRUE, sep = ",", stringsAsFactors = FALSE, skip = 0)	
  T1 <- data.frame(T1)	
  T1$Date <- as.Date(paste(T1$Date.Time), "%Y-%m-%d")
  T1$Tank <- 1
  DF_T1 <- rbind(T1,DF_T1) 	
}	

DF_T2 <- data.frame()	

files <- list.files(path="/Users/fionabeaty/Dropbox/Fiona/School/Chapter 2 - Local Adaptation/Ch2 Data Analysis/iButton_meso/Tank2", pattern="*.csv", full.names=TRUE, recursive=FALSE)	

for (i in 1:length(files)) {	
  T2 <- read.csv(as.character(files[i]),header = TRUE, sep = ",", stringsAsFactors = FALSE, skip = 0)	
  T2 <- data.frame(T2)	
  T2$Date <- as.Date(paste(T2$Date.Time), "%Y-%m-%d")
  T2$Tank <- 2
  DF_T2 <- rbind(T2,DF_T2) 
}	

DF_T3 <- data.frame()	

files <- list.files(path="/Users/fionabeaty/Dropbox/Fiona/School/Chapter 2 - Local Adaptation/Ch2 Data Analysis/iButton_meso/Tank3", pattern="*.csv", full.names=TRUE, recursive=FALSE)	

for (i in 1:length(files)) {	
  T3 <- read.csv(as.character(files[i]),header = TRUE, sep = ",", stringsAsFactors = FALSE, skip = 0)	
  T3 <- data.frame(T3)	
  T3$Date <- as.Date(paste(T3$Date.Time), "%Y-%m-%d")
  T3$Tank <- 3
  DF_T3 <- rbind(T3,DF_T3) 
}	

DF_T4 <- data.frame()	

files <- list.files(path="/Users/fionabeaty/Dropbox/Fiona/School/Chapter 2 - Local Adaptation/Ch2 Data Analysis/iButton_meso/Tank4", pattern="*.csv", full.names=TRUE, recursive=FALSE)	

for (i in 1:length(files)) {	
  T4 <- read.csv(as.character(files[i]),header = TRUE, sep = ",", stringsAsFactors = FALSE, skip = 0)	
  T4 <- data.frame(T4)	
  T4$Date <- as.Date(paste(T4$Date.Time), "%Y-%m-%d")
  T4$Tank <- 4
  DF_T4 <- rbind(T4,DF_T4) 
}	

DF_T5 <- data.frame()	

files <- list.files(path="/Users/fionabeaty/Dropbox/Fiona/School/Chapter 2 - Local Adaptation/Ch2 Data Analysis/iButton_meso/Tank5", pattern="*.csv", full.names=TRUE, recursive=FALSE)	

for (i in 1:length(files)) {	
  T5 <- read.csv(as.character(files[i]),header = TRUE, sep = ",", stringsAsFactors = FALSE, skip = 0)	
  T5 <- data.frame(T5)	
  T5$Date <- as.Date(paste(T5$Date.Time), "%Y-%m-%d")
  T5$Tank <- 5
  DF_T5 <- rbind(T5,DF_T5) 
}	

DF_T6 <- data.frame()	

files <- list.files(path="/Users/fionabeaty/Dropbox/Fiona/School/Chapter 2 - Local Adaptation/Ch2 Data Analysis/iButton_meso/Tank6", pattern="*.csv", full.names=TRUE, recursive=FALSE)	

for (i in 1:length(files)) {	
  T6 <- read.csv(as.character(files[i]),header = TRUE, sep = ",", stringsAsFactors = FALSE, skip = 0)	
  T6 <- data.frame(T6)	
  T6$Date <- as.Date(paste(T6$Date.Time), "%Y-%m-%d")
  T6$Tank <- 6
  DF_T6 <- rbind(T6,DF_T6) 
}	

DF_T7 <- data.frame()	

files <- list.files(path="/Users/fionabeaty/Dropbox/Fiona/School/Chapter 2 - Local Adaptation/Ch2 Data Analysis/iButton_meso/Tank7", pattern="*.csv", full.names=TRUE, recursive=FALSE)	

for (i in 1:length(files)) {	
  T7 <- read.csv(as.character(files[i]),header = TRUE, sep = ",", stringsAsFactors = FALSE, skip = 0)	
  T7 <- data.frame(T7)	
  T7$Date <- as.Date(paste(T7$Date.Time), "%Y-%m-%d")
  T7$Tank <- 7
  DF_T7 <- rbind(T7,DF_T7) 
}	

DF_T8 <- data.frame()	

files <- list.files(path="/Users/fionabeaty/Dropbox/Fiona/School/Chapter 2 - Local Adaptation/Ch2 Data Analysis/iButton_meso/Tank8", pattern="*.csv", full.names=TRUE, recursive=FALSE)	

for (i in 1:length(files)) {	
  T8 <- read.csv(as.character(files[i]),header = TRUE, sep = ",", stringsAsFactors = FALSE, skip = 0)	
  T8 <- data.frame(T8)	
  T8$Date <- as.Date(paste(T8$Date.Time), "%Y-%m-%d")
  T8$Tank <- 8
  DF_T8 <- rbind(T8,DF_T8) 
}	

DF_T9 <- data.frame()	

files <- list.files(path="/Users/fionabeaty/Dropbox/Fiona/School/Chapter 2 - Local Adaptation/Ch2 Data Analysis/iButton_meso/Tank9", pattern="*.csv", full.names=TRUE, recursive=FALSE)	

for (i in 1:length(files)) {	
  T9 <- read.csv(as.character(files[i]),header = TRUE, sep = ",", stringsAsFactors = FALSE, skip = 0)	
  T9 <- data.frame(T9)	
  T9$Date <- as.Date(paste(T9$Date.Time), "%Y-%m-%d")
  T9$Tank <- 9
  DF_T9 <- rbind(T9,DF_T9) 
}	

DF_T10 <- data.frame()	

files <- list.files(path="/Users/fionabeaty/Dropbox/Fiona/School/Chapter 2 - Local Adaptation/Ch2 Data Analysis/iButton_meso/Tank10", pattern="*.csv", full.names=TRUE, recursive=FALSE)	

for (i in 1:length(files)) {	
  T10 <- read.csv(as.character(files[i]),header = TRUE, sep = ",", stringsAsFactors = FALSE, skip = 0)	
  T10 <- data.frame(T10)	
  T10$Date <- as.Date(paste(T10$Date.Time), "%Y-%m-%d")
  T10$Tank <- 10
  DF_T10 <- rbind(T10,DF_T10) 
}	

DF_T11 <- data.frame()	

files <- list.files(path="/Users/fionabeaty/Dropbox/Fiona/School/Chapter 2 - Local Adaptation/Ch2 Data Analysis/iButton_meso/Tank11", pattern="*.csv", full.names=TRUE, recursive=FALSE)	

for (i in 1:length(files)) {	
  T11 <- read.csv(as.character(files[i]),header = TRUE, sep = ",", stringsAsFactors = FALSE, skip = 0)	
  T11 <- data.frame(T11)	
  T11$Date <- as.Date(paste(T11$Date.Time), "%Y-%m-%d")
  T11$Tank <- 11
  DF_T11 <- rbind(T11,DF_T11) 
}	

DF_T12 <- data.frame()	

files <- list.files(path="/Users/fionabeaty/Dropbox/Fiona/School/Chapter 2 - Local Adaptation/Ch2 Data Analysis/iButton_meso/Tank12", pattern="*.csv", full.names=TRUE, recursive=FALSE)	

for (i in 1:length(files)) {	
  T12 <- read.csv(as.character(files[i]),header = TRUE, sep = ",", stringsAsFactors = FALSE, skip = 0)	
  T12 <- data.frame(T12)	
  T12$Date <- as.Date(paste(T12$Date.Time), "%Y-%m-%d")
  T12$Tank <- 12
  DF_T12 <- rbind(T12,DF_T12) 
}	

DF_T13 <- data.frame()	

files <- list.files(path="/Users/fionabeaty/Dropbox/Fiona/School/Chapter 2 - Local Adaptation/Ch2 Data Analysis/iButton_meso/Tank13", pattern="*.csv", full.names=TRUE, recursive=FALSE)	

for (i in 1:length(files)) {	
  T13 <- read.csv(as.character(files[i]),header = TRUE, sep = ",", stringsAsFactors = FALSE, skip = 0)	
  T13 <- data.frame(T13)	
  T13$Date <- as.Date(paste(T13$Date.Time), "%Y-%m-%d")
  T13$Tank <- 13
  DF_T13 <- rbind(T13,DF_T13) 
}

DF_T14 <- data.frame()	
files <- list.files(path="/Users/fionabeaty/Dropbox/Fiona/School/Chapter 2 - Local Adaptation/Ch2 Data Analysis/iButton_meso/Tank14", pattern="*.csv", full.names=TRUE, recursive=FALSE)	

for (i in 1:length(files)) {	
  T14 <- read.csv(as.character(files[i]),header = TRUE, sep = ",", stringsAsFactors = FALSE, skip = 0)	
  T14 <- data.frame(T14)	
  T14$Date <- as.Date(paste(T14$Date.Time), "%Y-%m-%d")
  T14$Tank <- 14
  DF_T14 <- rbind(T14,DF_T14) 
}

DF_T15 <- data.frame()	

files <- list.files(path="/Users/fionabeaty/Dropbox/Fiona/School/Chapter 2 - Local Adaptation/Ch2 Data Analysis/iButton_meso/Tank15", pattern="*.csv", full.names=TRUE, recursive=FALSE)	

for (i in 1:length(files)) {	
  T15 <- read.csv(as.character(files[i]),header = TRUE, sep = ",", stringsAsFactors = FALSE, skip = 0)	
  T15 <- data.frame(T15)	
  T15$Date <- as.Date(paste(T15$Date.Time), "%Y-%m-%d")
  T15$Tank <- 15
  DF_T15 <- rbind(T15,DF_T15) 
}

DF_T16 <- data.frame()	

files <- list.files(path="/Users/fionabeaty/Dropbox/Fiona/School/Chapter 2 - Local Adaptation/Ch2 Data Analysis/iButton_meso/Tank16", pattern="*.csv", full.names=TRUE, recursive=FALSE)	

for (i in 1:length(files)) {	
  T16 <- read.csv(as.character(files[i]),header = TRUE, sep = ",", stringsAsFactors = FALSE, skip = 0)	
  T16 <- data.frame(T16)	
  T16$Date <- as.Date(paste(T16$Date.Time), "%Y-%m-%d")
  T16$Tank <- 16
  DF_T16 <- rbind(T16,DF_T16) 
}

DF_T17 <- data.frame()	

files <- list.files(path="/Users/fionabeaty/Dropbox/Fiona/School/Chapter 2 - Local Adaptation/Ch2 Data Analysis/iButton_meso/Tank17", pattern="*.csv", full.names=TRUE, recursive=FALSE)	

for (i in 1:length(files)) {	
  T17 <- read.csv(as.character(files[i]),header = TRUE, sep = ",", stringsAsFactors = FALSE, skip = 0)	
  T17 <- data.frame(T17)	
  T17$Date <- as.Date(paste(T17$Date.Time), "%Y-%m-%d")
  T17$Tank <- 17
  DF_T17 <- rbind(T17,DF_T17) 
}

DF_T18 <- data.frame()	

files <- list.files(path="/Users/fionabeaty/Dropbox/Fiona/School/Chapter 2 - Local Adaptation/Ch2 Data Analysis/iButton_meso/Tank18", pattern="*.csv", full.names=TRUE, recursive=FALSE)	

for (i in 1:length(files)) {	
  T18 <- read.csv(as.character(files[i]),header = TRUE, sep = ",", stringsAsFactors = FALSE, skip = 0)	
  T18 <- data.frame(T18)	
  T18$Date <- as.Date(paste(T18$Date.Time), "%Y-%m-%d")
  T18$Tank <- 18
  DF_T18 <- rbind(T18,DF_T18) 
}

DF_T19 <- data.frame()	

files <- list.files(path="/Users/fionabeaty/Dropbox/Fiona/School/Chapter 2 - Local Adaptation/Ch2 Data Analysis/iButton_meso/Tank19", pattern="*.csv", full.names=TRUE, recursive=FALSE)	

for (i in 1:length(files)) {	
  T19 <- read.csv(as.character(files[i]),header = TRUE, sep = ",", stringsAsFactors = FALSE, skip = 0)	
  T19 <- data.frame(T19)	
  T19$Date <- as.Date(paste(T19$Date.Time), "%Y-%m-%d")
  T19$Tank <- 19
  DF_T19 <- rbind(T19,DF_T19) 
}

DF_T20 <- data.frame()	

files <- list.files(path="/Users/fionabeaty/Dropbox/Fiona/School/Chapter 2 - Local Adaptation/Ch2 Data Analysis/iButton_meso/Tank20", pattern="*.csv", full.names=TRUE, recursive=FALSE)	

for (i in 1:length(files)) {	
  T20 <- read.csv(as.character(files[i]),header = TRUE, sep = ",", stringsAsFactors = FALSE, skip = 0)	
  T20 <- data.frame(T20)	
  T20$Date <- as.Date(paste(T20$Date.Time), "%Y-%m-%d")
  T20$Tank <- 20
  DF_T20 <- rbind(T20,DF_T20) 
}

DF_T21 <- data.frame()	

files <- list.files(path="/Users/fionabeaty/Dropbox/Fiona/School/Chapter 2 - Local Adaptation/Ch2 Data Analysis/iButton_meso/Tank21", pattern="*.csv", full.names=TRUE, recursive=FALSE)	

for (i in 1:length(files)) {	
  T21 <- read.csv(as.character(files[i]),header = TRUE, sep = ",", stringsAsFactors = FALSE, skip = 0)	
  T21 <- data.frame(T21)	
  T21$Date <- as.Date(paste(T21$Date.Time), "%Y-%m-%d")
  T21$Tank <- 21
  DF_T21 <- rbind(T21,DF_T21) 
}

DF_T22 <- data.frame()	

files <- list.files(path="/Users/fionabeaty/Dropbox/Fiona/School/Chapter 2 - Local Adaptation/Ch2 Data Analysis/iButton_meso/Tank22", pattern="*.csv", full.names=TRUE, recursive=FALSE)	

for (i in 1:length(files)) {	
  T22 <- read.csv(as.character(files[i]),header = TRUE, sep = ",", stringsAsFactors = FALSE, skip = 0)	
  T22 <- data.frame(T22)	
  T22$Date <- as.Date(paste(T22$Date.Time), "%Y-%m-%d")
  T22$Tank <- 22
  DF_T22 <- rbind(T22,DF_T22) 
}

DF_T23 <- data.frame()	

files <- list.files(path="/Users/fionabeaty/Dropbox/Fiona/School/Chapter 2 - Local Adaptation/Ch2 Data Analysis/iButton_meso/Tank23", pattern="*.csv", full.names=TRUE, recursive=FALSE)	

for (i in 1:length(files)) {	
  T23 <- read.csv(as.character(files[i]),header = TRUE, sep = ",", stringsAsFactors = FALSE, skip = 0)	
  T23 <- data.frame(T23)	
  T23$Date <- as.Date(paste(T23$Date.Time), "%Y-%m-%d")
  T23$Tank <- 23
  DF_T23 <- rbind(T23,DF_T23) 
}

DF_T24 <- data.frame()	

files <- list.files(path="/Users/fionabeaty/Dropbox/Fiona/School/Chapter 2 - Local Adaptation/Ch2 Data Analysis/iButton_meso/Tank24", pattern="*.csv", full.names=TRUE, recursive=FALSE)	

for (i in 1:length(files)) {	
  T24 <- read.csv(as.character(files[i]),header = TRUE, sep = ",", stringsAsFactors = FALSE, skip = 0)	
  T24 <- data.frame(T24)	
  T24$Date <- as.Date(paste(T24$Date.Time), "%Y-%m-%d")
  T24$Tank <- 24
  DF_T24 <- rbind(T24,DF_T24) 
}


#Remove unneeded objects----
rm(T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, T20, T21, T22, T23, T24, files, i)
#Combine dataframes into one----
all<- do.call("rbind", list(DF_T1, DF_T2, DF_T3, DF_T4, DF_T5, DF_T6, DF_T7, DF_T8, DF_T9,
                              DF_T10, DF_T11, DF_T12, DF_T13, DF_T14, DF_T15, DF_T16, DF_T17, 
                              DF_T18, DF_T19, DF_T20, DF_T21, DF_T22, DF_T23, DF_T24))

rm(DF_T1, DF_T2, DF_T3, DF_T4, DF_T5, DF_T6, DF_T7, DF_T8, DF_T9,
   DF_T10, DF_T11, DF_T12, DF_T13, DF_T14, DF_T15, DF_T16, DF_T17, 
   DF_T18, DF_T19, DF_T20, DF_T21, DF_T22, DF_T23, DF_T24)

#Clean dataframe (fix dates, currently year is increasing when day should), make tank a factor, parse date,
#filter out dates that you removed & measured the iButtons & experiment date range
#Experimental range (i.e. from when treatments were reached in each tank - end) was July x - Sept 2nd
#Also create a column with the treatments for each tank
#Remove tanks where all snails died at the start due to temp malfunction: 3, 5, 6,
#Also classify Tank 16 (which was supposed to be a 12) as a 15 degree treatment because you could never really bring that tank's temp down due to equipment issues
#Also remove tanks 19, 20 & 22 because their temp data don't match the daily YSI measurements
#Also remove 2018-08-09 from Tank 19 and 2018-08-06 from Tank 9 because of temp outliers that don't match YSI data 

all_cleaned <- all %>%
  select(Date, Value, Tank) %>% 
  mutate(Date = str_sub(Date, 3),
         Date = as.Date(Date, "%d-%m-%y"),
         Tank = as.factor(Tank),
         Year = year(Date),
         Month = month(Date),
         Day = day(Date)) %>% 
  filter(Date != "2018-07-31" & Date != "2018-08-13" & Date!= "2018-08-24" & Date != "2018-09-18") %>% 
  filter(Date > "2018-07-23" & Date <= "2018-09-02",
         Tank != 3 & Tank != 5 & Tank != 6 & Tank != 19 & Tank != 20 & Tank != 22) %>% 
  filter(!((Tank == 19 & Date == "2018-08-09" & Value == 30) | (Tank == 19 & Date == "2018-08-09" & Value == 27.0) | 
             (Tank == 9 & Date == "2018-08-06" & Value == 30.5))) %>% 
  mutate(Treat = as.factor(ifelse(Tank == 1, "19A",
                                  ifelse(Tank == 2, "22A",
                                        ifelse(Tank == 4, "15A", 
                                               ifelse(Tank == 7, "15A",
                                                      ifelse(Tank == 8, "22A",
                                                             ifelse(Tank == 9, "12A",
                                                                    ifelse(Tank == 10, "19A",
                                                                          ifelse(Tank == 11, "15A",
                                                                                ifelse(Tank == 12, "12A", 
                                                                                      ifelse(Tank == 13, "19A",
                                                                                            ifelse(Tank == 14, "15A",
                                                                                                  ifelse(Tank == 15, "22A", 
                                                                                                        ifelse(Tank == 16, "15A",
                                                                                                               ifelse(Tank == 17, "15L",
                                                                                                                     ifelse(Tank == 18, "22L",
                                                                                                                                  ifelse(Tank == 21, "15L",
                                                                                                                                        ifelse(Tank == 23, "22L", 
                                                                                                                                             ifelse(Tank == 24, "15L", 
                                                                                                                                                  NA))))))))))))))))))))



#Now create new dataframe with the summarized temps/treatment----
#Remove the tanks that jumped at the start of the experiment
all_treat <- all_cleaned %>% 
  group_by(Treat, Year, Month, Day) %>% 
  summarize(avgtemp = mean(Value), sdtemp = sd(Value)) %>% 
  mutate(Date = ymd(paste(Year, Month, Day))) %>% 
  ungroup()

#Now visualize data across treatments----
#Set theme aesthetics for font sizes
treat_meso_iButton <- ggplot(all_treat, aes(Date, avgtemp, fill = Treat)) + 
  geom_line (aes(colour = Treat), size = 1) +
  scale_color_viridis(discrete = TRUE, option = "D") +
  scale_fill_viridis(discrete = TRUE) +
  geom_ribbon(aes(ymin = avgtemp-sdtemp, ymax = avgtemp+sdtemp), alpha = 0.2) +
  labs(x = "Date, 2018", y = "Temp (Celsius)") +
  theme_cowplot(16)

ggsave(treat_meso_iButton, file = "plots/iButtons/treat_meso_iButton.pdf", height = 5, width = 9, dpi = 300)

#Now create a second set of plots for just the Aug 1 - Sept 2, which you'll use to calculate the temp averages----
all_treat_subset <- all_treat %>% 
  filter(Date >= "2018-08-01")

treat_subset<- ggplot(all_treat_subset, aes(Date, avgtemp, fill = Treat)) + 
  geom_line (aes(colour = Treat), size = 1) +
  scale_color_viridis(discrete = TRUE, option = "D") +
  scale_fill_viridis(discrete = TRUE) +
  geom_ribbon(aes(ymin = avgtemp-sdtemp, ymax = avgtemp+sdtemp), alpha = 0.2) +
  labs(x = "Date, 2018", y = "Temp (Celsius)") +
  theme_cowplot(16)

ggsave(treat_subset, file = "plots/iButtons/treat_subset.pdf", height = 5, width = 9, dpi = 300)

#Now calculate the average & sd per treatment for you to put into the table
treat_values <- all_treat %>% 
  group_by(Treat) %>% 
  summarize(avgtemp = mean(avgtemp), sdtemp = sd(sdtemp)) %>% 
  ungroup()

treat_subset <- all_treat_subset %>% 
  group_by(Treat) %>% 
  summarize(avgtemp = mean(avgtemp), sdtemp = sd(sdtemp)) %>% 
  ungroup()

#Remove all unneeded variables


#Remove all objects----
rm(all, all_cleaned, all_cleaned_1, all_treat, all_treat_subset, my_theme, treat_meso,
   treat_subset, treat_values, Date_revised, dates_excl, dates_exl)
