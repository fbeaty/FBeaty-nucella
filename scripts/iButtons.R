#iButtons script, 
#The cleaning code is based on Cass & my code (see RT_2 folder for more elaborate code, I simplified it here)
#The analysis code is based on Alyssa's code (see slack & scripts folder in this project, jan 25 2022)
#Last updated by FB Jan 2022

#Load packages----
pkgs <- c("plyr", "dplyr", "lattice", "readr", "MuMIn", "tidyr", "lubridate")
lapply(pkgs, library, character.only = TRUE)
rm(pkgs)

#Upload & clean iButton data from data folder----
#Load & clean ibutton data from Kwak and Pruth
#Kwak first timeseries (init-mid) using Cass's code (see Slack convo)
#The Kwak and Pruth data comes from the cleaned folders (I had to change the years for some of the ibuttons
#which were programmed to start at 2000). For Cedar and Heron the unaltered files are in the 'Original> Combined>Heron or Cedar folders.

DF_K1 <- data.frame()	
files <- list.files(path="data/ibutton data/Cleaned/Combined/Kwak_1", pattern="*.csv", full.names=TRUE, recursive=FALSE)	

for (i in 1:length(files)) {	
  Kwak_1<- read.csv(as.character(files[i]),header = TRUE, sep = ",", stringsAsFactors = FALSE, skip = 0)	
  Kwak_1$Date <- as.Date(paste(Kwak_1$Date.Time), formate = "%Y-%m-%d")
  Kwak_1 <- data.frame(Kwak_1)	
  DF_K1 <- rbind(Kwak_1,DF_K1) # add it to your list	
}	

#Now add a line that indicates it's Kwak
DF_K1<- DF_K1 %>% 
  mutate(SP = "Kwak") %>% 
  setNames(., c("DT" , "Time" , "Value" , "Temp" , "Date" , "SP"))

#delete any days when the ibuttons were recording in the lab. For Kwak remove first 6 rows, 06-04, 06-05, 
DF_K1 <- DF_K1 %>% 
  filter(Date != "2019-03-20") %>% 
  filter(Date != "2019-03-21") %>% 
  filter(Date != "2019-06-04") %>% 
  filter(Date != "2019-06-05")

#Now do the same code with the Pruth data sets from the first time point
DF_P1 <- data.frame()	
files_P1 <- list.files(path="data/ibutton data/Cleaned/Combined/Pruth_1", pattern="*.csv", full.names=TRUE, recursive=FALSE)	

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
files_H1 <- list.files(path="data/ibutton data/Original/Combined/Heron", pattern="*.csv", full.names=TRUE, recursive=FALSE)	

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
#2019-04-10 and anything after 2019-08-12 
dates_excl <- c("2019-04-04", "2019-04-05", "2019-04-06", "2019-04-07", "2019-04-08", "2019-04-09", 
                "2019-08-13", "2019-08-14", "2019-08-15", "2019-08-16", "2019-08-17", "2019-08-18",
                "2019-08-19", "2019-08-20", "2019-08-21", "2019-08-22")

DF_H1 <- DF_H1 %>% 
  filter(Date != dates_excl)

#Now with Cedar! 
DF_C1 <- data.frame()	

files_C1 <- list.files(path="data/ibutton data/Original/Combined/Cedar", pattern="*.csv", full.names=TRUE, recursive=FALSE)	

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
  filter(Date != dates_excl)

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

all_df <- rbind(nanaimo_df, calvert_df)

#Export this to csv
#write.csv(all_df, "cleaned_iButtons_2019.csv")

