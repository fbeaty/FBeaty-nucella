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
files <- list.files(path="/Users/fionabeaty/Dropbox/Fiona/School/Chapter 2 - Local Adaptation/Ch2 Data Analysis/ibutton data/Cleaned/Combined/Kwak_1", pattern="*.csv", full.names=TRUE, recursive=FALSE)	

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
files_P1 <- list.files(path="/Users/fionabeaty/Dropbox/Fiona/School/Chapter 2 - Local Adaptation/Ch2 Data Analysis/ibutton data/Cleaned/Combined/Pruth_1", pattern="*.csv", full.names=TRUE, recursive=FALSE)	

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
files_H1 <- list.files(path="/Users/fionabeaty/Dropbox/Fiona/School/Chapter 2 - Local Adaptation/Ch2 Data Analysis/ibutton data/Original/Combined/Heron", pattern="*.csv", full.names=TRUE, recursive=FALSE)	

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

files_C1 <- list.files(path="/Users/fionabeaty/Dropbox/Fiona/School/Chapter 2 - Local Adaptation/Ch2 Data Analysis/ibutton data/Original/Combined/Cedar", pattern="*.csv", full.names=TRUE, recursive=FALSE)	

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

#Merge into one cdataframe and add tidal height column based on RT_2_V1_Aug_KM excel sheet
all_df <- rbind(nanaimo_df, calvert_df) %>% 
  mutate(TideHeight = ifelse(SP == "Cedar", 1.15, 
                             ifelse(SP == "Heron", 0.925,
                                    ifelse(SP == "Kwak", 1.30,
                                           ifelse(SP == "Pruth", 1.65, ""))))) %>% 
  mutate(date.time = ymd_hms(paste(Date, Time)))


#Export this to csv so that you can add in a column with the submerged/air manually 
write.csv(all_df, "data/iButtons/cleaned_iButtons_2019.csv")

#Remove unnecessary objects
rm(calvert_df, Cedar_1, DF_C1, DF_H1, DF_K1, DF_P1, Heron_1, Kwak_1, nanaimo_df, Pruth_1,
   dates_excl, files, files_C1, files_H1, files_P1, i)

#Manually assign 0 or 1 based on whether the marker is emmersed or not based on tide data from----
#insert website here
#The code in this section is informed by Alyssa's code
#For the below code to run you need the column labels to read:
#  date.time = col_character(),
#  Unit = col_character(),
#  Value = col_double(),
#  `in.water` = col_integer()

#*Output of below code*
#  Three files with these variables:
#  avgair= the average air temp (not suggested to be used for analysis - just as a reference)
#  maxair=the absolute max air temp (could be used for analysis but generally this could be influenced by outliers)
#  sdair=the standard deviation for variation in air temperature experienced
#  air90th= the 90th quantile of air temperature experienced
#  air99th=the 99th quantile of air temperature experienced (a better max temperature estimate)
#  air10th=the 10th quantile of air temperature experienced

#  avgwater= the average water temp (not suggested to be used for analysis - just as a reference)
#  maxwater=the absolute max water temp (could be used for analysis but generally this could be influenced by outliers)
#  sdwater=the standard deviation for variation in water temperature experienced
#  water90th= the 90th quantile of water temperature experienced
#  water99th=the 99th quantile of water temperature experienced (a better max temperature estimate)
#  water10th=the 10th quantile of water temperature experienced
#load in new dataframe with tide levels assigned

#The code is slightly unnecessary, given that I have already merged my csvs into one file,
#but I'm going to see whether it will still work on my one csv! As it's easier than re-writing it all
#for just one file

#remove all lists
rm(list=ls(all=TRUE)) 

#reads all the files in a single folder that have a .csv extension (change so that it goes to your files)
fileNames <- Sys.glob("data/iButtons/cleaned_iButtons_2019.csv")

#creates a list of the file names so that you can label them appropriately below (change so that it goes to your files)
filename<-list.files(path="data/iButtons", pattern="csv")

##create empty vectors
f.air=NULL
f.water=NULL
airtemp=NULL
watertemp=NULL


# we'll be leaving the original files unaltered but using their data to create new files:
for(i in 1:37){
  sample <- read.csv(fileNames[i],
                     header = TRUE)
  
  #seperate file by water and air
  f.air<-sample %>% filter(in.water=="1")
  f.water<-sample %>% filter(in.water=="0")
  
  ## creating columns to label sites and replicates based off our file name extension: 'BSP.1 iButton Data.csv', or "site.plot iButton Data.csv"
  
  #a column the length of water or air data that names which file the data came from
  fileair<-rep(filename[i],length(f.air[,1]))
  filewater<-rep(filename[i],length(f.water[,1]))
  
  #pull out station (col1) and plot number (col2)
  sair1<-unlist(lapply(strsplit(as.character(fileair), " "),function(z) z[1]))
  swater1<-unlist(lapply(strsplit(as.character(filewater), " "),function(z) z[1]))
  
  sair2<-strsplit(as.character(sair1), "[.]")
  siteair<-do.call(rbind, sair2)
  
  swater2<-strsplit(as.character(swater1), "[.]")
  sitewater<-do.call(rbind, swater2)
  
  #combine site label with ibutton data
  f2.air<-cbind(siteair, f.air)  
  f2.water<-cbind(sitewater, f.water)
  
  #stack the sites in rows
  airtemp<-rbind(airtemp, f2.air)
  watertemp<-rbind(watertemp, f2.water)
}
