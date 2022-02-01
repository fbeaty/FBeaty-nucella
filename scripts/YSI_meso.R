#YSI script for mesocosm experiment to visualize the temp, salinity, pH data


#Load packages----
pkgs <- c("dplyr", "tidyverse", "tidyr", "viridis", "lubridate", "ggplot2", "purrr", "stringr")
lapply(pkgs, library, character.only = TRUE)
rm(pkgs)

#Load & clean csv from data/YSI_meso folder----
ysi <- read.csv("data/meso_YSI_DIC/Water parameters_final_combined_Dec 12.csv")

#Clean database: remove unncessary columns & rows (any rows w/ NAs), select for July 18-Sept 2
#reassign Tank 16 as 15A

ysi <- ysi %>% 
  mutate(year = "2018",
         Date = paste(year, Date, sep = "-"),
         Treat = ifelse(Tank == 16, "15A", Treat),
         Treat = as.factor(Treat)) %>% 
  filter(Date > "2018-07-18" & Date <= "2018-09-02") %>% 
  select(Tank, Treat, Date, Temperature, pH, Salinity)

#Now create new dataframe with the summarized temps/treatment----
#Remove the tanks that jumped at the start of the experiment
ysi_sum <- ysi %>% 
  group_by(Treat, Date) %>% 
  summarize(avgtemp = mean(Temperature), sdtemp = sd(Temperature),
            avgsal = mean(Salinity), sdsal = sd(Salinity),
            avgpH = mean(pH, na.rm = TRUE), sdpH = sd(pH, na.rm = TRUE)) %>% 
  mutate(Date = ymd(Date)) %>% 
  ungroup()

#Now visualize data across treatments----
#Set theme aesthetics for font sizes
my_theme <- theme(axis.title.x = element_text(size = 20), axis.text.x = element_text(size = 18),
                  axis.title.y = element_text(size = 20), axis.text.y = element_text(size = 18),
                  legend.text = element_text(size = 20))

temp_meso_ysi <- ggplot(ysi_sum, aes(Date, avgtemp, group = Treat)) + 
  geom_line (aes(colour = Treat), size = 1) +
  geom_ribbon(aes(ymin = avgtemp-sdtemp, ymax = avgtemp+sdtemp, fill = Treat), alpha = 0.2) +
  scale_color_viridis(discrete = TRUE, option = "D") +
  scale_fill_viridis(discrete = TRUE) +
  theme_bw() + labs(x = "Date, 2018", y = "Daily temp (°C)") +
  theme(panel.border = element_blank(), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), axis.line = element_line(colour = "black")) +
  my_theme

sal_meso_ysi <- ggplot(ysi_sum, aes(Date, avgsal, group = Treat)) + 
  geom_line (aes(colour = Treat), size = 1) +
  geom_ribbon(aes(ymin = avgsal-sdsal, ymax = avgsal+sdsal, fill = Treat), alpha = 0.2) +
  scale_color_viridis(discrete = TRUE, option = "D") +
  scale_fill_viridis(discrete = TRUE) +
  theme_bw() + labs(x = "Date, 2018", y = "Daily salinity (ppt)") +
  theme(panel.border = element_blank(), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), axis.line = element_line(colour = "black")) +
  my_theme

pH_meso_ysi <- ggplot(ysi_sum, aes(Date, avgpH, group = Treat)) + 
  geom_line (aes(colour = Treat), size = 1) +
  scale_color_viridis(discrete = TRUE, option = "D") +
  scale_fill_viridis(discrete = TRUE) +
  geom_ribbon(aes(ymin = avgpH-sdpH, ymax = avgpH+sdpH, fill = Treat), alpha = 0.2) +
  theme_bw() + labs(x = "Date, 2018", y = "Daily pH") +
  theme(panel.border = element_blank(), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), axis.line = element_line(colour = "black")) +
  my_theme

ggsave(temp_meso_ysi, file = "plots/YSI_DIC/temp_meso_ysi.pdf", height = 5, width = 9, dpi = 300)
ggsave(sal_meso_ysi, file = "plots/YSI_DIC/sal_meso_ysi.pdf", height = 5, width = 9, dpi = 300)
ggsave(pH_meso_ysi, file = "plots/YSI_DIC/pH_meso_ysi.pdf", height = 5, width = 9, dpi = 300)


#Now create a second set of plots for just the Aug 1 - Sept 2----
ysi_sum_subset <- ysi_sum %>% 
  filter(Date >= "2018-08-01")

temp_meso_ysi_subset <- ggplot(ysi_sum_subset, aes(Date, avgtemp, group = Treat)) + 
  geom_line (aes(colour = Treat), size = 1) +
  geom_ribbon(aes(ymin = avgtemp-sdtemp, ymax = avgtemp+sdtemp, fill = Treat), alpha = 0.2) +
  scale_color_viridis(discrete = TRUE, option = "D") +
  scale_fill_viridis(discrete = TRUE) +
  theme_bw() + labs(x = "Date, 2018", y = "Daily temp (°C)") +
  theme(panel.border = element_blank(), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), axis.line = element_line(colour = "black")) +
  my_theme

sal_meso_ysi_subset <- ggplot(ysi_sum_subset, aes(Date, avgsal, group = Treat)) + 
  geom_line (aes(colour = Treat), size = 1) +
  geom_ribbon(aes(ymin = avgsal-sdsal, ymax = avgsal+sdsal, fill = Treat), alpha = 0.2) +
  scale_color_viridis(discrete = TRUE, option = "D") +
  scale_fill_viridis(discrete = TRUE) +
  theme_bw() + labs(x = "Date, 2018", y = "Daily salinity (ppt)") +
  theme(panel.border = element_blank(), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), axis.line = element_line(colour = "black")) +
  my_theme

pH_meso_ysi_subset <- ggplot(ysi_sum_subset, aes(Date, avgpH, group = Treat)) + 
  geom_line (aes(colour = Treat), size = 1) +
  geom_ribbon(aes(ymin = avgpH-sdpH, ymax = avgpH+sdpH, fill = Treat), alpha = 0.2) +
  scale_color_viridis(discrete = TRUE, option = "D") +
  scale_fill_viridis(discrete = TRUE) +
  theme_bw() + labs(x = "Date, 2018", y = "Daily pH") +
  theme(panel.border = element_blank(), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), axis.line = element_line(colour = "black")) +
  my_theme

ggsave(temp_meso_ysi_subset, file = "plots/YSI_DIC/temp_meso_ysi_subset.pdf", height = 5, width = 9, dpi = 300)
ggsave(sal_meso_ysi_subset, file = "plots/YSI_DIC/sal_meso_ysi_subset.pdf", height = 5, width = 9, dpi = 300)
ggsave(pH_meso_ysi_subset, file = "plots/YSI_DIC/pH_meso_ysi_subset.pdf", height = 5, width = 9, dpi = 300)


#Now calculate the average & sd per treatment for you to put into the table----
ysi_sum_table <- ysi_sum %>% 
  group_by(Treat) %>% 
  summarize(avgtemp = mean(avgtemp), sdtemp = sd(sdtemp),
            avgsal = mean(avgsal), sdsal = sd(sdsal),
            avgpH = mean(avgpH, na.rm = TRUE), sdpH = mean(sdpH, na.rm = TRUE)) %>% 
  ungroup()

ysi_sum_subset_table <- ysi_sum_subset %>% 
  group_by(Treat) %>% 
  summarize(avgtemp = mean(avgtemp), sdtemp = sd(sdtemp),
            avgsal = mean(avgsal), sdsal = sd(sdsal),
            avgpH = mean(avgpH, na.rm = TRUE), sdpH = mean(sdpH, na.rm = TRUE)) %>% 
  ungroup()


#Remove all unneeded variables----
rm(ysi, ysi_sum, ysi_sum_subset, ysi_sum_subset_table, ysi_sum_table, pH_meso_ysi,
   pH_meso_ysi_subset, sal_meso_ysi, sal_meso_ysi_subset, temp_meso_ysi, 
   temp_meso_ysi_subset, temp_YSI, my_theme)
