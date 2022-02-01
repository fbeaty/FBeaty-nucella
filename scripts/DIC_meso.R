#This script is used to analyze & visualize the carbonate data from mesocosm experiment 2018
#Last updated Jan 2022

#Load packages----
pkgs <- c("tidyverse", "viridis", "ggpubr")
lapply(pkgs, library, character.only = TRUE)
rm(pkgs)

#Read in data----
dic <- read_csv("data/meso_YSI_DIC/DIC_collated.csv")

#Convert stage into a factor
dic <- dic %>% 
  mutate(stage = factor(stage, levels = c("init", "mid1", "mid2", "final", "final_wiley")),
         treatment = as.factor(treatment))

#Visualize data to figure out if you want to remove any stages based on outlier <- I think keep them all in for now
dic_stages_pCO2 <- ggplot(dic, aes(treatment, pCO2, fill = stage)) +
  geom_boxplot(alpha = 0.8, varwidth = TRUE) + 
  scale_fill_brewer() + 
  ylim(340, 920) +
  theme_bw() + labs(x = "Treatment", y = "pCO2") +
  theme(panel.border = element_blank(), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))

#Summarize the dataset grouped by treatment, stage & treatment, and split into the temp & factorial studies----
dic_sum <- dic %>% 
  group_by(treatment) %>% 
  summarize(avgpCO2 = mean(pCO2, na.rm = TRUE), sdpCO2 = sd(pCO2, na.rm = TRUE),
            avgomega = mean(omega_arag,na.rm = TRUE), sdomega = sd(omega_arag, na.rm = TRUE),
            avgTA = mean(TA,na.rm = TRUE), sdTA = sd(TA, na.rm = TRUE),
            avgpH = mean(pH,na.rm = TRUE), sdpH = sd(pH, na.rm = TRUE)) %>% 
  ungroup()

dic_temp <- dis_sum %>% 
  filter(treatment == "12A" | treatment == "15A" | treatment == "19A" | treatment == "22A") %>% 

dic_fact <- dic %>% 
  filter(treatment == "15A" | treatment == "15L" | treatment == "22A" | treatment == "22L")

#Visualize data ----
my_theme <- theme(axis.title.x = element_text(size = 20), axis.text.x = element_text(size = 18),
                  axis.title.y = element_text(size = 20), legend.text = element_text(size = 20))


dic_pCO2_stage <- ggplot(dic, aes(treatment, pCO2), fill = stage) +
  geom_boxplot(alpha = 0.8, varwidth = TRUE) + 
  scale_fill_viridis(discrete = TRUE) +
  theme_bw() + labs(x = "Treatment", y = "pCO2") +
  theme(panel.border = element_blank(), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), axis.line = element_line(colour = "black")) +
  my_theme

dic_pCO2 <- ggplot(dic_sum, aes(treatment, avgpCO2), fill = treatment) +
  geom_point(aes(color = treatment, size = 2)) + 
  scale_color_viridis(discrete = TRUE, option = "D") +
  scale_fill_viridis(discrete = TRUE) +
  geom_errorbar(aes(ymin = avgpCO2-sdpCO2, ymax = avgpCO2+sdpCO2, color = treatment), width = 0.2) +
  theme_bw() + labs(x = "Treatment", y = "pCO2") +
  theme(panel.border = element_blank(), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"),
        legend.position = "none") +
  my_theme

dic_TA <- ggplot(dic_sum, aes(treatment, avgTA), fill = treatment) +
  geom_point(aes(color = treatment, size = 2)) + 
  scale_color_viridis(discrete = TRUE, option = "D") +
  scale_fill_viridis(discrete = TRUE) +
  geom_errorbar(aes(ymin = avgTA-sdTA, ymax = avgTA+sdTA, color = treatment), width = 0.2) +
  theme_bw() + labs(x = "Treatment", y = "Total Alkalinity") +
  theme(panel.border = element_blank(), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"),
        legend.position = "none") +
  my_theme

dic_omega <- ggplot(dic_sum, aes(treatment, avgomega), fill = treatment) +
  geom_point(aes(color = treatment, size = 2)) + 
  scale_color_viridis(discrete = TRUE, option = "D") +
  scale_fill_viridis(discrete = TRUE) +
  geom_errorbar(aes(ymin = avgomega-sdomega, ymax = avgomega+sdomega, color = treatment), width = 0.2) +
  theme_bw() + labs(x = "Treatment", y = "Omega Arag") +
  theme(panel.border = element_blank(), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"),
        legend.position = "none") +
  my_theme

ggsave(dic_pCO2, file = "plots/YSI_DIC/dic_pCO2.pdf", height = 5, width = 9, dpi = 300)
ggsave(dic_TA, file = "plots/YSI_DIC/dic_TA.pdf", height = 5, width = 9, dpi = 300)
ggsave(dic_omega, file = "plots/YSI_DIC/dic_omega.pdf", height = 5, width = 9, dpi = 300)
ggsave(dic_stages_pCO2, file = "plots/YSI_DIC/dic_stages_pCO2.pdf", height = 5, width = 9, dpi = 300)

