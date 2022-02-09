#Script to analyze & visualize the snail growth RVs (length, shell thickness etc) from the 2019 Reciprocal Transplant

#Load packages----
pkgs <- c("tidyverse", "viridis", "lubridate", "car", "visreg", "cowplot")
lapply(pkgs, library, character.only = TRUE)
rm(pkgs)

#Load csv and clean it----
RV_base <- read.csv("data/snail_RVs/RT_2_final.csv")

#Convert to dates & factors, and calulate tissue weight based upon the TW & SW columns
#Note that there are no 'SG' for the initial time periods, so these values are NA
#Also, due to equipment failure we were unable to measure SW in the 'Mid' timepoint, so those values are also NA

RV_clean <- RV_base %>% 
  mutate(Date = as.Date(Date, format = "%m-%d-%Y"),
         Stage = factor(Stage, levels = c("Init", "Mid", "Final")),
         SR = as.factor(SR),
         SP = as.factor(SP),
         OR = as.factor(OR),
         OS = as.factor(OS),
         Block = as.factor(Block),
         TiW = TW-SW) %>% 
  select(Date, Stage, SR, SP, OR, OS, Block, ID, L, T, TW, SW, TiW, SG, DIED, OSTRINA)

#Calculate & visualize the average & SD of growth metrics in the 3 time periods ----
RV_sum_block <- RV_clean %>% 
  group_by(Stage, OR, OS, SR, SP, Block) %>% 
  summarize(meanL = mean(L, na.rm = TRUE), sdL = sd(L, na.rm = TRUE), nL = n()) %>% 
  ungroup()

RV_sum_OR <- RV_sum_block %>% 
  group_by(Stage, OR, SR, SP) %>% 
  summarize(meanL_OR = mean(meanL, na.rm = TRUE), sdL_OR = sd(meanL, na.rm = TRUE), nL_OR = n()) %>% 
  ungroup()

RV_sum_cal <- RV_sum_OR %>% 
  filter(OR == "Calvert")

RV_sum_nan <- RV_sum_OR %>% 
  filter(OR == "Nanaimo")

length_cal_stage <- ggplot(RV_sum_cal, aes(Stage, meanL_OR, group = SP, colour = SP)) + 
  geom_point(size = 3, position=position_dodge(0.3)) +
  geom_line(size = 0.8, position = position_dodge(0.3), alpha = 0.5) +
  geom_errorbar(aes(ymin=meanL_OR-sdL_OR, ymax=meanL_OR+sdL_OR), width=.2, size = 0.5,
                position=position_dodge(0.3)) +
  ylim(23.5, 39) +
  scale_colour_manual(values = c("coral", "coral3", "skyblue", "skyblue3")) +
  labs(x = "Stage, Calvert", y = "Mean shell length (mm)") +
  theme_cowplot(16)

length_nan_stage <- ggplot(RV_sum_nan, aes(Stage, meanL_OR, group = SP, colour = SP)) + 
  geom_point(size = 3, position=position_dodge(0.3)) +
  geom_line(size = 0.8, position = position_dodge(0.3), alpha = 0.5) +
  geom_errorbar(aes(ymin=meanL_OR-sdL_OR, ymax=meanL_OR+sdL_OR), width=.2, size = 0.5,
                position=position_dodge(0.3)) +
  scale_colour_manual(values = c("coral", "coral3", "skyblue", "skyblue3")) +
  ylim(23.5, 39) +
  labs(x = "Stage, Nanaimo", y = "Mean shell length (mm)") +
  theme_cowplot(16)

length_combined_stage <- plot_grid(length_cal_stage + theme(legend.position = "none"), length_nan_stage + theme(axis.text.y = element_blank(),
                                                   axis.ticks.y = element_blank(),
                                                   axis.title.y = element_blank()),
          axis = "b")

ggsave(length_cal_stage, file = "plots/snails/RT/length_cal_stage.pdf", height = 4, width = 8, dpi = 300)
ggsave(length_nan_stage, file = "plots/snails/RT/length_nan_stage.pdf", height = 4, width = 8, dpi = 300)
ggsave(length_combined_stage, file = "plots/snails/RT/length_combined_stage.pdf", height = 4, width = 8, dpi = 300)


#Test whether there's a significant effect of time (or an interesting one)----
anova_nan <- aov(meanL_OR ~ SP + Stage, data = RV_sum_nan)
Anova(anova_nan, type="II")
visreg(anova_nan)



#Calculate & visualize the average & SD of growth metrics clumped by SR & OR----
#First have to calculate the difference in growth metrics between final & initial
RV_diff <- RV_sum_block %>% 
  subset(Stage != "Mid") %>% 
  arrange(OS, SP, Block, Stage) %>% 
  group_by(Block) %>% 
  mutate(diff_meanl = meanL - lag(meanL, default= meanL[1])) %>% 
  subset(Stage == "Final") %>% 
  ungroup()

#Then summarize the mean difference grouped by OR & SR: there should be n = 16 for each grouping, and only 4 groupings
RV_sum_OR_SR <- RV_diff %>% 
  group_by(OR, SR) %>% 
  summarize(meanL_OR_SR = mean(diff_meanl, na.rm = TRUE), sdL_OR_SR = sd(diff_meanl, na.rm = TRUE), 
            nL_OR_SR = n()) %>% 
  ungroup()

length_OR_OS_box <- ggplot(RV_diff, aes(OR, diff_meanl, fill = SR, color = SR)) + 
  geom_boxplot(color = "grey24", varwidth = TRUE) +
  scale_fill_manual(values = c("coral", "skyblue")) +
  labs(x = "Outplant Region", y = "Mean shell length (mm)") +
  theme_cowplot(16)

length_OR_OS_points <- ggplot(RV_sum_OR_SR, aes(OR, meanL_OR_SR, group = SR, colour = SR)) + 
  geom_point(size = 3, position=position_dodge(0.3)) +
  geom_line(size = 0.8, position = position_dodge(0.3), alpha = 0.5) +
  geom_errorbar(aes(ymin=meanL_OR_SR-sdL_OR_SR, ymax=meanL_OR_SR+sdL_OR_SR), width=.2, size = 0.5,
                position=position_dodge(0.3)) +
  scale_colour_manual(values = c("coral", "skyblue")) +
  labs(x = "Outplant region", y = "Mean shell length (mm)") +
  theme_cowplot(16)

ggsave(length_OR_OS_box, file = "plots/snails/RT/length_OR_OS_box.pdf", height = 4, width = 8, dpi = 300)
ggsave(length_OR_OS_points, file = "plots/snails/RT/length_OR_OS_points.pdf", height = 4, width = 8, dpi = 300)

