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
#I removed the dead ones from the subsequent analyses because I only measured growth on surviving snails (obviously)

RV_alive <- RV_base %>% 
  filter(!OS == "" & DIED == 0) %>% 
  mutate(Date = as.Date(Date, format = "%m-%d-%Y"),
         Stage = factor(Stage, levels = c("Init", "Mid", "Final")),
         SR = as.factor(SR),
         SP = as.factor(SP),
         OR = as.factor(OR),
         OS = as.factor(OS),
         Block = as.factor(Block),
         TiW = TW-SW) %>% 
  select(Date, Stage, SR, SP, OR, OS, Block, ID, L, Th, TW, SW, TiW, SG, DIED, OSTRINA)

#Calculate & visualize the average & SD of growth metrics in the 3 time periods ----
RV_sum_block <- RV_alive %>% 
  group_by(Stage, OR, OS, SR, SP, Block) %>% 
  summarize(meanL = mean(L, na.rm = TRUE), sdL = sd(L, na.rm = TRUE),
            meanTh = mean(Th, na.rm = TRUE), sdTh = sd(Th, na.rm = TRUE),
            meanSW = mean(SW, na.rm = TRUE), sdSW = sd(SW, na.rm = TRUE),
            meanTiW = mean(TiW, na.rm = TRUE), sdTiW = sd(TiW, na.rm = TRUE),
            meanSG = mean(SG, na.rm = TRUE), sdSG = sd(SG, na.rm = TRUE), n = n()) %>% 
  ungroup()

RV_sum_OR <- RV_sum_block %>% 
  group_by(Stage, OR, SR, SP) %>% 
  summarize(meanL_OR = mean(meanL, na.rm = TRUE), sdL_OR = sd(meanL, na.rm = TRUE),
            meanTh_OR = mean(meanTh, na.rm = TRUE), sdTh_OR = sd(meanTh, na.rm = TRUE),
            meanSW_OR = mean(meanSW, na.rm = TRUE), sdSW_OR = sd(meanSW, na.rm = TRUE),
            meanTiW_OR = mean(meanTiW, na.rm = TRUE), sdTiW_OR = sd(meanTiW, na.rm = TRUE),
            meanSG_OR = mean(meanSG, na.rm = TRUE), sdSG_OR = sd(meanSG, na.rm = TRUE), n = n()) %>% 
  ungroup()

RV_sum_cal <- RV_sum_OR %>% 
  filter(OR == "Calvert")

RV_sum_nan <- RV_sum_OR %>% 
  filter(OR == "Nanaimo")

#Visualize the sublethal RVs over time----
length_cal_stage <- ggplot(RV_sum_cal, aes(Stage, meanL_OR, group = SP, colour = SP)) + 
  geom_point(size = 3, position=position_dodge(0.3)) +
  geom_line(size = 0.8, position = position_dodge(0.3), alpha = 0.5) +
  geom_errorbar(aes(ymin=meanL_OR-sdL_OR, ymax=meanL_OR+sdL_OR), width=.2, size = 0.5,
                position=position_dodge(0.3)) +
  ylim(22.5, 39) +
  scale_colour_manual(values = c("coral", "coral3", "skyblue", "skyblue3")) +
  labs(x = "Stage, Calvert", y = "SL (mm)") +
  theme_cowplot(16)

length_nan_stage <- ggplot(RV_sum_nan, aes(Stage, meanL_OR, group = SP, colour = SP)) + 
  geom_point(size = 3, position=position_dodge(0.3)) +
  geom_line(size = 0.8, position = position_dodge(0.3), alpha = 0.5) +
  geom_errorbar(aes(ymin=meanL_OR-sdL_OR, ymax=meanL_OR+sdL_OR), width=.2, size = 0.5,
                position=position_dodge(0.3)) +
  scale_colour_manual(values = c("coral", "coral3", "skyblue", "skyblue3")) +
  ylim(22.5, 39) +
  labs(x = "Stage, Nanaimo", y = "SL (mm)") +
  theme_cowplot(16)

thick_cal_stage <- ggplot(RV_sum_cal, aes(Stage, meanTh_OR, group = SP, colour = SP)) + 
  geom_point(size = 3, position=position_dodge(0.3)) +
  geom_line(size = 0.8, position = position_dodge(0.3), alpha = 0.5) +
  geom_errorbar(aes(ymin=meanTh_OR-sdTh_OR, ymax=meanTh_OR+sdTh_OR), width=.2, size = 0.5,
                position=position_dodge(0.3)) +
  ylim(0.95, 4.0) +
  scale_colour_manual(values = c("coral", "coral3", "skyblue", "skyblue3")) +
  labs(x = "Stage, Calvert", y = "ST (mm)") +
  theme_cowplot(16)

thick_nan_stage <- ggplot(RV_sum_nan, aes(Stage, meanTh_OR, group = SP, colour = SP)) + 
  geom_point(size = 3, position=position_dodge(0.3)) +
  geom_line(size = 0.8, position = position_dodge(0.3), alpha = 0.5) +
  geom_errorbar(aes(ymin=meanTh_OR-sdTh_OR, ymax=meanTh_OR+sdTh_OR), width=.2, size = 0.5,
                position=position_dodge(0.3)) +
  ylim(0.95, 4.0) +
  scale_colour_manual(values = c("coral", "coral3", "skyblue", "skyblue3")) +
  labs(x = "Stage, Nanaimo", y = "ST (mm)") +
  theme_cowplot(16)

SW_cal_stage <- ggplot(RV_sum_cal, aes(Stage, meanSW_OR, group = SP, colour = SP)) + 
  geom_point(size = 3, position=position_dodge(0.3)) +
  geom_line(size = 0.8, position = position_dodge(0.3), alpha = 0.5) +
  geom_errorbar(aes(ymin=meanSW_OR-sdSW_OR, ymax=meanSW_OR+sdSW_OR), width=.2, size = 0.5,
                position=position_dodge(0.3)) +
  ylim(0.6, 5.1) +
  scale_colour_manual(values = c("coral", "coral3", "skyblue", "skyblue3")) +
  labs(x = "Stage, Calvert", y = "SW (g)") +
  theme_cowplot(16)

SW_nan_stage <- ggplot(RV_sum_nan, aes(Stage, meanSW_OR, group = SP, colour = SP)) + 
  geom_point(size = 3, position=position_dodge(0.3)) +
  geom_line(size = 0.8, position = position_dodge(0.3), alpha = 0.5, data=RV_sum_nan[!is.na(RV_sum_nan$meanSW_OR),]) +
  geom_errorbar(aes(ymin=meanSW_OR-sdSW_OR, ymax=meanSW_OR+sdSW_OR), width=.2, size = 0.5,
                position=position_dodge(0.3)) +
  ylim(0.6, 5.1) +
  scale_colour_manual(values = c("coral", "coral3", "skyblue", "skyblue3")) +
  labs(x = "Stage, Nanaimo", y = "SW (g)") +
  theme_cowplot(16)

TiW_cal_stage <- ggplot(RV_sum_cal, aes(Stage, meanTiW_OR, group = SP, colour = SP)) + 
  geom_point(size = 3, position=position_dodge(0.3)) +
  geom_line(size = 0.8, position = position_dodge(0.3), alpha = 0.5) +
  geom_errorbar(aes(ymin=meanTiW_OR-sdTiW_OR, ymax=meanTiW_OR+sdTiW_OR), width=.2, size = 0.5,
                position=position_dodge(0.3)) +
  ylim(0.85, 4.6) +
  scale_colour_manual(values = c("coral", "coral3", "skyblue", "skyblue3")) +
  labs(x = "Stage, Calvert", y = "TiW (g)") +
  theme_cowplot(16)

TiW_nan_stage <- ggplot(RV_sum_nan, aes(Stage, meanTiW_OR, group = SP, colour = SP)) + 
  geom_point(size = 3, position=position_dodge(0.3)) +
  geom_line(size = 0.8, position = position_dodge(0.3), alpha = 0.5, data=RV_sum_nan[!is.na(RV_sum_nan$meanSW_OR),]) +
  geom_errorbar(aes(ymin=meanTiW_OR-sdTiW_OR, ymax=meanTiW_OR+sdTiW_OR), width=.2, size = 0.5,
                position=position_dodge(0.3)) +
  ylim(0.85, 4.6) +
  scale_colour_manual(values = c("coral", "coral3", "skyblue", "skyblue3")) +
  labs(x = "Stage, Nanaimo", y = "TiW (g)") +
  theme_cowplot(16)

SG_cal_stage <- ggplot(RV_sum_cal, aes(Stage, meanSG_OR, group = SP, colour = SP)) + 
  geom_point(size = 3, position=position_dodge(0.3)) +
  geom_line(size = 0.8, position = position_dodge(0.3), alpha = 0.5) +
  geom_errorbar(aes(ymin=meanSG_OR-sdSG_OR, ymax=meanSG_OR+sdSG_OR), width=.2, size = 0.5,
                position=position_dodge(0.3)) +
  ylim(0.5, 43) +
  scale_colour_manual(values = c("coral", "coral3", "skyblue", "skyblue3")) +
  labs(x = "Stage, Calvert", y = "SG (mm)") +
  theme_cowplot(16)

SG_nan_stage <- ggplot(RV_sum_nan, aes(Stage, meanSG_OR, group = SP, colour = SP)) + 
  geom_point(size = 3, position=position_dodge(0.3)) +
  geom_line(size = 0.8, position = position_dodge(0.3), alpha = 0.5) +
  geom_errorbar(aes(ymin=meanSG_OR-sdSG_OR, ymax=meanSG_OR+sdSG_OR), width=.2, size = 0.5,
                position=position_dodge(0.3)) +
  ylim(0.5, 43) +
  scale_colour_manual(values = c("coral", "coral3", "skyblue", "skyblue3")) +
  labs(x = "Stage, Nanaimo", y = "SG (mm)") +
  theme_cowplot(16)

RV_combined_stage <- plot_grid(length_cal_stage + theme(legend.position = "none", 
                                                            axis.text.x = element_blank(), axis.ticks.x = element_blank(), axis.title.x = element_blank()), 
                                   length_nan_stage + theme(legend.position = "none",
                                                            axis.text.x = element_blank(), axis.ticks.x = element_blank(), axis.title.x = element_blank(),
                                                            axis.text.y = element_blank(), axis.ticks.y = element_blank(), axis.title.y = element_blank()), 
                                   get_legend(length_cal_stage),
                                   TiW_cal_stage + theme(legend.position = "none", 
                                                           axis.text.x = element_blank(), axis.ticks.x = element_blank(), axis.title.x = element_blank()), 
                                   TiW_nan_stage + theme(legend.position = "none",
                                                           axis.text.x = element_blank(), axis.ticks.x = element_blank(), axis.title.x = element_blank(),
                                                           axis.text.y = element_blank(), axis.ticks.y = element_blank(), axis.title.y = element_blank()),
                                   NULL,
                                   SW_cal_stage + theme(legend.position = "none", 
                                                         axis.text.x = element_blank(), axis.ticks.x = element_blank(), axis.title.x = element_blank()), 
                                   SW_nan_stage + theme(legend.position = "none",
                                                         axis.text.x = element_blank(), axis.ticks.x = element_blank(), axis.title.x = element_blank(),
                                                         axis.text.y = element_blank(), axis.ticks.y = element_blank(), axis.title.y = element_blank()),
                                   NULL,
                                   SG_cal_stage + theme(legend.position = "none", 
                                                           axis.text.x = element_blank(), axis.ticks.x = element_blank(), axis.title.x = element_blank()), 
                                   SG_nan_stage + theme(legend.position = "none",
                                                           axis.text.x = element_blank(), axis.ticks.x = element_blank(), axis.title.x = element_blank(),
                                                           axis.text.y = element_blank(), axis.ticks.y = element_blank(), axis.title.y = element_blank()),
                                   NULL,
                                   thick_cal_stage + theme(legend.position = "none",axis.title.x = element_blank()), 
                                   thick_nan_stage + theme(legend.position = "none",axis.title.x = element_blank(), 
                                                           axis.text.y = element_blank(), axis.ticks.y = element_blank(), axis.title.y = element_blank()),
                                   NULL,
                               ncol = 3, nrow = 5, axis = "vb", align = "hv", rel_widths = c(1,1,0.2),
                               labels = c("Calvert", "Nanaimo"), label_size = 16, label_fontface = "plain",
                               label_x = 0.43, label_y = 1, hjust = 0, vjust = 1.3)

xaxistitle <- ggdraw() + draw_label("Stage", fontface = "plain", x = 0.43, hjust = 0, size = 16)
RV_combined_stage_title <- plot_grid(RV_combined_stage, xaxistitle, ncol = 1, rel_heights = c(1, 0.05))

ggsave(RV_combined_stage_title, file = "plots/snails/RT/RV_combined_stage.pdf", height = 12, width = 12, dpi = 300)

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
  mutate(diff_meanl = meanL - lag(meanL, default= meanL[1]),
         diff_meanTh = meanTh - lag(meanTh, default = meanTh[1]),
         diff_meanSW = meanSW - lag(meanSW, default = meanSW[1]),
         diff_meanTiW = meanTiW - lag(meanTiW, default = meanTiW[1])) %>% 
  subset(Stage == "Final") %>% 
  ungroup()

#Remove the Blue Pruth block at Cedar: high mortality in this block means the calculated difference is very negative
#Because it just so happened that the only surviving snail was a very small one! 
RV_diff <- RV_diff %>% 
  filter(!OS == "Cedar" | !SP == "PRUTH" | !Block == "B")

#Then summarize the mean difference grouped by OR & SR: there should be n = 16 for each grouping, and only 4 groupings
RV_sum_OR_SR <- RV_diff %>% 
  group_by(OR, SR) %>% 
  summarize(meanL_OR_SR = mean(diff_meanl, na.rm = TRUE), sdL_OR_SR = sd(diff_meanl, na.rm = TRUE),
            meanTh_OR_SR = mean(diff_meanTh, na.rm = TRUE), sdTh_OR_SR = sd(diff_meanTh, na.rm = TRUE),
            meanSW_OR_SR = mean(diff_meanSW, na.rm = TRUE), sdSW_OR_SR = sd(diff_meanSW, na.rm = TRUE),
            meanTiW_OR_SR = mean(diff_meanTiW, na.rm = TRUE), sdTiW_OR_SR = sd(diff_meanTiW, na.rm = TRUE),
            meanSG_OR_SR = mean(meanSG, na.rm = TRUE), sdSG_OR_SR = sd(meanSG, na.rm = TRUE),
            n_OR_SR = n()) %>% 
  ungroup()

#Visualize the RVs grouped by OR & SR----
length_OR_OS_box <- ggplot(RV_diff, aes(OR, diff_meanl, fill = SR, color = SR)) + 
  geom_boxplot(color = "grey24", varwidth = TRUE) +
  scale_fill_manual(values = c("skyblue", "coral")) +
  labs(x = "Outplant Region", y = "Change in SL (mm)") +
  theme_cowplot(16)

length_OR_OS_points <- ggplot(RV_sum_OR_SR, aes(OR, meanL_OR_SR, group = SR, colour = SR)) + 
  geom_point(size = 3, position=position_dodge(0.3)) +
  geom_line(size = 0.8, position = position_dodge(0.3), alpha = 0.5) +
  geom_errorbar(aes(ymin=meanL_OR_SR-sdL_OR_SR, ymax=meanL_OR_SR+sdL_OR_SR), width=.2, size = 0.5,
                position=position_dodge(0.3)) +
  scale_colour_manual(values = c("skyblue", "coral")) +
  labs(x = "Outplant region", y = "Change in SL (mm)") +
  theme_cowplot(16)

thick_OR_OS_box <- ggplot(RV_diff, aes(OR, diff_meanTh, fill = SR, color = SR)) + 
  geom_boxplot(color = "grey24", varwidth = TRUE) +
  scale_fill_manual(values = c("skyblue", "coral")) +
  labs(x = "Outplant Region", y = "Change in ST (mm)") +
  theme_cowplot(16)

thick_OR_OS_points <- ggplot(RV_sum_OR_SR, aes(OR, meanTh_OR_SR, group = SR, colour = SR)) + 
  geom_point(size = 3, position=position_dodge(0.3)) +
  geom_line(size = 0.8, position = position_dodge(0.3), alpha = 0.5) +
  geom_errorbar(aes(ymin=meanTh_OR_SR-sdTh_OR_SR, ymax=meanTh_OR_SR+sdTh_OR_SR), width=.2, size = 0.5,
                position=position_dodge(0.3)) +
  scale_colour_manual(values = c("skyblue", "coral")) +
  labs(x = "Outplant region", y = "Change in ST (mm)") +
  theme_cowplot(16)

SW_OR_OS_box <- ggplot(RV_diff, aes(OR, diff_meanSW, fill = SR, color = SR)) + 
  geom_boxplot(color = "grey24", varwidth = TRUE) +
  scale_fill_manual(values = c("skyblue", "coral")) +
  labs(x = "Outplant Region", y = "Change in SW (g)") +
  theme_cowplot(16)

SW_OR_OS_points <- ggplot(RV_sum_OR_SR, aes(OR, meanSW_OR_SR, group = SR, colour = SR)) + 
  geom_point(size = 3, position=position_dodge(0.3)) +
  geom_line(size = 0.8, position = position_dodge(0.3), alpha = 0.5) +
  geom_errorbar(aes(ymin=meanSW_OR_SR-sdSW_OR_SR, ymax=meanSW_OR_SR+sdSW_OR_SR), width=.2, size = 0.5,
                position=position_dodge(0.3)) +
  scale_colour_manual(values = c("skyblue", "coral")) +
  labs(x = "Outplant region", y = "Change in SW (g)") +
  theme_cowplot(16)

TiW_OR_OS_box <- ggplot(RV_diff, aes(OR, diff_meanTiW, fill = SR, color = SR)) + 
  geom_boxplot(color = "grey24", varwidth = TRUE) +
  scale_fill_manual(values = c("skyblue", "coral")) +
  labs(x = "Outplant Region", y = "Change in TiW (g)") +
  theme_cowplot(16)

TiW_OR_OS_points <- ggplot(RV_sum_OR_SR, aes(OR, meanTiW_OR_SR, group = SR, colour = SR)) + 
  geom_point(size = 3, position=position_dodge(0.3)) +
  geom_line(size = 0.8, position = position_dodge(0.3), alpha = 0.5) +
  geom_errorbar(aes(ymin=meanTiW_OR_SR-sdTiW_OR_SR, ymax=meanTiW_OR_SR+sdTiW_OR_SR), width=.2, size = 0.5,
                position=position_dodge(0.3)) +
  scale_colour_manual(values = c("skyblue", "coral")) +
  labs(x = "Outplant region", y = "Change in TiW (g)") +
  theme_cowplot(16)

SG_OR_OS_box <- ggplot(RV_diff, aes(OR, diff_meanSG, fill = SR, color = SR)) + 
  geom_boxplot(color = "grey24", varwidth = TRUE) +
  scale_fill_manual(values = c("skyblue", "coral")) +
  labs(x = "Outplant Region", y = "SG (mm)") +
  theme_cowplot(16)

SG_OR_OS_points <- ggplot(RV_sum_OR_SR, aes(OR, meanSG_OR_SR, group = SR, colour = SR)) + 
  geom_point(size = 3, position=position_dodge(0.3)) +
  geom_line(size = 0.8, position = position_dodge(0.3), alpha = 0.5) +
  geom_errorbar(aes(ymin=meanSG_OR_SR-sdSG_OR_SR, ymax=meanSG_OR_SR+sdSG_OR_SR), width=.2, size = 0.5,
                position=position_dodge(0.3)) +
  scale_colour_manual(values = c("skyblue", "coral")) +
  labs(x = "Outplant region", y = "SG (mm)") +
  theme_cowplot(16)

RV_combined_OR_SR <- plot_grid(length_OR_OS_points + theme(legend.position = "none", 
                                                        axis.text.x = element_blank(), axis.ticks.x = element_blank(), axis.title.x = element_blank()), 
                               get_legend(length_OR_OS_points),
                               TiW_OR_OS_points + theme(legend.position = "none", 
                                                     axis.text.x = element_blank(), axis.ticks.x = element_blank(), axis.title.x = element_blank()), 
                               SW_OR_OS_points + theme(legend.position = "none", 
                                                    axis.text.x = element_blank(), axis.ticks.x = element_blank(), axis.title.x = element_blank()), 
                               thick_OR_OS_points + theme(legend.position = "none", axis.title.x = element_blank()), 
                               SG_OR_OS_points + theme(legend.position = "none", axis.title.x = element_blank()), 
                               ncol = 2, nrow = 3, axis = "vb", align = "hv")

xaxistitle_OR <- ggdraw() + draw_label("Outplant Region", fontface = "plain", x = 0.43, hjust = 0, size = 16)
RV_combined_OR_SR_title <- plot_grid(RV_combined_OR_SR, xaxistitle_OR, ncol = 1, rel_heights = c(1, 0.05))

ggsave(RV_combined_OR_SR_title, file = "plots/snails/RT/RV_OR_SR.pdf", height = 12, width = 12, dpi = 300)

