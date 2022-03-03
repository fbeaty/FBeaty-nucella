#Script to analyze & visualize the snail growth RVs (length, shell thickness etc) from the 2019 Reciprocal Transplant

#Load packages----
pkgs <- c("tidyverse", "lubridate", "car", "visreg", "cowplot", "survminer", "survival",
          "emmeans", "lme4", "RVAideMemoire")
lapply(pkgs, library, character.only = TRUE)
rm(pkgs)

#Load csv and clean it----
#NOTE: on Feb 23 2022 I cleaned this csv directly in the csv (there were a few data typos). SO: from this point on make sure you only use this csv and not the excel in your RV_2 folder
#PO26 Mid TiW was off due to SW typo fixed in csv
#CG81 init ST was off due to typo fixed in csv
RV_base <- read.csv("data/snail_RVs/RT_2_final.csv")

#Clean datasets & separate growth & survival----
#Convert to dates & factors
#Note that there are no 'SG' for the initial time periods, so these values are NA
#Also, due to equipment failure we were unable to measure SW in the 'Mid' timepoint, so those values are also NA
#I removed the dead ones from the subsequent analyses because I only measured growth on surviving snails (obviously)
#Remove the IDs for snails you think were ostrina rather than lamellosa
#Estimate shell weight (ShW) based on the following submerged regressions for each population where x is SW (submerged weight):
#Pruth	y = 1.5889x + 0.1392
#Kwakshua	y = 1.5958x + 0.0646
#Cedar	y = 1.61x + 0.0266
#Heron	y = 1.6104x - .1292
#Calculate TiW (tissue weight) based on Shell weight and Total weight

ID_ostrina <- c("KW15", "KW20", "KW31", "KW22", "PO43", "PO34", "PO49", "PO63", "PO54", "PO07", "PO47", "PO28")
pruth_reg <- function(x){
  ShW <- 1.5889*x + 0.1392
  return(ShW)
}
kwak_reg <- function(x){
  ShW <- 1.5958*x + 0.0646
  return(ShW)
}
cedar_reg <- function(x){
  ShW <- 1.61*x + 0.0266
  return(ShW)
}
heron_reg <- function(x){
  ShW <- 1.6104*x - .1292
  return(ShW)
}

RV_alive <- RV_base %>% 
  filter(!ID %in% ID_ostrina) %>% 
  filter(!OS == "" & DIED == 0) %>% 
  mutate(Date = as.Date(Date, format = "%m-%d-%Y"),
         Stage = factor(Stage, levels = c("Init", "Mid", "Final")),
         SR = as.factor(SR),
         SP = as.factor(SP),
         OR = as.factor(OR),
         OS = as.factor(OS),
         Block = as.factor(Block),
         ShW = ifelse(SP == "CEDAR", cedar_reg(SW), 
                      ifelse(SP == "HERON", heron_reg(SW),
                             ifelse(SP == "KWAK", kwak_reg(SW),
                                    ifelse(SP == "PRUTH", pruth_reg(SW), NA)))),
         TiW = TW-ShW) %>% 
  select(Date, Stage, SR, SP, OR, OS, Block, ID, L, Th, ShW, TiW, SG)

rm(cedar_reg, heron_reg, pruth_reg, kwak_reg)

RV_survival <- RV_base %>% 
  filter(!ID %in% ID_ostrina) %>% 
  filter(!OS == "") %>% 
  mutate(Date = as.Date(Date, format = "%m-%d-%Y"),
         Stage = factor(Stage, levels = c("Init", "Mid", "Final")),
         SR = as.factor(SR),
         SP = as.factor(SP),
         OR = as.factor(OR),
         OS = as.factor(OS),
         Block = as.factor(Block)) %>% 
  select(Date, Stage, SR, SP, OR, OS, Block, ID, DIED)

#Calculate the average & SD of metrics to visualize across the 3 time periods ----
#First check for outliers caused by high mortality within block that only leaves 1 individual left (which often means the mean - mean values are disproportionate)
#Remove the following outliers due to high mortality causing outliers in data
#Blue Pruth outplanted at Cedar because only 1 survived and it was very small --> pulls balance down
#Blue KWAK at Heron in mid and final, because n = 1
remove <- c("Cedar_PRUTH_B_Final", "Heron_KWAK_B_Final", "Heron_KWAK_B_Mid")
#unite(comb_ID, c(OS, SP, Block, Stage), sep = "_", remove = FALSE) %>% 
#  filter(!comb_ID %in% remove) %>% 

RV_growth_block <- RV_alive %>% 
  group_by(Stage, OR, OS, SR, SP, Block) %>% 
  summarize(meanL = mean(L, na.rm = TRUE), sdL = sd(L, na.rm = TRUE),
            meanTh = mean(Th, na.rm = TRUE), sdTh = sd(Th, na.rm = TRUE),
            meanShW = mean(ShW, na.rm = TRUE), sdShW = sd(ShW, na.rm = TRUE),
            meanTiW = mean(TiW, na.rm = TRUE), sdTiW = sd(TiW, na.rm = TRUE),
            meanSG = mean(SG, na.rm = TRUE), sdSG = sd(SG, na.rm = TRUE), n = n()) %>% 
  ungroup()

#Calculate the cumulative survival over time by selecting only the surviving snails, and then
#calculating the proportion survived in the mid & final time point
RV_survival_block <- RV_survival %>% 
  filter(DIED == 0) %>% 
  group_by(Stage, OR, OS, SR, SP, Block) %>% 
  summarize(n_snl = n()) %>% 
  ungroup()

#Awkward code below: I had to switch around the order of the stage factor to use the 'lag' command
#to calculate proportion survived in each stage :\ 
RV_cumsurv_block <- RV_survival_block %>% 
  arrange(OR, OS, SR, SP, Block, Stage) %>%
  mutate(cumsurv = ifelse(Stage == "Init", n_snl/n_snl,
                          ifelse(Stage == "Mid", n_snl/lag(n_snl, default = n_snl[1]), NA)),
         Stage = factor(Stage, levels = c("Init", "Final", "Mid"))) %>% 
  arrange(OR, OS, SR, SP, Block, Stage) %>% 
  mutate(cumsurv = ifelse(Stage == "Final", n_snl/lag(n_snl, defaulr = n_snl[1]), cumsurv),
         Stage = factor(Stage, levels = c("Init", "Mid", "Final")),
         cumsurv = 100*cumsurv) %>% 
  arrange(OR, OS, SR, SP, Block, Stage)

RV_cumsurv_OR <- RV_cumsurv_block %>% 
  group_by(Stage, OR, SR, SP) %>% 
  summarize(meancumsurv = mean(cumsurv), sdcumsurv = sd(cumsurv), se=sd(cumsurv)/sqrt(n()), n_bl = n()) %>% 
  ungroup()

RV_sum_block <- cbind(RV_growth_block, cumsurv = RV_cumsurv_block$cumsurv) 

#Visualize the RVs over time----
length_stage <- ggplot(RV_sum_block, aes(Stage, meanL, group = SP, colour = SP)) +
  stat_summary(fun=mean, geom="point", size = 3, position=position_dodge(0.3)) +
  stat_summary(fun = mean, geom = "line", size = 0.8, position=position_dodge(0.3), alpha = 0.5) +
  stat_summary(fun.data = "mean_se", geom = "errorbar", width = 0.2, size = 0.5,
               position=position_dodge(0.3)) +
  facet_wrap(~ OR) +
  labs(colour = "Source Population") +
  scale_colour_manual(values = c("coral", "coral3", "skyblue", "skyblue3")) +
  labs(y = "Change in SL (mm)") +
  theme_cowplot(16) + theme(strip.background = element_blank(), strip.text = element_text(size = 16))

thick_stage <- ggplot(RV_sum_block, aes(Stage, meanTh, group = SP, colour = SP)) + 
  stat_summary(fun=mean, geom="point", size = 3, position=position_dodge(0.3)) +
  stat_summary(fun = mean, geom = "line", size = 0.8, position=position_dodge(0.3), alpha = 0.5) +
  stat_summary(fun.data = "mean_se", geom = "errorbar", width = 0.2, size = 0.5,
               position=position_dodge(0.3)) +
  facet_wrap(~ OR) +
  scale_colour_manual(values = c("coral", "coral3", "skyblue", "skyblue3")) +
  labs(y = "ST (mm)") +
  theme_cowplot(16) + theme(strip.text = element_blank())

ShW_stage <- ggplot(RV_sum_block, aes(Stage, meanShW, group = SP, colour = SP)) + 
  stat_summary(fun=mean, geom="point", size = 3, position=position_dodge(0.3)) +
  stat_summary(fun = mean, geom = "line", size = 0.8, position=position_dodge(0.3), alpha = 0.5) +
  stat_summary(fun.data = "mean_se", geom = "errorbar", width = 0.2, size = 0.5,
               position=position_dodge(0.3)) +
  facet_wrap(~ OR) +
  scale_colour_manual(values = c("coral", "coral3", "skyblue", "skyblue3")) +
  labs(y = "ShW (g)") +
  theme_cowplot(16) + theme(strip.text = element_blank())

TiW_stage <- ggplot(RV_sum_block, aes(Stage, meanTiW, group = SP, colour = SP)) + 
  stat_summary(fun=mean, geom="point", size = 3, position=position_dodge(0.3)) +
  stat_summary(fun = mean, geom = "line", size = 0.8, position=position_dodge(0.3), alpha = 0.5) +
  stat_summary(fun.data = "mean_se", geom = "errorbar", width = 0.2, size = 0.5,
               position=position_dodge(0.3)) +
   facet_wrap(~ OR) +
  scale_colour_manual(values = c("coral", "coral3", "skyblue", "skyblue3")) +
  labs(y = "TiW (g)") +
  theme_cowplot(16) + theme(strip.text = element_blank())

SG_stage <- ggplot(RV_sum_block, aes(Stage, meanSG, group = SP, colour = SP)) + 
  stat_summary(fun=mean, geom="point", size = 3, position=position_dodge(0.3)) +
  stat_summary(fun = mean, geom = "line", size = 0.8, position=position_dodge(0.3), alpha = 0.5) +
  stat_summary(fun.data = "mean_se", geom = "errorbar", width = 0.2, size = 0.5,
               position=position_dodge(0.3)) +
  facet_wrap(~ OR) +
  scale_colour_manual(values = c("coral", "coral3", "skyblue", "skyblue3")) +
  labs(y = "LSG (mm)") +
  theme_cowplot(16) + theme(strip.text = element_blank())

surv_stage <- ggplot(RV_cumsurv_OR, aes(Stage, meancumsurv, group = SP, colour = SP)) + 
  geom_point(size = 3, position=position_dodge(0.3)) +
  geom_line(size = 0.8, position = position_dodge(0.3), alpha = 0.5) +
  geom_errorbar(aes(ymin=meancumsurv-se, ymax=meancumsurv+se), width=.2, size = 0.5,
                position=position_dodge(0.3)) +
  facet_wrap(~ OR) +
  scale_y_continuous(breaks = c(0, 20, 40, 60, 80, 100), labels=c(0, 20, 40, 60, 80, 100), limits = c(0, 110)) +
  scale_colour_manual(values = c("coral", "coral3", "skyblue", "skyblue3")) +
  labs(y = "Cumulative Surv %") +
  theme_cowplot(16) + theme(strip.text = element_blank())

RV_stage <- plot_grid(length_stage + theme(legend.position = "none",
                                          axis.text.x = element_blank(), axis.title.x = element_blank()), 
                     get_legend(length_stage),
                     thick_stage + theme(legend.position = "none", 
                                         axis.text.x = element_blank(), axis.title.x = element_blank()), 
                     NULL,
                     SG_stage + theme(legend.position = "none", 
                                      axis.text.x = element_blank(), axis.title.x = element_blank()), 
                     NULL,
                     ShW_stage + theme(legend.position = "none",
                                       axis.text.x = element_blank(), axis.title.x = element_blank()), 
                     NULL,
                     TiW_stage + theme(legend.position = "none",
                                       axis.text.x = element_blank(), axis.title.x = element_blank()), 
                     NULL,
                     surv_stage + theme(legend.position = "none", axis.title.x = element_blank()), 
                     NULL,
                     ncol = 2, nrow = 6, axis = "lb", align = "hv", rel_widths = c(1,0.2))

xaxistitle <- ggdraw() + draw_label("Stage", fontface = "plain", x = 0.43, hjust = 0, size = 16)
RV_combined_stage_title <- plot_grid(RV_stage, xaxistitle, ncol = 1, rel_heights = c(1, 0.05))

#Make sure in your caption for this figure you reference that you're visualizing the mean metrics across blocks with sites pooled (i.e. n = 7-8)
ggsave(RV_combined_stage_title, file = "plots/snails/RT/RV_stage.pdf", height = 14, width = 12, dpi = 300)

#Calculate & visualize the average & SD of change in growth metrics clumped by SR & OR----
#First have to calculate the difference in growth metrics between final & initial
RV_diff <- RV_growth_block %>% 
  subset(Stage != "Mid") %>% 
  arrange(OS, SP, Block, Stage) %>% 
  group_by(Block) %>% 
  mutate(diff_meanl = meanL - lag(meanL, default= meanL[1]),
         diff_meanTh = meanTh - lag(meanTh, default = meanTh[1]),
         diff_meanShW = meanShW - lag(meanShW, default = meanShW[1]),
         diff_meanTiW = meanTiW - lag(meanTiW, default = meanTiW[1])) %>% 
  subset(Stage == "Final") %>% 
  arrange(Stage, OR, OS, SR, SP, Block) %>% 
  ungroup()

RV_diff_1 <- RV_cumsurv_block %>% 
  filter(Stage == "Final") %>% 
  arrange(Stage, OR, OS, SR, SP, Block)

#Blue HERON at Heron are -0.2 for TiW
test <- RV_growth_block %>% 
  filter(Block == "B") %>% 
  filter(SP == "HERON") %>% 
  filter(OS == "Heron")

## Feb 17 update: This line is no longer necessary with new ggplot, but I'll keep it in for the moment.
#Then summarize the mean difference grouped by OR & SR: there should be n = 16 for each grouping, and only 4 groupings
#RV_sum_OR_SR <- RV_diff %>% 
#  group_by(OR, SR) %>% 
#  summarize(meanL_OR_SR = mean(diff_meanl, na.rm = TRUE), sdL_OR_SR = sd(diff_meanl, na.rm = TRUE),
#            meanTh_OR_SR = mean(diff_meanTh, na.rm = TRUE), sdTh_OR_SR = sd(diff_meanTh, na.rm = TRUE),
#            meanShW_OR_SR = mean(diff_meanShW, na.rm = TRUE), sdShW_OR_SR = sd(diff_meanShW, na.rm = TRUE),
#            meanTiW_OR_SR = mean(diff_meanTiW, na.rm = TRUE), sdTiW_OR_SR = sd(diff_meanTiW, na.rm = TRUE),
#            meanSG_OR_SR = mean(meanSG, na.rm = TRUE), sdSG_OR_SR = sd(meanSG, na.rm = TRUE),
#            meancumsurv_OR_SR = mean(meancumsurv, na.rm = TRUE), sdcumsurv_OR_SR = sd(meancumsurv, na.rm = TRUE),
#            n_OR_SR = n()) %>% 
#  ungroup()

#Visualize the growth & var across blocks by SR----
ggplot(RV_diff, aes(Block, diff_meanl, group = SR, colour = SR)) +
  geom_point(alpha=0.3, position = position_jitterdodge(dodge.width = 0.3, jitter.width=0.05)) +
  stat_summary(fun=mean, geom="point", size = 3, position=position_dodge(0.3)) +
  stat_summary(fun.data = "mean_sd", geom = "errorbar", width = 0.2, size = 0.5,
               position=position_dodge(0.3)) +
  facet_wrap(~ OR) +
  labs(colour = "Source Region") +
  scale_colour_manual(values = c("skyblue", "coral")) +
  labs(x = "Block", y = "Change in SL (mm)") +
  theme_cowplot(16)

#Visualize the RVs grouped by OR & SR----
length_OR_OS_points <- ggplot(RV_diff, aes(OR, diff_meanl, group = SR, colour = SR)) +
  geom_point(alpha=0.3, position = position_jitterdodge(dodge.width = 0.3, jitter.width=0.05)) +
  stat_summary(fun=mean, geom="point", size = 3, position=position_dodge(0.3)) +
  stat_summary(fun = mean, geom = "line", size = 0.8, position=position_dodge(0.3), alpha = 0.5) +
  stat_summary(fun.data = "mean_se", geom = "errorbar", width = 0.2, size = 0.5,
               position=position_dodge(0.3)) +
  labs(colour = "Source Region") +
  scale_colour_manual(values = c("skyblue", "coral")) +
  labs(x = "Outplant region", y = "Change in SL (mm)") +
  theme_cowplot(16)

thick_OR_OS_points <- ggplot(RV_diff, aes(OR, diff_meanTh, group = SR, colour = SR)) +
  geom_point(alpha=0.3, position = position_jitterdodge(dodge.width = 0.3, jitter.width=0.05)) +
  stat_summary(fun=mean, geom="point", size = 3, position=position_dodge(0.3)) +
  stat_summary(fun = mean, geom = "line", size = 0.8, position=position_dodge(0.3), alpha = 0.5) +
  stat_summary(fun.data = "mean_se", geom = "errorbar", width = 0.2, size = 0.5,
               position=position_dodge(0.3)) +
  scale_colour_manual(values = c("skyblue", "coral")) +
  labs(x = "Outplant region", y = "Change in ST (mm)") +
  theme_cowplot(16)

ShW_OR_OS_points <- ggplot(RV_diff, aes(OR, diff_meanShW, group = SR, colour = SR)) +
  geom_point(alpha=0.3, position = position_jitterdodge(dodge.width = 0.3, jitter.width=0.05)) +
  stat_summary(fun=mean, geom="point", size = 3, position=position_dodge(0.3)) +
  stat_summary(fun = mean, geom = "line", size = 0.8, position=position_dodge(0.3), alpha = 0.5) +
  stat_summary(fun.data = "mean_se", geom = "errorbar", width = 0.2, size = 0.5,
               position=position_dodge(0.3)) +
  scale_colour_manual(values = c("skyblue", "coral")) +
  labs(x = "Outplant region", y = "Change in ShW (mm)") +
  theme_cowplot(16)

TiW_OR_OS_points <- ggplot(RV_diff, aes(OR, diff_meanTiW, group = SR, colour = SR)) +
  geom_point(alpha=0.3, position = position_jitterdodge(dodge.width = 0.3, jitter.width=0.05)) +
  stat_summary(fun=mean, geom="point", size = 3, position=position_dodge(0.3)) +
  stat_summary(fun = mean, geom = "line", size = 0.8, position=position_dodge(0.3), alpha = 0.5) +
  stat_summary(fun.data = "mean_se", geom = "errorbar", width = 0.2, size = 0.5,
               position=position_dodge(0.3)) +
  scale_colour_manual(values = c("skyblue", "coral")) +
  labs(x = "Outplant region", y = "Change in TiW (g)") +
  theme_cowplot(16)

SG_OR_OS_points <- ggplot(RV_diff, aes(OR, meanSG, group = SR, colour = SR)) +
  geom_point(alpha=0.3, position = position_jitterdodge(dodge.width = 0.3, jitter.width=0.05)) +
  stat_summary(fun=mean, geom="point", size = 3, position=position_dodge(0.3)) +
  stat_summary(fun = mean, geom = "line", size = 0.8, position=position_dodge(0.3), alpha = 0.5) +
  stat_summary(fun.data = "mean_se", geom = "errorbar", width = 0.2, size = 0.5,
               position=position_dodge(0.3)) +
  scale_colour_manual(values = c("skyblue", "coral")) +
  labs(x = "Outplant region", y = "Change in LSG (g)") +
  theme_cowplot(16)

CSurv_OR_OS_points <- ggplot(RV_diff_1, aes(OR, cumsurv, group = SR, colour = SR)) +
  geom_point(alpha=0.3, position = position_jitterdodge(dodge.width = 0.3, jitter.width=0.05)) +
  stat_summary(fun=mean, geom="point", size = 3, position=position_dodge(0.3)) +
  stat_summary(fun = mean, geom = "line", size = 0.8, position=position_dodge(0.3), alpha = 0.5) +
  stat_summary(fun.data = "mean_se", geom = "errorbar", width = 0.2, size = 0.5,
               position=position_dodge(0.3)) +
  scale_colour_manual(values = c("skyblue", "coral")) +
  labs(x = "Outplant region", y = "Cumulative Surv (%)") +
  theme_cowplot(16)

RV_combined_OR_SR <- plot_grid(length_OR_OS_points + theme(legend.position = "none", axis.text.x = element_blank(), axis.title.x = element_blank()),
                               thick_OR_OS_points + theme(legend.position = "none", axis.text.x = element_blank(), axis.title.x = element_blank()), 
                               SG_OR_OS_points + theme(legend.position = "none", axis.text.x = element_blank(), axis.title.x = element_blank()),
                               get_legend(length_OR_OS_points),
                               ShW_OR_OS_points + theme(legend.position = "none", axis.title.x = element_blank()), 
                               TiW_OR_OS_points + theme(legend.position = "none", axis.title.x = element_blank()),
                               CSurv_OR_OS_points + theme(legend.position = "none", axis.title.x = element_blank()),
                               NULL,
                               ncol = 4, nrow = 2, rel_widths= c(1, 1, 1, 0.3), axis = "lb", align = "hv")

xaxistitle_OR <- ggdraw() + draw_label("Outplant Region", fontface = "plain", x = 0.43, hjust = 0, size = 16)
RV_combined_OR_SR_title <- plot_grid(RV_combined_OR_SR, xaxistitle_OR, ncol = 1, rel_heights = c(1, 0.05))

ggsave(RV_combined_OR_SR_title, file = "plots/snails/RT/RV_OR_SR.pdf", height = 8, width = 17, dpi = 300)

#Create new dataframe for growth analysis with init size, change in growth metrics, and fixed & random effects for every snail----
#Calculate the difference in growth (this time by ID rather than block, as in previous code), and change labels to initL etc
RV_lm <- RV_alive %>% 
  subset(Stage != "Mid") %>% 
  arrange(ID, Stage) %>% 
  group_by(ID) %>% 
  mutate(diff_l = L - lag(L, default= L[1]),
         diff_Th = Th - lag(Th, default = Th[1]),
         diff_ShW = ShW - lag(ShW, default = ShW[1]),
         diff_TiW = TiW - lag(TiW, default = TiW[1])) %>% 
  subset(Stage == "Final") %>% 
  unite(OS_block, c("OS", "Block"), sep = "_", remove = FALSE) %>% 
  mutate(OS_block = as.factor(OS_block)) %>% 
  select(Stage, OR, OS, SR, SP, Block, OS_block, ID, SG, diff_l, diff_Th, diff_ShW, diff_TiW) %>%
  ungroup()

#Gather initial sizes for covariates in models
RV_lm_init <- RV_alive %>% 
  subset(Stage == "Init") %>% 
  select(ID, initL = L, initTh = Th, initShW = ShW, initTiW = TiW)

#Merge RV_lm and RV_lm_init by ID
RV_lm <- left_join(RV_lm, RV_lm_init, by = "ID")

#For the survival analysis I have to create a dataframe that has all the IDs in the final period with 0 or 1 and also the IDs that died in mid
RV_survival_glm <- RV_survival %>% 
  unite(OS_block, c("OS", "Block"), sep = "_", remove = FALSE) %>% 
  mutate(OS_block = as.factor(OS_block),
         Died_fin = ifelse(Stage == "Mid" & DIED == 1, 1,
                           ifelse(Stage == "Final", DIED, NA))) %>% 
  filter(!is.na(Died_fin))


#Feb 28th update: if I end up wanting to use this code I'll have to adjust the RV_diff for RV_diff_1 I think, because that's the one with survival
#The original dataframe I created for the linear models summarized growth by block, but then I realized that was incorrect. Keeping this code for now though.
#RV_lm_df <- RV_growth_block %>% 
#  subset(Stage == "Init") %>% 
#  arrange(Stage, OR, OS, SR, SP, Block) %>% 
#  select(Stage, OR, OS, SR, SP, Block, initL = meanL, initTh = meanTh, initShW = meanShW, initTiW = meanTiW) %>%
#  unite(comb_ID, c(OS, SP, Block), sep = "_", remove = FALSE) %>% 
#  filter(!comb_ID == "Cedar_PRUTH_B") %>% 
#  select(comb_ID, initL, initTh, initShW, initTiW) 

#RV_diff <- RV_diff %>% 
#  unite(comb_ID, c(OS, SP, Block), sep = "_", remove = FALSE)

#RV_lm_df <- left_join(RV_diff, RV_lm_df, by = "comb_ID") %>% 
#  unite(OS_block, c("OS", "Block"), sep = "_", remove = FALSE) %>% 
#  mutate(OS_block = as.factor(OS_block)) %>% 
#  select(OR, OS, SR, SP, Block, OS_block, n, initL, initTh, initShW, initTiW, meanSG, diff_meanl, diff_meanTh, diff_meanShW, 
#         diff_meanTiW, meancumsurv) 

#Build linear mixed effects models----
#Fixed effects: SR, OR, and the interaction between SR & OR
#Random effects: Block (I only want the intercept to vary, hence 1|Block notation). Block is nested in OS.
#Note: As per Schielzeth and Nakagawa, I created a unique red ID for each SP by combining Block & OS, so that each SP will have 16 reps rather than 4 groups of 4 identical reps
#OS_Block is used as Block, nested within OS.
#I also included SP as a random effect, since it will contribute variance toward the results. 

#I think this is the best model for me
#I chose to model the interactions w/ initL when there was a significant interaction between initL & SR or OR, so I created a multiple regression model
#according to https://stats.stackexchange.com/questions/281528/dealing-with-model-assumption-violation-homogeneity-of-regression-coefficients
lmer_length <- lmer(diff_l ~ OR*SR+ initL*SR + (1|OS/OS_block) + (1|SP), data = RV_lm)
summary(lmer_length)

#Verify assumptions of model
plot(lmer_length)
plotresid(lmer_length)
visreg(lmer_length)
visreg(lmer_length, "initL", by = "OR", overlay = TRUE)
visreg(lmer_length, "initL", by = "SR", overlay = TRUE)

#Analyse mixed-effects model using anova & Tukey posthoc test with emmeans, with kenward-roger df method
Anova(lmer_length, type = "III")
aov(lmer_length)
#Since there are positive interactions, use the following notation for the Tukey posthoc
grpMeans_length <- emmeans(lmer_length, ~ OR*SR + initL*SR, data = RV_lm)
pairs(grpMeans_length, simple = list("OR", "SR"))

#Shell thickness: note for this model I received a singular fit when OS_block was nested within OS, where OS variance = 0 --> removed OS from model as per Matuschek
lmer_thick <- lmer(diff_Th ~ OR*SR + initTh*SR + (1|OS/OS_block) + (1|SP), data = RV_lm)
summary(lmer_thick)

#Verify assumptions
plot(lmer_thick)
plotresid(lmer_thick)
qqnorm(resid(lmer_length))
visreg(lmer_thick)
visreg(lmer_thick, "initTh", by = "OR", overlay = TRUE)
visreg(lmer_thick, "initTh", by = "SR", overlay = TRUE)

Anova(lmer_thick, type = "III")
grpMeans_thick <- emmeans(lmer_thick, ~ SR*OR + initTh*SR, data = RV_lm)
pairs(grpMeans_thick, simple = list("OR", "SR"))

#Tissue weight
lmer_TiW <- lmer(diff_TiW ~ OR*SR + initTiW*SR + (1|OS/OS_block)+ (1|SP), data = RV_lm)
summary(lmer_TiW)

plotresid(lmer_TiW)
visreg(lmer_TiW)
visreg(lmer_TiW, "initTiW", by = "OR", overlay = TRUE)
visreg(lmer_TiW, "initTiW", by = "SR", overlay = TRUE)

Anova(lmer_TiW, type = "III")
grpMeans_TiW <- emmeans(lmer_TiW, ~ SR*OR + initTiW, data = RV_lm)
pairs(grpMeans_TiW, simple = list("OR", "SR"))

#Shell weight: importantly, I dropped (1|SP) from this model due to singular fit
lmer_ShW <- lmer(diff_ShW ~ OR*SR + initShW*SR + (1|OS/OS_block), data = RV_lm)
summary(lmer_ShW)

plotresid(lmer_ShW)
visreg(lmer_ShW)
visreg(lmer_ShW, "initShW", by = "OR", overlay = TRUE)
visreg(lmer_ShW, "initShW", by = "SR", overlay = TRUE)

Anova(lmer_ShW, type = "III")
grpMeans_ShW <- emmeans(lmer_ShW, ~ SR*OR + initShW*SR, data = RV_lm)
pairs(grpMeans_ShW, simple = list("OR", "SR"))

#Shell growth: included initL as covariate as it improves fit of model
lmer_SG <- lmer(SG ~ OR*SR + initL*SR + (1|OS/OS_block) + (1|SP), data = RV_lm)
summary(lmer_SG)

plotresid(lmer_SG)
visreg(lmer_SG)
visreg(lmer_SG, "initL", by = "OR", overlay = TRUE)
visreg(lmer_SG, "initL", by = "SR", overlay = TRUE)

Anova(lmer_SG, type = "III")
grpMeans_SG <- emmeans(lmer_SG, ~ OR*SR + initL*SR, data = RV_lm)
pairs(grpMeans_SG, simple = list("OR", "SR"))

#Survival: since these data are binomial, you have to run a generalized mixed-effects model, with the RV_survival df
glm_surv <- glmer(Died_fin ~ OR*SR + (1|OS/OS_block), family = binomial(link = "logit"), data = RV_survival_glm)
summary(glm_surv)
Anova(glm_surv, type = "III")

# Visualize fit 
visreg(glm_surv, "OR", by = "SR")
grpMeans_surv <- emmeans(glm_surv, ~ OR*SR, data = RV_survival)
pairs(grpMeans_surv, simple = list("OR", "SR"))

#Remove all the unneeded objects for survival analysis----
rm(length_OR_OS_points, RV_alive, RV_combined_OR_SR, RV_diff, test,
   RV_growth_block, RV_sum_OR, SG_OR_OS_points, ShW_OR_OS_points, 
   CSurv_OR_OS_points, length_stage, thick_stage, thick_OR_OS_points, RV_cumsurv_block, RV_cumsurv_OR, 
   TiW_stage, TiW_OR_OS_points, xaxistitle, xaxistitle_OR, RV_combined_stage_title,
   RV_growth_OR, RV_stage, RV_survival_block, SG_stage, surv_stage, ShW_stage, RV_combined_OR_SR_title)

#Analyze Survival data----
#First create a column that calculates the number of days between each measurement date
#Inport the dates you took measurements (ignore the warning you get upon importing this csv)
dates_df <- read.csv("data/snail_RVs/RT_2_V1_Aug_dates.csv")
dates_df <- dates_df %>% 
  mutate(init_date = as.Date(Init, '%m-%d-%Y'),
         mid_date = as.Date(Mid, '%m-%d-%Y'),
         final_date = as.Date(Final, '%m-%d-%Y'),
         diff_mid_init = mid_date - init_date,
         diff_final_init = final_date - init_date)

#Create a column with the days between the initial, middle, and end measurements, and change font of SR to all caps
RV_surv <- RV_survival %>% 
  mutate(days_diff = ifelse(Stage == "Init" & OS == "Kwak", 0,
                            ifelse(Stage == "Mid" & OS == "Kwak", 78,
                                   ifelse(Stage == "Final" & OS == "Kwak", 138, 
                                          ifelse(Stage == "Init" & OS == "Pruth", 0,
                                                 ifelse(Stage == "Mid" & OS == "Pruth", 77,
                                                        ifelse(Stage == "Final" & OS == "Pruth", 136, 
                                                               ifelse(Stage == "Init" & OR == "Nanaimo", 0,
                                                                      ifelse(Stage == "Mid" & OR == "Nanaimo", 69,
                                                                             ifelse(Stage == "Final" & OR == "Nanaimo", 125, NA)))))))))) %>% 
  mutate(SR = ifelse(SR == "Calvert", "CALVERT", "NANAIMO"))

  
#Analyse whether source region affects survival in the two outplanted regions 
#Based code upon https://bioconnector.github.io/workshops/r-survival.html
sfit <- survfit(Surv(days_diff, DIED) ~ SR + OR, data = RV_surv)
summary(sfit)
survdiff(Surv(days_diff, DIED) ~ SR + OR, data = RV_surv)
surv_pvalue(sfit)

#I don't think it's interesting to show the survival analysis, given that the cumulative survival shows
#the same information in a more visually intuitive way. Instead, I think I'll share the outputs of the coxph test in my text or a table
cph <- coxph(Surv(days_diff, DIED) ~ SR + OR, data = RV_surv)
cph.int <- coxph(Surv(days_diff, DIED) ~ SR * OR, data = RV_surv)
#When I include the interaction in the cph, some of the summary estimates become 0 & inf. The Anova output remains largely the same though, so I'll just go with the cph model
#I would like to test the interactive effect, but not sure how within this type of analysis
summary(cph)
summary(cph.int)
Anova(cph)
Anova(cph.int)
summary(cph)$sctest[3]
round(summary(cph)$sctest[3], digits = 9) == round(surv_pvalue(sfit)[,2], digits = 9)


ggsurvplot(sfit2, legend.title = "Outplant Region", xlab = "Time, days", pval = TRUE,
          data = RV_survival)


ggsurvplot(sfit2, facet.by = "OR", legend.title = "Source Region", xlab = "Time, days", pval = TRUE,
           palette = c("skyblue", "coral"), data = RV_survival)


#Remove all final variables----
rm(lmer_length, RV_base, RV_diff_1, RV_lm, RV_lm_init, RV_survival, RV_survival_glm, ID_ostrina, remove)
