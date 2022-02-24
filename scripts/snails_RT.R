#Script to analyze & visualize the snail growth RVs (length, shell thickness etc) from the 2019 Reciprocal Transplant

#Load packages----
pkgs <- c("tidyverse", "lubridate", "car", "visreg", "cowplot", "survminer", "survival",
          "emmeans", "lme4", "RVAideMemoire")
lapply(pkgs, library, character.only = TRUE)
rm(pkgs)

#Load csv and clean it----
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
RV_growth_block <- RV_alive %>% 
  group_by(Stage, OR, OS, SR, SP, Block) %>% 
  summarize(meanL = mean(L, na.rm = TRUE), sdL = sd(L, na.rm = TRUE),
            meanTh = mean(Th, na.rm = TRUE), sdTh = sd(Th, na.rm = TRUE),
            meanShW = mean(ShW, na.rm = TRUE), sdShW = sd(ShW, na.rm = TRUE),
            meanTiW = mean(TiW, na.rm = TRUE), sdTiW = sd(TiW, na.rm = TRUE),
            meanSG = mean(SG, na.rm = TRUE), sdSG = sd(SG, na.rm = TRUE), n = n()) %>% 
  ungroup()

RV_growth_OR <- RV_growth_block %>% 
  group_by(Stage, OR, SR, SP) %>% 
  summarize(meanL_OR = mean(meanL, na.rm = TRUE), sdL_OR = sd(meanL, na.rm = TRUE),
            meanTh_OR = mean(meanTh, na.rm = TRUE), sdTh_OR = sd(meanTh, na.rm = TRUE),
            meanShW_OR = mean(meanShW, na.rm = TRUE), sdShW_OR = sd(meanShW, na.rm = TRUE),
            meanTiW_OR = mean(meanTiW, na.rm = TRUE), sdTiW_OR = sd(meanTiW, na.rm = TRUE),
            meanSG_OR = mean(meanSG, na.rm = TRUE), sdSG_OR = sd(meanSG, na.rm = TRUE), n = n()) %>% 
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
  summarize(meancumsurv = mean(cumsurv), sdcumsurv = sd(cumsurv), n_bl = n()) %>% 
  ungroup()

RV_sum_OR <- cbind(RV_growth_OR, meancumsurv = RV_cumsurv_OR$meancumsurv, sdcumsurv = RV_cumsurv_OR$sdcumsurv) 

#Visualize the RVs over time----
length_stage <- ggplot(RV_sum_OR, aes(Stage, meanL_OR, group = SP, colour = SP)) + 
  geom_point(size = 3, position=position_dodge(0.3)) +
  geom_line(size = 0.8, position = position_dodge(0.3), alpha = 0.5) +
  geom_errorbar(aes(ymin=meanL_OR-sdL_OR, ymax=meanL_OR+sdL_OR), width=.2, size = 0.5,
                position=position_dodge(0.3)) +
  facet_wrap(~ OR) +
  labs(colour = "Source Population") +
  scale_colour_manual(values = c("coral", "coral3", "skyblue", "skyblue3")) +
  labs(y = "SL (mm)") +
  theme_cowplot(16) + theme(strip.background = element_blank(), strip.text = element_text(size = 16))

thick_stage <- ggplot(RV_sum_OR, aes(Stage, meanTh_OR, group = SP, colour = SP)) + 
  geom_point(size = 3, position=position_dodge(0.3)) +
  geom_line(size = 0.8, position = position_dodge(0.3), alpha = 0.5) +
  geom_errorbar(aes(ymin=meanTh_OR-sdTh_OR, ymax=meanTh_OR+sdTh_OR), width=.2, size = 0.5,
                position=position_dodge(0.3)) +
  facet_wrap(~ OR) +
  scale_colour_manual(values = c("coral", "coral3", "skyblue", "skyblue3")) +
  labs(y = "ST (mm)") +
  theme_cowplot(16) + theme(strip.text = element_blank())

ShW_stage <- ggplot(RV_sum_OR, aes(Stage, meanShW_OR, group = SP, colour = SP)) + 
  geom_point(size = 3, position=position_dodge(0.3)) +
  geom_line(size = 0.8, position = position_dodge(0.3), alpha = 0.5, data=RV_sum_OR[!is.na(RV_sum_OR$meanShW_OR),]) +
  geom_errorbar(aes(ymin=meanShW_OR-sdShW_OR, ymax=meanShW_OR+sdShW_OR), width=.2, size = 0.5,
                position=position_dodge(0.3)) +
  facet_wrap(~ OR) +
  scale_colour_manual(values = c("coral", "coral3", "skyblue", "skyblue3")) +
  labs(y = "ShW (g)") +
  theme_cowplot(16) + theme(strip.text = element_blank())

TiW_stage <- ggplot(RV_sum_OR, aes(Stage, meanTiW_OR, group = SP, colour = SP)) + 
  geom_point(size = 3, position=position_dodge(0.3)) +
  geom_line(size = 0.8, position = position_dodge(0.3), alpha = 0.5, data=RV_sum_OR[!is.na(RV_sum_OR$meanTiW_OR),]) +
  geom_errorbar(aes(ymin=meanTiW_OR-sdTiW_OR, ymax=meanTiW_OR+sdTiW_OR), width=.2, size = 0.5,
                position=position_dodge(0.3)) +
  facet_wrap(~ OR) +
  scale_colour_manual(values = c("coral", "coral3", "skyblue", "skyblue3")) +
  labs(y = "TiW (g)") +
  theme_cowplot(16) + theme(strip.text = element_blank())

SG_stage <- ggplot(RV_sum_OR, aes(Stage, meanSG_OR, group = SP, colour = SP)) + 
  geom_point(size = 3, position=position_dodge(0.3)) +
  geom_line(size = 0.8, position = position_dodge(0.3), alpha = 0.5) +
  geom_errorbar(aes(ymin=meanSG_OR-sdSG_OR, ymax=meanSG_OR+sdSG_OR), width=.2, size = 0.5,
                position=position_dodge(0.3)) +
  facet_wrap(~ OR) +
  scale_colour_manual(values = c("coral", "coral3", "skyblue", "skyblue3")) +
  labs(y = "SG (mm)") +
  theme_cowplot(16) + theme(strip.text = element_blank())

surv_stage <- ggplot(RV_sum_OR, aes(Stage, meancumsurv, group = SP, colour = SP)) + 
  geom_point(size = 3, position=position_dodge(0.3)) +
  geom_line(size = 0.8, position = position_dodge(0.3), alpha = 0.5) +
  geom_errorbar(aes(ymin=meancumsurv-sdcumsurv, ymax=meancumsurv+sdcumsurv), width=.2, size = 0.5,
                position=position_dodge(0.3)) +
  facet_wrap(~ OR) +
  scale_y_continuous(breaks = c(0, 20, 40, 60, 80, 100), labels=c(0, 20, 40, 60, 80, 100), limits = c(0, 110)) +
  scale_colour_manual(values = c("coral", "coral3", "skyblue", "skyblue3")) +
  labs(y = "Cumulative Surv %") +
  theme_cowplot(16) + theme(strip.text = element_blank())

RV_stage <- plot_grid(length_stage + theme(legend.position = "none",
                                          axis.text.x = element_blank(), axis.title.x = element_blank()), 
                     get_legend(length_stage),
                     TiW_stage + theme(legend.position = "none",
                                       axis.text.x = element_blank(), axis.title.x = element_blank()), 
                     NULL,
                     ShW_stage + theme(legend.position = "none",
                                      axis.text.x = element_blank(), axis.title.x = element_blank()), 
                     NULL,
                     SG_stage + theme(legend.position = "none", 
                                      axis.text.x = element_blank(), axis.title.x = element_blank()), 
                     NULL,
                     thick_stage + theme(legend.position = "none", 
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

RV_diff_2 <- RV_cumsurv_block %>% 
  filter(Stage == "Final") %>% 
  arrange(Stage, OR, OS, SR, SP, Block)

RV_diff <- cbind(RV_diff, meancumsurv = RV_diff_2$cumsurv)

#Remove the Blue Pruth block at Cedar: high mortality in this block means the calculated difference is very negative
#Because it just so happened that the only surviving snail was a very small one! 
RV_diff <- RV_diff %>% 
  filter(!OS == "Cedar" | !SP == "PRUTH" | !Block == "B")

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
  stat_summary(fun.data = "mean_sd", geom = "errorbar", width = 0.2, size = 0.5,
               position=position_dodge(0.3)) +
  labs(colour = "Source Region") +
  scale_colour_manual(values = c("skyblue", "coral")) +
  labs(x = "Outplant region", y = "Change in SL (mm)") +
  theme_cowplot(16)

thick_OR_OS_points <- ggplot(RV_diff, aes(OR, diff_meanTh, group = SR, colour = SR)) +
  geom_point(alpha=0.3, position = position_jitterdodge(dodge.width = 0.3, jitter.width=0.05)) +
  stat_summary(fun=mean, geom="point", size = 3, position=position_dodge(0.3)) +
  stat_summary(fun = mean, geom = "line", size = 0.8, position=position_dodge(0.3), alpha = 0.5) +
  stat_summary(fun.data = "mean_sd", geom = "errorbar", width = 0.2, size = 0.5,
               position=position_dodge(0.3)) +
  scale_colour_manual(values = c("skyblue", "coral")) +
  labs(x = "Outplant region", y = "Change in ST (mm)") +
  theme_cowplot(16)

ShW_OR_OS_points <- ggplot(RV_diff, aes(OR, diff_meanShW, group = SR, colour = SR)) +
  geom_point(alpha=0.3, position = position_jitterdodge(dodge.width = 0.3, jitter.width=0.05)) +
  stat_summary(fun=mean, geom="point", size = 3, position=position_dodge(0.3)) +
  stat_summary(fun = mean, geom = "line", size = 0.8, position=position_dodge(0.3), alpha = 0.5) +
  stat_summary(fun.data = "mean_sd", geom = "errorbar", width = 0.2, size = 0.5,
               position=position_dodge(0.3)) +
  scale_colour_manual(values = c("skyblue", "coral")) +
  labs(x = "Outplant region", y = "Change in ShW (mm)") +
  theme_cowplot(16)

TiW_OR_OS_points <- ggplot(RV_diff, aes(OR, diff_meanTiW, group = SR, colour = SR)) +
  geom_point(alpha=0.3, position = position_jitterdodge(dodge.width = 0.3, jitter.width=0.05)) +
  stat_summary(fun=mean, geom="point", size = 3, position=position_dodge(0.3)) +
  stat_summary(fun = mean, geom = "line", size = 0.8, position=position_dodge(0.3), alpha = 0.5) +
  stat_summary(fun.data = "mean_sd", geom = "errorbar", width = 0.2, size = 0.5,
               position=position_dodge(0.3)) +
  scale_colour_manual(values = c("skyblue", "coral")) +
  labs(x = "Outplant region", y = "Change in TiW (g)") +
  theme_cowplot(16)

SG_OR_OS_points <- ggplot(RV_diff, aes(OR, meanSG, group = SR, colour = SR)) +
  geom_point(alpha=0.3, position = position_jitterdodge(dodge.width = 0.3, jitter.width=0.05)) +
  stat_summary(fun=mean, geom="point", size = 3, position=position_dodge(0.3)) +
  stat_summary(fun = mean, geom = "line", size = 0.8, position=position_dodge(0.3), alpha = 0.5) +
  stat_summary(fun.data = "mean_sd", geom = "errorbar", width = 0.2, size = 0.5,
               position=position_dodge(0.3)) +
  scale_colour_manual(values = c("skyblue", "coral")) +
  labs(x = "Outplant region", y = "Change in SG (g)") +
  theme_cowplot(16)

CSurv_OR_OS_points <- ggplot(RV_diff, aes(OR, meancumsurv, group = SR, colour = SR)) +
  geom_point(alpha=0.3, position = position_jitterdodge(dodge.width = 0.3, jitter.width=0.05)) +
  stat_summary(fun=mean, geom="point", size = 3, position=position_dodge(0.3)) +
  stat_summary(fun = mean, geom = "line", size = 0.8, position=position_dodge(0.3), alpha = 0.5) +
  stat_summary(fun.data = "mean_sd", geom = "errorbar", width = 0.2, size = 0.5,
               position=position_dodge(0.3)) +
  scale_colour_manual(values = c("skyblue", "coral")) +
  labs(x = "Outplant region", y = "Cumulative Surv (%)") +
  theme_cowplot(16)

RV_combined_OR_SR <- plot_grid(length_OR_OS_points + theme(legend.position = "none", axis.text.x = element_blank(), axis.title.x = element_blank()), 
                               TiW_OR_OS_points + theme(legend.position = "none", axis.text.x = element_blank(), axis.title.x = element_blank()), 
                               get_legend(length_OR_OS_points),
                               ShW_OR_OS_points + theme(legend.position = "none", axis.text.x = element_blank(), axis.title.x = element_blank()), 
                               thick_OR_OS_points + theme(legend.position = "none", axis.text.x = element_blank(), axis.title.x = element_blank()), 
                               NULL,
                               SG_OR_OS_points + theme(legend.position = "none", axis.title.x = element_blank()), 
                               CSurv_OR_OS_points + theme(legend.position = "none", axis.title.x = element_blank()), 
                               ncol = 3, nrow = 3, rel_widths= c(1, 1, 0.3), axis = "lb", align = "hv")

xaxistitle_OR <- ggdraw() + draw_label("Outplant Region", fontface = "plain", x = 0.43, hjust = 0, size = 16)
RV_combined_OR_SR_title <- plot_grid(RV_combined_OR_SR, xaxistitle_OR, ncol = 1, rel_heights = c(1, 0.05))

ggsave(RV_combined_OR_SR_title, file = "plots/snails/RT/RV_OR_SR.pdf", height = 12, width = 12, dpi = 300)

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

#The original dataframe I created for the linear models summarized growth by block, but then I realized that was incorrect. Keeping this code for now though.
RV_lm_df <- RV_growth_block %>% 
  subset(Stage == "Init") %>% 
  arrange(Stage, OR, OS, SR, SP, Block) %>% 
  select(Stage, OR, OS, SR, SP, Block, initL = meanL, initTh = meanTh, initShW = meanShW, initTiW = meanTiW) %>%
  unite(comb_ID, c(OS, SP, Block), sep = "_", remove = FALSE) %>% 
  filter(!comb_ID == "Cedar_PRUTH_B") %>% 
  select(comb_ID, initL, initTh, initShW, initTiW) 

RV_diff <- RV_diff %>% 
  unite(comb_ID, c(OS, SP, Block), sep = "_", remove = FALSE)

RV_lm_df <- left_join(RV_diff, RV_lm_df, by = "comb_ID") %>% 
  unite(OS_block, c("OS", "Block"), sep = "_", remove = FALSE) %>% 
  mutate(OS_block = as.factor(OS_block)) %>% 
  select(OR, OS, SR, SP, Block, OS_block, n, initL, initTh, initShW, initTiW, meanSG, diff_meanl, diff_meanTh, diff_meanShW, 
         diff_meanTiW, meancumsurv) 

#Analyze growth data using mixed effects linear models----
#I have included the following terms in my models:
#Fixed effects: SR, OR, and the interaction between SR & OR
#Random effects: Block (I only want the intercept to vary, hence 1|Block notation). Block is nested in OS.
#Note: As per Schielzeth and Nakagawa, I created a unique red ID for each SP by combining Block & OS, so that each SP will have 16 reps rather than 4 groups of 4 identical reps
#OS_Block is used as Block, nested within OS.
#I also included SP as a random effect, since it will contribute variance toward the results. 

#I think this is the best model for me, but it has a kind of wacky residuals fit... check with Alyssa
lmer_length_2 <- lmer(diff_l ~ SR + OR + initL + SR:OR + (1|OS/OS_block) + (1|SP), data = RV_lm)

summary(lmer_length_2)
Anova(lmer_length_2)

plot(lmer_length_2)
plotresid(lmer_length_2)
qqnorm(resid(lmer_length_2))
visreg(lmer_length_1)
visreg(lmer_length_2)
visreg(lmer_length_2, "initL", by = "OR", overlay = TRUE)

#Do Tukey posthoc test with emmeans, with kenward-roger df method
grpMeans_length_2 <- emmeans(lmer_length_2, c("OR", "SR"), data = RV_lm_df)
pairs(grpMeans_length_2)

#Shell thickness: note for this model I received a singular fit when OS_block was nested within OS, where OS variance = 0 --> removed OS from model as per Matuschek
lmer_thick <- lmer(diff_meanTh ~ SR + OR + initTh + SR:OR + (1|OS/Block)+ (1|SP), data = RV_lm_df)
lmer_thick_1 <- lmer(diff_Th ~ SR + OR + initTh + SR:OR + (1|OS_block) + (1|SP), data = RV_lm)

summary(lmer_thick_1)
Anova(lmer_thick_1)
visreg(lmer_thick_1)
visreg(lmer_thick_1, "initTh", by = "OR", overlay = TRUE)
plot(lmer_thick_1)
plotresid(lmer_thick_1)

grpMeans_thick_1 <- emmeans(lmer_thick_1, c("OR", "SR"), data = RV_lm)
pairs(grpMeans_thick_1)

#Tissue weight: no errors or warnings
lmer_TiW <- lmer(diff_meanTiW ~ SR + OR + SR:OR + (1|OS/OS_block)+ (1|SP), data = RV_lm_df)
lmer_TiW_1 <- lmer(diff_TiW ~ SR + OR + initTiW + SR:OR + (1|OS/OS_block)+ (1|SP), data = RV_lm)

summary(lmer_TiW)
summary(lmer_TiW_1)
Anova(lmer_TiW)
Anova(lmer_TiW_1)
grpMeans_TiW_1 <- emmeans(lmer_TiW_1, c("OR", "SR"), data = RV_lm)
pairs(grpMeans_TiW_1)
plotresid(lmer_TiW_1)
visreg(lmer_TiW_1)

#Shell weight: no errors or warnings, could perhaps drop covariate
lmer_ShW <- lmer(diff_meanShW ~ SR + OR + SR:OR + (1|OS/Block) + (1|SP), data = RV_lm_df)
lmer_ShW_1 <- lmer(diff_ShW ~ SR + OR + initShW + SR:OR + (1|OS/OS_block) + (1|SP), data = RV_lm)

summary(lmer_ShW_1)
Anova(lmer_ShW_1)
plotresid(lmer_ShW_1)

grpMeans_ShW_1 <- emmeans(lmer_ShW_1, c("OR", "SR"), data = RV_lm)
pairs(grpMeans_ShW_1)

#Shell growth (note that there is no covariate here): no errors or warnings
lmer_SG <- lmer(SG ~ SR + OR + initL + SR:OR + (1|OS/OS_block) + (1|SP), data = RV_lm)

summary(lmer_SG_1)
Anova(lmer_SG)
plotresid(lmer_SG_1)
grpMeans_SG_1 <- emmeans(lmer_SG_1, c("OR", "SR"), data = RV_lm)
pairs(grpMeans_SG_1)

#I don't analyze survival because data is not in the correct structure (I guess I could use a binomial structure? But I think I'll just go with the survival analysis)
#Create glm for this


#Remove all the unneeded objects for survival analysis----
rm(length_OR_OS_box, length_OR_OS_points, RV_alive, RV_combined_OR_SR, RV_diff, 
   RV_growth_block, RV_sum_OR, RV_sum_OR_SR, SG_OR_OS_points, ShW_OR_OS_points, 
   CSurv_OR_OS_points, length_stage, thick_stage, thick_OR_OS_points, RV_cumsurv_block, RV_cumsurv_OR, 
   TiW_stage, TiW_OR_OS_box, TiW_OR_OS_points, xaxistitle, xaxistitle_OR, RV_diff_2, RV_combined_stage_title,
   RV_growth_OR, RV_stage, RV_survival_block, SG_stage, surv_stage, ShW_stage, RV_combined_OR_SR_title)

#Analyze Survival data----
#First create a column that calculates the number of days between each measurement date
#Inport the dates you took measurements
dates_df <- read.csv("data/snail_RVs/RT_2_V1_Aug_dates.csv")
dates_df <- dates_df %>% 
  mutate(init_date = as.Date(Init, '%m-%d-%Y'),
         mid_date = as.Date(Mid, '%m-%d-%Y'),
         final_date = as.Date(Final, '%m-%d-%Y'),
         diff_mid_init = mid_date - init_date,
         diff_final_init = final_date - init_date)

#Create a column with the days between the initial, middle, and end measurements, and change font of SR to all caps
RV_survival <- RV_survival %>% 
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

sfit <- survfit(Surv(days_diff, DIED) ~ SR + OR, data = RV_survival)
summary(sfit)
survdiff(Surv(days_diff, DIED) ~ SR + OR, data = RV_survival)
surv_pvalue(sfit)

#I don't think it's interesting to show the survival analysis, given that the cumulative survival shows
#the same information in a more visually intuitive way. Instead, I think I'll share the outputs of the coxph test in my text or a table
cph <- coxph(Surv(days_diff, DIED) ~ SR + OR, data = RV_survival)
cph.int <- coxph(Surv(days_diff, DIED) ~ SR * OR, data = RV_survival)
#When I include the interaction in the cph, some of the summary estimates become 0 & inf. The Anova output remains largely the same though,
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

