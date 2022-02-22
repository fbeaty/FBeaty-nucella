#Script to analyze & visualize the snail growth RVs (length, shell thickness etc) from the 2019 Reciprocal Transplant

#Load packages----
pkgs <- c("tidyverse", "lubridate", "car", "visreg", "cowplot", "survminer", "survival",
          "emmeans", "lme4")
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
         Stage = factor(Stage, levels = c("Init", "Mid", "Final"))) %>% 
  select(Date, Stage, Days_diff, SR, SP, OR, OS, Block, ID, DIED)

#Calculate the average & SD of metrics in the 3 time periods ----
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

#Calculate & visualize the average & SD of growth metrics clumped by SR & OR----
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

#Create new dataframe for growth analysis with init size, change in growth metrics, and fixed & random effects----
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
#Random effects: Block (I only want the intercept to vary, hence 1|Block notation). Block is nested in OS
#I also included SP nested within SR as a random effect, since it will contribute variance toward the results
#I think this is the best model for me

head(RV_lm_df)
lmer_length_1 <- lmer(diff_meanl ~ SR + OR + SR:OR + (1|OS/Block) + (1|SP), data = RV_lm_df)
lmer_length_2 <- lmer(diff_meanl ~ SR + OR + initL + SR:OR + (1|OS/Block) + (1|SP), data = RV_lm_df)

summary(lmer_length_1)
summary(lmer_length_2)
Anova(lmer_length_1)
Anova(lmer_length_2)
plot(lmer_length_1)
qqnorm(resid(lmer_length_1))
visreg(lmer_length_1)
visreg(lmer_length_2, "initL", by = "OR", overlay = TRUE)

#Do Tukey posthoc test with emmeans, with kenward-roger df method
grpMeans_length_1 <- emmeans(lmer_length_1, c("OR", "SR"), data = RV_lm_df)
pairs(grpMeans_length_1)

lmer_thick <- lmer(diff_meanTh ~ SR + OR + SR:OR + (1|OS/Block)+ (1|SP), data = RV_lm_df)
summary(lmer_thick)
Anova(lmer_thick)
grpMeans_thick <- emmeans(lmer_thick, c("OR", "SR"), data = RV_lm_df)
pairs(grpMeans_thick)

lmer_TiW <- lmer(diff_meanTiW ~ SR + OR + SR:OR + (1|OS/Block)+ (1|SP), data = RV_lm_df)
lmer_TiW_dropped <- lmer(diff_meanTiW ~ SR + OR + SR:OR + (1|OS), data = RV_lm_df)

#warning: boundary (singular) fit <- you get this message when the random effects are very small --> can I re-specify the model without these effects?
summary(lmer_TiW)
summary(lmer_TiW_dropped)
Anova(lmer_TiW)
Anova(lmer_TiW_dropped)
grpMeans_TiW <- emmeans(lmer_TiW, c("OR", "SR"), data = RV_lm_df)
pairs(grpMeans_TiW)

lmer_ShW <- lmer(diff_meanShW ~ SR + OR + SR:OR + (1|OS/Block) + (1|SP), data = RV_lm_df)
#warning: boundary (singular) fit, SP was close to zero, so I removed SP in this one
lmer_ShW_dropped <- lmer(diff_meanShW ~ SR + OR + SR:OR + (1|OS/Block), data = RV_lm_df)
summary(lmer_ShW)
summary(lmer_ShW_dropped)
Anova(lmer_ShW)
Anova(lmer_ShW_dropped)
grpMeans_ShW <- emmeans(lmer_ShW, c("OR", "SR"), data = RV_lm_df)
pairs(grpMeans_ShW)

lmer_SG <- lmer(meanSG ~ SR + OR + SR:OR + (1|OS/Block) + (1|SP), data = RV_lm_df)
summary(lmer_SG)
Anova(lmer_SG)
grpMeans_SG <- emmeans(lmer_SG, c("OR", "SR"), data = RV_lm_df)
pairs(grpMeans_SG)

lmer_CSurv <- lmer(meancumsurv ~ SR + OR + SR:OR + (1|OS/Block) + (1|SP), data = RV_lm_df)
#warning: boundary(singular) fit
summary(lmer_CSurv)
Anova(lmer_CSurv)
grpMeans_CSurv <- emmeans(lmer_CSurv, c("OR", "SR"), data = RV_lm_df)
pairs(grpMeans_CSurv)

#Alternative models:
#lmer_length_2 <- lmer(diff_meanl ~ SP + OS + SP:OS + (1|OS/Block), data = RV_lm_df)
#lmer_length_3 <- lmer(diff_meanl ~ SR + OR + SP + OS + SR:OR + (1|OS/Block), data = RV_lm_df)

#AIC(lmer_length, lmer_length_1, lmer_length_2, lmer_length_3)
#lmer_length_2 is best fit, but it doesn't include the SR or OR terms, which is what I'm interested at the end of the day

#I tried to incorporate some residual variance structures, but they didn't seem to make any difference... not sure what I'm doing wrong!
lme_length_var <- lme(diff_meanl ~ SP + OS + SP:OS, random = ~1|OS/Block, 
                     weights = varIdent(~1|SP*OS), data = RV_lm_df)
lme_length <- lme(diff_meanl ~ SP + OS + SP:OS, random = ~1|OS/Block, 
                  data = RV_lm_df)
AIC(lme_length_var, lme_length)
anova(lme_length_var, lme_length)
#There's no difference between these models...

#Check assumptions of the model as per Zuur Ch 2 pgs 23-27
#1: Test for homogeneity of variance by plotting residuals versus fitted
plot(lmer_length_1)
#Residuals look relatively evenly spread out along the line, although they definitly spread out with higher fitted values
plot(lmer_thick)
plot(lmer_TiW)
plot(lmer_ShW)
plot(lmer_SG)
plot(lmer_CSurv)
#2: Test for normal distribution of residuals via a qq-plot or histogram
qqnorm(resid(lmer_length_1))
hist(resid(lmer_length_1), xlab = "Residuals", main = "")
#Looks like a good qq distribution, some deviation but pretty minimal
#Histogram has a small right tail
#3: Test for independence by checking residuals against each explanatory value
plot(RV_lm_df$OR, resid(lmer_length_1), xlab = "OR", ylab = "Residuals")
plot(RV_lm_df$SR, resid(lmer_length_1), xlab = "SR", ylab = "Residuals")

#Spread of residuals is very similar between OR & SR --> good to go here
plot(RV_lm_df$OS, resid(lm_length), xlab = "OS", ylab = "Residuals")
plot(RV_lm_df$Block, resid(lm_length), xlab = "Block", ylab = "Residuals")


#Use emmeans to calculate effect sizes
lm_length_emm <- emmeans(lm_length, c("SR", "OR"), data = RV_lm_df)
lm_length_emm

#Remove all the unneeded objects for survival analysis----
rm(length_OR_OS_box, length_OR_OS_points, RV_alive, RV_combined_OR_SR, RV_diff, 
   RV_growth_block, RV_sum_OR, RV_sum_OR_SR, SG_OR_OS_points, ShW_OR_OS_points, 
   CSurv_OR_OS_points, length_stage, thick_stage, thick_OR_OS_points, RV_cumsurv_block, RV_cumsurv_OR, 
   TiW_stage, TiW_OR_OS_box, TiW_OR_OS_points, xaxistitle, xaxistitle_OR, RV_diff_2, RV_combined_stage_title,
   RV_growth_OR, RV_stage, RV_survival_block, SG_stage, surv_stage, ShW_stage, RV_combined_OR_SR_title)

#Analyze Survival data----
#first subset by outplant site
RV_surv_cal <- RV_survival %>% 
  filter(OR == "Calvert")

RV_surv_nan <- RV_survival %>% 
  filter(OR == "Nanaimo")

#Analyse whether source region affects survival in the two outplanted regions 
RV_survival <- RV_survival %>% 
  mutate(SR = as.factor(SR),
         OR = as.factor(OR))

both <- survfit(Surv(Days_diff, DIED) ~ SR, data = RV_survival)
both_1 <- survfit(Surv(Days_diff, DIED) ~ SR + OR, data = RV_survival)

summary(both)
summary(both_1)
survanalysis <- ggsurvplot(both, facet.by = "OR", legend.title = "Source Region", xlab = "Time, days", pval = TRUE,
           palette = c("skyblue", "coral")) 


ggsave(survanalysis, file = "plots/snails/RT/survanalysis.pdf", height = 4, width = 8, dpi = 300)

cal <- survfit(Surv(Days_diff, DIED) ~ SR, data = RV_surv_cal)
summary(cal)
ggsurvplot(
  fit = cal, 
  xlab = "Days", 
  ylab = "Overall survival probability", pval = TRUE)
cal_coxph <- coxph(Surv(Days_diff, DIED) ~ SR, data = RV_surv_cal)
Anova(cal_coxph)

nan <- survfit(Surv(Days_diff, DIED) ~ SR, data = RV_surv_nan)
summary(nan)
ggsurvplot(
  fit = nan, 
  xlab = "Days", 
  ylab = "Overall survival probability")
nan_coxph <- coxph(Surv(Days_diff, DIED) ~ SR, data = RV_surv_nan)
Anova(nan_coxph)

