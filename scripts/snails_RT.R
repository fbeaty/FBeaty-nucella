#Script to analyze & visualize the snail growth RVs (length, shell thickness etc) from the 2019 Reciprocal Transplant

#Load packages----
pkgs <- c("tidyverse", "viridis", "lubridate", "car", "visreg", "cowplot", "survminer", "survival",
          "emmeans", "gtsummary")
lapply(pkgs, library, character.only = TRUE)
rm(pkgs)

#Load csv and clean it----
RV_base <- read.csv("data/snail_RVs/RT_2_final.csv")

#Clean datasets & separate growth & survival----
#Convert to dates & factors, and calulate tissue weight based upon the TW & SW columns
#Note that there are no 'SG' for the initial time periods, so these values are NA
#Also, due to equipment failure we were unable to measure SW in the 'Mid' timepoint, so those values are also NA
#I removed the dead ones from the subsequent analyses because I only measured growth on surviving snails (obviously)
#Finally, remove the IDs for snails you think were ostrina rather than lamellosa
ID_ostrina <- c("KW15", "KW20", "KW31", "KW22", "PO43", "PO34", "PO49", "PO63", "PO54", "PO07", "PO47", "PO28")

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
         TiW = TW-SW) %>% 
  select(Date, Stage, SR, SP, OR, OS, Block, ID, L, Th, TW, SW, TiW, SG)

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
            meanSW = mean(SW, na.rm = TRUE), sdSW = sd(SW, na.rm = TRUE),
            meanTiW = mean(TiW, na.rm = TRUE), sdTiW = sd(TiW, na.rm = TRUE),
            meanSG = mean(SG, na.rm = TRUE), sdSG = sd(SG, na.rm = TRUE), n = n()) %>% 
  ungroup()

RV_growth_OR <- RV_growth_block %>% 
  group_by(Stage, OR, SR, SP) %>% 
  summarize(meanL_OR = mean(meanL, na.rm = TRUE), sdL_OR = sd(meanL, na.rm = TRUE),
            meanTh_OR = mean(meanTh, na.rm = TRUE), sdTh_OR = sd(meanTh, na.rm = TRUE),
            meanSW_OR = mean(meanSW, na.rm = TRUE), sdSW_OR = sd(meanSW, na.rm = TRUE),
            meanTiW_OR = mean(meanTiW, na.rm = TRUE), sdTiW_OR = sd(meanTiW, na.rm = TRUE),
            meanSG_OR = mean(meanSG, na.rm = TRUE), sdSG_OR = sd(meanSG, na.rm = TRUE), n = n()) %>% 
  ungroup()

RV_survival_block <- RV_survival %>% 
  filter(DIED == 0) %>% 
  group_by(Stage, OR, OS, SR, SP, Block) %>% 
  summarize(n_snl = n()) %>% 
  ungroup()

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
  summarize(meancumsurv = mean(cumsurv), sdcumsurv = sd(cumsurv), n_bl = n())

RV_sum_OR <- cbind(RV_growth_OR, meancumsurv = RV_cumsurv_OR$meancumsurv, sdcumsurv = RV_cumsurv_OR$sdcumsurv) 

#Visualize the RVs over time----
length_stage <- ggplot(RV_sum_OR, aes(Stage, meanL_OR, group = SP, colour = SP)) + 
  geom_point(size = 3, position=position_dodge(0.3)) +
  geom_line(size = 0.8, position = position_dodge(0.3), alpha = 0.5) +
  geom_errorbar(aes(ymin=meanL_OR-sdL_OR, ymax=meanL_OR+sdL_OR), width=.2, size = 0.5,
                position=position_dodge(0.3)) +
  facet_wrap(~ OR) +
  labs(colour = "Source Population") +
  ylim(22.5, 40) +
  scale_colour_manual(values = c("coral", "coral3", "skyblue", "skyblue3")) +
  labs(y = "SL (mm)") +
  theme_cowplot(16) + theme(strip.background = element_blank(), strip.text = element_text(size = 16))

thick_stage <- ggplot(RV_sum_OR, aes(Stage, meanTh_OR, group = SP, colour = SP)) + 
  geom_point(size = 3, position=position_dodge(0.3)) +
  geom_line(size = 0.8, position = position_dodge(0.3), alpha = 0.5) +
  geom_errorbar(aes(ymin=meanTh_OR-sdTh_OR, ymax=meanTh_OR+sdTh_OR), width=.2, size = 0.5,
                position=position_dodge(0.3)) +
  facet_wrap(~ OR) +
  ylim(0.95, 4.0) +
  scale_colour_manual(values = c("coral", "coral3", "skyblue", "skyblue3")) +
  labs(y = "ST (mm)") +
  theme_cowplot(16) + theme(strip.text = element_blank())

SW_stage <- ggplot(RV_sum_OR, aes(Stage, meanSW_OR, group = SP, colour = SP)) + 
  geom_point(size = 3, position=position_dodge(0.3)) +
  geom_line(size = 0.8, position = position_dodge(0.3), alpha = 0.5, data=RV_sum_OR[!is.na(RV_sum_OR$meanSW_OR),]) +
  geom_errorbar(aes(ymin=meanSW_OR-sdSW_OR, ymax=meanSW_OR+sdSW_OR), width=.2, size = 0.5,
                position=position_dodge(0.3)) +
  facet_wrap(~ OR) +
  ylim(0.6, 5.1) +
  scale_colour_manual(values = c("coral", "coral3", "skyblue", "skyblue3")) +
  labs(y = "SW (g)") +
  theme_cowplot(16) + theme(strip.text = element_blank())

TiW_stage <- ggplot(RV_sum_OR, aes(Stage, meanTiW_OR, group = SP, colour = SP)) + 
  geom_point(size = 3, position=position_dodge(0.3)) +
  geom_line(size = 0.8, position = position_dodge(0.3), alpha = 0.5, data=RV_sum_OR[!is.na(RV_sum_OR$meanTiW_OR),]) +
  geom_errorbar(aes(ymin=meanTiW_OR-sdTiW_OR, ymax=meanTiW_OR+sdTiW_OR), width=.2, size = 0.5,
                position=position_dodge(0.3)) +
  facet_wrap(~ OR) +
  ylim(0.85, 4.6) +
  scale_colour_manual(values = c("coral", "coral3", "skyblue", "skyblue3")) +
  labs(y = "TiW (g)") +
  theme_cowplot(16) + theme(strip.text = element_blank())

SG_stage <- ggplot(RV_sum_OR, aes(Stage, meanSG_OR, group = SP, colour = SP)) + 
  geom_point(size = 3, position=position_dodge(0.3)) +
  geom_line(size = 0.8, position = position_dodge(0.3), alpha = 0.5) +
  geom_errorbar(aes(ymin=meanSG_OR-sdSG_OR, ymax=meanSG_OR+sdSG_OR), width=.2, size = 0.5,
                position=position_dodge(0.3)) +
  facet_wrap(~ OR) +
  ylim(0.5, 48.5) +
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
                     SW_stage + theme(legend.position = "none",
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

#Test whether there's a significant effect of time (or an interesting one)----
anova_nan <- aov(meanL_OR ~ SP + Stage, data = RV_sum_nan)
Anova(anova_nan, type="II")
visreg(anova_nan)

#Calculate & visualize the average & SD of growth metrics clumped by SR & OR----
#First have to calculate the difference in growth metrics between final & initial
RV_diff <- RV_growth_block %>% 
  subset(Stage != "Mid") %>% 
  arrange(OS, SP, Block, Stage) %>% 
  group_by(Block) %>% 
  mutate(diff_meanl = meanL - lag(meanL, default= meanL[1]),
         diff_meanTh = meanTh - lag(meanTh, default = meanTh[1]),
         diff_meanSW = meanSW - lag(meanSW, default = meanSW[1]),
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

#Then summarize the mean difference grouped by OR & SR: there should be n = 16 for each grouping, and only 4 groupings
RV_sum_OR_SR <- RV_diff %>% 
  group_by(OR, SR) %>% 
  summarize(meanL_OR_SR = mean(diff_meanl, na.rm = TRUE), sdL_OR_SR = sd(diff_meanl, na.rm = TRUE),
            meanTh_OR_SR = mean(diff_meanTh, na.rm = TRUE), sdTh_OR_SR = sd(diff_meanTh, na.rm = TRUE),
            meanSW_OR_SR = mean(diff_meanSW, na.rm = TRUE), sdSW_OR_SR = sd(diff_meanSW, na.rm = TRUE),
            meanTiW_OR_SR = mean(diff_meanTiW, na.rm = TRUE), sdTiW_OR_SR = sd(diff_meanTiW, na.rm = TRUE),
            meanSG_OR_SR = mean(meanSG, na.rm = TRUE), sdSG_OR_SR = sd(meanSG, na.rm = TRUE),
            meancumsurv_OR_SR = mean(meancumsurv, na.rm = TRUE), sdcumsurv_OR_SR = sd(meancumsurv, na.rm = TRUE),
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
  labs(colour = "Source Region") +
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

CSurv_OR_OS_box <- ggplot(RV_diff, aes(OR, meancumsurv, fill = SR, color = SR)) + 
  geom_boxplot(color = "grey24", varwidth = TRUE) +
  scale_fill_manual(values = c("skyblue", "coral")) +
  labs(x = "Outplant Region", y = "Cumulative Surv (%)") +
  theme_cowplot(16)

CSurv_OR_OS_points <- ggplot(RV_sum_OR_SR, aes(OR, meancumsurv_OR_SR, group = SR, colour = SR)) + 
  geom_point(size = 3, position=position_dodge(0.3)) +
  geom_line(size = 0.8, position = position_dodge(0.3), alpha = 0.5) +
  geom_errorbar(aes(ymin=meancumsurv_OR_SR-sdcumsurv_OR_SR, ymax=meancumsurv_OR_SR+sdcumsurv_OR_SR), width=.2, size = 0.5,
                position=position_dodge(0.3)) +
  scale_colour_manual(values = c("skyblue", "coral")) +
  labs(x = "Outplant region", y = "Cumulative Surv (%)") +
  theme_cowplot(16)

RV_combined_OR_SR <- plot_grid(length_OR_OS_points + theme(legend.position = "none", 
                                                        axis.text.x = element_blank(), axis.ticks.x = element_blank(), axis.title.x = element_blank()), 
                               TiW_OR_OS_points + theme(legend.position = "none", 
                                                     axis.text.x = element_blank(), axis.ticks.x = element_blank(), axis.title.x = element_blank()), 
                               get_legend(length_OR_OS_points),
                               SW_OR_OS_points + theme(legend.position = "none", 
                                                    axis.text.x = element_blank(), axis.ticks.x = element_blank(), axis.title.x = element_blank()), 
                               thick_OR_OS_points + theme(legend.position = "none", axis.title.x = element_blank()), 
                               NULL,
                               SG_OR_OS_points + theme(legend.position = "none", axis.title.x = element_blank()), 
                               CSurv_OR_OS_points + theme(legend.position = "none", axis.title.x = element_blank()), 
                               ncol = 3, nrow = 3, rel_widths= c(1, 1, 0.3), axis = "lb", align = "hv")

xaxistitle_OR <- ggdraw() + draw_label("Outplant Region", fontface = "plain", x = 0.43, hjust = 0, size = 16)
RV_combined_OR_SR_title <- plot_grid(RV_combined_OR_SR, xaxistitle_OR, ncol = 1, rel_heights = c(1, 0.05))

ggsave(RV_combined_OR_SR_title, file = "plots/snails/RT/RV_OR_SR.pdf", height = 12, width = 12, dpi = 300)

#Remove all the unneeded objects for survival analysis----
rm(length_OR_OS_box, length_OR_OS_points, RV_alive, RV_clean, RV_combined_OR_SR, RV_diff, RV_diff_1, RV_growth_block, RV_sum_cal, RV_sum_nan, RV_sum_OR, RV_sum_OR_SR, 
   SG_OR_OS_box, SG_OR_OS_points, SW_OR_OS_box, SW_OR_OS_points, CSurv_OR_OS_box, CSurv_OR_OS_points, length_stage,
   thick_stage, thick_OR_OS_box, thick_OR_OS_points, RV_cumsurv_block, RV_cumsurv_OR, 
   TiW_stage, TiW_OR_OS_box, TiW_OR_OS_points, xaxistitle, xaxistitle_OR)

#Analyze Survival data----
#first subset by outplant site
RV_surv_cal <- RV_survival %>% 
  filter(OR == "Calvert")

RV_surv_nan <- RV_survival %>% 
  filter(OR == "Nanaimo")

#Analyse whether source region affects survival in the two outplanted regions 
both <- survfit(Surv(Days_diff, DIED) ~ SR, data = RV_survival)
summary(both)
ggsurvplot(both, facet.by = "OR", legend.title = "Source Region", xlab = "Time, days", pval = TRUE,
           palette = c("skyblue", "coral")) 

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

####Now test whether there's a difference in survival in Nanaimo across SPs####
survi3 <- survi1 %>% 
  subset(OR == "Nanaimo")

survi3.KM <- survfit(Surv(Days_diff, DIED) ~ SP, data = survi3)
survi3.coxph <- coxph(Surv(Days_diff, DIED) ~ SP + Block, data = survi3)
summary(survi3.coxph)
p2 <- ggforest(survi3.coxph, data = survi3)
p2

#dev.off()

#cumulative hazard by outplant site
p3 <- ggsurvplot_facet(survi3.KM, data = survi3, fun = "cumhaz", facet.by = c("OS"), conf.int = T, 
                       risk.table.col = "strata")
p3


#Now test whether there's a diff in mortality in Calvert outplant sites
survi4 <- survi1 %>% 
  subset(OR == "Calvert")

survi4.KM <- survfit(Surv(Days_diff, DIED) ~ SP, data = survi4)
survi4.coxph <- coxph(Surv(Days_diff, DIED) ~ SP + Block, data = survi4)
summary(survi4.coxph)
p5 <- ggforest(survi4.coxph, data = survi4)
p5

#dev.off()

#cumulative hazard by outplant site
p6 <- ggsurvplot_facet(survi4.KM, data = survi4, fun = "cumhaz", facet.by = c("OS"), conf.int = T, 
                       risk.table.col = "strata")
p6


#cumulative hazard by treatment
#ggsurvplot_facet(surv4, data=surv3, facet.by=“tmt”, fun=“cumhaz”, conf.int = T, risk.table.col=“strata”)

