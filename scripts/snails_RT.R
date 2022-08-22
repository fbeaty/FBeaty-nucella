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
#HB59 TW was typo so fixed in csv
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
         SR = ifelse(SR == "Nanaimo", "Strait of Georgia", "Central Coast"),
         SP = as.factor(SP),
         OR = as.factor(OR),
         OR = ifelse(OR == "Nanaimo", "Strait of Georgia", "Central Coast"),
         OS = factor(OS, levels = c("Kwak", "Pruth", "Cedar", "Heron")),
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
         OR = ifelse(OR == "Nanaimo", "Strait of Georgia", "Central Coast"),
         OS = as.factor(OS),
         Block = as.factor(Block)) %>% 
  select(Date, Stage, SR, SP, OR, OS, Block, ID, DIED)

#Calculate the average & SD of growth & survival metrics to visualize across the 3 time periods ----
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

#Create a dataset with just the final survival averaged by block for your OR by SR visualization
RV_cumsurv_final <- RV_cumsurv_block %>% 
  filter(Stage == "Final") %>% 
  arrange(Stage, OR, OS, SR, SP, Block)

#Visualize the RVs over time----
plot_stage_RT_me <- function(df, x, y, grp, clr.values, lbl.y){
  ggplot(df, aes({{x}}, {{y}}, group = {{grp}}, colour = {{grp}})) + 
    stat_summary(fun=mean, geom="point", size = 3, position=position_dodge(0.3)) +
    stat_summary(fun = mean, geom = "line", size = 0.8, position=position_dodge(0.3), alpha = 0.5) +
    stat_summary(fun.data = "mean_se", geom = "errorbar", width = 0.2, size = 0.5,
                 position=position_dodge(0.3)) +
    facet_wrap(~ OR) +
    scale_colour_manual(values = clr.values) +
    labs(y = lbl.y) +
    theme_cowplot(16) + theme(strip.text = element_blank())
}

length_stage <- plot_stage_RT_me(RV_sum_block, Stage, meanL, SP, c("coral", "coral3", "skyblue", "skyblue3"), "SL (mm)") +
  labs(colour = "Source Population") + theme(strip.background = element_blank(), strip.text = element_text(size = 16))
thick_stage <- plot_stage_RT_me(RV_sum_block, Stage, meanTh, SP, c("coral", "coral3", "skyblue", "skyblue3"), "ST (mm)")
ShW_stage <- plot_stage_RT_me(RV_sum_block, Stage, meanShW, SP, c("coral", "coral3", "skyblue", "skyblue3"), "ShW (g)")
TiW_stage <- plot_stage_RT_me(RV_sum_block, Stage, meanTiW, SP, c("coral", "coral3", "skyblue", "skyblue3"), "TiW (g)")
SG_stage <- plot_stage_RT_me(RV_sum_block, Stage, meanSG, SP, c("coral", "coral3", "skyblue", "skyblue3"), "LSG (g)")

#This one draws from a different dataframe so just code out the full plot 
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
ggsave(RV_combined_stage_title, file = "plots/supp_figs/FigS5_RV_stage.pdf", height = 14, width = 12, dpi = 300)


#Create new dataframe for growth analysis & region visuals with init size, change in growth metrics, and fixed & random effects for every snail----
#Calculate the difference in growth by ID, and change labels to initL etc
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

#Gather initial sizes for covariates in models & for standardized growth calculation
RV_lm_init <- RV_alive %>% 
  subset(Stage == "Init") %>% 
  select(ID, initL = L, initTh = Th, initShW = ShW, initTiW = TiW)

#Gather final sizes for visualization 
RV_lm_fin <- RV_alive %>% 
  subset(Stage == "Final") %>% 
  select(ID, finL = L, finTh = Th, finShW = ShW, finTiW = TiW)

#Merge RV_lm and RV_lm_init by ID & create new column with the diff growth standardized by initial size
RV_lm <- left_join(RV_lm, RV_lm_init, by = "ID") %>% 
  left_join(RV_lm_fin)

#Calculate the sample size for each SP at each OS for your supplemental initial size vs change in growth
RV_lm_sample <- RV_lm %>% 
  group_by(OS, SP) %>% 
  summarize(n_snail = n())

#Now create a separate dataframe with the meandiff & meandiff_i summarized by block for your graphs
RV_lm_block <- RV_lm %>% 
  group_by(OR, OS, SR, SP, Block) %>% 
  summarize(meandiff_l = mean(diff_l, na.rm = TRUE),
            meandiff_Th = mean(diff_Th, na.rm = TRUE),
            meandiff_ShW = mean(diff_ShW, na.rm = TRUE),
            meandiff_TiW = mean(diff_TiW, na.rm = TRUE),
            mean_SG = mean(SG, na.rm = TRUE),
            meanfinL = mean(finL, na.rm = TRUE),
            meanfinTh = mean(finTh, na.rm = TRUE),
            meanfinShW = mean(finShW, na.rm = TRUE),
            meanfinTiW = mean(finTiW, na.rm = TRUE)) %>% 
  ungroup()

#Might delete this code if don't do survival amalysis
#For the survival analysis I have to create a dataframe that has all the IDs in the final period with 0 or 1 and also the IDs that died in mid
RV_survival_glm <- RV_survival %>% 
  unite(OS_block, c("OS", "Block"), sep = "_", remove = FALSE) %>% 
  mutate(OS_block = as.factor(OS_block),
         Died_fin = ifelse(Stage == "Mid" & DIED == 1, 1,
                           ifelse(Stage == "Final", DIED, NA))) %>% 
  filter(!is.na(Died_fin))

#Visualize the change in RVs grouped by OR & SR and by OR & SP----
plot_OR_RT_box <- function(df, x, y, grp, fill.values, clr.values, lbl.y){
  ggplot(df, aes({{x}}, {{y}}, fill = {{grp}}, colour = {{grp}})) + 
    geom_boxplot(colour = "black", varwidth = TRUE, alpha = 0.8) +
    geom_point(size = 3, alpha=0.5, position = position_jitterdodge(dodge.width = 0.7, jitter.width=0.3))  +
    stat_summary(fun = median, geom = 'line', size = 1, aes(group = {{grp}}, colour = {{grp}}), position = position_dodge(width = 0.7)) +
    scale_fill_manual(values = fill.values) +
    scale_colour_manual(values = clr.values) +
    labs(y = lbl.y) +
    theme_cowplot(16)
}

#Create boxplots with change in growth
length_OR_box <- plot_OR_RT_box(RV_lm_block, OR, meandiff_l, SR, c("skyblue", "coral"), c("skyblue3", "coral3"), "Shell length growth (mm)") +
  labs(colour = "Source Region", fill = "Source Region") + draw_plot_label("D", 0.4, 15, fontface = "plain")
thick_OR_box <- plot_OR_RT_box(RV_lm_block, OR, meandiff_Th, SR, c("skyblue", "coral"), c("skyblue3", "coral3"), "Shell thickness growth (mm)") +
  draw_plot_label("E", 0.4, 2.4, fontface = "plain")
ShW_OR_box <- plot_OR_RT_box(RV_lm_block, OR, meandiff_ShW, SR, c("skyblue", "coral"), c("skyblue3", "coral3"), "Shell weight growth (g)") +
  draw_plot_label("C", 0.4, 5.2, fontface = "plain")
TiW_OR_box <- plot_OR_RT_box(RV_lm_block, OR, meandiff_TiW, SR, c("skyblue", "coral"), c("skyblue3", "coral3"), "Tissue weight growth (g)") +
  draw_plot_label("B", 0.4, 1.3, fontface = "plain")
CSurv_OR_box <- plot_OR_RT_box(RV_cumsurv_final, OR, cumsurv, SR, c("skyblue", "coral"), c("skyblue3", "coral3"), "Survival (%)") +
  draw_plot_label("A", 0.4, 100, fontface = "plain")

RV_combined_OR_box <- plot_grid(CSurv_OR_box + theme(legend.position = "none", axis.text.x = element_blank(), axis.title.x = element_blank()),
                                TiW_OR_box + theme(legend.position = "none", axis.text.x = element_blank(), axis.title.x = element_blank()), 
                                ShW_OR_box + theme(legend.position = "none", axis.text.x = element_blank(), axis.title.x = element_blank()),
                                length_OR_box + theme(legend.position = "none", axis.title.x = element_blank()),
                                thick_OR_box + theme(legend.position = "none", axis.title.x = element_blank()),
                                get_legend(length_OR_box),
                                ncol = 3, nrow = 2, axis = "lb", align = "hv") 

xaxistitle_OR <- ggdraw() + draw_label("Outplant Region", fontface = "plain", x = 0.43, hjust = 0, size = 16)
RV_combined_OR_box_title <- plot_grid(RV_combined_OR_box, xaxistitle_OR, ncol = 1, rel_heights = c(1, 0.05))

#Save both OR by SR plots
ggsave(RV_combined_OR_box_title, file = "plots/snails/RT/Fig_5_RV_OR_box.pdf", height = 8, width = 17, dpi = 300)

#Create boxplots with change in growth by SP
length_OR_box_SP <- plot_OR_RT_box(RV_lm_block, OR, meandiff_l, SP, c("coral", "coral3", "skyblue", "skyblue3"), c("coral", "coral3", "skyblue", "skyblue3"), "Change in SL (mm)") +
  labs(colour = "Source Region", fill = "Source Region")
thick_OR_box_SP <- plot_OR_RT_box(RV_lm_block, OR, meandiff_Th, SP, c("coral", "coral3", "skyblue", "skyblue3"), c("coral", "coral3", "skyblue", "skyblue3"), "Change in ST (mm)")
ShW_OR_box_SP <- plot_OR_RT_box(RV_lm_block,OR, meandiff_ShW, SP, c("coral", "coral3", "skyblue", "skyblue3"), c("coral", "coral3", "skyblue", "skyblue3"), "Change in ShW (g)")
TiW_OR_box_SP <- plot_OR_RT_box(RV_lm_block, OR, meandiff_TiW, SP, c("coral", "coral3", "skyblue", "skyblue3"), c("coral", "coral3", "skyblue", "skyblue3"), "Change in TiW (g)")
SG_OR_box_SP <- plot_OR_RT_box(RV_lm_block, OR, mean_SG, SP, c("coral", "coral3", "skyblue", "skyblue3"), c("coral", "coral3", "skyblue", "skyblue3"), "Change in LSG (mm)")
CSurv_OR_box_SP <- plot_OR_RT_box(RV_cumsurv_final, OR, cumsurv, SP, c("coral", "coral3", "skyblue", "skyblue3"), c("coral", "coral3", "skyblue", "skyblue3"), "Survival (%)")

#Create new survival plots without the lines connecting the median (for your combined mesocosm plots)
CSurv_OR_box <- plot_OR_RT_box(RV_cumsurv_final, OR, cumsurv, SR, c("skyblue", "coral"), c("skyblue3", "coral3"), "Survival (%)")


CSurv_OR_box_noLines <- ggplot(RV_cumsurv_final, aes(OR, cumsurv, fill = SR, colour = SR)) + 
  geom_boxplot(colour = "black", varwidth = TRUE, alpha = 0.8) +
  geom_point(size = 3, alpha=0.5, position = position_jitterdodge(dodge.width = 0.7, jitter.width=0.3))  +
  scale_fill_manual(values = c("skyblue", "coral")) +
  scale_colour_manual(values = c("skyblue3", "coral3")) +
  labs(y = "Survival (%)") +
  theme_cowplot(16)

#Visualize the final RVs grouped by OR & SR & SP----
fin_length_OR_box <- plot_OR_RT_box(RV_lm_block, OR, meanfinL, SR, c("skyblue", "coral"), c("skyblue3", "coral3"), "Mean SL (mm)") +
  labs(colour = "Source Region", fill = "Source Region")
fin_thick_OR_box <- plot_OR_RT_box(RV_lm_block, OR, meanfinTh, SR, c("skyblue", "coral"), c("skyblue3", "coral3"), "Mean ST (mm)")
fin_ShW_OR_box <- plot_OR_RT_box(RV_lm_block, OR, meanfinShW, SR, c("skyblue", "coral"), c("skyblue3", "coral3"), "Mean ShW (g)")
fin_TiW_OR_box <- plot_OR_RT_box(RV_lm_block, OR, meanfinTiW, SR, c("skyblue", "coral"), c("skyblue3", "coral3"), "Mean TiW (g)")
fin_SG_OR_box <- plot_OR_RT_box(RV_lm_block, OR, mean_SG, SR, c("skyblue", "coral"), c("skyblue3", "coral3"), "Mean LSG (mm)")
fin_CSurv_OR_box <- plot_OR_RT_box(RV_cumsurv_final, OR, cumsurv, SR, c("skyblue", "coral"), c("skyblue3", "coral3"), "Survival (%)") +
  labs(colour = "Source Region", fill = "Source Region", x = "Outplant Region") + theme(legend.position = c(0.1, 0.3))

fin_length_OR_box_SP <- plot_OR_RT_box(RV_lm_block, OR, meanfinL, SP, c("coral", "coral3", "skyblue", "skyblue3"), c("coral", "coral3", "skyblue", "skyblue3"), "Mean SL (mm)") +
  labs(colour = "Source Population", fill = "Source Population")
fin_thick_OR_box_SP <- plot_OR_RT_box(RV_lm_block, OR, meanfinTh, SP, c("coral", "coral3", "skyblue", "skyblue3"), c("coral", "coral3", "skyblue", "skyblue3"), "Mean ST (mm)")
fin_ShW_OR_box_SP <- plot_OR_RT_box(RV_lm_block, OR, meanfinShW, SP, c("coral", "coral3", "skyblue", "skyblue3"), c("coral", "coral3", "skyblue", "skyblue3"), "Mean ShW (g)")
fin_TiW_OR_box_SP <- plot_OR_RT_box(RV_lm_block, OR, meanfinTiW, SP, c("coral", "coral3", "skyblue", "skyblue3"), c("coral", "coral3", "skyblue", "skyblue3"), "Mean TiW (g)")
fin_SG_OR_box_SP <- plot_OR_RT_box(RV_lm_block, OR, mean_SG, SP, c("coral", "coral3", "skyblue", "skyblue3"), c("coral", "coral3", "skyblue", "skyblue3"), "Mean LSG (mm)")
fin_CSurv_OR_box_SP <- plot_OR_RT_box(RV_cumsurv_final, OR, cumsurv, SP, c("coral", "coral3", "skyblue", "skyblue3"), c("coral", "coral3", "skyblue", "skyblue3"), "Survival (%)") +
  labs(colour = "Source Region", fill = "Source Region", x = "Outplant Region") + theme(legend.position = c(0.1, 0.3))

RV_fin_OR_box <- plot_grid(fin_length_OR_box + theme(legend.position = "none", axis.text.x = element_blank(), axis.title.x = element_blank()),
                           fin_thick_OR_box + theme(legend.position = "none", axis.text.x = element_blank(), axis.title.x = element_blank()), 
                           fin_SG_OR_box + theme(legend.position = "none", axis.text.x = element_blank(), axis.title.x = element_blank()),
                                get_legend(fin_length_OR_box),
                           fin_ShW_OR_box + theme(legend.position = "none", axis.title.x = element_blank()), 
                           fin_TiW_OR_box + theme(legend.position = "none", axis.title.x = element_blank()),
                           CSurv_OR_box + theme(legend.position = "none", axis.title.x = element_blank()),
                                NULL,
                                ncol = 4, nrow = 2, rel_widths= c(1, 1, 1, 0.3), axis = "lb", align = "hv")

xaxistitle_OR <- ggdraw() + draw_label("Outplant Region", fontface = "plain", x = 0.43, hjust = 0, size = 16)
RV_fin_OR_box_title <- plot_grid(RV_fin_OR_box, xaxistitle_OR, ncol = 1, rel_heights = c(1, 0.05))

#Save both OR by SR plots
ggsave(RV_fin_OR_box_title, file = "plots/snails/RT/RV_OR_box_fin.pdf", height = 8, width = 17, dpi = 300)

#Create a new combined figure that has the phenotype & change in growth side by side & separate survival figure----
#First plot by SR
RV_fin_change_SR <- plot_grid(length_OR_box + theme(legend.position = "none", axis.text.x = element_blank(), axis.title.x = element_blank()),
                           fin_length_OR_box + theme(legend.position = "none", axis.text.x = element_blank(), axis.title.x = element_blank()),
                           get_legend(fin_length_OR_box),
                           thick_OR_box + theme(legend.position = "none", axis.text.x = element_blank(), axis.title.x = element_blank()), 
                           fin_thick_OR_box + theme(legend.position = "none", axis.text.x = element_blank(), axis.title.x = element_blank()), 
                           NULL,
                           ShW_OR_box + theme(legend.position = "none", axis.text.x = element_blank(), axis.title.x = element_blank()), 
                           fin_ShW_OR_box + theme(legend.position = "none", axis.text.x = element_blank(), axis.title.x = element_blank()), 
                           NULL,
                           TiW_OR_box + theme(legend.position = "none", axis.title.x = element_blank()),
                           fin_TiW_OR_box + theme(legend.position = "none", axis.title.x = element_blank()),
                           NULL,
                           ncol = 3, nrow = 4, rel_widths= c(1, 1, 0.3), axis = "lb", align = "hv", labels = c('A', 'B', ''), label_size = 18)

RV_fin_change_title_SR <- plot_grid(RV_fin_change_SR, xaxistitle_OR, ncol = 1, rel_heights = c(1, 0.05))

#Create another one by SP
RV_fin_change_SP <- plot_grid(length_OR_box_SP + theme(legend.position = "none", axis.text.x = element_blank(), axis.title.x = element_blank()),
                              fin_length_OR_box_SP + theme(legend.position = "none", axis.text.x = element_blank(), axis.title.x = element_blank()),
                              get_legend(fin_length_OR_box_SP),
                              thick_OR_box_SP + theme(legend.position = "none", axis.text.x = element_blank(), axis.title.x = element_blank()), 
                              fin_thick_OR_box_SP + theme(legend.position = "none", axis.text.x = element_blank(), axis.title.x = element_blank()), 
                              NULL,
                              ShW_OR_box_SP + theme(legend.position = "none", axis.text.x = element_blank(), axis.title.x = element_blank()), 
                              fin_ShW_OR_box_SP + theme(legend.position = "none", axis.text.x = element_blank(), axis.title.x = element_blank()), 
                              NULL,
                              TiW_OR_box_SP + theme(legend.position = "none", axis.title.x = element_blank()),
                              fin_TiW_OR_box_SP + theme(legend.position = "none", axis.title.x = element_blank()),
                              NULL,
                              ncol = 3, nrow = 4, rel_widths= c(1, 1, 0.3), axis = "lb", align = "hv", labels = c('A', 'B', ''), label_size = 18)

RV_fin_change_title_SP <- plot_grid(RV_fin_change_SP, xaxistitle_OR, ncol = 1, rel_heights = c(1, 0.05))

#Save both OR by SR plots
ggsave(RV_fin_change_title_SR, file = "plots/snails/RT/RT_fin_change_SR.pdf", height = 12, width = 12, dpi = 300)
ggsave(RV_fin_change_title_SP, file = "plots/snails/RT/RT_fin_change_SP.pdf", height = 12, width = 12, dpi = 300)
ggsave(fin_CSurv_OR_box, file = "plots/snails/RT/RT_survival_SR.pdf", height = 6, width = 6, dpi = 300)
ggsave(fin_CSurv_SP_box, file = "plots/snails/RT/RT_survival_SP.pdf", height = 6, width = 6, dpi = 300)

#Test for the effects of initial size across SP & OS----
length_ced <- RV_lm %>% 
  filter(SP == "CEDAR")

length_size <- lm(initL ~ OS, data = length_ced)
Anova(length_size)

grpMeans <- emmeans(length_size, ~ OS, data = length_ced)
pairs(grpMeans)


#Visualize the growth & var across blocks by SR----
ggplot(RV_lm, aes(Block, diff_l_i, group = SR, colour = SR)) +
  geom_point(alpha=0.3, position = position_jitterdodge(dodge.width = 0.3, jitter.width=0.05)) +
  stat_summary(fun=mean, geom="point", size = 3, position=position_dodge(0.3)) +
  stat_summary(fun.data = "mean_sd", geom = "errorbar", width = 0.2, size = 0.5,
               position=position_dodge(0.3)) +
  facet_wrap(~ OR) +
  labs(colour = "Source Region") +
  scale_colour_manual(values = c("skyblue", "coral")) +
  labs(x = "Block", y = "Change in SL (mm)") +
  theme_cowplot(16)


#Build linear mixed effects models----
#Fixed effects: SR, OR, and the interaction between SR & OR
#Random effects: Block (I only want the intercept to vary, hence 1|Block notation). Block is nested in OS.
#Note: As per Schielzeth and Nakagawa, I created a unique red ID for each SP by combining Block & OS, so that each SP will have 16 reps rather than 4 groups of 4 identical reps
#OS_Block is used as Block, nested within OS.
#I also included SP as a random effect, since it will contribute variance toward the results. 

#Use this resource for the nesting notation: https://bbolker.github.io/mixedmodels-misc/glmmFAQ.html

#I chose to model the interactions w/ initL when there was a significant interaction between initL & SR or OR, so I created a multiple regression model
#according to https://stats.stackexchange.com/questions/281528/dealing-with-model-assumption-violation-homogeneity-of-regression-coefficients
#If initL or interactions were non-significant (e.g. lmer_length_2), I dropped those terms from the model
lmer_length_1 <- lmer(diff_l ~ OR*SR + initL + (1|OS/OS_block) + (1|SP), data = RV_lm)
summary(lmer_length_1)

#Verify assumptions of model
plot(lmer_length_1)
visreg(lmer_length_1, "SR", by = "OR")

#Analyse mixed-effects model using type 3 anova
Anova(lmer_length_1, type = "III")

#Since there are positive interactions, use the following notation for the Tukey posthoc, with kenward-roger df method
grpMeans_length_1 <- emmeans(lmer_length_1, ~ OR*SR, data = RV_lm)
pairs(grpMeans_length_1, simple = list("OR", "SR"))

#Shell thickness: 
lmer_thick_1 <- lmer(diff_Th ~ OR*SR + initTh + (1|OS/OS_block) + (1|SP), data = RV_lm)
summary(lmer_thick_1)

#Verify assumptions of model (dispersal increases as the variables increase...)
plot(lmer_thick_1)
visreg(lmer_thick_1, "SR", by = "OR")

#Analyse mixed-effects model using anova & Tukey posthoc test with emmeans, with kenward-roger df method
Anova(lmer_thick_1, type = "III")

#Since there are positive interactions, use the following notation for the Tukey posthoc
grpMeans_thick_1 <- emmeans(lmer_thick_1, ~ OR*SR, data = RV_lm)
pairs(grpMeans_thick_1, simple = list("OR", "SR"))

#Shell weight: 
lmer_ShW_1 <- lmer(diff_ShW ~ OR*SR + initShW + (1|OS/OS_block) + (1|SP), data = RV_lm)
summary(lmer_ShW_1)

#Verify assumptions of model (dispersal increases as the variables increase...)
plot(lmer_ShW_1)
visreg(lmer_ShW_1, "SR", by = "OR")

#Analyse mixed-effects model using anova & Tukey posthoc test with emmeans, with kenward-roger df method
Anova(lmer_ShW_1, type = "III")

#Since there are positive interactions, use the following notation for the Tukey posthoc
grpMeans_ShW_1 <- emmeans(lmer_ShW_1, ~ OR*SR, data = RV_lm)
pairs(grpMeans_ShW_1, simple = list("OR", "SR"))

#Tissue weight
lmer_TiW_1 <- lmer(diff_TiW ~ OR*SR + initTiW + (1|OS/OS_block) + (1|SP), data = RV_lm)
summary(lmer_TiW_1)

#Verify assumptions of model (dispersal increases as the variables increase...)
plot(lmer_TiW_1)
visreg(lmer_TiW_1, "SR", by = "OR")

#Analyse mixed-effects model using anova & Tukey posthoc test with emmeans, with kenward-roger df method
Anova(lmer_TiW_1, type = "III")

#Since there are positive interactions, use the following notation for the Tukey posthoc
grpMeans_TiW_1 <- emmeans(lmer_TiW_1, ~ OR*SR, data = RV_lm)
pairs(grpMeans_TiW_1, simple = list("OR", "SR"))

#Survival: since these data are proportion, you have to run a generalized mixed-effects model, with the RV_survival df
lmer_surv_1 <- lmer(cumsurv ~ OR*SR +  (1|OS), data = RV_cumsurv_final) #<- removed 1|SP and block because singular fit
summary(lmer_surv_1)
#Verify assumptions of model (dispersal increases as the variables increase...)
plot(lmer_surv_1)
visreg(lmer_surv_1, "SR", by = "OR")

Anova(lmer_surv_1, type = "II")
Anova(lmer_surv_1, type = "III")

#Since there are no positive interactions, use the following notation for the Tukey posthoc
grpMeans_surv_1 <- emmeans(lmer_surv_1, ~ OR*SR, data = RV_lm)
pairs(grpMeans_surv_1, simple = list("OR", "SR"))

#Remove all the unneeded objects for survival analysis----
rm(length_OR_box, length_OR_me, RV_alive, RV_combined_OR_SR, RV_diff, test,
   RV_growth_block, RV_sum_OR, SG_OR_OS_points, ShW_OR_OS_points, 
   CSurv_OR_box, length_stage, thick_stage, thick_OR_OS_points, RV_cumsurv_block, RV_cumsurv_OR, 
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
                                                               ifelse(Stage == "Init" & OR == "Strait of Georgia", 0,
                                                                      ifelse(Stage == "Mid" & OR == "Strait of Georgia", 69,
                                                                             ifelse(Stage == "Final" & OR == "Strait of Georgia", 125, NA)))))))))) %>% 
  mutate(SR = ifelse(SR == "Calvert", "CALVERT", "STRAIT OF GEORGIA"))

  
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


#Visualize change in growth by initial size for each SP at each OS----
#Just tried it for length, can do with the other variables too & make a formula, but later... only if necessary!
plot_init <- function(df, x, y, fill.clr, lbl.y, lbl.fill) {
  init_plot <- ggplot(df, aes({{x}}, {{y}}, fill = {{fill.clr}}, colour = {{fill.clr}})) + 
    geom_point () + geom_smooth(method = lm, se = FALSE) +
    scale_colour_manual(values = c("coral", "coral3", "skyblue", "skyblue3")) +
    labs(x = "Initial size", y = lbl.y, fill = "Source Population", colour = "Source Population") +
    facet_wrap(~OS, nrow = 1) +
    theme_cowplot(16) + theme(strip.background = element_blank())
  return(init_plot)
}

init_length <- plot_init(RV_lm, initL, diff_l, SP, "Change in SL (mm)")
init_thickness <- plot_init(RV_lm, initTh, diff_Th, SP, "Change in ST (mm)")
init_ShW <- plot_init(RV_lm, initShW, diff_ShW, SP, "Change in ShW (g)")
init_TiW <- plot_init(RV_lm, initTiW, diff_TiW, SP, "Change in TiW (g)")
init_SG <- plot_init(RV_lm, initL, SG, SP, "LSG (mm)")

comb_init_figs <- plot_grid(init_length + theme(legend.position = "none", axis.title.x = element_blank()), 
                      get_legend(init_length),
                      init_thickness + theme(legend.position = "none", axis.title.x = element_blank()), 
                      NULL,
                      init_SG + theme(legend.position = "none", axis.title.x = element_blank()), 
                      NULL,
                      init_ShW + theme(legend.position = "none", axis.title.x = element_blank()), 
                      NULL,
                      init_TiW + theme(legend.position = "none", axis.title.x = element_blank()), 
                      NULL,
                      ncol = 2, nrow = 5, axis = "lb", align = "hv", rel_widths = c(1,0.2))

xaxistitle <- ggdraw() + draw_label("Initial size", fontface = "plain", x = 0.43, hjust = 0, size = 16)
comb_init_figs <- plot_grid(comb_init_figs, xaxistitle, ncol = 1, rel_heights = c(1, 0.05))

ggsave(comb_init_figs, file = "plots/supp_figs/FigS6_initial_size.pdf", height = 12, width = 12, dpi = 300)

#Visualize the change in RVs grouped by OS & SP----
plot_OS_RT_box <- function(df, x, y, grp, fill.values, clr.values, lbl.y){
  ggplot(df, aes({{x}}, {{y}}, fill = {{grp}}, colour = {{grp}})) + 
    geom_boxplot(colour = "black", varwidth = TRUE, alpha = 0.8) +
    geom_point(size = 3, alpha=0.5, position = position_jitterdodge(dodge.width = 0.7, jitter.width=0.3))  +
    scale_fill_manual(values = fill.values) +
    scale_colour_manual(values = clr.values) +
    labs(y = lbl.y) +
    theme_cowplot(16)
}

#Create boxplots with change in growth 
length_OS_box <- plot_OS_RT_box(RV_lm_block, OS, meandiff_l, SP, c("coral", "coral3", "skyblue", "skyblue3"), c("coral", "coral3", "skyblue", "skyblue3"), "Change in SL (mm)") +
  labs(colour = "Source Region", fill = "Source Region")
thick_OS_box <- plot_OS_RT_box(RV_lm_block, OS, meandiff_Th, SP, c("coral", "coral3", "skyblue", "skyblue3"), c("coral", "coral3", "skyblue", "skyblue3"), "Change in ST (mm)")
ShW_OS_box <- plot_OS_RT_box(RV_lm_block, OS, meandiff_ShW, SP, c("coral", "coral3", "skyblue", "skyblue3"), c("coral", "coral3", "skyblue", "skyblue3"), "Change in ShW (g)")
TiW_OS_box <- plot_OS_RT_box(RV_lm_block, OS, meandiff_TiW, SP, c("coral", "coral3", "skyblue", "skyblue3"), c("coral", "coral3", "skyblue", "skyblue3"), "Change in TiW (g)")
SG_OS_box <- plot_OS_RT_box(RV_lm_block, OS, mean_SG, SP, c("coral", "coral3", "skyblue", "skyblue3"), c("coral", "coral3", "skyblue", "skyblue3"), "Change in LSG (mm)")
CSurv_OS_box <- plot_OS_RT_box(RV_cumsurv_final, OS, cumsurv, SP, c("coral", "coral3", "skyblue", "skyblue3"), c("coral", "coral3", "skyblue", "skyblue3"), "Survival (%)")

#Create boxplots withc hange in growth standardized by initial size
length_OS_box_i <- plot_OS_RT_box(RV_lm_block, OS, meandiff_l_i, SP, c("coral", "coral3", "skyblue", "skyblue3"), c("coral", "coral3", "skyblue", "skyblue3"), "S change in SL (mm)") +
  labs(colour = "Source Region", fill = "Source Region")
thick_OS_box_i <- plot_OS_RT_box(RV_lm_block, OS, meandiff_Th_i, SP, c("coral", "coral3", "skyblue", "skyblue3"), c("coral", "coral3", "skyblue", "skyblue3"), "S change in ST (mm)")
ShW_OS_box_i <- plot_OS_RT_box(RV_lm_block, OS, meandiff_ShW_i, SP, c("coral", "coral3", "skyblue", "skyblue3"), c("coral", "coral3", "skyblue", "skyblue3"), "S change in ShW (g)")
TiW_OS_box_i <- plot_OS_RT_box(RV_lm_block, OS, meandiff_TiW_i, SP, c("coral", "coral3", "skyblue", "skyblue3"), c("coral", "coral3", "skyblue", "skyblue3"), "S change in TiW (g)")
SG_OS_box_i <- plot_OS_RT_box(RV_lm_block, OS, meanSG_i, SP, c("coral", "coral3", "skyblue", "skyblue3"), c("coral", "coral3", "skyblue", "skyblue3"), "S change in LSG (mm)")
CSurv_OS_box <- plot_OS_RT_box(RV_cumsurv_final, OS, cumsurv, SP, c("coral", "coral3", "skyblue", "skyblue3"), c("coral", "coral3", "skyblue", "skyblue3"), "Survival (%)")

RV_combined_OS_box <- plot_grid(length_OS_box + theme(legend.position = "none", axis.text.x = element_blank(), axis.title.x = element_blank()),
                                thick_OS_box + theme(legend.position = "none", axis.text.x = element_blank(), axis.title.x = element_blank()), 
                                SG_OS_box + theme(legend.position = "none", axis.text.x = element_blank(), axis.title.x = element_blank()),
                                get_legend(length_OS_box),
                                ShW_OS_box + theme(legend.position = "none", axis.title.x = element_blank()), 
                                TiW_OS_box + theme(legend.position = "none", axis.title.x = element_blank()),
                                CSurv_OS_box + theme(legend.position = "none", axis.title.x = element_blank()),
                                NULL,
                                ncol = 4, nrow = 2, rel_widths= c(1, 1, 1, 0.3), axis = "lb", align = "hv")

RV_combined_OS_box_i <- plot_grid(length_OS_box_i + theme(legend.position = "none", axis.text.x = element_blank(), axis.title.x = element_blank()),
                                  thick_OS_box_i + theme(legend.position = "none", axis.text.x = element_blank(), axis.title.x = element_blank()), 
                                  SG_OS_box_i + theme(legend.position = "none", axis.text.x = element_blank(), axis.title.x = element_blank()),
                                  get_legend(length_OS_box_i),
                                  ShW_OS_box_i + theme(legend.position = "none", axis.title.x = element_blank()), 
                                  TiW_OS_box_i + theme(legend.position = "none", axis.title.x = element_blank()),
                                  CSurv_OS_box + theme(legend.position = "none", axis.title.x = element_blank()),
                                  NULL,
                                  ncol = 4, nrow = 2, rel_widths= c(1, 1, 1, 0.3), axis = "lb", align = "hv")

xaxistitle_OS <- ggdraw() + draw_label("Outplant Site", fontface = "plain", x = 0.43, hjust = 0, size = 16)
RV_combined_OS_box_title <- plot_grid(RV_combined_OS_box, xaxistitle_OS, ncol = 1, rel_heights = c(1, 0.05))
RV_combined_OS_box_i_title <- plot_grid(RV_combined_OS_box_i, xaxistitle_OS, ncol = 1, rel_heights = c(1, 0.05))

#Save both OR by SR plots
ggsave(RV_combined_OS_box_title, file = "plots/snails/RT/RV_OS_box.pdf", height = 8, width = 17, dpi = 300)
ggsave(RV_combined_OS_box_i_title, file = "plots/snails/RT/RV_OS_box_i.pdf", height = 8, width = 17, dpi = 300)

#Old code for mean & error plots & standardized growth----
#Create ME plots with change in growth for OR
plot_OR_RT_me <- function(df, x, y, grp, clr.values, lbl.y){
  ggplot(df, aes({{x}}, {{y}}, group = {{grp}}, colour = {{grp}})) + 
    geom_point(alpha=0.3, position = position_jitterdodge(dodge.width = 0.3, jitter.width=0.05)) +
    stat_summary(fun=mean, geom="point", size = 3, position=position_dodge(0.3)) +
    stat_summary(fun = mean, geom = "line", size = 0.8, position=position_dodge(0.3), alpha = 0.5) +
    stat_summary(fun.data = "mean_se", geom = "errorbar", width = 0.2, size = 0.5,
                 position=position_dodge(0.3)) +
    scale_colour_manual(values = clr.values) +
    labs(y = lbl.y) +
    theme_cowplot(16)
}
length_OR_me <- plot_OR_RT_me(RV_lm_block, OR, meandiff_l, SR, c("skyblue", "coral"), "Change in SL (mm)") +
  labs(colour = "Source Region")
thick_OR_me <- plot_OR_RT_me(RV_lm_block, OR, meandiff_Th, SR, c("skyblue", "coral"), "Change in ST (mm)")
ShW_OR_me <- plot_OR_RT_me(RV_lm_block, OR, meandiff_ShW, SR, c("skyblue", "coral"), "Change in ShW (g)")
TiW_OR_me <- plot_OR_RT_me(RV_lm_block, OR, meandiff_TiW, SR, c("skyblue", "coral"), "Change in TiW (g)")
SG_OR_me <- plot_OR_RT_me(RV_lm_block, OR, mean_SG, SR, c("skyblue", "coral"), "Change in LSG (mm)")
CSurv_OR_me <- plot_OR_RT_me(RV_cumsurv_final, OR, cumsurv, SR, c("skyblue", "coral"), "Survival (%)")

#Create ME plots with standardized change in growth
length_OR_me_i <- plot_OR_RT_me(RV_lm_block, OR, meandiff_l_i, SR, c("skyblue", "coral"), "S change in SL (mm)") +
  labs(colour = "Source Region")
thick_OR_me_i <- plot_OR_RT_me(RV_lm_block, OR, meandiff_Th_i, SR, c("skyblue", "coral"), "S change in ST (mm)")
ShW_OR_me_i <- plot_OR_RT_me(RV_lm_block, OR, meandiff_ShW_i, SR, c("skyblue", "coral"), "S change in ShW (g)")
TiW_OR_me_i <- plot_OR_RT_me(RV_lm_block, OR, meandiff_TiW_i, SR, c("skyblue", "coral"), "S change in TiW (g)")
SG_OR_me_i <- plot_OR_RT_me(RV_lm_block, OR, meanSG_i, SR, c("skyblue", "coral"), "S change in LSG (mm)")


#Create boxplots withchange in growth standardized by initial size
length_OR_box_i <- plot_OR_RT_box(RV_lm_block, OR, meandiff_l_i, SR, c("skyblue", "coral"), c("skyblue3", "coral3"), "S change in SL (mm)") +
  labs(colour = "Source Region", fill = "Source Region")
thick_OR_box_i <- plot_OR_RT_box(RV_lm_block, OR, meandiff_Th_i, SR, c("skyblue", "coral"), c("skyblue3", "coral3"), "S change in ST (mm)")
ShW_OR_box_i <- plot_OR_RT_box(RV_lm_block, OR, meandiff_ShW_i, SR, c("skyblue", "coral"), c("skyblue3", "coral3"), "S change in ShW (g)")
TiW_OR_box_i <- plot_OR_RT_box(RV_lm_block, OR, meandiff_TiW_i, SR, c("skyblue", "coral"), c("skyblue3", "coral3"), "S change in TiW (g)")
SG_OR_box_i <- plot_OR_RT_box(RV_lm_block, OR, meanSG_i, SR, c("skyblue", "coral"), c("skyblue3", "coral3"), "S change in LSG (mm)")

RV_combined_OR_me <- plot_grid(length_OR_me + theme(legend.position = "none", axis.text.x = element_blank(), axis.title.x = element_blank()),
                               thick_OR_me + theme(legend.position = "none", axis.text.x = element_blank(), axis.title.x = element_blank()), 
                               SG_OR_me + theme(legend.position = "none", axis.text.x = element_blank(), axis.title.x = element_blank()),
                               get_legend(length_OR_me),
                               ShW_OR_me + theme(legend.position = "none", axis.title.x = element_blank()), 
                               TiW_OR_me + theme(legend.position = "none", axis.title.x = element_blank()),
                               CSurv_OR_me + theme(legend.position = "none", axis.title.x = element_blank()),
                               NULL,
                               ncol = 4, nrow = 2, rel_widths= c(1, 1, 1, 0.3), axis = "lb", align = "hv")

RV_combined_OR_me_i <- plot_grid(length_OR_me_i + theme(legend.position = "none", axis.text.x = element_blank(), axis.title.x = element_blank()),
                                 thick_OR_me_i + theme(legend.position = "none", axis.text.x = element_blank(), axis.title.x = element_blank()), 
                                 SG_OR_me_i + theme(legend.position = "none", axis.text.x = element_blank(), axis.title.x = element_blank()),
                                 get_legend(length_OR_me_i),
                                 ShW_OR_me_i + theme(legend.position = "none", axis.title.x = element_blank()), 
                                 TiW_OR_me_i + theme(legend.position = "none", axis.title.x = element_blank()),
                                 CSurv_OR_me + theme(legend.position = "none", axis.title.x = element_blank()),
                                 NULL,
                                 ncol = 4, nrow = 2, rel_widths= c(1, 1, 1, 0.3), axis = "lb", align = "hv")


RV_combined_OR_box_i <- plot_grid(length_OR_box_i + theme(legend.position = "none", axis.text.x = element_blank(), axis.title.x = element_blank()),
                                  thick_OR_box_i + theme(legend.position = "none", axis.text.x = element_blank(), axis.title.x = element_blank()), 
                                  SG_OR_box_i + theme(legend.position = "none", axis.text.x = element_blank(), axis.title.x = element_blank()),
                                  get_legend(length_OR_box_i),
                                  ShW_OR_box_i + theme(legend.position = "none", axis.title.x = element_blank()), 
                                  TiW_OR_box_i + theme(legend.position = "none", axis.title.x = element_blank()),
                                  CSurv_OR_box + theme(legend.position = "none", axis.title.x = element_blank()),
                                  NULL,
                                  ncol = 4, nrow = 2, rel_widths= c(1, 1, 1, 0.3), axis = "lb", align = "hv")

RV_combined_OR_me_title <- plot_grid(RV_combined_OR_me, xaxistitle_OR, ncol = 1, rel_heights = c(1, 0.05))
RV_combined_OR_me_i_title <- plot_grid(RV_combined_OR_me_i, xaxistitle_OR, ncol = 1, rel_heights = c(1, 0.05))
RV_combined_OR_box_i_title <- plot_grid(RV_combined_OR_box_i, xaxistitle_OR, ncol = 1, rel_heights = c(1, 0.05))

ggsave(RV_combined_OR_me_title, file = "plots/snails/RT/RV_OR_me.pdf", height = 8, width = 17, dpi = 300)
ggsave(RV_combined_OR_me_i_title, file = "plots/snails/RT/RV_OR_me_i.pdf", height = 8, width = 17, dpi = 300)
ggsave(RV_combined_OR_box_i_title, file = "plots/snails/RT/RV_OR_box_i.pdf", height = 8, width = 17, dpi = 300)



#Remove all final variables----
rm(lmer_length, RV_base, RV_diff_1, RV_lm, RV_lm_init, RV_survival, RV_survival_glm, ID_ostrina, remove)

