#Script to analyze snail response variables from the mesocosm experiment
#Datasheets include the following measurements:
#July 1st (initial): length, thickness, total weight
#August 2nd (mid): submerged weight, total weight
#Sept 2nd (final): length, thickness, total weight, submerged weight

#Load packages----
pkgs <- c("tidyverse", "lubridate", "car", "visreg", "cowplot", "survminer", "survival",
          "emmeans", "lme4")
lapply(pkgs, library, character.only = TRUE)
rm(pkgs)

#Load csvs ----
#March 2nd: I saved the meso_collated from the Mesocosm folder into the CH 2 Analysis folder as a csv. If you make any changes to the base df, replace the one in git folder
#March 12th: CO24 was outlier, typo in raw datasheet, changed fmor 3.42 to 2.42 in csv
#March 4th: I saved the survival datasheet
#March 1th: I saved feeding rate datasheet from the 'Weekly feeding rate_Oct 23.xlsx'in the Meso > feeding rate data sheets folder
meso_base <- read.csv("data/snail_RVs/meso_collated.csv")
meso_survival <- read.csv("data/snail_RVs/meso_collated_survival.csv")
meso_feed <- read.csv("data/snail_RVs/meso_percap_feeding_rate.csv")

#Clean growth, feeding & survival dataframes----
#Add a Source Region column
#Ensure that all treatments line up with the correct tank
#Classify Tank 16 (which was supposed to be a 12) as a 15 degree treatment because you could never really bring that tank's temp down due to equipment issues
#Remove the Died & Notes column
#Rename column headers
#Remove the tanks that experienced chiller failures & 100% mortality within first experimental days: 3, 5, 6 :( :(
#Remove the lowered pH treatment tanks: 17, 20, 21, 24, 18, 19, 22, 23
#Estimate shell weight (ShW) & tissue weight (TiW) based on the following submerged regressions for each population where x is SW (submerged weight):
#Pruth	y = 1.5889x + 0.1392
#Kwakshua	y = 1.5958x + 0.0646
#Cedar	y = 1.61x + 0.0266
#Heron	y = 1.6104x - .1292
#Calculate TiW (tissue weight) based on Shell weight and Total weight
#Remove any rows with NAs in the L, T, SG, TiW & ShW columns (these died, but I kept them in my original datasheet)
#Separate the Treat into Temp & pH columns

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

tanks_remove <- c(3, 5, 6, 17, 20, 21, 24, 18, 19, 22, 23)

meso_clean <- meso_base %>% 
  filter(!Tank %in% tanks_remove) %>% 
  filter(!is.na(Tank)) %>% 
  rename(L = Length, Th = Thickness, SG = Linear_shell_growth, TW = Total_weight, SW = Submerged_weight) %>% 
  mutate(ShW = ifelse(SP == "Cedar", cedar_reg(SW), 
                      ifelse(SP == "Heron", heron_reg(SW),
                             ifelse(SP == "Kwak", kwak_reg(SW),
                                    ifelse(SP == "Pruth", pruth_reg(SW), NA)))),
         SR = as.factor(ifelse(SP == "Cedar" | SP == "Heron", "Strait of Georgia", "Central Coast")),
         SP = as.factor(SP),
         Stage = as.factor(Stage),
         Treat = as.factor(ifelse(Tank == 9 | Tank == 12, "12",
                                  ifelse(Tank == 4 | Tank == 7 | Tank == 11 | Tank == 14 | Tank == 16, "15",
                                                ifelse(Tank == 1 | Tank == 10 | Tank == 13, "19",
                                                       ifelse(Tank == 2 | Tank == 8 | Tank == 15, "22", NA))))), 
         TiW = TW-ShW) %>% 
  filter_at(vars(L,Th, SG, ShW, TiW), any_vars(!is.na(.))) %>% 
  select(Stage, SR, SP, Tank, Treat, ID, L, Th, ShW, TiW, SG)

#You have to add the TiW & ShW values for each ID to the init ID so that you only have 1 row/snail (otherwise it messes up the sample sizes later
meso_clean_i <- meso_clean %>% 
  filter(Stage == "Init") %>% 
  select(!c(TiW, ShW))
meso_clean_m <- meso_clean %>% 
  filter(Stage == "Mid") %>% 
  select(ID, TiW, ShW)
meso_clean_f <- meso_clean %>% 
  filter(Stage == "Final") %>% 
  rename(L_final = L, Th_final = Th, ShW_final = ShW, TiW_final = TiW) %>% 
  select(Stage, ID, L_final, Th_final, ShW_final, TiW_final, SG)

meso_clean_1 <- left_join(meso_clean_i, meso_clean_m, by = "ID") %>% 
  left_join(meso_clean_f, by = "ID") %>% 
  droplevels

#For some reason CB44 was duplicated twice and has 2 incomplete rows --> remove rows 64-66
meso_clean_1 <- meso_clean_1[-c(64:66), ]

#Now we have a dataframe with rows for all the right IDs, but we need to switch it into a long format keeping the IDs. 
#SO: split it into 2 dataframes again, and then rbind them
meso_clean_2 <- meso_clean_1 %>% 
  select(SR, SP, Tank, Treat, ID, Stage.y, L_final, Th_final, ShW_final, TiW_final, SG.y) %>% 
  rename(L = L_final, Th = Th_final, ShW = ShW_final, TiW = TiW_final, Stage = Stage.y, SG = SG.y) %>%
  mutate(Alive = ifelse(is.na(Stage), 0, 1),
         Stage = ifelse(is.na(Stage), "Final", "Final")) %>% 
  select(Stage, SR, SP, Tank, Treat, ID, L, Th, ShW, TiW, SG, Alive)
  
meso_clean <- meso_clean_1 %>% 
  select(Stage.x, SR, SP, Tank, Treat, ID, L, Th, ShW, TiW, SG.x) %>% 
  rename(Stage = Stage.x, SG = SG.x) %>% 
  mutate(Alive = 1) %>% 
  rbind(meso_clean_2)

#Clean the feeding rate data
meso_food_clean <-meso_feed %>% 
  filter(!Tank %in% tanks_remove) %>% 
  filter(!is.na(Tank)) %>% 
  select(!c(Notes, Treatment)) %>% 
  mutate(SP = as.factor(ifelse(SP == "C", "Cedar",
                               ifelse(SP == "H", "Heron", 
                                      ifelse(SP == "K", "Kwak",
                                             ifelse(SP == "P", "Pruth", NA))))),
         Date = as.Date(Date, format = "%m-%d-%Y"),
         tank_sp = paste(Tank, SP, sep = "_"),
         Treat = as.factor(ifelse(Tank == 9 | Tank == 12, "12",
                                  ifelse(Tank == 4 | Tank == 7 | Tank == 11 | Tank == 14 | Tank == 16, "15",
                                                ifelse(Tank == 1 | Tank == 10 | Tank == 13, "19",
                                                       ifelse(Tank == 2 | Tank == 8 | Tank == 15, "22", NA)))))) %>% 
  mutate(SR = ifelse(SP == "Cedar" | SP == "Heron", "Strait of Georgia", "Central Coast")) %>% 
  select(Date, Tank, SR, SP, tank_sp, Treat, Per_cap)

#Now summarize the average feeding rate for each tank_sp
meso_food_tank <- meso_food_clean %>% 
  group_by(Treat, Tank, SR, SP, tank_sp) %>% 
  summarize(meanPer_cap = mean(Per_cap)) %>% 
  ungroup()

#Calculate the proportion of surviving snails based on the start and end sample size of each SP in each tank
#Calculate the number of snails in each tank at the beginning and end
meso_clean_surv <- meso_clean %>% 
  group_by(Stage, SR, SP, Treat, Tank) %>% 
  summarize(n_snl = sum(Alive)) %>% 
  ungroup()

#Calculate the proportion of surviving snails at the end of the experiment & separate treatment into the temp & pH columns
meso_clean_surv_1 <- meso_clean_surv %>% 
  arrange(SR, SP, Treat, Tank, Stage) %>%
  mutate(cumsurv = ifelse(Stage == "Init", n_snl/n_snl,
                          ifelse(Stage == "Final", n_snl/lag(n_snl, default = n_snl[1]), NA)),
         cumsurv = 100*cumsurv) %>% 
  arrange(SR, SP, Treat, Tank, Stage)

meso_clean_surv <- meso_clean_surv_1 %>% 
  filter(Stage == "Final") 
    
#Calculate the average & SD of metrics to visualize across the 2 time periods ----
#Note: Because submerged weight was only collected in the middle of the experiment, changes to tissue & shell weight are only reflective of
#the growth between mid & final stages (reference this in figure captions?)

#First check for outliers caused by high mortality within block that only leaves 1 individual left (which often means the mean - mean values are disproportionate)
#Remove the following outliers due to high mortality causing outliers in data
#The only 2 surviving snails in Tank 15 were abnormally large --> average values for that tank pull up the length & distort the Treatment SD for Cedar --> remove from visualization
remove <- c("Cedar_15_Final")

meso_growth_tank <- meso_clean %>% 
  unite(unique_ID, c(SP, Tank, Stage), sep = "_", remove = FALSE) %>% 
  filter(!unique_ID %in% remove) %>% 
  group_by(Stage, SP, Treat, Tank) %>% 
  summarize(meanL = mean(L, na.rm = TRUE), sdL = sd(L, na.rm = TRUE),
            meanTh = mean(Th, na.rm = TRUE), sdTh = sd(Th, na.rm = TRUE),
            meanShW = mean(ShW, na.rm = TRUE), sdShW = sd(ShW, na.rm = TRUE),
            meanTiW = mean(TiW, na.rm = TRUE), sdTiW = sd(TiW, na.rm = TRUE),
            meanSG = mean(SG, na.rm = TRUE), sdSG = sd(SG, na.rm = TRUE), n = n()) %>% 
  ungroup()

#Calculate sample size for stage figure (i.e. number of tanks/treatment at each stage)
meso_growth_tank_sample <- meso_growth_tank %>% 
  group_by(Stage, Treat, SP) %>% 
  summarize(n_sample = n())

#Mid Calvert Pruth 22 only has 1 snail --> no SD
#Final Calvert Pruth 2 only has 1 snail --> no SD
#Final Calvert Pruth 22 only has 1 snail --> no SD

#Visualize the RVs over time by SP for temp exp----
plot_temp_stage <- function(df, x, y, grp, clr.values, lbl.y){
  temp_plot <- ggplot(df, aes({{x}}, {{y}}, group = {{grp}}, colour = {{grp}})) +
    stat_summary(fun=mean, geom="point", size = 3, position=position_dodge(0.3)) +
    stat_summary(fun = mean, geom = "line", size = 0.8, position=position_dodge(0.3), alpha = 0.5) +
    stat_summary(fun.data = "mean_se", geom = "errorbar", width = 0.2, size = 0.5,
                 position=position_dodge(0.3)) +
    facet_wrap(~ Treat, ncol = 4) +
    scale_colour_manual(values = clr.values) +
    labs(y = lbl.y) +
    theme_cowplot(16) + theme(strip.text = element_blank())
  return(temp_plot)
}

length_stage_temp <- plot_temp_stage(meso_growth_tank, Stage, meanL, SP, c("coral", "coral3", "skyblue", "skyblue3"), "SL (mm)") + 
  labs(colour = "Source Population") + theme(strip.background = element_blank(), strip.text = element_text(size = 16))
thick_stage_temp <- plot_temp_stage(meso_growth_tank, Stage, meanTh, SP, c("coral", "coral3", "skyblue", "skyblue3"), "ST (mm)")
ShW_stage_temp <- plot_temp_stage(meso_growth_tank, Stage, meanShW, SP, c("coral", "coral3", "skyblue", "skyblue3"), "ShW (g)")
TiW_stage_temp <- plot_temp_stage(meso_growth_tank, Stage, meanTiW, SP, c("coral", "coral3", "skyblue", "skyblue3"), "TiW (g)")
SG_stage_temp <- plot_temp_stage(meso_growth_tank, Stage, meanSG, SP, c("coral", "coral3", "skyblue", "skyblue3"), "LSG (mm)")
Surv_stage_temp <- plot_temp_stage(meso_clean_surv_1, Stage, cumsurv, SP, c("coral", "coral3", "skyblue", "skyblue3"), "% Survival")

meso_stage_temp <- plot_grid(length_stage_temp + theme(legend.position = "none",
                                           axis.text.x = element_blank(), axis.title.x = element_blank()), 
                      get_legend(length_stage_temp),
                      thick_stage_temp + theme(legend.position = "none", 
                                          axis.text.x = element_blank(), axis.title.x = element_blank()), 
                      NULL,
                      SG_stage_temp + theme(legend.position = "none", 
                                       axis.text.x = element_blank(), axis.title.x = element_blank()), 
                      NULL,
                      ShW_stage_temp + theme(legend.position = "none",
                                        axis.text.x = element_blank(), axis.title.x = element_blank()), 
                      NULL,
                      TiW_stage_temp + theme(legend.position = "none", 
                                             axis.text.x = element_blank(), axis.title.x = element_blank()), 
                      NULL,
                      Surv_stage_temp + theme(legend.position = "none", axis.title.x = element_blank()), 
                      NULL,
                      ncol = 2, nrow = 6, axis = "lb", align = "hv", rel_widths = c(1,0.2))

xaxistitle <- ggdraw() + draw_label("Stage", fontface = "plain", x = 0.43, hjust = 0, size = 16)
meso_growth_temp_comb <- plot_grid(meso_stage_temp, xaxistitle, ncol = 1, rel_heights = c(1, 0.05))

#Make sure in your caption for this figure you reference that you're visualizing the mean metrics across blocks with sites pooled (i.e. n = 7-8)
ggsave(meso_growth_temp_comb, file = "plots/supp_figs/FigS7_meso_stage_temp.pdf", height = 14, width = 12, dpi = 300)

#Create new dataframe for growth analysis with init size, change in growth metrics, and fixed & random effects for every snail----
#Calculate the difference in growth (this time by ID rather than block, as in previous code), and change labels to initL etc
meso_lm <- meso_clean %>% 
  arrange(ID, Stage) %>% 
  group_by(ID) %>% 
  mutate(diff_l = L - lag(L, default= L[1]),
         diff_Th = Th - lag(Th, default = Th[1]),
         diff_ShW = ShW - lag(ShW, default = ShW[1]),
         diff_TiW = TiW - lag(TiW, default = TiW[1])) %>% 
  subset(Stage == "Final") %>% 
  select(Stage, SR, SP, Treat, Tank, ID, SG, diff_l, diff_Th, diff_ShW, diff_TiW) %>%
  ungroup()

#Gather initial sizes for covariates in models
meso_lm_init <- meso_clean %>% 
  subset(Stage == "Init") %>% 
  select(ID, initL = L, initTh = Th, initShW = ShW, initTiW = TiW)

meso_lm_fin <- meso_clean %>% 
  subset(Stage == "Final") %>% 
  select(ID, finL = L, finTh = Th, finShW = ShW, finTiW = TiW)

#Merge 2 datasets by ID
meso_lm <- left_join(meso_lm, meso_lm_init, by = "ID")  %>% 
  left_join(meso_lm_fin, by = "ID")

#Now create a new dataset with the change in growth summarized by block
meso_lm_block <- meso_lm %>% 
  group_by(SR, SP, Treat, Tank) %>% 
  summarize(meandiff_l = mean(diff_l, na.rm = TRUE),
            meandiff_Th = mean(diff_Th, na.rm = TRUE),
            meandiff_ShW = mean(diff_ShW, na.rm = TRUE),
            meandiff_TiW = mean(diff_TiW, na.rm = TRUE),
            mean_SG = mean(SG, na.rm = TRUE),
            meanfinL = mean(finL, na.rm = TRUE),
            meanfinTh = mean(finTh, na.rm = TRUE),
            meanfinShW = mean(finShW, na.rm = TRUE),
            meanfinTiW = mean(finTiW, na.rm = TRUE)) %>% 
  mutate(tank_sp = paste(Tank, SP, sep = "_")) %>% 
  ungroup()

#Add on the survival & feeding rate in each tank from the meso_clean_surv df
meso_clean_surv_1 <- meso_clean_surv %>% 
  mutate(tank_sp = paste(Tank, SP, sep = "_")) %>% 
  select(tank_sp, cumsurv)

meso_food_1 <- meso_food_clean %>% 
  select(tank_sp, Per_cap) %>% 
  group_by(tank_sp) %>% 
  summarize(meanPer_cap = mean(Per_cap))

meso_lm_block <- meso_lm_block %>% 
  left_join(meso_clean_surv_1, by = "tank_sp") %>% 
  left_join(meso_food_1, by = "tank_sp") %>% 
  select(!tank_sp)

#Summarize the sample size for each treatment for your figure captions
meso_temp_sampl <- meso_lm_block %>% 
  group_by(SP, Treat) %>% 
  summarize(n_tanks = n())

#Visualize change in growth across treatments grouped by SR for temp exp----
#The datapoints being visualized are each tank  within each treatment for each SR :) The correct unit of replication! 
plot_temp_box <- function(df, x, y, grp, fill.values, clr.values, lbl.y){
  ggplot(df, aes({{x}}, {{y}}, fill = {{grp}}, colour = {{grp}})) + 
    geom_boxplot(colour = "black", varwidth = TRUE, alpha = 0.8) +
    geom_point(size = 3, alpha=0.5, position = position_jitterdodge(dodge.width = 0.6, jitter.width=0.3))  +
    scale_fill_manual(values = fill.values) +
    scale_colour_manual(values = clr.values) +
    labs(y = lbl.y) +
    theme_cowplot(16)
}

length_temp_SR_box <- plot_temp_box(meso_lm_block, Treat, meandiff_l, SR, c("skyblue", "coral"), c("skyblue3", "coral3"), "Shell length growth (mm)") + 
  labs(colour = "Source Region", fill = "Source Region") + draw_plot_label("(e)", 0.4, 11.5, fontface = "plain")
thick_temp_SR_box <- plot_temp_box(meso_lm_block, Treat, meandiff_Th, SR, c("skyblue", "coral"), c("skyblue3", "coral3"), "Change in shell lip thickness (mm)") +
  draw_plot_label("(f)", 0.4, 0.5, fontface = "plain")
ShW_temp_SR_box <- plot_temp_box(meso_lm_block, Treat, meandiff_ShW, SR, c("skyblue", "coral"), c("skyblue3", "coral3"), "Shell weight growth (g)")+
  draw_plot_label("(d)", 0.4, 0.7, fontface = "plain")
TiW_temp_SR_box <- plot_temp_box(meso_lm_block, Treat, meandiff_TiW, SR, c("skyblue", "coral"), c("skyblue3", "coral3"), "Tissue weight growth (g)")+
  draw_plot_label("(c)", 0.4, 1.4, fontface = "plain")
SG_temp_SR_box <- plot_temp_box(meso_lm_block, Treat, mean_SG, SR, c("skyblue", "coral"), c("skyblue3", "coral3"), "Change in LSG (mm)")
Feed_temp_SR_box <- plot_temp_box(meso_lm_block, Treat, meanPer_cap, SR, c("skyblue", "coral"), c("skyblue3", "coral3"), "Per capita weekly feeding rate")+
  draw_plot_label("(b)", 0.4, 1.4, fontface = "plain")
Surv_temp_SR_box <- plot_temp_box(meso_lm_block, Treat, cumsurv, SR, c("skyblue", "coral"), c("skyblue3", "coral3"), "Survival (%)")+
  draw_plot_label("(a)", 0.4, 100, fontface = "plain")

meso_temp_comb_SR_box <- plot_grid(Surv_temp_SR_box + theme(legend.position = "none", axis.text.x = element_blank(), axis.title.x = element_blank()),
                                   Feed_temp_SR_box + theme(legend.position = "none", axis.text.x = element_blank(), axis.title.x = element_blank()), 
                                   TiW_temp_SR_box + theme(legend.position = "none", axis.text.x = element_blank(),  axis.title.x = element_blank()), 
                                  get_legend(length_temp_SR_box),
                                  ShW_temp_SR_box + theme(legend.position = "none", axis.title.x = element_blank()),
                                  length_temp_SR_box + theme(legend.position = "none", axis.title.x = element_blank()),
                                  thick_temp_SR_box + theme(legend.position = "none", axis.title.x = element_blank()),
                                  NULL,
                                  ncol = 4, nrow = 2, rel_widths = c(1,1,1,0.35), axis = "lb", align = "hv")

xaxistitle_treat <- ggdraw() + draw_label("Temperature °C", fontface = "plain", x = 0.5, hjust = 0, size = 16)
meso_temp_comb_title_SR_box <- plot_grid(meso_temp_comb_SR_box, xaxistitle_treat, ncol = 1, rel_heights = c(1, 0.05))

ggsave(meso_temp_comb_title_SR_box, file = "plots/snails/meso/Fig_2_meso_temp_SR_box.pdf", height = 8, width = 17, dpi = 300)

#Test whether initial size differs across tanks----
initL_aov <- lm(initL ~ Tank + Treat, data = meso_lm)
Anova(initL_aov)
initTh_aov <- lm(initTh ~ Tank + Treat, data = meso_lm)
Anova(initTh_aov)
initTiW_aov <- lm(initTiW ~ Tank + Treat, data = meso_lm)
Anova(initTiW_aov)
initShW_aov <- lm(initShW ~ Tank + Treat, data = meso_lm)
Anova(initShW_aov)

rm(initL_aov, initTh_aov, initTiW_aov, initShW_aov)

#There is no significant difference in starting size across tanks or treatment

#Build linear mixed effects models for temp exp----
#Fixed effects: SR, Treat & intxn
#Random effects: Tank & Sp (1|Tank), (1|SP)
lmer_length_1 <- lmer(diff_l ~ SR*Treat + initL + (1|Tank) + (1|SP), data = meso_lm)
summary(lmer_length_1)

#Verify assumptions of model
plot(lmer_length_1)
visreg(lmer_length_1, "SR", by = "Treat")
visreg(lmer_length_1, "initL", by = "SR", overlay = TRUE)
visreg(lmer_length_1, "initL", by = "Treat", overlay = TRUE)

#Analyse mixed-effects model using anova & Tukey posthoc test with emmeans, with kenward-roger df method
Anova(lmer_length_1, type = "III")

#Since there are significant interactions, use the following notation for the Tukey posthoc
grpMeans_length_1 <- emmeans(lmer_length_1, ~ SR*Treat, data = meso_lm)
pairs(grpMeans_length_1, simple = list("SR", "Treat"))

#Shell thickness: 
lmer_thick_1 <- lmer(diff_Th ~ SR*Treat + initTh + (1|Tank) + (1|SP), data = meso_lm) 
summary(lmer_thick_1)

#Verify assumptions of model
plot(lmer_thick_1)
visreg(lmer_thick_1, "SR", by = "Treat")
visreg(lmer_thick_1, "iniTh", by = "SR", overlay = TRUE)
visreg(lmer_thick_1, "initTh", by = "Treat", overlay = TRUE)

#Analyse mixed-effects model using anova & Tukey posthoc test with emmeans, with kenward-roger df method
Anova(lmer_thick_1, type = "III")

#Since there are significant interactions, use the following notation for the Tukey posthoc
grpMeans_thick_1 <- emmeans(lmer_thick_1, ~ SR + Treat, data = meso_lm)
pairs(grpMeans_thick_1, simple = list("SR", "Treat"))

#Shell weight:
lmer_ShW_1 <- lmer(diff_ShW ~ SR*Treat + initShW + (1|Tank) + (1|SP), data = meso_lm) 
summary(lmer_ShW_1)

#Verify assumptions of model
plot(lmer_ShW_1)
visreg(lmer_ShW_1, "SR", by = "Treat")

#Analyse mixed-effects model using anova & Tukey posthoc test with emmeans, with kenward-roger df method
Anova(lmer_ShW_1, type = "III") 

#Since there are no significant interactions, use the following notation for the Tukey posthoc
grpMeans_ShW_1 <- emmeans(lmer_ShW_1, ~ SR + Treat, data = meso_lm)
pairs(grpMeans_ShW_1, simple = list("SR", "Treat"))

#Tissue weight
lmer_TiW_1 <- lmer(diff_TiW ~ SR*Treat + initTiW + (1|Tank) + (1|SP), data = meso_lm) 
summary(lmer_TiW_1)

#Verify assumptions of model
plot(lmer_TiW_1)
visreg(lmer_TiW_1, "SR", by = "Treat")
visreg(lmer_TiW_1, "initTiW", by = "SR", overlay = TRUE)
visreg(lmer_TiW_1, "initTiW", by = "Treat", overlay = TRUE)

#Analyse mixed-effects model using anova & Tukey posthoc test with emmeans, with kenward-roger df method
Anova(lmer_TiW_1, type = "III") 

#Since there are no significant interactions, use the following notation for the Tukey posthoc
grpMeans_TiW_1 <- emmeans(lmer_TiW_1, ~ SR + Treat, data = meso_lm)
pairs(grpMeans_TiW_1, simple = list("SR", "Treat"))

#Feeding rate: I'm  going to analyze the final per capita weekly feeding rate. 
lmer_food_temp <- lmer(meanPer_cap ~ SR*Treat + (1|SP) + (1|Tank), data = meso_food_tank)

summary(lmer_food_temp)
plot(lmer_food_temp)
visreg(lmer_food_temp, "SR", by = "Treat")

Anova(lmer_food_temp, type = "III")

#Survival: 
meso_surv <- lmer(cumsurv ~ SR*Treat + (1|SP), data = meso_clean_surv) #I removed 1|Tank because of singular fit
summary(meso_surv)

#Verify assumptions of model (dispersal increases as the variables increase...)
plot(meso_surv)
visreg(meso_surv, "SR", by = "Treat")
Anova(meso_surv, type = "III") 

#Since there are no positive interactions, use the following notation for the Tukey posthoc
grpMeans_surv <- emmeans(meso_surv, ~ SR + Treat, data = meso_clean_surv)
pairs(grpMeans_surv, simple = list("SR", "Treat"))

