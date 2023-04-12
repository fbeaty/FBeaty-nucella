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

RV_i <- RV_alive %>% 
  filter(Stage == "Init")
#So, you had 372 snails in your initial database

test <- RV_base %>% 
  filter(OR == "Calvert" & SR == "Nanaimo") %>% 
  select(OR, OS, SR, SP, DIED, ID)

RV_survival <- RV_base %>% 
  filter(!ID %in% ID_ostrina) %>% 
  mutate(OS = ifelse(ID == "CG**", "Kwak", OS),
         Block = ifelse(ID == "CG**", "O", Block)) %>% 
  mutate(Date = as.Date(Date, format = "%m-%d-%Y"),
         Stage = factor(Stage, levels = c("Init", "Mid", "Final")),
         SR = as.factor(SR),
         SP = as.factor(SP),
         OR = as.factor(OR),
         OR = ifelse(OR == "Nanaimo", "Strait of Georgia", "Central Coast"),
         OS = as.factor(OS),
         Block = as.factor(Block)) %>% 
  select(Date, Stage, SR, SP, OR, OS, Block, ID, DIED) %>% 
  unite(OS_block, c("OS", "Block"), sep = "_", remove = FALSE) 

RV_survival_glm <- RV_survival %>% 
  filter(Stage != "Init",
        !c(Stage == "Mid" & DIED == 0)) 
#This leaves us with 340 as your total sample size (somehow 32 went missing...)
#init = 372
#mid = 345
#final = 323

#total = 340
#total missing from middle = 27
#total missing between middle and final = 5
str(RV_survival_glm)

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
  mutate(cumsurv = ifelse(Stage == "Final", n_snl/lag(n_snl, default = n_snl[1]), cumsurv),
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

#Calculate the mean survival by source & outplant region
RV_cumsurv_final_diff <- RV_cumsurv_final %>% 
  group_by(OR, SR) %>% 
  summarize(mean_surv = mean(cumsurv))

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
  labs(colour = "Source Region", fill = "Source Region") + draw_plot_label("(d)", 0.4, 15, fontface = "plain")
thick_OR_box <- plot_OR_RT_box(RV_lm_block, OR, meandiff_Th, SR, c("skyblue", "coral"), c("skyblue3", "coral3"), "Change in shell lip thickness (mm)") +
  draw_plot_label("(e)", 0.4, 2.4, fontface = "plain")
ShW_OR_box <- plot_OR_RT_box(RV_lm_block, OR, meandiff_ShW, SR, c("skyblue", "coral"), c("skyblue3", "coral3"), "Shell weight growth (g)") +
  draw_plot_label("(c)", 0.4, 5.2, fontface = "plain")
TiW_OR_box <- plot_OR_RT_box(RV_lm_block, OR, meandiff_TiW, SR, c("skyblue", "coral"), c("skyblue3", "coral3"), "Tissue weight growth (g)") +
  draw_plot_label("(b)", 0.4, 1.3, fontface = "plain")
CSurv_OR_box <- plot_OR_RT_box(RV_cumsurv_final, OR, cumsurv, SR, c("skyblue", "coral"), c("skyblue3", "coral3"), "Survival (%)") +
  draw_plot_label("(a)", 0.4, 100, fontface = "plain")

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
ggsave(RV_combined_OR_box_title, file = "plots/snails/RT/Fig_4_RV_OR_box.pdf", height = 8, width = 17, dpi = 300)

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

#Test for the effects of initial size across SP & OS----
length_ced <- RV_lm %>% 
  filter(SP == "CEDAR")

length_size <- lm(initL ~ OS, data = length_ced)
Anova(length_size)

grpMeans <- emmeans(length_size, ~ OS, data = length_ced)
pairs(grpMeans)


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
grpMeans_surv_1 <- emmeans(lmer_surv_1, ~ OR+SR, data = RV_lm)
pairs(grpMeans_surv_1, simple = list("OR", "SR"))

#Build generalized linear mixed effects model for survival----
fit <- glmer(DIED ~ OR*SR + (1|OS_block:OS), family = binomial(link = "logit"), 
             #control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)),
             data = RV_survival_glm)
summary(fit)
visreg(fit, "OR", by = "SR")
Anova(fit, type = "III")

#Since there are no positive interactions, use the following notation for the Tukey posthoc
grpMeans_surv_2 <- emmeans(fit, ~ OR*SR, data = RV_survival_glm)
pairs(grpMeans_surv_2, simple = list("OR", "SR"))

#Remove all the unneeded objects for survival analysis----
rm(length_OR_box, length_OR_me, RV_alive, RV_combined_OR_SR, RV_diff, test,
   RV_growth_block, RV_sum_OR, SG_OR_OS_points, ShW_OR_OS_points, 
   CSurv_OR_box, length_stage, thick_stage, thick_OR_OS_points, RV_cumsurv_block, RV_cumsurv_OR, 
   TiW_stage, TiW_OR_OS_points, xaxistitle, xaxistitle_OR, RV_combined_stage_title,
   RV_growth_OR, RV_stage, RV_survival_block, SG_stage, surv_stage, ShW_stage, RV_combined_OR_SR_title)

