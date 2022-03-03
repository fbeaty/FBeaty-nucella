#Script to analyze snail response variables from the mesocosm experiment
#Datasheets include the following measurements:
#July 1st (initial): length, thickness, total weight
#August 2nd (mid): submerged weight, total weight
#Sept 2nd (final): length, thickness, total weight, submerged weight

#Load packages----
pkgs <- c("tidyverse", "lubridate", "car", "visreg", "cowplot", "survminer", "survival",
          "emmeans", "lme4", "RVAideMemoire")
lapply(pkgs, library, character.only = TRUE)
rm(pkgs)

#Load csv and clean it----
#March 2nd: I saved the meso_collated from the Mesocosm folder into the CH 2 Analysis folder as a csv. If you make any changes to the base df, replace the one in git folder
meso_base <- read.csv("data/snail_RVs/meso_collated.csv")

#Clean dataframe----
#Add a Source Region column
#Ensure that all treatments line up with the correct tank
#Classify Tank 16 (which was supposed to be a 12) as a 15 degree treatment because you could never really bring that tank's temp down due to equipment issues
#Remove the Died & Notes column
#Rename column headers
#Remove the tanks that experienced chiller failures & 100% mortality within first experimental days: 3, 5, 6 :( :(
#Estimate shell weight (ShW) & tissue weight (TiW) based on the following submerged regressions for each population where x is SW (submerged weight):
#Pruth	y = 1.5889x + 0.1392
#Kwakshua	y = 1.5958x + 0.0646
#Cedar	y = 1.61x + 0.0266
#Heron	y = 1.6104x - .1292
#Calculate TiW (tissue weight) based on Shell weight and Total weight
#Remove any rows with NAs in the L, T, SG, TiW & ShW columns (these died, but I kept them in my original datasheet)

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

tanks_remove <- c(3, 5, 6)

meso_clean <- meso_base %>% 
  filter(!Tank %in% tanks_remove) %>% 
  filter(!is.na(Tank)) %>% 
  rename(L = Length, Th = Thickness, SG = Linear_shell_growth, TW = Total_weight, SW = Submerged_weight) %>% 
  mutate(ShW = ifelse(SP == "Cedar", cedar_reg(SW), 
                      ifelse(SP == "Heron", heron_reg(SW),
                             ifelse(SP == "Kwak", kwak_reg(SW),
                                    ifelse(SP == "Pruth", pruth_reg(SW), NA)))),
         SR = as.factor(ifelse(SP == "Cedar" | SP == "Heron", "Nanaimo", "Calvert")),
         SP = as.factor(SP),
         Stage = as.factor(Stage),
         Treat = as.factor(ifelse(Tank == 9 | Tank == 12, "12A",
                                  ifelse(Tank == 4 | Tank == 7 | Tank == 11 | Tank == 14 | Tank == 16, "15A",
                                         ifelse(Tank == 17 |  Tank == 20 | Tank == 21 | Tank == 24, "15L",
                                                ifelse(Tank == 1 | Tank == 10 | Tank == 13, "19A",
                                                       ifelse(Tank == 2 | Tank == 8 | Tank == 15, "22A",
                                                              ifelse(Tank == 18 | Tank == 19 | Tank == 22 | Tank == 23, "22L", NA))))))),
         TiW = TW-ShW) %>% 
  filter_at(vars(L,Th, SG, ShW, TiW), any_vars(!is.na(.))) %>% 
  select(Stage, SR, SP, Tank, Treat, ID, L, Th, ShW, TiW, SG)

#You have to add the TiW & ShW values for each ID to the init ID so that you only have 1 row/snail (otherwise it messes up the sample sizes later
meso_clean_1 <- meso_clean %>% 
  filter(Stage == "Init") %>% 
  select(!c(TiW, ShW))
meso_clean_2 <- meso_clean %>% 
  filter(Stage == "Mid") %>% 
  select(ID, TiW, ShW)
meso_clean_3 <- meso_clean %>% 
  filter(Stage == "Final")

meso_clean <- left_join(meso_clean_1, meso_clean_2, by = "ID") %>% 
  rbind(meso_clean_3) %>% 
  mutate(Stage = factor(Stage, levels = c("Init", "Final")))

rm(cedar_reg, heron_reg, pruth_reg, kwak_reg, tanks_remove, meso_clean_1, meso_clean_2, meso_clean_3)

#Create code for cleaning the survival data, but maybe in a different section? 

#Calculate the average & SD of metrics to visualize across the 2 time periods ----
#Note: Because submerged weight was only collected in the middle of the experiment, changes to tissue & shell weight are only reflective of
#the growth between mid & final stages (reference this in figure captions?)

#First check for outliers caused by high mortality within block that only leaves 1 individual left (which often means the mean - mean values are disproportionate)
#Remove the following outliers due to high mortality causing outliers in data
#The only 2 surviving snails in Tank 15 were abnormally large --> average values for that tank pull up the length & distort the Treatment SD for Cedar --> remove from visualization
#remove <- c("Cedar_15_Final")
#unite(unique_ID, c(SP, Tank, Stage), sep = "_", remove = FALSE) %>% 
#filter(!unique_ID %in% remove) %>% 

meso_growth_tank <- meso_clean %>% 
  group_by(Stage, SP, Treat, Tank) %>% 
  summarize(meanL = mean(L, na.rm = TRUE), sdL = sd(L, na.rm = TRUE),
            meanTh = mean(Th, na.rm = TRUE), sdTh = sd(Th, na.rm = TRUE),
            meanShW = mean(ShW, na.rm = TRUE), sdShW = sd(ShW, na.rm = TRUE),
            meanTiW = mean(TiW, na.rm = TRUE), sdTiW = sd(TiW, na.rm = TRUE),
            meanSG = mean(SG, na.rm = TRUE), sdSG = sd(SG, na.rm = TRUE), n = n()) %>% 
  ungroup()

test <- meso_clean %>% 
  filter(SP == "Pruth" & Stage == "Init" & Treat == "12A")

#Mid Calvert Pruth 22 only has 1 snail --> no SD
#Final Calvert Pruth 2 only has 1 snail --> no SD
#Final Calvert Pruth 22 only has 1 snail --> no SD

#Subset temp & factorial exps
meso_growth_treat_temp <- meso_growth_tank %>% 
  filter(Treat == "12A" | Treat == "15A" | Treat == "19A" | Treat == "22A")
meso_growth_treat_fact <- meso_growth_tank %>% 
  filter(Treat == "15A" | Treat == "15L" | Treat == "22A" | Treat == "22L")

#May have to create new code for adding survival to this dataframe

#Visualize the RVs over time for temp exp----
length_stage_temp <- ggplot(meso_growth_treat_temp, aes(Stage, meanL, group = SP, colour = SP)) +
  stat_summary(fun=mean, geom="point", size = 3, position=position_dodge(0.3)) +
  stat_summary(fun = mean, geom = "line", size = 0.8, position=position_dodge(0.3), alpha = 0.5) +
  stat_summary(fun.data = "mean_se", geom = "errorbar", width = 0.2, size = 0.5,
               position=position_dodge(0.3)) +
  facet_wrap(~ Treat, ncol = 4) +
  labs(colour = "Source Population") +
  scale_colour_manual(values = c("coral", "coral3", "skyblue", "skyblue3")) +
  labs(y = "Change in SL (mm)") +
  theme_cowplot(16) + theme(strip.background = element_blank(), strip.text = element_text(size = 16))

thick_stage_temp <- ggplot(meso_growth_treat_temp, aes(Stage, meanTh, group = SP, colour = SP)) + 
  stat_summary(fun=mean, geom="point", size = 3, position=position_dodge(0.3)) +
  stat_summary(fun = mean, geom = "line", size = 0.8, position=position_dodge(0.3), alpha = 0.5) +
  stat_summary(fun.data = "mean_se", geom = "errorbar", width = 0.2, size = 0.5,
               position=position_dodge(0.3)) +
  facet_wrap(~ Treat, ncol = 4) +
  scale_colour_manual(values = c("coral", "coral3", "skyblue", "skyblue3")) +
  labs(y = "ST (mm)") +
  theme_cowplot(16) + theme(strip.text = element_blank())

ShW_stage_temp <- ggplot(meso_growth_treat_temp, aes(Stage, meanShW, group = SP, colour = SP)) + 
  stat_summary(fun=mean, geom="point", size = 3, position=position_dodge(0.3)) +
  stat_summary(fun = mean, geom = "line", size = 0.8, position=position_dodge(0.3), alpha = 0.5) +
  stat_summary(fun.data = "mean_se", geom = "errorbar", width = 0.2, size = 0.5,
               position=position_dodge(0.3)) +
  facet_wrap(~ Treat, ncol = 4) +
  scale_colour_manual(values = c("coral", "coral3", "skyblue", "skyblue3")) +
  labs(y = "ShW (g)") +
  theme_cowplot(16) + theme(strip.text = element_blank())

TiW_stage_temp <- ggplot(meso_growth_treat_temp, aes(Stage, meanTiW, group = SP, colour = SP)) + 
  stat_summary(fun=mean, geom="point", size = 3, position=position_dodge(0.3)) +
  stat_summary(fun = mean, geom = "line", size = 0.8, position=position_dodge(0.3), alpha = 0.5) +
  stat_summary(fun.data = "mean_se", geom = "errorbar", width = 0.2, size = 0.5,
               position=position_dodge(0.3)) +
  facet_wrap(~ Treat, ncol = 4) +
  scale_colour_manual(values = c("coral", "coral3", "skyblue", "skyblue3")) +
  labs(y = "TiW (g)") +
  theme_cowplot(16) + theme(strip.text = element_blank())

SG_stage_temp <- ggplot(meso_growth_treat_temp, aes(Stage, meanSG, group = SP, colour = SP)) + 
  stat_summary(fun=mean, geom="point", size = 3, position=position_dodge(0.3)) +
  stat_summary(fun = mean, geom = "line", size = 0.8, position=position_dodge(0.3), alpha = 0.5) +
  stat_summary(fun.data = "mean_se", geom = "errorbar", width = 0.2, size = 0.5,
               position=position_dodge(0.3)) +
  facet_wrap(~ Treat, ncol = 4) +
  scale_colour_manual(values = c("coral", "coral3", "skyblue", "skyblue3")) +
  labs(y = "LSG (mm)") +
  theme_cowplot(16) + theme(strip.text = element_blank())

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
                      TiW_stage_temp + theme(legend.position = "none", axis.title.x = element_blank()), 
                      NULL,
                      ncol = 2, nrow = 5, axis = "lb", align = "hv", rel_widths = c(1,0.2))

xaxistitle <- ggdraw() + draw_label("Stage", fontface = "plain", x = 0.43, hjust = 0, size = 16)
meso_growth_temp_comb <- plot_grid(meso_stage_temp, xaxistitle, ncol = 1, rel_heights = c(1, 0.05))

#Make sure in your caption for this figure you reference that you're visualizing the mean metrics across blocks with sites pooled (i.e. n = 7-8)
ggsave(meso_growth_temp_comb, file = "plots/snails/meso/meso_stage_temp.pdf", height = 14, width = 12, dpi = 300)

#Visualize the RVs over time for fact exp----
length_stage_fact <- ggplot(meso_growth_treat_fact, aes(Stage, meanL, group = SP, colour = SP)) +
  stat_summary(fun=mean, geom="point", size = 3, position=position_dodge(0.3)) +
  stat_summary(fun = mean, geom = "line", size = 0.8, position=position_dodge(0.3), alpha = 0.5) +
  stat_summary(fun.data = "mean_se", geom = "errorbar", width = 0.2, size = 0.5,
               position=position_dodge(0.3)) +
  facet_wrap(~ Treat, ncol = 4) +
  labs(colour = "Source Population") +
  scale_colour_manual(values = c("coral", "coral3", "skyblue", "skyblue3")) +
  labs(y = "Change in SL (mm)") +
  theme_cowplot(16) + theme(strip.background = element_blank(), strip.text = element_text(size = 16))

thick_stage_fact <- ggplot(meso_growth_treat_fact, aes(Stage, meanTh, group = SP, colour = SP)) + 
  stat_summary(fun=mean, geom="point", size = 3, position=position_dodge(0.3)) +
  stat_summary(fun = mean, geom = "line", size = 0.8, position=position_dodge(0.3), alpha = 0.5) +
  stat_summary(fun.data = "mean_se", geom = "errorbar", width = 0.2, size = 0.5,
               position=position_dodge(0.3)) +
  facet_wrap(~ Treat, ncol = 4) +
  scale_colour_manual(values = c("coral", "coral3", "skyblue", "skyblue3")) +
  labs(y = "ST (mm)") +
  theme_cowplot(16) + theme(strip.text = element_blank())

ShW_stage_fact <- ggplot(meso_growth_treat_fact, aes(Stage, meanShW, group = SP, colour = SP)) + 
  stat_summary(fun=mean, geom="point", size = 3, position=position_dodge(0.3)) +
  stat_summary(fun = mean, geom = "line", size = 0.8, position=position_dodge(0.3), alpha = 0.5) +
  stat_summary(fun.data = "mean_se", geom = "errorbar", width = 0.2, size = 0.5,
               position=position_dodge(0.3)) +
  facet_wrap(~ Treat, ncol = 4) +
  scale_colour_manual(values = c("coral", "coral3", "skyblue", "skyblue3")) +
  labs(y = "ShW (g)") +
  theme_cowplot(16) + theme(strip.text = element_blank())

TiW_stage_fact <- ggplot(meso_growth_treat_fact, aes(Stage, meanTiW, group = SP, colour = SP)) + 
  stat_summary(fun=mean, geom="point", size = 3, position=position_dodge(0.3)) +
  stat_summary(fun = mean, geom = "line", size = 0.8, position=position_dodge(0.3), alpha = 0.5) +
  stat_summary(fun.data = "mean_se", geom = "errorbar", width = 0.2, size = 0.5,
               position=position_dodge(0.3)) +
  facet_wrap(~ Treat, ncol = 4) +
  scale_colour_manual(values = c("coral", "coral3", "skyblue", "skyblue3")) +
  labs(y = "TiW (g)") +
  theme_cowplot(16) + theme(strip.text = element_blank())

SG_stage_fact <- ggplot(meso_growth_treat_fact, aes(Stage, meanSG, group = SP, colour = SP)) + 
  stat_summary(fun=mean, geom="point", size = 3, position=position_dodge(0.3)) +
  stat_summary(fun = mean, geom = "line", size = 0.8, position=position_dodge(0.3), alpha = 0.5) +
  stat_summary(fun.data = "mean_se", geom = "errorbar", width = 0.2, size = 0.5,
               position=position_dodge(0.3)) +
  facet_wrap(~ Treat, ncol = 4) +
  scale_colour_manual(values = c("coral", "coral3", "skyblue", "skyblue3")) +
  labs(y = "LSG (mm)") +
  theme_cowplot(16) + theme(strip.text = element_blank())

meso_stage_fact <- plot_grid(length_stage_fact + theme(legend.position = "none",
                                                       axis.text.x = element_blank(), axis.title.x = element_blank()), 
                             get_legend(length_stage_fact),
                             thick_stage_fact + theme(legend.position = "none", 
                                                      axis.text.x = element_blank(), axis.title.x = element_blank()), 
                             NULL,
                             SG_stage_fact + theme(legend.position = "none", 
                                                   axis.text.x = element_blank(), axis.title.x = element_blank()), 
                             NULL,
                             ShW_stage_fact + theme(legend.position = "none",
                                                    axis.text.x = element_blank(), axis.title.x = element_blank()), 
                             NULL,
                             TiW_stage_fact + theme(legend.position = "none", axis.title.x = element_blank()), 
                             NULL,
                             ncol = 2, nrow = 5, axis = "lb", align = "hv", rel_widths = c(1,0.2))

xaxistitle <- ggdraw() + draw_label("Stage", fontface = "plain", x = 0.43, hjust = 0, size = 16)
meso_growth_temp_comb <- plot_grid(meso_stage_temp, xaxistitle, ncol = 1, rel_heights = c(1, 0.05))

#Make sure in your caption for this figure you reference that you're visualizing the mean metrics across blocks with sites pooled (i.e. n = 7-8)
ggsave(meso_growth_temp_comb, file = "plots/snails/meso/meso_stage_temp.pdf", height = 14, width = 12, dpi = 300)


#Create a new df with the change in growth metrics. Group by ID then summarize by tank----
meso_diff <- meso_clean %>% 
  arrange(ID, Stage) %>% 
  group_by(ID)%>% 
  mutate(diff_l = L - lag(L, default= L[1]),
         diff_Th = Th - lag(Th, default = Th[1]),
         diff_ShW = ShW - lag(ShW, default = ShW[1]),
         diff_TiW = TiW - lag(TiW, default = TiW[1])) %>% 
  subset(Stage == "Final") %>% 
  select(Stage, SR, SP, Tank, Treat, ID, SG, diff_l, diff_Th, diff_ShW, diff_TiW) %>%
  ungroup()

#Summarize by tank so that you can visualize the n/treatment
meso_diff_tank <- meso_diff %>% 
  group_by(SR, SP, Treat, Tank) %>% 
  summarize(meanL_treat = mean(diff_l, na.rm = TRUE), sdL_treat = sd(diff_l, na.rm = TRUE),
                        meanTh_treat = mean(diff_Th, na.rm = TRUE), sdTh_treat = sd(diff_Th, na.rm = TRUE),
                        meanShW_treat= mean(diff_ShW, na.rm = TRUE), sdShW_treat = sd(diff_ShW, na.rm = TRUE),
                        meanTiW_treat = mean(diff_TiW, na.rm = TRUE), sdTiW_treat = sd(diff_TiW, na.rm = TRUE),
                        meanSG_treat = mean(SG, na.rm = TRUE), sdSG_treat = sd(SG, na.rm = TRUE),
            n_OR_SR = n()) %>% 
    ungroup()

meso_diff_temp <- meso_diff_tank %>% 
  filter(Treat == "12A" | Treat == "15A" | Treat == "19A" | Treat == "22A")
meso_diff_fact <- meso_diff_tank %>% 
  filter(Treat == "15A" | Treat == "15L" | Treat == "22A" | Treat == "22L") %>% 
  mutate(temp = ifelse(Treat == "15A" | Treat == "15L", "15", "22"))

#Visualize change in growth across treatments grouped by SP for temp exp----
#The datapoints being visualized are each tank  within each treatment for each SP :) The correct unit of replication! 
plot_temp <- function(df, x, y, grp, clr.values, lbl.y){
  temp_SR_plot <- ggplot(df, aes({{x}}, {{y}}, group = {{grp}}, colour = {{grp}})) +
    geom_point(alpha=0.3, position = position_jitterdodge(dodge.width = 0.3, jitter.width=0.05)) +
    stat_summary(fun=mean, geom="point", size = 3, position=position_dodge(0.3)) +
    stat_summary(fun = mean, geom = "line", size = 0.8, position=position_dodge(0.3), alpha = 0.5) +
    stat_summary(fun.data = "mean_se", geom = "errorbar", width = 0.2, size = 0.5,
                 position=position_dodge(0.3)) +
    scale_colour_manual(values = clr.values) +
    labs(x = "Treatment", y = lbl.y) +
    theme_cowplot(16)
  return(temp_SR_plot)
}

length_treat_temp_SP <- plot_temp(meso_diff_temp, Treat, meanL_treat, SP, c("coral", "coral3", "skyblue", "skyblue3"), "Change in SL (mm)") + labs(colour = "Source Population")
thick_treat_temp_SP <- plot_temp(meso_diff_temp, Treat, meanTh_treat, SP, c("coral", "coral3", "skyblue", "skyblue3"), "Change in ST (mm)")
ShW_treat_temp_SP <- plot_temp(meso_diff_temp, Treat, meanShW_treat, SP, c("coral", "coral3", "skyblue", "skyblue3"), "Change in ShW (g)")
TiW_treat_temp_SP <- plot_temp(meso_diff_temp, Treat, meanTiW_treat, SP, c("coral", "coral3", "skyblue", "skyblue3"), "Change in TiW (g)")
SG_treat_temp_SP <- plot_temp(meso_diff_temp, Treat, meanSG_treat, SP, c("coral", "coral3", "skyblue", "skyblue3"), "Change in LSG (mm)")

meso_treat_temp_comb_SP <- plot_grid(length_treat_temp_SP + theme(legend.position = "none", axis.text.x = element_blank(), axis.title.x = element_blank()),
                               thick_treat_temp_SP + theme(legend.position = "none", axis.text.x = element_blank(), axis.title.x = element_blank()), 
                               get_legend(length_treat_temp_SP),
                               SG_treat_temp_SP + theme(legend.position = "none", axis.title.x = element_blank()),
                               ShW_treat_temp_SP + theme(legend.position = "none", axis.title.x = element_blank()), 
                               TiW_treat_temp_SP + theme(legend.position = "none", axis.title.x = element_blank()),
                               ncol = 3, nrow = 2, axis = "lb", align = "hv")

xaxistitle_treat <- ggdraw() + draw_label("Treatment", fontface = "plain", x = 0.43, hjust = 0, size = 16)
meso_treat_temp_comb_title_SP <- plot_grid(meso_treat_temp_comb_SP, xaxistitle_treat, ncol = 1, rel_heights = c(1, 0.05))

ggsave(meso_treat_temp_comb_title_SP, file = "plots/snails/meso/meso_treat_temp_SP.pdf", height = 8, width = 17, dpi = 300)

#Visualize change in growth across treatments grouped by SP for fact exp----
#The datapoints being visualized are each tank  within each treatment for each SP :) The correct unit of replication! 
plot_fact <- function(df, x, y, grp, temp, clr.values, lbl.y) {
  plot_fact <- ggplot(df, aes({{x}}, {{y}}, group = {{grp}}, colour = {{grp}})) +
    geom_point(alpha=0.3, position = position_jitterdodge(dodge.width = 0.3, jitter.width=0.05)) +
    stat_summary(fun=mean, geom="point", size = 3, position=position_dodge(0.3)) +
    stat_summary(fun = mean, geom = "line", size = 0.8, position=position_dodge(0.3), alpha = 0.5, aes(group = interaction(temp, {{grp}}))) +
    stat_summary(fun.data = "mean_se", geom = "errorbar", width = 0.2, size = 0.5,
                 position=position_dodge(0.3)) +
    scale_colour_manual(values = clr.values) +
    labs(x = "Treatment", y = lbl.y) +
    theme_cowplot(16)
  return(plot_fact)
}

length_treat_fact_SP <- plot_fact(meso_diff_fact, Treat, meanL_treat, SP, temp, c("coral", "coral3", "skyblue", "skyblue3"), "Change in SL (mm)") + labs(colour = "Source Population")
thick_treat_fact_SP <- plot_fact(meso_diff_fact, Treat, meanTh_treat, SP, temp, c("coral", "coral3", "skyblue", "skyblue3"), "Change in ST (mm)")
ShW_treat_fact_SP <- plot_fact(meso_diff_fact, Treat, meanShW_treat, SP, temp, c("coral", "coral3", "skyblue", "skyblue3"), "Change in ShW (g)")
TiW_treat_fact_SP <- plot_fact(meso_diff_fact, Treat, meanTiW_treat, SP, temp, c("coral", "coral3", "skyblue", "skyblue3"), "Change in TiW (g)")
SG_treat_fact_SP <- plot_fact(meso_diff_fact, Treat, meanSG_treat, SP, temp, c("coral", "coral3", "skyblue", "skyblue3"), "Change in LSG (mm)")

meso_treat_fact_comb_SP <- plot_grid(length_treat_fact_SP + theme(legend.position = "none", axis.text.x = element_blank(), axis.title.x = element_blank()),
                                  thick_treat_fact_SP + theme(legend.position = "none", axis.text.x = element_blank(), axis.title.x = element_blank()), 
                                  get_legend(length_treat_fact_SP),
                                  SG_treat_fact_SP + theme(legend.position = "none", axis.title.x = element_blank()),
                                  ShW_treat_fact_SP + theme(legend.position = "none", axis.title.x = element_blank()), 
                                  TiW_treat_fact_SP + theme(legend.position = "none", axis.title.x = element_blank()),
                                  ncol = 3, nrow = 2, axis = "lb", align = "hv")

xaxistitle_treat <- ggdraw() + draw_label("Treatment", fontface = "plain", x = 0.43, hjust = 0, size = 16)
meso_treat_fact_comb_title_SP <- plot_grid(meso_treat_fact_comb_SP, xaxistitle_treat, ncol = 1, rel_heights = c(1, 0.05))

ggsave(meso_treat_fact_comb_title_SP, file = "plots/snails/meso/meso_treat_fact_SP.pdf", height = 8, width = 17, dpi = 300)

#Visualize change in growth across treatments grouped by SR for temp exp----
#The datapoints being visualized are each tank  within each treatment for each SR :) The correct unit of replication! 
length_treat_temp_SR <- plot_temp(meso_diff_temp, Treat, meanL_treat, SR, c("skyblue", "coral"), "Change in SL (mm)") + labs(colour = "Source Region")
thick_treat_temp_SR <- plot_temp(meso_diff_temp, Treat, meanTh_treat, SR, c("skyblue", "coral"), "Change in ST (mm)")
ShW_treat_temp_SR <- plot_temp(meso_diff_temp, Treat, meanShW_treat, SR, c("skyblue", "coral"), "Change in ShW (g)")
TiW_treat_temp_SR <- plot_temp(meso_diff_temp, Treat, meanTiW_treat, SR, c("skyblue", "coral"), "Change in TiW (g)")
SG_treat_temp_SR <- plot_temp(meso_diff_temp, Treat, meanSG_treat, SR, c("skyblue", "coral"), "Change in LSG (mm)")

meso_treat_temp_comb_SR <- plot_grid(length_treat_temp_SR + theme(legend.position = "none", axis.text.x = element_blank(), axis.title.x = element_blank()),
                                  thick_treat_temp_SR + theme(legend.position = "none", axis.text.x = element_blank(), axis.title.x = element_blank()), 
                                  get_legend(length_treat_temp),
                                  SG_treat_temp_SR + theme(legend.position = "none", axis.title.x = element_blank()),
                                  ShW_treat_temp_SR + theme(legend.position = "none", axis.title.x = element_blank()), 
                                  TiW_treat_temp_SR + theme(legend.position = "none", axis.title.x = element_blank()),
                                  ncol = 3, nrow = 2, axis = "lb", align = "hv")

xaxistitle_treat <- ggdraw() + draw_label("Treatment", fontface = "plain", x = 0.43, hjust = 0, size = 16)
meso_treat_temp_comb_title_SR <- plot_grid(meso_treat_temp_comb_SR, xaxistitle_treat, ncol = 1, rel_heights = c(1, 0.05))

ggsave(meso_treat_temp_comb_title_SR, file = "plots/snails/meso/meso_treat_temp_SR.pdf", height = 8, width = 17, dpi = 300)

#Visualize change in growth across treatments grouped by SR for fact exp----
#The datapoints being visualized are each tank  within each treatment for each SR :) The correct unit of replication! 
length_treat_fact_SR <- plot_fact(meso_diff_fact, Treat, meanL_treat, SR, temp, c("skyblue", "coral"), "Change in SL (mm)") + labs(colour = "Source Region")
thick_treat_fact_SR <- plot_fact(meso_diff_fact, Treat, meanTh_treat, SR, temp, c("skyblue", "coral"), "Change in ST (mm)")
ShW_treat_fact_SR <- plot_fact(meso_diff_fact, Treat, meanShW_treat, SR, temp, c("skyblue", "coral"), "Change in ShW (g)")
TiW_treat_fact_SR <- plot_fact(meso_diff_fact, Treat, meanTiW_treat, SR, temp, c("skyblue", "coral"), "Change in TiW (g)")
SG_treat_fact_SR <- plot_fact(meso_diff_fact, Treat, meanSG_treat, SR, temp, c("skyblue", "coral"), "Change in LSG (mm)")

meso_treat_fact_comb_SR <- plot_grid(length_treat_fact_SR + theme(legend.position = "none", axis.text.x = element_blank(), axis.title.x = element_blank()),
                                     thick_treat_fact_SR + theme(legend.position = "none", axis.text.x = element_blank(), axis.title.x = element_blank()), 
                                     get_legend(length_treat_fact),
                                     SG_treat_fact_SR + theme(legend.position = "none", axis.title.x = element_blank()),
                                     ShW_treat_fact_SR + theme(legend.position = "none", axis.title.x = element_blank()), 
                                     TiW_treat_fact_SR + theme(legend.position = "none", axis.title.x = element_blank()),
                                     ncol = 3, nrow = 2, axis = "lb", align = "hv")

xaxistitle_treat <- ggdraw() + draw_label("Treatment", fontface = "plain", x = 0.43, hjust = 0, size = 16)
meso_treat_fact_comb_title_SR <- plot_grid(meso_treat_fact_comb_SR, xaxistitle_treat, ncol = 1, rel_heights = c(1, 0.05))

ggsave(meso_treat_fact_comb_title_SR, file = "plots/snails/meso/meso_treat_fact_SR.pdf", height = 8, width = 17, dpi = 300)


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

#Merge 2 datasets by ID
meso_lm <- left_join(meso_lm, meso_lm_init, by = "ID")

#Create a temp & fact set
meso_lm_temp <- meso_lm %>% 
  filter(Treat == "12A"|Treat=="15A"|Treat=="19A"|Treat=="22A")
meso_lm_fact <- meso_lm %>% 
  filter(Treat == "15A"|Treat=="15L"|Treat=="22A"|Treat=="22L")

#Make another df for the survival data

#Build linear mixed effects models----
#Fixed effects: SP, Treat & intxn, init size (and potential interactions)
#Random effects: Tank (1|Tank)

#OR: 
#Fixed effects: SR, Treat & intxn
#Random effects: Tank & Sp (1|Tank), (1|SP)

lmer_length <- lmer(diff_l ~ SP*Treat + (1|Tank), data = meso_lm_temp)
lmer_length_1 <- lmer(diff_l ~ SP*Treat + initL + (1|Tank), data = meso_lm_temp)
lmer_length_2 <- lmer(diff_l ~ SR*Treat + (1|Tank) + (1|SP), data = meso_lm_temp)
lmer_length_3 <- lmer(diff_l ~ SR*Treat + initL + (1|Tank) + (1|SP), data = meso_lm_temp)
lmer_length_5 <- lmer(diff_l ~ SR + SP + Treat + initL + (1|Tank), data = meso_lm_temp)

summary(lmer_length)
summary(lmer_length_1)
summary(lmer_length_2)
summary(lmer_length_3)
summary(lmer_length_4)

Anova(lmer_length, type = "III")
Anova(lmer_length_1, type = "III")
Anova(lmer_length_2, type = "III")
Anova(lmer_length_3, type = "III")
Anova(lmer_length_4, type = "III")

Anova(lmer_length)
Anova(lmer_length_1)
Anova(lmer_length_2)
Anova(lmer_length_3)
Anova(lmer_length_4)
Anova(lmer_length_5)

visreg(lmer_length_4)
visreg(lmer_length_4, "initL", by = "SP", overlay = TRUE)

AIC(lmer_length, lmer_length_1, lmer_length_2, lmer_length_3, lmer_length_4)

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

