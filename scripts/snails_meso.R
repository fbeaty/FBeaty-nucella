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
         Treat = as.factor(ifelse(Tank == 9 | Tank == 12, "12.A",
                                  ifelse(Tank == 4 | Tank == 7 | Tank == 11 | Tank == 14 | Tank == 16, "15.A",
                                         ifelse(Tank == 17 |  Tank == 20 | Tank == 21 | Tank == 24, "15.L",
                                                ifelse(Tank == 1 | Tank == 10 | Tank == 13, "19.A",
                                                       ifelse(Tank == 2 | Tank == 8 | Tank == 15, "22.A",
                                                              ifelse(Tank == 18 | Tank == 19 | Tank == 22 | Tank == 23, "22.L", NA))))))),
         TiW = TW-ShW) %>% 
  separate(Treat, c("Temp", "pH"), remove = FALSE) %>% 
  mutate(Treat = as.factor(ifelse(Treat == "12.A", "12A",
                       ifelse(Treat == "15.A", "15A", 
                              ifelse(Treat == "15.L", "15L",
                                     ifelse(Treat == "19.A", "19A", 
                                            ifelse(Treat == "22.A", "22A",
                                                   ifelse(Treat == "22.L", "22L", NA)))))))) %>% 
  filter_at(vars(L,Th, SG, ShW, TiW), any_vars(!is.na(.))) %>% 
  select(Stage, SR, SP, Tank, Treat, Temp, pH, ID, L, Th, ShW, TiW, SG)

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
meso_clean_1 <- meso_clean_1[c(1:63,67:591), ]

#Now we have a dataframe with rows for all the right IDs, but we need to switch it into a long format keeping the IDs. 
#SO: split it into 2 dataframes again, and then rbind them
meso_clean_2 <- meso_clean_1 %>% 
  select(SR, SP, Tank, Treat, Temp, pH, ID, Stage.y, L_final, Th_final, ShW_final, TiW_final, SG.y) %>% 
  rename(L = L_final, Th = Th_final, ShW = ShW_final, TiW = TiW_final, Stage = Stage.y, SG = SG.y) %>%
  mutate(Alive = ifelse(is.na(Stage), 0, 1),
         Stage = ifelse(is.na(Stage), "Final", "Final")) %>% 
  select(Stage, SR, SP, Tank, Treat, Temp, pH, ID, L, Th, ShW, TiW, SG, Alive)
  
meso_clean <- meso_clean_1 %>% 
  select(Stage.x, SR, SP, Tank, Treat, Temp, pH, ID, L, Th, ShW, TiW, SG.x) %>% 
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
         Treat = as.factor(ifelse(Tank == 9 | Tank == 12, "12.A",
                                  ifelse(Tank == 4 | Tank == 7 | Tank == 11 | Tank == 14 | Tank == 16, "15.A",
                                         ifelse(Tank == 17 |  Tank == 20 | Tank == 21 | Tank == 24, "15.L",
                                                ifelse(Tank == 1 | Tank == 10 | Tank == 13, "19.A",
                                                       ifelse(Tank == 2 | Tank == 8 | Tank == 15, "22.A",
                                                              ifelse(Tank == 18 | Tank == 19 | Tank == 22 | Tank == 23, "22.L", NA)))))))) %>% 
  separate(Treat, c("Temp", "pH"), remove = FALSE) %>% 
  mutate(SR = ifelse(SP == "Cedar" | SP == "Heron", "Nanaimo", "Calvert"),
         Treat = as.factor(ifelse(Treat == "12.A", "12A",
                                  ifelse(Treat == "15.A", "15A", 
                                         ifelse(Treat == "15.L", "15L",
                                                ifelse(Treat == "19.A", "19A", 
                                                       ifelse(Treat == "22.A", "22A",
                                                              ifelse(Treat == "22.L", "22L", NA)))))))) %>% 
  select(Date, Tank, SR, SP, tank_sp, Treat, Temp, pH, Per_cap)

#Now summarize the average feeding rate for each tank_sp
meso_food_tank <- meso_food_clean %>% 
  group_by(Treat, Temp, pH, Tank, SR, SP, tank_sp) %>% 
  summarize(meanPer_cap = mean(Per_cap)) %>% 
  ungroup()

meso_food_tank_temp <- meso_food_tank %>% 
  filter(pH == "A") %>% 
  droplevels
meso_food_tank_fact <- meso_food_tank %>% 
  filter(Temp == 15 | Temp == 22) %>% 
  droplevels

#Calculate the proportion of surviving snails based on the start and end sample size of each SP in each tank
#Calculate the number of snails in each tank at the beginning and end
meso_clean_surv <- meso_clean %>% 
  group_by(Stage, SR, SP, Treat, Temp, pH, Tank) %>% 
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

meso_clean_surv_temp <- meso_clean_surv %>% 
  filter(pH == "A") %>% 
  droplevels
meso_clean_surv_fact <- meso_clean_surv %>% 
  filter(Temp == 15 | Temp == 22) %>% 
  droplevels

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
  group_by(Stage, SP, Treat, Temp, pH, Tank) %>% 
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

#Subset temp & factorial exps
meso_growth_temp <- meso_growth_tank %>% 
  filter(pH == "A") %>% 
  droplevels
meso_growth_fact <- meso_growth_tank %>% 
  filter(Temp == 15 | Temp == 22) %>% 
  droplevels

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

length_stage_temp <- plot_temp_stage(meso_growth_temp, Stage, meanL, SP, c("coral", "coral3", "skyblue", "skyblue3"), "SL (mm)") + 
  labs(colour = "Source Population") + theme(strip.background = element_blank(), strip.text = element_text(size = 16))
thick_stage_temp <- plot_temp_stage(meso_growth_temp, Stage, meanTh, SP, c("coral", "coral3", "skyblue", "skyblue3"), "ST (mm)")
ShW_stage_temp <- plot_temp_stage(meso_growth_temp, Stage, meanShW, SP, c("coral", "coral3", "skyblue", "skyblue3"), "ShW (g)")
TiW_stage_temp <- plot_temp_stage(meso_growth_temp, Stage, meanTiW, SP, c("coral", "coral3", "skyblue", "skyblue3"), "TiW (g)")
SG_stage_temp <- plot_temp_stage(meso_growth_temp, Stage, meanSG, SP, c("coral", "coral3", "skyblue", "skyblue3"), "LSG (mm)")
Surv_stage_temp <- plot_temp_stage(meso_clean_surv_temp, Stage, cumsurv, SP, c("coral", "coral3", "skyblue", "skyblue3"), "% Survival")

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
ggsave(meso_growth_temp_comb, file = "plots/snails/meso/meso_stage_temp.pdf", height = 14, width = 12, dpi = 300)

#Visualize the RVs over time for fact exp----
length_stage_fact <- plot_temp_stage(meso_growth_fact, Stage, meanL, SP, c("coral", "coral3", "skyblue", "skyblue3"), "SL (mm)") + 
  labs(colour = "Source Population") + theme(strip.background = element_blank(), strip.text = element_text(size = 16))
thick_stage_fact <- plot_temp_stage(meso_growth_fact, Stage, meanTh, SP, c("coral", "coral3", "skyblue", "skyblue3"), "ST (mm)")
ShW_stage_fact <- plot_temp_stage(meso_growth_fact, Stage, meanShW, SP, c("coral", "coral3", "skyblue", "skyblue3"), "ShW (g)")
TiW_stage_fact <- plot_temp_stage(meso_growth_fact, Stage, meanTiW, SP, c("coral", "coral3", "skyblue", "skyblue3"), "TiW (g)")
SG_stage_fact <- plot_temp_stage(meso_growth_fact, Stage, meanSG, SP, c("coral", "coral3", "skyblue", "skyblue3"), "LSG (mm)")
Surv_stage_fact <- plot_temp_stage(meso_clean_surv_fact, Stage, cumsurv, SP, c("coral", "coral3", "skyblue", "skyblue3"), "% Survival")

meso_stage_fact <- plot_grid(length_stage_fact + theme(legend.position = "none",
                                                       axis.text.x = element_blank(), axis.title.x = element_blank()), 
                             get_legend(length_stage_fact_SP),
                             thick_stage_fact + theme(legend.position = "none", 
                                                      axis.text.x = element_blank(), axis.title.x = element_blank()), 
                             NULL,
                             SG_stage_fact + theme(legend.position = "none", 
                                                   axis.text.x = element_blank(), axis.title.x = element_blank()), 
                             NULL,
                             ShW_stage_fact + theme(legend.position = "none",
                                                    axis.text.x = element_blank(), axis.title.x = element_blank()), 
                             NULL,
                             TiW_stage_fact + theme(legend.position = "none", 
                                                    axis.text.x = element_blank(), axis.title.x = element_blank()), 
                             NULL,
                             Surv_stage_fact + theme(legend.position = "none", axis.title.x = element_blank()), 
                             NULL,
                             ncol = 2, nrow = 6, axis = "lb", align = "hv", rel_widths = c(1,0.2))

xaxistitle <- ggdraw() + draw_label("Stage", fontface = "plain", x = 0.43, hjust = 0, size = 16)
meso_growth_fact_comb <- plot_grid(meso_stage_fact, xaxistitle, ncol = 1, rel_heights = c(1, 0.05))

#Make sure in your caption for this figure you reference that you're visualizing the mean metrics across blocks with sites pooled (i.e. n = 7-8)
ggsave(meso_growth_fact_comb, file = "plots/snails/meso/meso_stage_fact.pdf", height = 14, width = 12, dpi = 300)

#Visualize the RVs over time for all 6 treatments exp----
length_stage_all <- plot_temp_stage(meso_growth_tank, Stage, meanL, SP, c("coral", "coral3", "skyblue", "skyblue3"), "SL (mm)") + 
  labs(colour = "Source Population") + theme(strip.background = element_blank(), strip.text = element_text(size = 16)) + facet_wrap(~ Treat, ncol = 6)
thick_stage_all <- plot_temp_stage(meso_growth_tank, Stage, meanTh, SP, c("coral", "coral3", "skyblue", "skyblue3"), "ST (mm)")+ facet_wrap(~ Treat, ncol = 6)
ShW_stage_all <- plot_temp_stage(meso_growth_tank, Stage, meanShW, SP, c("coral", "coral3", "skyblue", "skyblue3"), "ShW (g)")+ facet_wrap(~ Treat, ncol = 6)
TiW_stage_all <- plot_temp_stage(meso_growth_tank, Stage, meanTiW, SP, c("coral", "coral3", "skyblue", "skyblue3"), "TiW (g)")+ facet_wrap(~ Treat, ncol = 6)
SG_stage_all <- plot_temp_stage(meso_growth_tank, Stage, meanSG, SP, c("coral", "coral3", "skyblue", "skyblue3"), "LSG (mm)")+ facet_wrap(~ Treat, ncol = 6)
Surv_stage_all <- plot_temp_stage(meso_clean_surv_1, Stage, cumsurv, SP, c("coral", "coral3", "skyblue", "skyblue3"), "% Survival")+ facet_wrap(~ Treat, ncol = 6)

feeding_time <- ggplot(meso_food_clean, aes(Date, Per_cap, group = SP, colour = SP)) +
  stat_summary(fun=mean, geom="point", size = 3, position=position_dodge(1.8)) +
  stat_summary(fun = mean, geom = "line", size = 0.8, position=position_dodge(1.8), alpha = 0.5) +
  stat_summary(fun.data = "mean_se", geom = "errorbar", 
               position=position_dodge(1.8), alpha = 0.8) +
  facet_wrap(~ Treat, ncol = 6) +
  scale_colour_manual(values = c("coral", "coral3", "skyblue", "skyblue3")) +
  labs(y = "Per capita feeding rate") +
  theme_cowplot(16) + theme(axis.text.x = element_text(size = 10)) + 
  labs(colour = "Source Population") + theme(strip.text = element_blank())


meso_stage_all <- plot_grid(length_stage_all + theme(legend.position = "none",
                                                       axis.text.x = element_blank(), axis.title.x = element_blank()), 
                             get_legend(length_stage_all),
                             thick_stage_all + theme(legend.position = "none", 
                                                      axis.text.x = element_blank(), axis.title.x = element_blank()), 
                             NULL,
                             SG_stage_all + theme(legend.position = "none", 
                                                   axis.text.x = element_blank(), axis.title.x = element_blank()), 
                             NULL,
                             ShW_stage_all + theme(legend.position = "none",
                                                    axis.text.x = element_blank(), axis.title.x = element_blank()), 
                             NULL,
                             TiW_stage_all + theme(legend.position = "none", 
                                                    axis.text.x = element_blank(), axis.title.x = element_blank()), 
                             NULL,
                            Surv_stage_all + theme(legend.position = "none", axis.title.x = element_blank()), 
                            NULL,
                            feeding_time + theme(legend.position = "none", axis.title.x = element_blank()), 
                            NULL, 
                            ncol = 2, nrow = 7, axis = "lb", align = "hv", rel_widths = c(1,0.2))

xaxistitle <- ggdraw() + draw_label("Stage/Date", fontface = "plain", x = 0.43, hjust = 0, size = 16)
meso_growth_all_comb <- plot_grid(meso_stage_all, xaxistitle, ncol = 1, rel_heights = c(1, 0.05))

#Make sure in your caption for this figure you reference that you're visualizing the mean metrics across blocks with sites pooled (i.e. n = 7-8)
ggsave(meso_growth_all_comb, file = "plots/snails/meso/meso_stage_all.pdf", height = 14, width = 14, dpi = 300)

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
  select(Stage, SR, SP, Treat, Temp, pH, Tank, ID, SG, diff_l, diff_Th, diff_ShW, diff_TiW) %>%
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

#Create a temp & fact set
meso_lm_temp <- meso_lm %>% 
  filter(pH == "A") %>% 
  droplevels
meso_lm_fact <- meso_lm %>% 
  filter(Temp == 15 | Temp == 22) %>% 
  droplevels

#Now create a new dataset with the change in growth summarized by block
meso_lm_block <- meso_lm %>% 
  group_by(SR, SP, Treat, Temp, pH, Tank) %>% 
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

meso_lm_block_temp <- meso_lm_block %>% 
  filter(pH == "A") %>% 
  droplevels
meso_lm_block_fact <- meso_lm_block %>% 
  filter(Temp == 15 | Temp == 22) %>% 
  droplevels

#Summarize the sample size for each treatment for your figure captions
meso_temp_sampl <- meso_lm_block_temp %>% 
  group_by(SP, Treat) %>% 
  summarize(n_tanks = n())
meso_fact_sampl <- meso_lm_block_fact %>% 
  group_by(SR, Treat) %>% 
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

length_temp_SR_box <- plot_temp_box(meso_lm_block_temp, Temp, meandiff_l, SR, c("skyblue", "coral"), c("skyblue3", "coral3"), "Change in SL (mm)") + labs(colour = "Source Region", fill = "Source Region")
thick_temp_SR_box <- plot_temp_box(meso_lm_block_temp, Temp, meandiff_Th, SR, c("skyblue", "coral"), c("skyblue3", "coral3"), "Change in ST (mm)")
ShW_temp_SR_box <- plot_temp_box(meso_lm_block_temp, Temp, meandiff_ShW, SR, c("skyblue", "coral"), c("skyblue3", "coral3"), "Change in ShW (g)")
TiW_temp_SR_box <- plot_temp_box(meso_lm_block_temp, Temp, meandiff_TiW, SR, c("skyblue", "coral"), c("skyblue3", "coral3"), "Change in TiW (g)")
SG_temp_SR_box <- plot_temp_box(meso_lm_block_temp, Temp, mean_SG, SR, c("skyblue", "coral"), c("skyblue3", "coral3"), "Change in LSG (mm)")
Feed_temp_SR_box <- plot_temp_box(meso_lm_block_temp, Temp, meanPer_cap, SR, c("skyblue", "coral"), c("skyblue3", "coral3"), "Per capita weekly feeding rate")
Surv_temp_SR_box <- plot_temp_box(meso_lm_block_temp, Temp, cumsurv, SR, c("skyblue", "coral"), c("skyblue3", "coral3"), "Survival(%)")

meso_temp_comb_SR_box <- plot_grid(length_temp_SR_box + theme(legend.position = "none", axis.text.x = element_blank(), axis.title.x = element_blank()),
                                  thick_temp_SR_box + theme(legend.position = "none", axis.text.x = element_blank(), axis.title.x = element_blank()), 
                                  SG_temp_SR_box + theme(legend.position = "none", axis.text.x = element_blank(), axis.title.x = element_blank()),
                                  get_legend(length_temp_SR_box),
                                  ShW_temp_SR_box + theme(legend.position = "none", axis.title.x = element_blank()), 
                                  TiW_temp_SR_box + theme(legend.position = "none", axis.title.x = element_blank()),
                                  Feed_temp_SR_box + theme(legend.position = "none", axis.title.x = element_blank()),
                                  Surv_temp_SR_box + theme(legend.position = "none", axis.title.x = element_blank()),
                                  ncol = 4, nrow = 2, axis = "lb", align = "hv")

xaxistitle_treat <- ggdraw() + draw_label("Treatment", fontface = "plain", x = 0.5, hjust = 0, size = 16)
meso_temp_comb_title_SR_box <- plot_grid(meso_temp_comb_SR_box, xaxistitle_treat, ncol = 1, rel_heights = c(1, 0.05))

ggsave(meso_temp_comb_title_SR_box, file = "plots/snails/meso/meso_temp_SR_box.pdf", height = 8, width = 17, dpi = 300)

#Visualize change in growth across treatments grouped by SR for fact exp----
#The datapoints being visualized are each tank  within each treatment for each SR :) The correct unit of replication! 
length_fact_SR_box <- plot_temp_box(meso_lm_block_fact, Treat, meandiff_l, SR, c("skyblue", "coral"), c("skyblue3", "coral3"), "Change in SL (mm)") + labs(colour = "Source Region", fill = "Source Region")
thick_fact_SR_box <- plot_temp_box(meso_lm_block_fact, Treat, meandiff_Th, SR, c("skyblue", "coral"), c("skyblue3", "coral3"), "Change in ST (mm)")
ShW_fact_SR_box <- plot_temp_box(meso_lm_block_fact, Treat, meandiff_ShW, SR, c("skyblue", "coral"), c("skyblue3", "coral3"), "Change in ShW (g)")
TiW_fact_SR_box <- plot_temp_box(meso_lm_block_fact, Treat, meandiff_TiW, SR, c("skyblue", "coral"), c("skyblue3", "coral3"), "Change in TiW (g)")
SG_fact_SR_box <- plot_temp_box(meso_lm_block_fact, Treat, mean_SG, SR, c("skyblue", "coral"), c("skyblue3", "coral3"), "Change in LSG (mm)")
Feed_fact_SR_box <- plot_temp_box(meso_lm_block_fact, Treat, meanPer_cap, SR, c("skyblue", "coral"), c("skyblue3", "coral3"), "Per capita weekly feeding rate")
Surv_fact_SR_box <- plot_temp_box(meso_lm_block_fact, Treat, cumsurv, SR, c("skyblue", "coral"), c("skyblue3", "coral3"), "Survival(%)")

meso_fact_comb_SR_box <- plot_grid(length_fact_SR_box + theme(legend.position = "none", axis.text.x = element_blank(), axis.title.x = element_blank()),
                                   thick_fact_SR_box + theme(legend.position = "none", axis.text.x = element_blank(), axis.title.x = element_blank()), 
                                   SG_fact_SR_box + theme(legend.position = "none", axis.text.x = element_blank(), axis.title.x = element_blank()),
                                   get_legend(length_fact_SR_box),
                                   ShW_fact_SR_box + theme(legend.position = "none", axis.title.x = element_blank()), 
                                   TiW_fact_SR_box + theme(legend.position = "none", axis.title.x = element_blank()),
                                   Feed_fact_SR_box + theme(legend.position = "none", axis.title.x = element_blank()),
                                   Surv_fact_SR_box + theme(legend.position = "none", axis.title.x = element_blank()),
                                   ncol = 4, nrow = 2, axis = "lb", align = "hv")

xaxistitle_treat <- ggdraw() + draw_label("Treatment", fontface = "plain", x = 0.5, hjust = 0, size = 16)
meso_fact_comb_title_SR_box <- plot_grid(meso_fact_comb_SR_box, xaxistitle_treat, ncol = 1, rel_heights = c(1, 0.05))
ggsave(meso_fact_comb_title_SR_box, file = "plots/snails/meso/meso_fact_SR_box.pdf", height = 8, width = 17, dpi = 300)

#Visualize change in growth & final phenotype grouped by SR for temp----
length_temp_SR_box_fin <- plot_temp_box(meso_lm_block_temp, Temp, meanfinL, SR, c("skyblue", "coral"), c("skyblue3", "coral3"), "Mean SL (mm)") + labs(colour = "Source Region", fill = "Source Region")
thick_temp_SR_box_fin <- plot_temp_box(meso_lm_block_temp, Temp, meanfinTh, SR, c("skyblue", "coral"), c("skyblue3", "coral3"), "Mean ST (mm)")
ShW_temp_SR_box_fin <- plot_temp_box(meso_lm_block_temp, Temp, meanfinShW, SR, c("skyblue", "coral"), c("skyblue3", "coral3"), "Mean ShW (g)")
TiW_temp_SR_box_fin <- plot_temp_box(meso_lm_block_temp, Temp, meanfinTiW, SR, c("skyblue", "coral"), c("skyblue3", "coral3"), "Mean TiW (g)")

meso_temp_comb_SR_box_fin <- plot_grid(length_temp_SR_box + theme(legend.position = "none", axis.text.x = element_blank(), axis.title.x = element_blank()),
                                       length_temp_SR_box_fin + theme(legend.position = "none", axis.text.x = element_blank(), axis.title.x = element_blank()),
                                       get_legend(length_temp_SR_box),
                                   thick_temp_SR_box + theme(legend.position = "none", axis.text.x = element_blank(), axis.title.x = element_blank()), 
                                   thick_temp_SR_box_fin + theme(legend.position = "none", axis.text.x = element_blank(), axis.title.x = element_blank()), 
                                   NULL,
                                   ShW_temp_SR_box + theme(legend.position = "none", axis.text.x = element_blank(), axis.title.x = element_blank()), 
                                   ShW_temp_SR_box_fin + theme(legend.position = "none", axis.text.x = element_blank(), axis.title.x = element_blank()), 
                                   NULL,
                                   TiW_temp_SR_box + theme(legend.position = "none", axis.title.x = element_blank()),
                                   TiW_temp_SR_box_fin + theme(legend.position = "none", axis.title.x = element_blank()),
                                   NULL,
                                   ncol = 3, nrow = 4, rel_widths= c(1, 1, 0.3), axis = "lb", align = "hv", labels = c('A', 'B', ''), label_size = 18)

xaxistitle_treat <- ggdraw() + draw_label("Treatment", fontface = "plain", x = 0.43, hjust = 0, size = 16)
meso_temp_comb_title_SR_box_fin <- plot_grid(meso_temp_comb_SR_box_fin, xaxistitle_treat, ncol = 1, rel_heights = c(1, 0.05))

ggsave(meso_temp_comb_title_SR_box_fin, file = "plots/snails/meso/meso_temp_fin_change.pdf", height = 12, width = 12, dpi = 300)

#Visualize change in growth & final phenotype grouped by SR for fact ----
length_fact_SR_box_fin <- plot_temp_box(meso_lm_block_fact, Treat, meanfinL, SR, c("skyblue", "coral"), c("skyblue3", "coral3"), "Mean SL (mm)") + labs(colour = "Source Region", fill = "Source Region")
thick_fact_SR_box_fin <- plot_temp_box(meso_lm_block_fact, Treat, meanfinTh, SR, c("skyblue", "coral"), c("skyblue3", "coral3"), "Mean ST (mm)")
ShW_fact_SR_box_fin <- plot_temp_box(meso_lm_block_fact, Treat, meanfinShW, SR, c("skyblue", "coral"), c("skyblue3", "coral3"), "Mean ShW (g)")
TiW_fact_SR_box_fin <- plot_temp_box(meso_lm_block_fact, Treat, meanfinTiW, SR, c("skyblue", "coral"), c("skyblue3", "coral3"), "Mean TiW (g)")

thick_fact_SR_box <- plot_temp_box(meso_lm_block_fact, Treat, meandiff_Th, SR, c("skyblue", "coral"), c("skyblue3", "coral3"), "Change in ST (mm)")


meso_fact_comb_SR_box_fin <- plot_grid(length_fact_SR_box + theme(legend.position = "none", axis.text.x = element_blank(), axis.title.x = element_blank()),
                                       length_fact_SR_box_fin + theme(legend.position = "none", axis.text.x = element_blank(), axis.title.x = element_blank()),
                                       get_legend(length_fact_SR_box),
                                       thick_fact_SR_box + theme(legend.position = "none", axis.text.x = element_blank(), axis.title.x = element_blank()), 
                                       thick_fact_SR_box_fin + theme(legend.position = "none", axis.text.x = element_blank(), axis.title.x = element_blank()), 
                                       NULL,
                                       ShW_fact_SR_box + theme(legend.position = "none", axis.text.x = element_blank(), axis.title.x = element_blank()), 
                                       ShW_fact_SR_box_fin + theme(legend.position = "none", axis.text.x = element_blank(), axis.title.x = element_blank()), 
                                       NULL,
                                       TiW_fact_SR_box + theme(legend.position = "none", axis.title.x = element_blank()),
                                       TiW_fact_SR_box_fin + theme(legend.position = "none", axis.title.x = element_blank()),
                                       NULL,
                                       ncol = 3, nrow = 4, rel_widths= c(1, 1, 0.3), axis = "lb", align = "hv", labels = c('A', 'B', ''), label_size = 18)

meso_fact_comb_title_SR_box_fin <- plot_grid(meso_fact_comb_SR_box_fin, xaxistitle_treat, ncol = 1, rel_heights = c(1, 0.05))

ggsave(meso_fact_comb_title_SR_box_fin, file = "plots/snails/meso/meso_fact_fin_change.pdf", height = 12, width = 12, dpi = 300)

#Visualize the survival & feeding rate for the temp & fact experiments (separately)----
Feed_all_meso <- plot_temp_box(meso_lm_block, Treat, meanPer_cap, SR, c("skyblue", "coral"), c("skyblue3", "coral3"), "Per capita weekly feeding rate") + labs(colour = "Source Region", fill = "Source Region")
Surv_all_meso <- plot_temp_box(meso_lm_block, Treat, cumsurv, SR, c("skyblue", "coral"), c("skyblue3", "coral3"), "Survival(%)")
legend <- get_legend(Feed_all_meso + theme(legend.justification="center" , legend.box.just = "bottom"))

#For this line of code you have to have the survival figure from the RT loaded (just run through that script to get the CSurv_OR_box figure)
surv_feed_comb <- plot_grid(Surv_all_meso + theme(legend.position ="none", axis.title.x = element_blank()),
                            CSurv_OR_box_noLines + theme(legend.position ="none", axis.text.y = element_blank(), axis.title.y = element_blank()) + labs(x = "Outplant Region"),
                            Feed_all_meso + theme(legend.position ="none") + labs(x = "Treatment"),
                            legend,
                            ncol = 2, nrow = 2, rel_widths = c(1, 0.7), axis = "lb", align = "hv", labels = c('A', 'C', 'B', ''), label_size = 18)

ggsave(surv_feed_comb, file = "plots/snails/surv_feed_comb.pdf", height = 9, width = 12, dpi = 300)

#Visualize final size across treatments grouped by SR for temp exp----
#The datapoints being visualized are each tank  within each treatment for each SR :) The correct unit of replication! 
fin_length_temp_SR_box <- plot_temp_box(meso_lm_block_temp, Temp, meanfinL, SR, c("skyblue", "coral"), c("skyblue3", "coral3"), "Mean SL (mm)") + labs(colour = "Source Region", fill = "Source Region")
fin_thick_temp_SR_box <- plot_temp_box(meso_lm_block_temp, Temp, meanfinTh, SR, c("skyblue", "coral"), c("skyblue3", "coral3"), "Mean ST (mm)")
fin_ShW_temp_SR_box <- plot_temp_box(meso_lm_block_temp, Temp, meanfinShW, SR, c("skyblue", "coral"), c("skyblue3", "coral3"), "Mean ShW (g)")
fin_TiW_temp_SR_box <- plot_temp_box(meso_lm_block_temp, Temp, meanfinTiW, SR, c("skyblue", "coral"), c("skyblue3", "coral3"), "Mean TiW (g)")
fin_SG_temp_SR_box <- plot_temp_box(meso_lm_block_temp, Temp, mean_SG, SR, c("skyblue", "coral"), c("skyblue3", "coral3"), "Mean LSG (mm)")
fin_Feed_temp_SR_box <- plot_temp_box(meso_lm_block_temp, Temp, meanPer_cap, SR, c("skyblue", "coral"), c("skyblue3", "coral3"), "Per capita weekly feeding rate")
fin_Surv_temp_SR_box <- plot_temp_box(meso_lm_block_temp, Temp, cumsurv, SR, c("skyblue", "coral"), c("skyblue3", "coral3"), "Survival(%)")

meso_temp_fin_SR_box <- plot_grid(fin_length_temp_SR_box + theme(legend.position = "none", axis.text.x = element_blank(), axis.title.x = element_blank()),
                                  fin_thick_temp_SR_box + theme(legend.position = "none", axis.text.x = element_blank(), axis.title.x = element_blank()), 
                                  fin_SG_temp_SR_box + theme(legend.position = "none", axis.text.x = element_blank(), axis.title.x = element_blank()),
                                   get_legend(fin_length_temp_SR_box),
                                  fin_ShW_temp_SR_box + theme(legend.position = "none", axis.title.x = element_blank()), 
                                  fin_TiW_temp_SR_box + theme(legend.position = "none", axis.title.x = element_blank()),
                                  fin_Feed_temp_SR_box + theme(legend.position = "none", axis.title.x = element_blank()),
                                  fin_Surv_temp_SR_box + theme(legend.position = "none", axis.title.x = element_blank()),
                                   ncol = 4, nrow = 2, axis = "lb", align = "hv")

xaxistitle_treat <- ggdraw() + draw_label("Treatment", fontface = "plain", x = 0.5, hjust = 0, size = 16)
meso_temp_fin_title_SR_box <- plot_grid(meso_temp_fin_SR_box, xaxistitle_treat, ncol = 1, rel_heights = c(1, 0.05))

ggsave(meso_temp_fin_title_SR_box, file = "plots/snails/meso/meso_temp_SR_box_fin.pdf", height = 8, width = 17, dpi = 300)


#Test whether initial size differs across tanks----
initL_aov <- lm(initL ~ Tank + Temp, data = meso_lm_temp)
Anova(initL_aov)
initTh_aov <- lm(initTh ~ Tank + Temp, data = meso_lm_temp)
Anova(initTh_aov)
initTiW_aov <- lm(initTiW ~ Tank + Temp, data = meso_lm_temp)
Anova(initTiW_aov)
initShW_aov <- lm(initShW ~ Tank + Temp, data = meso_lm_temp)
Anova(initShW_aov)

rm(initL_aov, initTh_aov, initTiW_aov, initShW_aov)

#There is no significant difference in starting size across tanks or treatment

#Build linear mixed effects models for temp exp----
#Fixed effects: SR, Treat & intxn
#Random effects: Tank & Sp (1|Tank), (1|SP)
lmer_length_1 <- lmer(diff_l ~ SR*Treat + initL + (1|Tank) + (1|SP), data = meso_lm_temp)
lmer_length_2 <- lmer(finL ~ SR*Treat + initL + (1|Tank) + (1|SP), data = meso_lm_temp)
summary(lmer_length_1)
summary(lmer_length_2)

#Verify assumptions of model
plot(lmer_length_1)
visreg(lmer_length_1, "SR", by = "Treat")
visreg(lmer_length_1, "initL", by = "SR", overlay = TRUE)
visreg(lmer_length_1, "initL", by = "Treat", overlay = TRUE)

#Analyse mixed-effects model using anova & Tukey posthoc test with emmeans, with kenward-roger df method
Anova(lmer_length_1, type = "III")
Anova(lmer_length_2, type = "III") 

#Since there are significant interactions, use the following notation for the Tukey posthoc
grpMeans_length_1 <- emmeans(lmer_length_1, ~ SR*Treat, data = meso_lm_temp)
pairs(grpMeans_length_1, simple = list("SR", "Treat"))
grpMeans_length_2 <- emmeans(lmer_length_2, ~ SR*Treat, data = meso_lm_temp)
pairs(grpMeans_length_2, simple = list("SR", "Treat"))

#Shell thickness: note for this model I received a singular fit when OS_block was nested within OS, where OS variance = 0 --> removed OS from model as per Matuschek
lmer_thick_1 <- lmer(diff_Th ~ SR*Treat + initTh + (1|Tank) + (1|SP), data = meso_lm_temp) 
lmer_thick_2 <- lmer(finTh ~ SR*Treat + initTh + (1|Tank) + (1|SP), data = meso_lm_temp)
summary(lmer_thick_1)
summary(lmer_thick_2)

#Verify assumptions of model
plot(lmer_thick_1)
visreg(lmer_thick_1, "SR", by = "Treat")
visreg(lmer_thick_1, "iniTh", by = "SR", overlay = TRUE)
visreg(lmer_thick_1, "initTh", by = "Treat", overlay = TRUE)
plot(lmer_thick_2)
visreg(lmer_thick_2, "SP", by = "Treat")
visreg(lmer_thick_1, "initTh", by = "SP", overlay = TRUE)

#Analyse mixed-effects model using anova & Tukey posthoc test with emmeans, with kenward-roger df method
Anova(lmer_thick_1, type = "III")
Anova(lmer_thick_2, type = "III")

#Since there are significant interactions, use the following notation for the Tukey posthoc
grpMeans_thick_1 <- emmeans(lmer_thick_1, ~ SR + Treat, data = meso_lm_temp)
pairs(grpMeans_thick_1, simple = list("SR", "Treat"))
grpMeans_thick_2 <- emmeans(lmer_thick_2, ~ SR*Treat, data = meso_lm_temp)
pairs(grpMeans_thick_2, simple = list("SR", "Treat"))

#Shell weight: importantly, I dropped (1|SP) from this model due to singular fit
lmer_ShW_1 <- lmer(diff_ShW ~ SR*Treat + (1|Tank) + (1|SP), data = meso_lm_temp) #<- removed initShW because it was non-significant
lmer_ShW_2 <- lmer(finShW ~ SR*Treat + initShW + (1|Tank) + (1|SP), data = meso_lm_temp) #<- removed initShW because it was non-significant
summary(lmer_ShW_1)
summary(lmer_ShW_2)

#Verify assumptions of model
plot(lmer_ShW_1)
visreg(lmer_ShW_1, "SR", by = "Treat")
visreg(lmer_ShW_1, "initShW", by = "SR", overlay = TRUE)
visreg(lmer_ShW_1, "initShW", by = "Treat", overlay = TRUE)
plot(lmer_ShW_2)
visreg(lmer_ShW_2, "SR", by = "Treat")
visreg(lmer_ShW_2, "initShW", by = "SR", overlay = TRUE)

#Analyse mixed-effects model using anova & Tukey posthoc test with emmeans, with kenward-roger df method
Anova(lmer_ShW_1, type = "II") #<- ran Type II because interaction was non-significant with Type III
Anova(lmer_ShW_2, type = "II") #<- ran Type II because interaction was non-significant with Type III

#Since there are no significant interactions, use the following notation for the Tukey posthoc
grpMeans_ShW_1 <- emmeans(lmer_ShW_1, ~ SR + Treat, data = meso_lm_temp)
pairs(grpMeans_ShW_1, simple = list("SR", "Treat"))
grpMeans_ShW_2 <- emmeans(lmer_ShW_2, ~ SR + Treat, data = meso_lm_temp)
pairs(grpMeans_ShW_2, simple = list("SR", "Treat"))

#Tissue weight
lmer_TiW_1 <- lmer(diff_TiW ~ SR*Treat + initTiW + (1|Tank) + (1|SP), data = meso_lm_temp) 
lmer_TiW_2 <- lmer(finTiW ~ SR*Treat + initTiW + (1|Tank) + (1|SP), data = meso_lm_temp)
summary(lmer_TiW_1)
summary(lmer_TiW_2)

#Verify assumptions of model
plot(lmer_TiW_1)
visreg(lmer_TiW_1, "SR", by = "Treat")
visreg(lmer_TiW_1, "initTiW", by = "SR", overlay = TRUE)
visreg(lmer_TiW_1, "initTiW", by = "Treat", overlay = TRUE)
plot(lmer_TiW_2)
visreg(lmer_TiW_2, "SR", by = "Treat")
visreg(lmer_TiW_1, "initTiW", by = "SR", overlay = TRUE)

#Analyse mixed-effects model using anova & Tukey posthoc test with emmeans, with kenward-roger df method
Anova(lmer_TiW_1, type = "II") #<- ran Type II because interaction was non-significant with Type III
Anova(lmer_TiW_2, type = "II") #<- ran Type II because interaction was non-significant with Type III

#Since there are no significant interactions, use the following notation for the Tukey posthoc
grpMeans_TiW_1 <- emmeans(lmer_TiW_1, ~ SR + Treat, data = meso_lm_temp)
pairs(grpMeans_TiW_1, simple = list("SR", "Treat"))
grpMeans_TiW_2 <- emmeans(lmer_TiW_2, ~ SR + Treat, data = meso_lm_temp)
pairs(grpMeans_TiW_2, simple = list("SR", "Treat"))

#Feeding rate: I'm just going to analyze the final per capita weekly feeding rate. Note that because tank is your unit of replication here, 
#you don't need it as a random effect
lmer_food_temp_1 <- lmer(meanPer_cap ~ SR*Treat + (1|SP), data = meso_food_tank_temp)
summary(lmer_food_temp_1)
plot(lmer_food_temp_1)
visreg(lmer_food_temp_1, "SR", by = "Treat")
Anova(lmer_food_temp_1, type = "II") #<- ran with Type II beacuse interaction was non-significant
grpMeans_food_1 <- emmeans(lmer_food_temp_1, ~ SR*Treat, data = meso_food_tank_temp)
pairs(grpMeans_food_1, simple = list("SR", "Treat"))

#Survival: since these data are proportion, you have to run a generalized mixed-effects model, with the RV_survival df
#Because I have averaged the survival within tanks, tank is now my 'unit of observation' 
meso_surv_1 <- lmer(cumsurv ~ SR*Treat + (1|SP), data = meso_clean_surv_temp)
summary(meso_surv_1)

#Verify assumptions of model (dispersal increases as the variables increase...)
plot(meso_surv_1)
visreg(meso_surv_1, "SR", by = "Treat")
Anova(meso_surv_1, type = "III") 

#Since there are no positive interactions, use the following notation for the Tukey posthoc
grpMeans_surv_1 <- emmeans(meso_surv_1, ~ SR + Treat, data = meso_clean_surv_temp)
pairs(grpMeans_surv_1, simple = list("SR", "Treat"))

#Build linear mixed effects models for fact exp----
#Fixed effects: SR, Temp, pH & intxns
#Random effects: Tank & Sp (1|Tank), (1|SP)
lmer_length_1 <- lmer(diff_l ~ SR*Temp + pH + initL + (1|Tank) + (1|SP), data = meso_lm_fact)
lmer_length_2 <- lmer(finL ~ SR*Temp + pH + initL + (1|Tank) + (1|SP), data = meso_lm_fact)
summary(lmer_length_1)
summary(lmer_length_2)

#Verify assumptions of model
plot(lmer_length_1)
visreg(lmer_length_1, "SR", by = "Treat")
visreg(lmer_length_1, "initL", by = "SR", overlay = TRUE)
visreg(lmer_length_1, "initL", by = "Treat", overlay = TRUE)

#Analyse mixed-effects model using anova & Tukey posthoc test with emmeans, with kenward-roger df method
Anova(lmer_length_1, type = "III")
Anova(lmer_length_2, type = "III") 

#Since there are significant interactions, use the following notation for the Tukey posthoc
grpMeans_length_1 <- emmeans(lmer_length_1, ~ SR*Temp, data = meso_lm_fact)
pairs(grpMeans_length_1, simple = list("SR", "Temp"))
grpMeans_length_2 <- emmeans(lmer_length_2, ~ SR*Temp, data = meso_lm_fact)
pairs(grpMeans_length_2, simple = list("SR", "Temp"))

#Shell thickness: 
lmer_thick_1 <- lmer(diff_Th ~ SR*Temp + SR*pH + initTh + (1|Tank), data = meso_lm_fact) #<- (1|SP) was singular fit so removed it
lmer_thick_2 <- lmer(finTh ~ SR*Temp + SR*pH + initTh + (1|Tank), data = meso_lm_fact)
summary(lmer_thick_1)
summary(lmer_thick_2)

#Verify assumptions of model
plot(lmer_thick_1)
visreg(lmer_thick_1, "SR", by = "Temp")
visreg(lmer_thick_1, "iniTh", by = "SR", overlay = TRUE)
visreg(lmer_thick_1, "initTh", by = "Temp", overlay = TRUE)
plot(lmer_thick_2)
visreg(lmer_thick_2, "SP", by = "Temp")
visreg(lmer_thick_1, "initTh", by = "SP", overlay = TRUE)

#Analyse mixed-effects model using anova & Tukey posthoc test with emmeans, with kenward-roger df method
Anova(lmer_thick_1, type = "III")
Anova(lmer_thick_2, type = "III")

#Since there are significant interactions, use the following notation for the Tukey posthoc
grpMeans_thick_1 <- emmeans(lmer_thick_1, ~ SR*Temp + SR*pH, data = meso_lm_fact)
pairs(grpMeans_thick_1, simple = list("SR", "Temp", "pH"))
grpMeans_thick_2 <- emmeans(lmer_thick_2, ~ SR*Temp + SP*pH, data = meso_lm_fact)
pairs(grpMeans_thick_2, simple = list("SR", "Temp", "pH"))

#Shell weight: importantly, I dropped (1|SP) from this model due to singular fit
lmer_ShW_1 <- lmer(diff_ShW ~ SR + Temp*pH + (1|Tank) + (1|SP), data = meso_lm_fact) #<- removed interactions & initShW b/c non-significant
lmer_ShW_2 <- lmer(finShW ~ SR + Temp*pH + initShW + (1|Tank) + (1|SP), data = meso_lm_fact)
summary(lmer_ShW_1)
summary(lmer_ShW_2)

#Verify assumptions of model
plot(lmer_ShW_1)
visreg(lmer_ShW_1, "SR", by = "Temp")
visreg(lmer_ShW_1, "initShW", by = "SR", overlay = TRUE)
visreg(lmer_ShW_1, "initShW", by = "Temp", overlay = TRUE)
plot(lmer_ShW_2)
visreg(lmer_ShW_2, "SP", by = "Temp")
visreg(lmer_ShW_1, "initShW", by = "SP", overlay = TRUE)

#Analyse mixed-effects model using anova & Tukey posthoc test with emmeans, with kenward-roger df method
Anova(lmer_ShW_1, type = "III") 
Anova(lmer_ShW_2, type = "III")

#Since there are no significant interactions, use the following notation for the Tukey posthoc
grpMeans_ShW_1 <- emmeans(lmer_ShW_1, ~ SR + pH*Temp, data = meso_lm_fact)
pairs(grpMeans_ShW_1, simple = list("SR", "Temp", "pH"))
grpMeans_ShW_2 <- emmeans(lmer_ShW_2, ~ SP + Temp, data = meso_lm_fact)
pairs(grpMeans_ShW_2, simple = list("SP", "Temp"))

#Tissue weight
lmer_TiW_1 <- lmer(diff_TiW ~ SR*Temp + pH + (1|Tank) + (1|SP), data = meso_lm_fact) #<- removed init TiW because non-significant
lmer_TiW_2 <- lmer(finTiW ~ SR*Temp + pH + initTiW + (1|Tank) + (1|SP), data = meso_lm_fact) 
summary(lmer_TiW_1)
summary(lmer_TiW_2)

#Verify assumptions of model
plot(lmer_TiW_1)
visreg(lmer_TiW_1, "SR", by = "Temp")
visreg(lmer_TiW_1, "initTiW", by = "SR", overlay = TRUE)
visreg(lmer_TiW_1, "initTiW", by = "Temp", overlay = TRUE)
plot(lmer_TiW_2)
visreg(lmer_TiW_2, "SP", by = "Temp")
visreg(lmer_TiW_1, "initTiW", by = "SP", overlay = TRUE)

#Analyse mixed-effects model using anova & Tukey posthoc test with emmeans, with kenward-roger df method
Anova(lmer_TiW_1, type = "III")
Anova(lmer_TiW_2, type = "III") 

#Since there are significant interactions, use the following notation for the Tukey posthoc
grpMeans_TiW_1 <- emmeans(lmer_TiW_1, ~ SR*Temp + pH, data = meso_lm_fact)
pairs(grpMeans_TiW_1, simple = list("SR", "Temp", "pH"))
grpMeans_TiW_2 <- emmeans(lmer_TiW_2, ~ SP*Treat + pH, data = meso_lm_fact)
pairs(grpMeans_TiW_2, simple = list("SP", "Temp", "pH"))

#Feeding rate: I'm just going to analyze the final per capita weekly feeding rate. Note that because tank is your unit of replication here, 
#you don't need it as a random effect
lmer_food_fact_1 <- lmer(meanPer_cap ~ SR*pH + Temp + (1|SP), data = meso_food_tank_fact)
summary(lmer_food_fact_1)
plot(lmer_food_fact_1)
visreg(lmer_food_fact_1, "SR", by = "Temp")
visreg(lmer_food_fact_1, "SR", by = "pH")
Anova(lmer_food_fact_1, type = "III")
grpMeans_food_1 <- emmeans(lmer_food_fact_1, ~ SR*pH + Temp, data = meso_food_tank_fact)
pairs(grpMeans_food_1, simple = list("SR", "Temp", "pH"))

#Survival: since these data are proportion, you have to run a generalized mixed-effects model, with the RV_survival df
#Because I have averaged the survival within tanks, tank is now my 'unit of observation' 
meso_surv_1 <- lm(cumsurv ~ SR*Temp + pH, data = meso_clean_surv_fact) #<- removed (1|SP) because singular fit
summary(meso_surv_1)

#Verify assumptions of model (dispersal increases as the variables increase...)
plot(meso_surv_1)
plot(meso_surv_2)
visreg(lmer_surv_1, "SR", by = "OR")
visreg(lmer_surv_2, "SP", by = "OS")

Anova(meso_surv_1, type = "III") 
Anova(meso_surv_2, type = "III") 

#Since there are no positive interactions, use the following notation for the Tukey posthoc
grpMeans_surv_1 <- emmeans(meso_surv_1, ~ SR + Temp + pH, data = meso_clean_surv_fact)
pairs(grpMeans_surv_1, simple = list("SR", "Temp", "pH"))
grpMeans_surv_2 <- emmeans(meso_surv_2, ~ SP + Temp + pH, data = meso_clean_surv_fact)
pairs(grpMeans_surv_2, simple = list("SP", "Temp", "pH"))

#Remove this at the end of your process once you're decided to not run the survival analysis for anything, since data violate assumptions----
sfit <- survfit(Surv(days_diff, Dead) ~ SR + Temp, data = meso_surv_temp)
summary(sfit)
survdiff(Surv(days_diff, Dead) ~ SR + Temp, data = meso_surv_temp)
surv_pvalue(sfit)
print(sfit, rmean = 49)

#Have to plot these results, rmean calculates the area under the curve for each treatment, can select the time if I have a biologically meaningful duration (e.g. a month) to estimate survival times for each population

#Importantly, there was no mortality in the 12 treatments --> cox model is not good fit b/c infinite values introduced
#https://ramaanathan.github.io/SurvivalAnalysis/
survreg(formula = Surv(days_diff, Dead) ~ Temp, data = meso_surv_temp)

meso_surv_temp_2 <- meso_surv_temp %>% 
  filter(Temp == 15 | Temp == 19 | Temp == 22) %>% 
  droplevels

cph <- coxph(Surv(days_diff, Dead) ~ SR + Temp, data = meso_surv_temp)
cph_2 <- coxph(Surv(days_diff, Dead) ~ SR + Temp, data = meso_surv_temp_2)

ggforest(cph, data = meso_surv_temp)
summary(cph)

#Tests assumptions of proportional hazards across treatments
test <- cox.zph(cph)
plot(test)
#The fact that the global model is significant means that I violated the assumption of proportional hazards --> cannot use cph

cph.int <- coxph(Surv(days_diff, DIED) ~ SR * OR, data = RV_surv)

#When I include the interaction in the cph, some of the summary estimates become 0 & inf. The Anova output remains largely the same though, so I'll just go with the cph model
#I would like to test the interactive effect, but not sure how within this type of analysis
summary(cph)
summary(cph.int)
Anova(cph)
Anova(cph.int)
summary(cph)$sctest[3]
round(summary(cph)$sctest[3], digits = 9) == round(surv_pvalue(sfit)[,2], digits = 9)


ggsurvplot(sfit, legend.title = "Source Region", xlab = "Time, days", data = meso_surv_temp)


#Remove this as well: Clean the survival data for analysis----
#Remove the failed tanks (3, 5, 6) and assign the correct tank treatments, separate treat into temp & pH columns
meso_survival_clean <- meso_survival[, c(1:7)] %>% 
  filter(!Tank %in% tanks_remove) %>% 
  mutate(Treat = as.factor(ifelse(Tank == 9 | Tank == 12, "12.A",
                                  ifelse(Tank == 4 | Tank == 7 | Tank == 11 | Tank == 14 | Tank == 16, "15.A",
                                         ifelse(Tank == 17 |  Tank == 20 | Tank == 21 | Tank == 24, "15.L",
                                                ifelse(Tank == 1 | Tank == 10 | Tank == 13, "19.A",
                                                       ifelse(Tank == 2 | Tank == 8 | Tank == 15, "22.A",
                                                              ifelse(Tank == 18 | Tank == 19 | Tank == 22 | Tank == 23, "22.L", NA)))))))) %>% 
  separate(Treat, c("Temp", "pH"), remove = FALSE) %>% 
  mutate(SP = as.factor(SP),
         SR = as.factor(SR),
         Temp = as.factor(Temp),
         pH = as.factor(pH),
         Date = as.Date(Date))

##Create a column that calculates the number of days between each measurement date
dates_surv <- as.factor(meso_survival_clean$Date)
dates_surv <- data.frame(levels(dates_surv))

dates_surv <- dates_surv %>% 
  rename(date_record = levels.dates_surv.) %>% 
  mutate(init_date = as.Date("2018-07-16"),
         date_record = as.Date(date_record),
         days_diff = date_record - init_date)

#Create a column with the days between the start and end of each snail's experiment duration (longest is 49 days unless they died earlier)
meso_surv <- meso_survival_clean %>% 
  mutate(days_diff = ifelse(Date == "2018-07-16", 0,
                            ifelse(Date == "2018-07-28", 12,
                                   ifelse(Date == "2018-07-30", 14,
                                          ifelse(Date == "2018-08-01", 16, 
                                                 ifelse(Date == "2018-08-02", 17,
                                                        ifelse(Date == "2018-08-03", 18,
                                                               ifelse(Date == "2018-08-04", 19,
                                                                      ifelse(Date == "2018-08-05", 20,
                                                                             ifelse(Date == "2018-08-06", 21,
                                                                                    ifelse(Date == "2018-08-08", 23,
                                                                                           ifelse(Date == "2018-08-10", 25,
                                                                                                  ifelse(Date == "2018-08-12", 27,
                                                                                                         ifelse(Date == "2018-08-13", 28,
                                                                                                                ifelse(Date == "2018-08-14", 29,
                                                                                                                       ifelse(Date == "2018-08-21", 36,
                                                                                                                              ifelse(Date == "2018-08-23", 38,
                                                                                                                                     ifelse(Date == "2018-08-24", 39,
                                                                                                                                            ifelse(Date == "2018-08-28", 43,
                                                                                                                                                   ifelse(Date == "2018-08-29", 44,
                                                                                                                                                          ifelse(Date == "2018-09-03", 49, NA)))))))))))))))))))))

#Now create a new dataset with daily observations of death or survival (i.e. for any snails that died, make sure that they have a 1 for every day after until the end of the experiment)
#First remove CO14 from Tank 18 because it was a typo (that snail was never in that tank...)
meso_surv_1 <- meso_surv %>% 
  mutate(unique_ID = paste(ID, Tank, sep = "_")) %>% 
  filter(unique_ID != "CO14_18") %>% 
  select(!unique_ID)

#Remove the days_diff == 0, because that takes away the double event for each ID
meso_surv_1 <- meso_surv_1 %>% 
  filter(!days_diff == 0)

#Create a new datasframe that duplicates this one 49 times
meso_surv_2 <- meso_surv_1 %>% 
  slice(rep(1:n(), each = 49)) %>% 
  arrange(ID)

meso_surv_2 <- meso_surv_2 %>% 
  group_by(ID) %>% 
  mutate(day_exp = c(1:49)) %>% 
  mutate(dead_repeat = ifelse(days_diff == 49, 0,
                              ifelse(day_exp >= days_diff, 1, 0)))

meso_surv_temp <- meso_surv_1 %>% 
  filter(pH == "A") %>% 
  droplevels
meso_surv_fact <- meso_surv %>% 
  filter(Temp == 15 | Temp == 22) %>% 
  droplevels

#Survival: since these data are binomial, you have to run a generalized mixed-effects model, with the RV_survival df

#Double check that there aren't any duplicate IDs

#Change structure of datasheet
#Analysis will be repeated measures, and unit of measurement is individual by tank + (1|Tank/ID) <- intercepts vary by tank and by ID within Tank
#Need time as a predictor variable also
#Need continuous 0 and 1 (i.e. for any point that you measured record whether it was alive or dead)

glm_surv_temp <- glmer(dead_repeat ~ day_exp + SP + Temp + (1|Tank/ID), family = binomial, data = meso_surv_temp)
summary(glm_surv_temp)
visreg(glm_surv_temp)


summary(glm_surv_temp_1)
Anova(glm_surv_temp, type = "III")
Anova(glm_surv_temp_2, type = "III")

# Visualize fit 
visreg(glm_surv_temp_1)
visreg(glm_surv_temp_1, "Temp", by = "SR", overlay = TRUE)
plotresid(glm_surv_temp_1)


ggplot(meso_surv_temp, aes(day_exp, dead_repeat)) + 
  geom_point(size = 2, col = "firebrick") + 
  geom_smooth(method = "loess", size = 1, col = "black", span = 0.8) +
  theme_classic()

grpMeans_surv <- emmeans(glm_surv_temp, ~ SP*Temp, data = meso_surv_temp)
pairs(grpMeans_surv, simple = list("SP", "Temp"))


#Visualize change in growth across treatments grouped by SP for temp exp----
#The datapoints being visualized are each tank  within each treatment for each SP :) The correct unit of replication! 
plot_temp_me <- function(df, x, y, grp, clr.values, lbl.y){
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

length_temp_SP_me <- plot_temp_me(meso_lm_block_temp, Temp, meandiff_l, SP, c("coral", "coral3", "skyblue", "skyblue3"), "Change in SL (mm)") + labs(colour = "Source Population")
thick_temp_SP_me <- plot_temp_me(meso_lm_block_temp, Temp, meandiff_Th, SP, c("coral", "coral3", "skyblue", "skyblue3"), "Change in ST (mm)")
ShW_temp_SP_me <- plot_temp_me(meso_lm_block_temp, Temp, meandiff_ShW, SP, c("coral", "coral3", "skyblue", "skyblue3"), "Change in ShW (g)")
TiW_temp_SP_me <- plot_temp_me(meso_lm_block_temp, Temp, meandiff_TiW, SP, c("coral", "coral3", "skyblue", "skyblue3"), "Change in TiW (g)")
SG_temp_SP_me <- plot_temp_me(meso_lm_block_temp, Temp, mean_SG, SP, c("coral", "coral3", "skyblue", "skyblue3"), "Change in LSG (mm)")
Food_temp_SP_me <- plot_temp_me(meso_lm_block_temp, Temp, meanPer_cap, SP, c("coral", "coral3", "skyblue", "skyblue3"), "Per capita weekly feeding rate")
Surv_temp_SP_me <- plot_temp_me(meso_lm_block_temp, Temp, cumsurv, SP, c("coral", "coral3", "skyblue", "skyblue3"), "Survival (%)")

meso_temp_comb_SP_me <- plot_grid(length_temp_SP_me + theme(legend.position = "none", axis.text.x = element_blank(), axis.title.x = element_blank()),
                                  thick_temp_SP_me + theme(legend.position = "none", axis.text.x = element_blank(), axis.title.x = element_blank()), 
                                  SG_temp_SP_me + theme(legend.position = "none", axis.text.x = element_blank(), axis.title.x = element_blank()),
                                  get_legend(length_temp_SP_me),
                                  ShW_temp_SP_me + theme(legend.position = "none", axis.title.x = element_blank()), 
                                  TiW_temp_SP_me + theme(legend.position = "none", axis.title.x = element_blank()),
                                  Food_temp_SP_me + theme(legend.position = "none", axis.title.x = element_blank()),
                                  Surv_temp_SP_me + theme(legend.position = "none", axis.title.x = element_blank()),
                                  ncol = 4, nrow = 2, axis = "lb", align = "hv")

xaxistitle_treat <- ggdraw() + draw_label("Treatment", fontface = "plain", x = 0.5, hjust = 0, size = 16)
meso_temp_comb_title_SP_me <- plot_grid(meso_temp_comb_SP_me, xaxistitle_treat, ncol = 1, rel_heights = c(1, 0.05))

ggsave(meso_temp_comb_title_SP_me, file = "plots/snails/meso/meso_temp_SP_me.pdf", height = 8, width = 17, dpi = 300)

#Visualize change in growth across treatments grouped by SP for fact exp----
#The datapoints being visualized are each tank  within each treatment for each SP :) The correct unit of replication! 
plot_fact_me <- function(df, x, y, grp, tmp, clr.values, lbl.y) {
  plot_fact <- ggplot(df, aes({{x}}, {{y}}, group = {{grp}}, colour = {{grp}})) +
    geom_point(alpha=0.3, position = position_jitterdodge(dodge.width = 0.3, jitter.width=0.05)) +
    stat_summary(fun=mean, geom="point", size = 3, position=position_dodge(0.3)) +
    stat_summary(fun = mean, geom = "line", size = 0.8, position=position_dodge(0.3), alpha = 0.5, aes(group = interaction({{tmp}}, {{grp}}))) +
    stat_summary(fun.data = "mean_se", geom = "errorbar", width = 0.2, size = 0.5,
                 position=position_dodge(0.3)) +
    scale_colour_manual(values = clr.values) +
    labs(x = "Treatment", y = lbl.y) +
    theme_cowplot(16)
  return(plot_fact)
}

length_fact_SP_me <- plot_fact_me(meso_lm_block_fact, Treat, meandiff_l, SP, Temp, c("coral", "coral3", "skyblue", "skyblue3"), "Change in SL (mm)") + labs(colour = "Source Population")
thick_fact_SP_me <- plot_fact_me(meso_lm_block_fact, Treat, meandiff_Th, SP, Temp, c("coral", "coral3", "skyblue", "skyblue3"), "Change in ST (mm)")
ShW_fact_SP_me <- plot_fact_me(meso_lm_block_fact, Treat, meandiff_ShW,  SP, Temp, c("coral", "coral3", "skyblue", "skyblue3"), "Change in ShW (g)")
TiW_fact_SP_me <- plot_fact_me(meso_lm_block_fact, Treat, meandiff_TiW,  SP, Temp, c("coral", "coral3", "skyblue", "skyblue3"), "Change in TiW (g)")
SG_fact_SP_me <- plot_fact_me(meso_lm_block_fact, Treat, mean_SG, SP, Temp, c("coral", "coral3", "skyblue", "skyblue3"), "Change in LSG (mm)")
Food_fact_SP_me <- plot_fact_me(meso_lm_block_fact, Treat, meanPer_cap, SP, Temp, c("coral", "coral3", "skyblue", "skyblue3"), "Weekly per capita feeding rate")
Surv_fact_SP_me <- plot_fact_me(meso_lm_block_fact, Treat, cumsurv, SP, Temp, c("coral", "coral3", "skyblue", "skyblue3"), "Survival (%)")

length_fact_SP_me_temp <- plot_fact_me(meso_lm_block_fact, Treat, meandiff_l, SP, pH, c("coral", "coral3", "skyblue", "skyblue3"), "Change in SL (mm)") + labs(colour = "Source Population")
thick_fact_SP_me_temp <- plot_fact_me(meso_lm_block_fact, Treat, meandiff_Th, SP, pH, c("coral", "coral3", "skyblue", "skyblue3"), "Change in ST (mm)")
ShW_fact_SP_me_temp <- plot_fact_me(meso_lm_block_fact, Treat, meandiff_ShW,  SP, pH, c("coral", "coral3", "skyblue", "skyblue3"), "Change in ShW (g)")
TiW_fact_SP_me_temp <- plot_fact_me(meso_lm_block_fact, Treat, meandiff_TiW,  SP, pH, c("coral", "coral3", "skyblue", "skyblue3"), "Change in TiW (g)")
SG_fact_SP_me_temp <- plot_fact_me(meso_lm_block_fact, Treat, mean_SG, SP, pH, c("coral", "coral3", "skyblue", "skyblue3"), "Change in LSG (mm)")
Food_fact_SP_me_temp <- plot_fact_me(meso_lm_block_fact, Treat, meanPer_cap, SP, pH, c("coral", "coral3", "skyblue", "skyblue3"), "Weely per capita feeding rate")
Surv_fact_SP_me_temp <- plot_fact_me(meso_lm_block_fact, Treat, cumsurv, SP, pH, c("coral", "coral3", "skyblue", "skyblue3"), "Survival (%)")

meso_fact_comb_SP_me <- plot_grid(length_fact_SP_me + theme(legend.position = "none", axis.text.x = element_blank(), axis.title.x = element_blank()),
                                  thick_fact_SP_me + theme(legend.position = "none", axis.text.x = element_blank(), axis.title.x = element_blank()), 
                                  SG_fact_SP_me + theme(legend.position = "none", axis.text.x = element_blank(), axis.title.x = element_blank()),
                                  get_legend(length_fact_SP_me),
                                  ShW_fact_SP_me + theme(legend.position = "none", axis.title.x = element_blank()), 
                                  TiW_fact_SP_me + theme(legend.position = "none", axis.title.x = element_blank()),
                                  Food_fact_SP_me + theme(legend.position = "none", axis.title.x = element_blank()),
                                  Surv_fact_SP_me + theme(legend.position = "none", axis.title.x = element_blank()),
                                  ncol = 4, nrow = 2, axis = "lb", align = "hv")

meso_fact_comb_SP_me_temp <- plot_grid(length_fact_SP_me_temp + theme(legend.position = "none", axis.text.x = element_blank(), axis.title.x = element_blank()),
                                       thick_fact_SP_me_temp + theme(legend.position = "none", axis.text.x = element_blank(), axis.title.x = element_blank()), 
                                       SG_fact_SP_me_temp + theme(legend.position = "none", axis.text.x = element_blank(), axis.title.x = element_blank()),
                                       get_legend(length_fact_SP_me_temp),
                                       ShW_fact_SP_me_temp + theme(legend.position = "none", axis.title.x = element_blank()), 
                                       TiW_fact_SP_me_temp + theme(legend.position = "none", axis.title.x = element_blank()),
                                       Food_fact_SP_me_temp + theme(legend.position = "none", axis.title.x = element_blank()),
                                       Surv_fact_SP_me_temp + theme(legend.position = "none", axis.title.x = element_blank()),
                                       ncol = 4, nrow = 2,  axis = "lb", align = "hv")

xaxistitle_treat <- ggdraw() + draw_label("Treatment", fontface = "plain", x = 0.5, hjust = 0, size = 16)
meso_fact_comb_title_SP_me <- plot_grid(meso_fact_comb_SP_me, xaxistitle_treat, ncol = 1, rel_heights = c(1, 0.05))
meso_fact_comb_title_SP_me_temp <- plot_grid(meso_fact_comb_SP_me_temp, xaxistitle_treat, ncol = 1, rel_heights = c(1, 0.05))

ggsave(meso_fact_comb_title_SP_me, file = "plots/snails/meso/meso_fact_SP_me.pdf", height = 8, width = 17, dpi = 300)
ggsave(meso_fact_comb_title_SP_me_temp, file = "plots/snails/meso/meso_fact_SP_me_temp.pdf", height = 8, width = 17, dpi = 300)


#Remove variables----
rm(cedar_reg, heron_reg, kwak_reg, pruth_reg, snails_remove, tanks_remove, meso_clean_1, meso_clean_2, meso_clean_3,
   meso_clean_surv_1)
