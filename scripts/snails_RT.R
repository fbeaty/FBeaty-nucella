#Script to analyze & visualize the snail growth RVs (length, shell thickness etc) from the 2019 Reciprocal Transplant

#Load packages----
pkgs <- c("tidyverse", "viridis", "lubridate", "car")
lapply(pkgs, library, character.only = TRUE)
rm(pkgs)

#Load csv and clean it----
RV_base <- read.csv("data/snail_RVs/RT_2_final.csv")

#Convert to dates & factors, and calulate tissue weight based upon the TW & SW columns
#Note that there are no 'SG' for the initial time periods, so these values are NA
#Also, due to equipment failure we were unable to measure SW in the 'Mid' timepoint, so those values are also NA

RV_clean <- RV_base %>% 
  mutate(Date = as.Date(Date, format = "%m-%d-%Y"),
         Stage = factor(Stage, levels = c("Init", "Mid", "Final")),
         SR = as.factor(SR),
         SP = as.factor(SP),
         OR = as.factor(OR),
         OS = as.factor(OS),
         Block = as.factor(Block),
         TiW = TW-SW) %>% 
  select(Date, Stage, SR, SP, OR, OS, Block, ID, L, T, TW, SW, TiW, SG, DIED, OSTRINA)

#Calculate & visualize the average & SD of growth metrics in the 3 time periods ----
RV_sum_block <- RV_clean %>% 
  group_by(Stage, OR, SP, Block) %>% 
  summarize(meanL = mean(L, na.rm = TRUE), sdL = sd(L, na.rm = TRUE)) %>% 
  ungroup()

RV_sum_OR <- RV_sum_block %>% 
  group_by(Stage, OR, SP) %>% 
  summarize(meanL_OR = mean(meanL, na.rm = TRUE), sdL_OR = sd(meanL, na.rm = TRUE)) %>% 
  ungroup()

RV_sum_cal <- RV_sum_OR %>% 
  filter(OR == "Calvert")

RV_sum_nan <- RV_sum_OR %>% 
  filter(OR == "Nanaimo")

my_theme <- theme(axis.title.x = element_text(size = 20), axis.text.x = element_text(size = 18),
                  axis.text.y = element_text(size = 18), axis.title.y = element_text(size = 20), 
                  legend.text = element_text(size = 20),
                  panel.border = element_blank(), panel.grid.major = element_blank(),
                  panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))

length_cal_stage<- ggplot(RV_sum_cal, aes(Stage, meanL_OR, group = SP, colour = SP)) + 
  geom_point(aes(size = 0.5), position=position_dodge(0.3)) +
  geom_line(aes(size= 0.3), position = position_dodge(0.3)) +
  geom_errorbar(aes(ymin=meanL_OR-sdL_OR, ymax=meanL_OR+sdL_OR, size = 0.2), width=.2,
                position=position_dodge(0.3)) +
  scale_colour_manual(values = c("coral", "coral3", "skyblue", "skyblue3")) +
  theme_bw() + labs(x = "Stage, Calvert", y = "Mean Length") +
  my_theme

length_nan_stage <- ggplot(RV_sum_nan, aes(Stage, meanL_OR, group = SP, colour = SP)) + 
  geom_point(aes(size = 0.5), position=position_dodge(0.3)) +
  geom_line(aes(size = 0.3), position = position_dodge(0.3)) +
  geom_errorbar(aes(ymin=meanL_OR-sdL_OR, ymax=meanL_OR+sdL_OR, size = 0.2), width=.2,
                position=position_dodge(0.3)) +
  scale_colour_manual(values = c("coral", "coral3", "skyblue", "skyblue3")) +
  theme_bw() + labs(x = "Stage, Nanaimo", y = "Mean Length") +
  my_theme

ggsave(length_cal_stage, file = "plots/snails/RT/length_cal_stage.pdf", height = 4, width = 8, dpi = 300)
ggsave(length_nan_stage, file = "plots/snails/RT/length_nan_stage.pdf", height = 4, width = 8, dpi = 300)

#Test whether there's a significant effect of time (or an interesting one)
anova_model <- aov(meanL ~ SP + Stage, data = sum_both)
Anova(ancova_model, type="III")
visreg(ancova_model)

