#Snail weight regressions

#Load in necessary packages----
pkgs <- c("dplyr", "tidyverse", "ggplot2", "cowplot", "ggpubr")
lapply(pkgs, library, character.only = TRUE)
rm(pkgs)

#Read & clean csv----
reg <- read.csv("data/snail_RVs/Weight_regressions.csv")

reg_clean <- reg %>% 
  mutate(Site = as.factor(Site),
         SR = ifelse(Site == "Cedar" | Site == "Heron", "Nanaimo", "Calvert")) %>% 
  rename(SW = Submersed.weight, DW = Dry.shell.weight)

reg_clean_nan <- reg_clean %>% 
  filter(SR == "Nanaimo")
reg_clean_cal <- reg_clean %>% 
  filter(SR == "Calvert")

#Plot relationship with regression line
nan <- ggscatter(reg_clean_nan, x = "SW", y = "DW") + 
  stat_regline_equation(label.y = 20) +
  stat_regline_equation(label.y = 18, aes(label = ..rr.label..)) +
  geom_smooth(method = "lm", se=FALSE, color = "black") +
  facet_wrap(~Site) + 
  labs(y = "DW (g)")

cal <- ggscatter(reg_clean_cal, x = "SW", y = "DW") + 
  stat_regline_equation(label.y = 5) +
  stat_regline_equation(label.y = 4.5, aes(label = ..rr.label..)) +
  geom_smooth(method = "lm", se=FALSE, color = "black") +
  facet_wrap(~Site) +
  labs(y = "DW (g)", x = "SW (g)")

both <- plot_grid(nan + theme(axis.title.x = element_blank()), cal, nrow = 2)
 
ggsave(both, file = "plots/supp_figs/FigS3.pdf", height = 8, width = 8, dpi = 300)
