#Calculate Thermal Safety Margins!!
#This script requires the following objects:
#     temp95th_month_day from the lighthouse script
#     temp95th_water from the iButtons_RT script
#     temp95th_all from the iButtons_RT script
#     params_bootstrapped from the Bootstrapping_TPC script

#Collate the dataframes into one df
#You can calculate the variance of the TSM metric for the lighthouse parameter using the bootstrapped variance from the CTmax 
#estimate and the max 95th percentile variance in the years between 2012-2020

TPC_df <- params_bootstrapped %>% 
  filter(param == "CTmax") %>% 
  mutate(t95th_iButton_water = ifelse(SR == "Central Coast", 16.58333, 19.25000),
         t95th_lighthouse = ifelse(SR == "Central Coast", 16.0, 20.2),
         t95th_iButton_all = ifelse(SR == "Central Coast", 17.5, 24)) %>% 
  select(!c(model, method)) %>% 
  mutate(TSM_water_iButton = estimate - t95th_iButton_water,
         TSM_water_lighthouse = estimate - t95th_lighthouse,
         TSM_all_iButton = estimate - t95th_iButton_all) %>% 
  pivot_longer(names_to = 'TSM_metric', values_to = 'TSM_estimate', TSM_water_iButton:TSM_all_iButton) %>% 
  mutate(t95th_ci.lower = ifelse(TSM_metric == "TSM_water_lighthouse", 
                                 ifelse(SR == "Central Coast", 15.16543, 19.39737), NA),
         t95th_ci.upper = ifelse(TSM_metric == "TSM_water_lighthouse", 
                                 ifelse(SR == "Central Coast", 16.36457, 20.46513), NA)) %>% 
  mutate(TSM_lower_ci = ifelse(TSM_metric == "TSM_water_lighthouse", conf_lower - t95th_ci.lower, NA),
         TSM_upper_ci = ifelse(TSM_metric == "TSM_water_lighthouse", conf_upper - t95th_ci.upper, NA))  %>% 
  arrange(desc(TSM_metric))

#Create final table to export!
TPC_select <- TPC_df %>% 
  select(!param) %>% 
  rename("CTmax" = "estimate", "CTmax.ci.lower" = "conf_lower", "CTmax.ci.upper" = "conf_upper", 
         "iButton_water" = "t95th_iButton_water", "iButton_all" = "t95th_iButton_all",
         "lighthouse" = "t95th_lighthouse") %>% 
  select(SR, RV, TSM_metric, CTmax, CTmax.ci.lower, CTmax.ci.upper, iButton_water, iButton_all, lighthouse, TSM_estimate)

TPC_select_round <- TPC_select %>% 
  mutate(across(where(is.numeric), ~ round(., 2)))

write.csv(TPC_select_round, file = "data/TSM/TPC_select.csv")

TPC_select_average <- TPC_select %>% 
  group_by(SR, TSM_metric) %>% 
  summarize(RV = 'ave', TSM_estimate = mean(TSM_estimate)) %>% 
  bind_rows(TPC_select %>% 
              mutate(RV = as.character(RV)), .)

TPC_summarized <- TPC_select %>% 
  group_by(SR, TSM_metric) %>% 
  summarize(mean_TSM = mean(TSM_estimate), sd_TSM = sd(TSM_estimate), n_TSM = n()) %>% 
  mutate(se_TSM = sd_TSM/sqrt(n_TSM),
         lower.ci.TSM = mean_TSM - qt(1 - (0.05 / 2), n_TSM - 1) * se_TSM,
         upper.ci.TSM = mean_TSM + qt(1 - (0.05 / 2), n_TSM - 1) * se_TSM) %>% 
  select(!c(sd_TSM, se_TSM, n_TSM)) %>% 
  mutate(across(where(is.numeric), ~ round(., 2)),
         RV = "ave")

write_csv(TPC_summarized, file = "data/TSM/TPC_summarized.csv")

#Try visualizing these
# plot data and model fit
level_order = c("TiW", "ShW", "L", "FR", "ave")
name_RV = c("Tissue weight", "Shell weight", "Shell length", "Feeding rate", "Average")

piButton_water <- ggplot(data = subset(TPC_select_average, TSM_metric %in% "TSM_water_iButton"), aes(y = TSM_estimate, x = RV, col = SR)) +
  geom_point(size = 3, position=position_dodge(0.8)) +
  geom_vline(xintercept = 4.5, linetype = "dashed", alpha = 0.8, col = "grey") +
  scale_colour_manual(values = c("skyblue", "coral")) +
  scale_x_discrete(limits = level_order, labels = name_RV) +
  labs(x = 'Response variable',
       y = 'Thermal safety margin',
       col = "Source Region") + 
  theme_cowplot(16) +
  theme(legend.position = c(0.1, 0.8))

piButton_water

ggsave(piButton_water, file = "plots/TSM/TSM_iButton_water.pdf", height = 5, width = 8, dpi = 300)

piButton_all <- ggplot(data = subset(TPC_select_average, TSM_metric %in% "TSM_all_iButton"), aes(y = TSM_estimate, x = RV, col = SR)) +
  geom_point(size = 3, position=position_dodge(0.8)) +
  geom_vline(xintercept = 4.5, linetype = "dashed", alpha = 0.8, col = "grey") +
  geom_hline(yintercept = 0, linetype = "dashed", alpha = 0.8, col = "grey") +
  scale_colour_manual(values = c("skyblue", "coral")) +
  scale_x_discrete(limits = level_order, labels = name_RV) +
  labs(x = 'Response variable',
       y = 'Thermal safety margin',
       col = "Source Region",
       title = "iButton water and air") + 
  theme_cowplot(16) +
  theme(legend.position = c(0.1, 0.8))

piButton_all

plighthouse <- ggplot(data = subset(TPC_select_average, TSM_metric %in% "TSM_water_lighthouse"), aes(y = TSM_estimate, x = RV, col = SR)) +
  geom_point(size = 3, position=position_dodge(0.8)) +
  geom_vline(xintercept = 4.5, linetype = "dashed", alpha = 0.8, col = "grey") +
  scale_colour_manual(values = c("skyblue", "coral")) +
  scale_x_discrete(limits = level_order, labels = name_RV) +
  labs(x = 'Response variable',
       y = 'Thermal safety margin',
       col = "Source Region",
       title = "Lighthouse, 2012-2020") + 
  ylim(1, 9) +
  theme_cowplot(16) +
  theme(legend.position = c(0.1, 0.8))

p_TSM_comb <- plot_grid(piButton_all + theme(legend.position = "none"),
                        plighthouse + theme(legend.position = "none", axis.title.y = element_blank()), 
                        get_legend(piButton_water),
                        ncol = 3, rel_widths = c(1, 1, 0.3))

p_TSM_comb

ggsave(p_TSM_comb, file = "plots/TSM/TSM_iButton_all_lighthouse.pdf", height = 5, width = 15.5, dpi = 300)
  

