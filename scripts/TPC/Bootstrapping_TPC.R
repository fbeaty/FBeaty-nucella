#Bootstrapping my models! Based on Padfield code
#https://padpadpadpad.github.io/rTPC/articles/bootstrapping_models.html

#Run this one after the TPC_meso so you have the right dataframes in your system

#TiW: Briere works well
#ShW: Briere misses lower CI for SoG and no error for 22 on CC, quadratic works just fine
#l: Briere has skewed variance in the lower and at 22... 
#fr: Briere works well for CC but again, low variance in the 22 but I think it's accurate in this case

# load packages----
library(boot)
library(car)
library(rTPC)
library(nls.multstart)
library(broom)
library(tidyverse)
library(patchwork)
library(minpack.lm)


#CC_TiW: Quadratic: Fit data----
# fit with Gaussian model
d_fit <- nest(CC_TiW, data = c(temp, rate)) %>%
  mutate(quadratic = map(data, ~nls_multstart(rate~quadratic_2008(temp = temp, a, b, c),
                                              data = .x,
                                              iter = c(4,4,4),
                                              start_lower = get_start_vals(.x$temp, .x$rate, model_name = 'quadratic_2008') - 10,
                                              start_upper = get_start_vals(.x$temp, .x$rate, model_name = 'quadratic_2008') + 10,
                                              lower = get_lower_lims(.x$temp, .x$rate, model_name = 'quadratic_2008'),
                                              upper = get_upper_lims(.x$temp, .x$rate, model_name = 'quadratic_2008'),
                                              supp_errors = 'Y',
                                              convergence_count = FALSE)),
         # create new temperature data
         new_data = map(data, ~tibble(temp = seq(min(.x$temp), max(.x$temp), length.out = 100))),
         # predict over that data,
         preds =  map2(quadratic, new_data, ~augment(.x, newdata = .y)))

# unnest predictions
d_preds_CC <- select(d_fit, preds) %>%
  unnest(preds) %>% 
  mutate(SR = "Central Coast")

# plot data and predictions
ggplot() +
  geom_line(aes(temp, .fitted), d_preds_CC, col = 'blue') +
  geom_point(aes(temp, rate), CC_TiW, size = 2, alpha = 0.5) +
  theme_bw(base_size = 12) +
  labs(x = 'Temperature (ºC)',
       y = 'Shell growth rate',
       title = 'Central Coast')

#CC_TiW: Quadratic: refit model using nlsLM----
fit_nlsLM <- minpack.lm::nlsLM(rate~quadratic_2008(temp = temp, a, b, c),
                               data = CC_TiW,
                               start = coef(d_fit$quadratic[[1]]),
                               lower = get_lower_lims(CC_TiW$temp, CC_TiW$rate, model_name = 'quadratic_2008'),
                               upper = get_upper_lims(CC_TiW$temp, CC_TiW$rate, model_name = 'quadratic_2008'),
                               weights = rep(1, times = nrow(CC_TiW)))

# bootstrap using case resampling
boot1 <- Boot(fit_nlsLM, method = 'case')

# look at the data
head(boot1$t)

hist(boot1, layout = c(2,2))

#CC_TiW: quadratic: Now plot the bootstrapped models----
#create predictions of each bootstrapped model
boot1_preds <- boot1$t %>%
  as.data.frame() %>%
  drop_na() %>%
  mutate(iter = 1:n()) %>%
  group_by_all() %>%
  do(data.frame(temp = seq(min(CC_TiW$temp), max(CC_TiW$temp), length.out = 100))) %>%
  ungroup() %>%
  mutate(pred = quadratic_2008(temp = temp, a, b, c))

# calculate bootstrapped confidence intervals
boot1_conf_preds_CC <- group_by(boot1_preds, temp) %>%
  summarise(conf_lower = quantile(pred, 0.025),
            conf_upper = quantile(pred, 0.975)) %>%
  ungroup() %>% 
  mutate(SR = "Central Coast")

# plot bootstrapped CIs
p_quadratic_CC_TiW <- ggplot() +
  geom_line(aes(temp, .fitted), d_preds_CC, col = 'blue') +
  geom_ribbon(aes(temp, ymin = conf_lower, ymax = conf_upper), boot1_conf_preds_CC, fill = 'blue', alpha = 0.3) +
  geom_point(aes(temp, rate), CC_TiW, size = 2, alpha = 0.5) +
  theme_bw(base_size = 12) +
  labs(x = 'Temperature (ºC)',
       y = 'Tissue weight growth',
       title = 'Quadratic: CC')

p_quadratic_CC_TiW

#CC_TiW: quadratic: Estimate parameters & CI intervals ----
extra_params <- calc_params(fit_nlsLM) %>%
  pivot_longer(everything(), names_to =  'param', values_to = 'estimate')

ci_extra_params <- Boot(fit_nlsLM, f = function(x){unlist(calc_params(x))}, labels = names(calc_params(fit_nlsLM)), R = 200, method = 'case') %>%
  confint(., method = 'bca') %>%
  as.data.frame() %>%
  rename(conf_lower = 1, conf_upper = 2) %>%
  rownames_to_column(., var = 'param') %>%
  mutate(method = 'case bootstrap')

ci_extra_params <- left_join(ci_extra_params, extra_params)

ci_params_select_CC_TiW <- ci_extra_params %>% 
  filter(param == "ctmax" | param == "topt") %>% 
  mutate(SR = "Central Coast",
         RV = "TiW",
         model = "quadratic")

ggplot(ci_params_select_CC_TiW, aes(param, estimate)) +
  geom_point(size = 4) +
  geom_linerange(aes(ymin = conf_lower, ymax = conf_upper)) +
  theme_bw() +
  facet_wrap(~param, scales = 'free') +
  scale_x_discrete('') +
  labs(title = 'quadratic - CC')

#SoG_TiW: quadratic: Fit data----
# fit with Gaussian model
d_fit <- nest(SoG_TiW, data = c(temp, rate)) %>%
  mutate(quadratic = map(data, ~nls_multstart(rate~quadratic_2008(temp = temp, a, b, c),
                                              data = .x,
                                              iter = c(4,4,4),
                                              start_lower = get_start_vals(.x$temp, .x$rate, model_name = 'quadratic_2008') - 10,
                                              start_upper = get_start_vals(.x$temp, .x$rate, model_name = 'quadratic_2008') + 10,
                                              lower = get_lower_lims(.x$temp, .x$rate, model_name = 'quadratic_2008'),
                                              upper = get_upper_lims(.x$temp, .x$rate, model_name = 'quadratic_2008'),
                                              supp_errors = 'Y',
                                              convergence_count = FALSE)),
         # create new temperature data
         new_data = map(data, ~tibble(temp = seq(min(.x$temp), max(.x$temp), length.out = 100))),
         # predict over that data,
         preds =  map2(quadratic, new_data, ~augment(.x, newdata = .y)))

# unnest predictions
d_preds_SoG <- select(d_fit, preds) %>%
  unnest(preds) %>% 
  mutate(SR = "Strait of Georgia")

# plot data and predictions
ggplot() +
  geom_line(aes(temp, .fitted), d_preds_SoG, col = 'blue') +
  geom_point(aes(temp, rate), SoG_TiW, size = 2, alpha = 0.5) +
  theme_bw(base_size = 12) +
  labs(x = 'Temperature (ºC)',
       y = 'Tissue growth rate',
       title = 'Strait of Georgia')

#SoG_TiW: quadratic: refit model using nlsLM----
fit_nlsLM <- minpack.lm::nlsLM(rate~quadratic_2008(temp = temp, a, b, c),
                               data = SoG_TiW,
                               start = coef(d_fit$quadratic[[1]]),
                               lower = get_lower_lims(SoG_TiW$temp, SoG_TiW$rate, model_name = 'quadratic_2008'),
                               upper = get_upper_lims(SoG_TiW$temp, SoG_TiW$rate, model_name = 'quadratic_2008'),
                               weights = rep(1, times = nrow(SoG_TiW)))

# bootstrap using case resampling
boot1 <- Boot(fit_nlsLM, method = 'case')

# look at the data
head(boot1$t)

hist(boot1, layout = c(2,2))

#SoG_TiW: quadratic: Now plot the bootstrapped models----
#create predictions of each bootstrapped model
boot1_preds <- boot1$t %>%
  as.data.frame() %>%
  drop_na() %>%
  mutate(iter = 1:n()) %>%
  group_by_all() %>%
  do(data.frame(temp = seq(min(SoG_TiW$temp), max(SoG_TiW$temp), length.out = 100))) %>%
  ungroup() %>%
  mutate(pred = quadratic_2008(temp = temp, a, b, c))

# calculate bootstrapped confidence intervals
boot1_conf_preds_SoG <- group_by(boot1_preds, temp) %>%
  summarise(conf_lower = quantile(pred, 0.025),
            conf_upper = quantile(pred, 0.975)) %>%
  ungroup() %>% 
  mutate(SR = "Strait of Georgia")

# plot bootstrapped CIs
p_quadratic_SoG_TiW <- ggplot() +
  geom_line(aes(temp, .fitted), d_preds_SoG, col = 'blue') +
  geom_ribbon(aes(temp, ymin = conf_lower, ymax = conf_upper), boot1_conf_preds_SoG, fill = 'blue', alpha = 0.3) +
  geom_point(aes(temp, rate), SoG_TiW, size = 2, alpha = 0.5) +
  theme_bw(base_size = 12) +
  labs(x = 'Temperature (ºC)',
       y = 'Tissue weight growth',
       title = 'Quadratic: SoG')

p_quadratic_SoG_TiW

#SoG_TiW: quadratic: Estimate parameters & CI intervals ----
extra_params <- calc_params(fit_nlsLM) %>%
  pivot_longer(everything(), names_to =  'param', values_to = 'estimate')

ci_extra_params <- Boot(fit_nlsLM, f = function(x){unlist(calc_params(x))}, labels = names(calc_params(fit_nlsLM)), R = 200, method = 'case') %>%
  confint(., method = 'bca') %>%
  as.data.frame() %>%
  rename(conf_lower = 1, conf_upper = 2) %>%
  rownames_to_column(., var = 'param') %>%
  mutate(method = 'case bootstrap')

ci_extra_params <- left_join(ci_extra_params, extra_params)

ci_params_select_SoG_TiW <- ci_extra_params %>% 
  filter(param == "ctmax" | param == "topt") %>% 
  mutate(SR = "Strait of Georgia",
         RV = "TiW",
         model = "quadratic")

ggplot(ci_params_select_SoG_TiW, aes(param, estimate)) +
  geom_point(size = 4) +
  geom_linerange(aes(ymin = conf_lower, ymax = conf_upper)) +
  theme_bw() +
  facet_wrap(~param, scales = 'free') +
  scale_x_discrete('') +
  labs(title = 'Quadratic - SoG')

#Create combined TiW plot----
preds_all <- d_preds_CC %>% 
  rbind(d_preds_SoG) %>% 
  mutate(SR = as.factor(SR))

boot_conf_all <- boot1_conf_preds_CC %>% 
  rbind(boot1_conf_preds_SoG) %>% 
  mutate(SR = as.factor(SR))

CC_TiW_2 <- CC_TiW %>% 
  mutate(SR = "Central Coast")
SoG_TiW_2 <- SoG_TiW %>% 
  mutate(SR = "Strait of Georgia")

TiW_all <- CC_TiW_2 %>% 
  rbind(SoG_TiW_2)

# plot data and model fit
#Inserted vertical dotted lines for 95th percentile seawater temps measured by iButtons
#Inserted vertical dashed lines for CTmax based on bootstrapped values and 
pTiW_quadratic <- ggplot() +
  stat_summary(aes(y = rate, x = temp, col = SR), data = TiW_all, fun=mean, geom="point", size = 3) +
  stat_summary(aes(y = rate, x = temp, col = SR), data = TiW_all, fun.data = "mean_se", geom = "errorbar", width = 0.2, size = 0.5) +
  geom_line(aes(temp, .fitted, col = SR), preds_all, linewidth = 1) +
  geom_ribbon(aes(temp, ymin = conf_lower, ymax = conf_upper, fill = SR), boot_conf_all,  alpha = 0.3) +
  scale_colour_manual(values = c("skyblue", "coral")) +
  scale_fill_manual(values = c("skyblue3", "coral3")) +
  labs(x = 'Temperature (ºC)',
       y = 'Tissue weight growth (g)',
       col = "Source Region",
       fill = "Source Region") + 
  theme_cowplot(16) + 
  scale_x_continuous(breaks = c(12, 14, 16, 18, 20, 22, 24)) +
  expand_limits(x = c(12, 24.7)) +
  geom_vline(xintercept = 16.6, linetype = "dotted", colour = "skyblue", size = 1) +
  geom_vline(xintercept = 19.2, linetype = "dotted", colour = "coral", size = 1) +
  geom_vline(xintercept = 22.08, linetype = "dashed", colour = "skyblue", size = 1) +
  geom_vline(xintercept = 22.16, linetype = "dashed", colour = "coral", size = 1) +
  theme(legend.position = "none")

pTiW_quadratic

#CC_ShW: Quadratic: Fit data----
# fit with Gaussian model
d_fit <- nest(CC_ShW, data = c(temp, rate)) %>%
  mutate(quadratic = map(data, ~nls_multstart(rate~quadratic_2008(temp = temp, a, b, c),
                                           data = .x,
                                           iter = c(4,4,4),
                                           start_lower = get_start_vals(.x$temp, .x$rate, model_name = 'quadratic_2008') - 10,
                                           start_upper = get_start_vals(.x$temp, .x$rate, model_name = 'quadratic_2008') + 10,
                                           lower = get_lower_lims(.x$temp, .x$rate, model_name = 'quadratic_2008'),
                                           upper = get_upper_lims(.x$temp, .x$rate, model_name = 'quadratic_2008'),
                                           supp_errors = 'Y',
                                           convergence_count = FALSE)),
         # create new temperature data
         new_data = map(data, ~tibble(temp = seq(min(.x$temp), max(.x$temp), length.out = 100))),
         # predict over that data,
         preds =  map2(quadratic, new_data, ~augment(.x, newdata = .y)))

# unnest predictions
d_preds_CC <- select(d_fit, preds) %>%
  unnest(preds) %>% 
  mutate(SR = "Central Coast")

# plot data and predictions
ggplot() +
  geom_line(aes(temp, .fitted), d_preds_CC, col = 'blue') +
  geom_point(aes(temp, rate), CC_ShW, size = 2, alpha = 0.5) +
  theme_bw(base_size = 12) +
  labs(x = 'Temperature (ºC)',
       y = 'Shell growth rate',
       title = 'Central Coast')

#CC_ShW: Quadratic: refit model using nlsLM----
fit_nlsLM <- minpack.lm::nlsLM(rate~quadratic_2008(temp = temp, a, b, c),
                               data = CC_ShW,
                               start = coef(d_fit$quadratic[[1]]),
                               lower = get_lower_lims(CC_ShW$temp, CC_ShW$rate, model_name = 'quadratic_2008'),
                               upper = get_upper_lims(CC_ShW$temp, CC_ShW$rate, model_name = 'quadratic_2008'),
                               weights = rep(1, times = nrow(CC_ShW)))

# bootstrap using case resampling
boot1 <- Boot(fit_nlsLM, method = 'case')

# look at the data
head(boot1$t)

hist(boot1, layout = c(2,2))

#CC_ShW: quadratic: Now plot the bootstrapped models----
#create predictions of each bootstrapped model
boot1_preds <- boot1$t %>%
  as.data.frame() %>%
  drop_na() %>%
  mutate(iter = 1:n()) %>%
  group_by_all() %>%
  do(data.frame(temp = seq(min(CC_ShW$temp), max(CC_ShW$temp), length.out = 100))) %>%
  ungroup() %>%
  mutate(pred = quadratic_2008(temp = temp, a, b, c))

# calculate bootstrapped confidence intervals
boot1_conf_preds_CC <- group_by(boot1_preds, temp) %>%
  summarise(conf_lower = quantile(pred, 0.025),
            conf_upper = quantile(pred, 0.975)) %>%
  ungroup() %>% 
  mutate(SR = "Central Coast")

# plot bootstrapped CIs
p_quadratic_CC <- ggplot() +
  geom_line(aes(temp, .fitted), d_preds_CC, col = 'blue') +
  geom_ribbon(aes(temp, ymin = conf_lower, ymax = conf_upper), boot1_conf_preds_CC, fill = 'blue', alpha = 0.3) +
  geom_point(aes(temp, rate), CC_ShW, size = 2, alpha = 0.5) +
  theme_bw(base_size = 12) +
  labs(x = 'Temperature (ºC)',
       y = 'Shell weight growth',
       title = 'Quadratic: CC')

p_quadratic_CC

#CC_ShW: quadratic: Estimate parameters & CI intervals ----
extra_params <- calc_params(fit_nlsLM) %>%
  pivot_longer(everything(), names_to =  'param', values_to = 'estimate')

ci_extra_params <- Boot(fit_nlsLM, f = function(x){unlist(calc_params(x))}, labels = names(calc_params(fit_nlsLM)), R = 200, method = 'case') %>%
  confint(., method = 'bca') %>%
  as.data.frame() %>%
  rename(conf_lower = 1, conf_upper = 2) %>%
  rownames_to_column(., var = 'param') %>%
  mutate(method = 'case bootstrap')

ci_extra_params <- left_join(ci_extra_params, extra_params)

ci_params_select_CC_ShW <- ci_extra_params %>% 
  filter(param == "ctmax" | param == "topt") %>% 
  mutate(SR = "Central Coast",
         RV = "ShW",
         model = "quadratic")

ggplot(ci_params_select_CC_ShW, aes(param, estimate)) +
  geom_point(size = 4) +
  geom_linerange(aes(ymin = conf_lower, ymax = conf_upper)) +
  theme_bw() +
  facet_wrap(~param, scales = 'free') +
  scale_x_discrete('') +
  labs(title = 'quadratic - CC')

#SoG_ShW: quadratic: Fit data----
# fit with Gaussian model
d_fit <- nest(SoG_ShW, data = c(temp, rate)) %>%
  mutate(quadratic = map(data, ~nls_multstart(rate~quadratic_2008(temp = temp, a, b, c),
                                           data = .x,
                                           iter = c(4,4,4),
                                           start_lower = get_start_vals(.x$temp, .x$rate, model_name = 'quadratic_2008') - 10,
                                           start_upper = get_start_vals(.x$temp, .x$rate, model_name = 'quadratic_2008') + 10,
                                           lower = get_lower_lims(.x$temp, .x$rate, model_name = 'quadratic_2008'),
                                           upper = get_upper_lims(.x$temp, .x$rate, model_name = 'quadratic_2008'),
                                           supp_errors = 'Y',
                                           convergence_count = FALSE)),
         # create new temperature data
         new_data = map(data, ~tibble(temp = seq(min(.x$temp), max(.x$temp), length.out = 100))),
         # predict over that data,
         preds =  map2(quadratic, new_data, ~augment(.x, newdata = .y)))

# unnest predictions
d_preds_SoG <- select(d_fit, preds) %>%
  unnest(preds) %>% 
  mutate(SR = "Strait of Georgia")

# plot data and predictions
ggplot() +
  geom_line(aes(temp, .fitted), d_preds_SoG, col = 'blue') +
  geom_point(aes(temp, rate), SoG_ShW, size = 2, alpha = 0.5) +
  theme_bw(base_size = 12) +
  labs(x = 'Temperature (ºC)',
       y = 'Shell growth rate',
       title = 'Strait of Georgia')

#SoG_ShW: quadratic: refit model using nlsLM----
fit_nlsLM <- minpack.lm::nlsLM(rate~quadratic_2008(temp = temp, a, b, c),
                               data = SoG_ShW,
                               start = coef(d_fit$quadratic[[1]]),
                               lower = get_lower_lims(SoG_ShW$temp, SoG_ShW$rate, model_name = 'quadratic_2008'),
                               upper = get_upper_lims(SoG_ShW$temp, SoG_ShW$rate, model_name = 'quadratic_2008'),
                               weights = rep(1, times = nrow(SoG_ShW)))

# bootstrap using case resampling
boot1 <- Boot(fit_nlsLM, method = 'case')

# look at the data
head(boot1$t)

hist(boot1, layout = c(2,2))

#SoG_ShW: quadratic: Now plot the bootstrapped models----
#create predictions of each bootstrapped model
boot1_preds <- boot1$t %>%
  as.data.frame() %>%
  drop_na() %>%
  mutate(iter = 1:n()) %>%
  group_by_all() %>%
  do(data.frame(temp = seq(min(SoG_ShW$temp), max(SoG_ShW$temp), length.out = 100))) %>%
  ungroup() %>%
  mutate(pred = quadratic_2008(temp = temp, a, b, c))

# calculate bootstrapped confidence intervals
boot1_conf_preds_SoG <- group_by(boot1_preds, temp) %>%
  summarise(conf_lower = quantile(pred, 0.025),
            conf_upper = quantile(pred, 0.975)) %>%
  ungroup() %>% 
  mutate(SR = "Strait of Georgia")

# plot bootstrapped CIs
p_quadratic_SoG <- ggplot() +
  geom_line(aes(temp, .fitted), d_preds_SoG, col = 'blue') +
  geom_ribbon(aes(temp, ymin = conf_lower, ymax = conf_upper), boot1_conf_preds_SoG, fill = 'blue', alpha = 0.3) +
  geom_point(aes(temp, rate), SoG_ShW, size = 2, alpha = 0.5) +
  theme_bw(base_size = 12) +
  labs(x = 'Temperature (ºC)',
       y = 'Shell weight growth',
       title = 'Quadratic: SoG')

p_quadratic_SoG

#SoG_ShW: quadratic: Estimate parameters & CI intervals ----
extra_params <- calc_params(fit_nlsLM) %>%
  pivot_longer(everything(), names_to =  'param', values_to = 'estimate')

ci_extra_params <- Boot(fit_nlsLM, f = function(x){unlist(calc_params(x))}, labels = names(calc_params(fit_nlsLM)), R = 200, method = 'case') %>%
  confint(., method = 'bca') %>%
  as.data.frame() %>%
  rename(conf_lower = 1, conf_upper = 2) %>%
  rownames_to_column(., var = 'param') %>%
  mutate(method = 'case bootstrap')

ci_extra_params <- left_join(ci_extra_params, extra_params)

ci_params_select_SoG_ShW <- ci_extra_params %>% 
  filter(param == "ctmax" | param == "topt") %>% 
  mutate(SR = "Strait of Georgia",
         RV = "ShW",
         model = "quadratic")

ggplot(ci_params_select_SoG_ShW, aes(param, estimate)) +
  geom_point(size = 4) +
  geom_linerange(aes(ymin = conf_lower, ymax = conf_upper)) +
  theme_bw() +
  facet_wrap(~param, scales = 'free') +
  scale_x_discrete('') +
  labs(title = 'Quadratic - SoG')

#Create combined ShW plot----
preds_all <- d_preds_CC %>% 
  rbind(d_preds_SoG) %>% 
  mutate(SR = as.factor(SR))

boot_conf_all <- boot1_conf_preds_CC %>% 
  rbind(boot1_conf_preds_SoG) %>% 
  mutate(SR = as.factor(SR))

CC_ShW_2 <- CC_ShW %>% 
  mutate(SR = "Central Coast")
SoG_ShW_2 <- SoG_ShW %>% 
  mutate(SR = "Strait of Georgia")

ShW_all <- CC_ShW_2 %>% 
  rbind(SoG_ShW_2)

# plot data and model fit
pShW_quadratic <- ggplot() +
  stat_summary(aes(y = rate, x = temp, col = SR), data = ShW_all, fun=mean, geom="point", size = 3) +
  stat_summary(aes(y = rate, x = temp, col = SR), data = ShW_all, fun.data = "mean_se", geom = "errorbar", width = 0.2, size = 0.5) +
  geom_line(aes(temp, .fitted, col = SR), preds_all, linewidth = 1) +
  geom_ribbon(aes(temp, ymin = conf_lower, ymax = conf_upper, fill = SR), boot_conf_all,  alpha = 0.3) +
  scale_colour_manual(values = c("skyblue", "coral")) +
  scale_fill_manual(values = c("skyblue3", "coral3")) +
  labs(x = 'Temperature (ºC)',
       y = 'Shell weight growth (g)',
       col = "Source Region",
       fill = "Source Region") + 
  theme_cowplot(16) + 
  scale_x_continuous(breaks = c(12, 14, 16, 18, 20, 22, 24)) +
  expand_limits(x = c(12, 24.7)) +
  geom_vline(xintercept = 16.6, linetype = "dotted", colour = "skyblue", size = 1) +
  geom_vline(xintercept = 19.2, linetype = "dotted", colour = "coral", size = 1) +
  geom_vline(xintercept = 22.24, linetype = "dashed", colour = "skyblue", size = 1) +
  geom_vline(xintercept = 21.84, linetype = "dashed", colour = "coral", size = 1) +
  theme(legend.position = "none")

pShW_quadratic

#CC_l: Quadratic: Fit data----
# fit with Gaussian model
d_fit <- nest(CC_l, data = c(temp, rate)) %>%
  mutate(quadratic = map(data, ~nls_multstart(rate~quadratic_2008(temp = temp, a, b, c),
                                              data = .x,
                                              iter = c(4,4,4),
                                              start_lower = get_start_vals(.x$temp, .x$rate, model_name = 'quadratic_2008') - 10,
                                              start_upper = get_start_vals(.x$temp, .x$rate, model_name = 'quadratic_2008') + 10,
                                              lower = get_lower_lims(.x$temp, .x$rate, model_name = 'quadratic_2008'),
                                              upper = get_upper_lims(.x$temp, .x$rate, model_name = 'quadratic_2008'),
                                              supp_errors = 'Y',
                                              convergence_count = FALSE)),
         # create new temperature data
         new_data = map(data, ~tibble(temp = seq(min(.x$temp), max(.x$temp), length.out = 100))),
         # predict over that data,
         preds =  map2(quadratic, new_data, ~augment(.x, newdata = .y)))

# unnest predictions
d_preds_CC <- select(d_fit, preds) %>%
  unnest(preds) %>% 
  mutate(SR = "Central Coast")

# plot data and predictions
ggplot() +
  geom_line(aes(temp, .fitted), d_preds_CC, col = 'blue') +
  geom_point(aes(temp, rate), CC_l, size = 2, alpha = 0.5) +
  theme_bw(base_size = 12) +
  labs(x = 'Temperature (ºC)',
       y = 'Shell growth rate',
       title = 'Central Coast')

#CC_l: Quadratic: refit model using nlsLM----
fit_nlsLM <- minpack.lm::nlsLM(rate~quadratic_2008(temp = temp, a, b, c),
                               data = CC_l,
                               start = coef(d_fit$quadratic[[1]]),
                               lower = get_lower_lims(CC_l$temp, CC_l$rate, model_name = 'quadratic_2008'),
                               upper = get_upper_lims(CC_l$temp, CC_l$rate, model_name = 'quadratic_2008'),
                               weights = rep(1, times = nrow(CC_l)))

# bootstrap using case resampling
boot1 <- Boot(fit_nlsLM, method = 'case')

# look at the data
head(boot1$t)

hist(boot1, layout = c(2,2))

#CC_l: quadratic: Now plot the bootstrapped models----
#create predictions of each bootstrapped model
boot1_preds <- boot1$t %>%
  as.data.frame() %>%
  drop_na() %>%
  mutate(iter = 1:n()) %>%
  group_by_all() %>%
  do(data.frame(temp = seq(min(CC_l$temp), max(CC_l$temp), length.out = 100))) %>%
  ungroup() %>%
  mutate(pred = quadratic_2008(temp = temp, a, b, c))

# calculate bootstrapped confidence intervals
boot1_conf_preds_CC <- group_by(boot1_preds, temp) %>%
  summarise(conf_lower = quantile(pred, 0.025),
            conf_upper = quantile(pred, 0.975)) %>%
  ungroup() %>% 
  mutate(SR = "Central Coast")

# plot bootstrapped CIs
p_quadratic_CC <- ggplot() +
  geom_line(aes(temp, .fitted), d_preds_CC, col = 'blue') +
  geom_ribbon(aes(temp, ymin = conf_lower, ymax = conf_upper), boot1_conf_preds_CC, fill = 'blue', alpha = 0.3) +
  geom_point(aes(temp, rate), CC_l, size = 2, alpha = 0.5) +
  theme_bw(base_size = 12) +
  labs(x = 'Temperature (ºC)',
       y = 'Shell length',
       title = 'Quadratic: CC')

p_quadratic_CC

#CC_l: quadratic: Estimate parameters & CI intervals ----
extra_params <- calc_params(fit_nlsLM) %>%
  pivot_longer(everything(), names_to =  'param', values_to = 'estimate')

ci_extra_params <- Boot(fit_nlsLM, f = function(x){unlist(calc_params(x))}, labels = names(calc_params(fit_nlsLM)), R = 200, method = 'case') %>%
  confint(., method = 'bca') %>%
  as.data.frame() %>%
  rename(conf_lower = 1, conf_upper = 2) %>%
  rownames_to_column(., var = 'param') %>%
  mutate(method = 'case bootstrap')

ci_extra_params <- left_join(ci_extra_params, extra_params)

ci_params_select_CC_l <- ci_extra_params %>% 
  filter(param == "ctmax" | param == "topt") %>% 
  mutate(SR = "Central Coast",
         RV = "l",
         model = "quadratic")

ggplot(ci_params_select_CC_l, aes(param, estimate)) +
  geom_point(size = 4) +
  geom_linerange(aes(ymin = conf_lower, ymax = conf_upper)) +
  theme_bw() +
  facet_wrap(~param, scales = 'free') +
  scale_x_discrete('') +
  labs(title = 'quadratic - CC')

#SoG_l: quadratic: Fit data----
# fit with Gaussian model
d_fit <- nest(SoG_l, data = c(temp, rate)) %>%
  mutate(quadratic = map(data, ~nls_multstart(rate~quadratic_2008(temp = temp, a, b, c),
                                              data = .x,
                                              iter = c(4,4,4),
                                              start_lower = get_start_vals(.x$temp, .x$rate, model_name = 'quadratic_2008') - 10,
                                              start_upper = get_start_vals(.x$temp, .x$rate, model_name = 'quadratic_2008') + 10,
                                              lower = get_lower_lims(.x$temp, .x$rate, model_name = 'quadratic_2008'),
                                              upper = get_upper_lims(.x$temp, .x$rate, model_name = 'quadratic_2008'),
                                              supp_errors = 'Y',
                                              convergence_count = FALSE)),
         # create new temperature data
         new_data = map(data, ~tibble(temp = seq(min(.x$temp), max(.x$temp), length.out = 100))),
         # predict over that data,
         preds =  map2(quadratic, new_data, ~augment(.x, newdata = .y)))

# unnest predictions
d_preds_SoG <- select(d_fit, preds) %>%
  unnest(preds) %>% 
  mutate(SR = "Strait of Georgia")

# plot data and predictions
ggplot() +
  geom_line(aes(temp, .fitted), d_preds_SoG, col = 'blue') +
  geom_point(aes(temp, rate), SoG_l, size = 2, alpha = 0.5) +
  theme_bw(base_size = 12) +
  labs(x = 'Temperature (ºC)',
       y = 'Shell length',
       title = 'Strait of Georgia')

#SoG_l: quadratic: refit model using nlsLM----
fit_nlsLM <- minpack.lm::nlsLM(rate~quadratic_2008(temp = temp, a, b, c),
                               data = SoG_l,
                               start = coef(d_fit$quadratic[[1]]),
                               lower = get_lower_lims(SoG_l$temp, SoG_l$rate, model_name = 'quadratic_2008'),
                               upper = get_upper_lims(SoG_l$temp, SoG_l$rate, model_name = 'quadratic_2008'),
                               weights = rep(1, times = nrow(SoG_l)))

# bootstrap using case resampling
boot1 <- Boot(fit_nlsLM, method = 'case')

# look at the data
head(boot1$t)

hist(boot1, layout = c(2,2))

#SoG_l: quadratic: Now plot the bootstrapped models----
#create predictions of each bootstrapped model
boot1_preds <- boot1$t %>%
  as.data.frame() %>%
  drop_na() %>%
  mutate(iter = 1:n()) %>%
  group_by_all() %>%
  do(data.frame(temp = seq(min(SoG_l$temp), max(SoG_l$temp), length.out = 100))) %>%
  ungroup() %>%
  mutate(pred = quadratic_2008(temp = temp, a, b, c))

# calculate bootstrapped confidence intervals
boot1_conf_preds_SoG <- group_by(boot1_preds, temp) %>%
  summarise(conf_lower = quantile(pred, 0.025),
            conf_upper = quantile(pred, 0.975)) %>%
  ungroup() %>% 
  mutate(SR = "Strait of Georgia")

# plot bootstrapped CIs
p_quadratic_SoG <- ggplot() +
  geom_line(aes(temp, .fitted), d_preds_SoG, col = 'blue') +
  geom_ribbon(aes(temp, ymin = conf_lower, ymax = conf_upper), boot1_conf_preds_SoG, fill = 'blue', alpha = 0.3) +
  geom_point(aes(temp, rate), SoG_l, size = 2, alpha = 0.5) +
  theme_bw(base_size = 12) +
  labs(x = 'Temperature (ºC)',
       y = 'Shell length',
       title = 'Quadratic: SoG')

p_quadratic_SoG

#SoG_l: quadratic: Estimate parameters & CI intervals ----
extra_params <- calc_params(fit_nlsLM) %>%
  pivot_longer(everything(), names_to =  'param', values_to = 'estimate')

ci_extra_params <- Boot(fit_nlsLM, f = function(x){unlist(calc_params(x))}, labels = names(calc_params(fit_nlsLM)), R = 200, method = 'case') %>%
  confint(., method = 'bca') %>%
  as.data.frame() %>%
  rename(conf_lower = 1, conf_upper = 2) %>%
  rownames_to_column(., var = 'param') %>%
  mutate(method = 'case bootstrap')

ci_extra_params <- left_join(ci_extra_params, extra_params)

ci_params_select_SoG_l <- ci_extra_params %>% 
  filter(param == "ctmax" | param == "topt") %>% 
  mutate(SR = "Strait of Georgia",
         RV = "l",
         model = "quadratic")

ggplot(ci_params_select_SoG_l, aes(param, estimate)) +
  geom_point(size = 4) +
  geom_linerange(aes(ymin = conf_lower, ymax = conf_upper)) +
  theme_bw() +
  facet_wrap(~param, scales = 'free') +
  scale_x_discrete('') +
  labs(title = 'Quadratic - SoG')

#Create combined l plot----
preds_all <- d_preds_CC %>% 
  rbind(d_preds_SoG) %>% 
  mutate(SR = as.factor(SR))

boot_conf_all <- boot1_conf_preds_CC %>% 
  rbind(boot1_conf_preds_SoG) %>% 
  mutate(SR = as.factor(SR))

CC_l_2 <- CC_l %>% 
  mutate(SR = "Central Coast")
SoG_l_2 <- SoG_l %>% 
  mutate(SR = "Strait of Georgia")

l_all <- CC_l_2 %>% 
  rbind(SoG_l_2)

# plot data and model fit
pl_quadratic <- ggplot() +
  stat_summary(aes(y = rate, x = temp, col = SR), data = l_all, fun=mean, geom="point", size = 3) +
  stat_summary(aes(y = rate, x = temp, col = SR), data = l_all, fun.data = "mean_se", geom = "errorbar", width = 0.2, size = 0.5) +
  geom_line(aes(temp, .fitted, col = SR), preds_all, linewidth = 1) +
  geom_ribbon(aes(temp, ymin = conf_lower, ymax = conf_upper, fill = SR), boot_conf_all,  alpha = 0.3) +
  scale_colour_manual(values = c("skyblue", "coral")) +
  scale_fill_manual(values = c("skyblue3", "coral3")) +
  labs(x = 'Temperature (ºC)',
       y = 'Shell length growth (g)',
       col = "Source Region",
       fill = "Source Region") + 
  theme_cowplot(16) + 
  scale_x_continuous(breaks = c(12, 14, 16, 18, 20, 22, 24)) +
  expand_limits(x = c(12, 24.7)) +
  geom_vline(xintercept = 16.6, linetype = "dotted", colour = "skyblue", size = 1) +
  geom_vline(xintercept = 19.2, linetype = "dotted", colour = "coral", size = 1) +
  geom_vline(xintercept = 22.33, linetype = "dashed", colour = "skyblue", size = 1) +
  geom_vline(xintercept = 22.05, linetype = "dashed", colour = "coral", size = 1) +
  theme(legend.position = "none")

pl_quadratic

#CC_fr: Quadratic: Fit data----
# fit with Gaussian model
d_fit <- nest(CC_fr, data = c(temp, rate)) %>%
  mutate(quadratic = map(data, ~nls_multstart(rate~quadratic_2008(temp = temp, a, b, c),
                                              data = .x,
                                              iter = c(4,4,4),
                                              start_lower = get_start_vals(.x$temp, .x$rate, model_name = 'quadratic_2008') - 10,
                                              start_upper = get_start_vals(.x$temp, .x$rate, model_name = 'quadratic_2008') + 10,
                                              lower = get_lower_lims(.x$temp, .x$rate, model_name = 'quadratic_2008'),
                                              upper = get_upper_lims(.x$temp, .x$rate, model_name = 'quadratic_2008'),
                                              supp_errors = 'Y',
                                              convergence_count = FALSE)),
         # create new temperature data
         new_data = map(data, ~tibble(temp = seq(min(.x$temp), max(.x$temp), length.out = 100))),
         # predict over that data,
         preds =  map2(quadratic, new_data, ~augment(.x, newdata = .y)))

# unnest predictions
d_preds_CC <- select(d_fit, preds) %>%
  unnest(preds) %>% 
  mutate(SR = "Central Coast")

# plot data and predictions
ggplot() +
  geom_line(aes(temp, .fitted), d_preds_CC, col = 'blue') +
  geom_point(aes(temp, rate), CC_fr, size = 2, alpha = 0.5) +
  theme_bw(base_size = 12) +
  labs(x = 'Temperature (ºC)',
       y = 'Feeding rate',
       title = 'Central Coast')

#CC_fr: Quadratic: refit model using nlsLM----
fit_nlsLM <- minpack.lm::nlsLM(rate~quadratic_2008(temp = temp, a, b, c),
                               data = CC_fr,
                               start = coef(d_fit$quadratic[[1]]),
                               lower = get_lower_lims(CC_fr$temp, CC_fr$rate, model_name = 'quadratic_2008'),
                               upper = get_upper_lims(CC_fr$temp, CC_fr$rate, model_name = 'quadratic_2008'),
                               weights = rep(1, times = nrow(CC_fr)))

# bootstrap using case resampling
boot1 <- Boot(fit_nlsLM, method = 'case')

# look at the data
head(boot1$t)

hist(boot1, layout = c(2,2))

#CC_fr: quadratic: Now plot the bootstrapped models----
#create predictions of each bootstrapped model
boot1_preds <- boot1$t %>%
  as.data.frame() %>%
  drop_na() %>%
  mutate(iter = 1:n()) %>%
  group_by_all() %>%
  do(data.frame(temp = seq(min(CC_fr$temp), max(CC_fr$temp), length.out = 100))) %>%
  ungroup() %>%
  mutate(pred = quadratic_2008(temp = temp, a, b, c))

# calculate bootstrapped confidence intervals
boot1_conf_preds_CC <- group_by(boot1_preds, temp) %>%
  summarise(conf_lower = quantile(pred, 0.025),
            conf_upper = quantile(pred, 0.975)) %>%
  ungroup() %>% 
  mutate(SR = "Central Coast")

# plot bootstrapped CIs
ggplot() +
  geom_line(aes(temp, .fitted), d_preds_CC, col = 'blue') +
  geom_ribbon(aes(temp, ymin = conf_lower, ymax = conf_upper), boot1_conf_preds_CC, fill = 'blue', alpha = 0.3) +
  geom_point(aes(temp, rate), CC_fr, size = 2, alpha = 0.5) +
  theme_bw(base_size = 12) +
  labs(x = 'Temperature (ºC)',
       y = 'Feeding rate',
       title = 'Quadratic: CC')

#CC_fr: quadratic: Estimate parameters & CI intervals ----
extra_params <- calc_params(fit_nlsLM) %>%
  pivot_longer(everything(), names_to =  'param', values_to = 'estimate')

ci_extra_params <- Boot(fit_nlsLM, f = function(x){unlist(calc_params(x))}, labels = names(calc_params(fit_nlsLM)), R = 200, method = 'case') %>%
  confint(., method = 'bca') %>%
  as.data.frame() %>%
  rename(conf_lower = 1, conf_upper = 2) %>%
  rownames_to_column(., var = 'param') %>%
  mutate(method = 'case bootstrap')

ci_extra_params <- left_join(ci_extra_params, extra_params)

ci_params_select_CC_fr <- ci_extra_params %>% 
  filter(param == "ctmax" | param == "topt") %>% 
  mutate(SR = "Central Coast",
         RV = "fr",
         model = "quadratic")

ggplot(ci_params_select_CC_fr, aes(param, estimate)) +
  geom_point(size = 4) +
  geom_linerange(aes(ymin = conf_lower, ymax = conf_upper)) +
  theme_bw() +
  facet_wrap(~param, scales = 'free') +
  scale_x_discrete('') +
  labs(title = 'quadratic - CC')

#SoG_fr: quadratic: Fit data----
# fit with Gaussian model
d_fit <- nest(SoG_fr, data = c(temp, rate)) %>%
  mutate(quadratic = map(data, ~nls_multstart(rate~quadratic_2008(temp = temp, a, b, c),
                                              data = .x,
                                              iter = c(4,4,4),
                                              start_lower = get_start_vals(.x$temp, .x$rate, model_name = 'quadratic_2008') - 10,
                                              start_upper = get_start_vals(.x$temp, .x$rate, model_name = 'quadratic_2008') + 10,
                                              lower = get_lower_lims(.x$temp, .x$rate, model_name = 'quadratic_2008'),
                                              upper = get_upper_lims(.x$temp, .x$rate, model_name = 'quadratic_2008'),
                                              supp_errors = 'Y',
                                              convergence_count = FALSE)),
         # create new temperature data
         new_data = map(data, ~tibble(temp = seq(min(.x$temp), max(.x$temp), length.out = 100))),
         # predict over that data,
         preds =  map2(quadratic, new_data, ~augment(.x, newdata = .y)))

# unnest predictions
d_preds_SoG <- select(d_fit, preds) %>%
  unnest(preds) %>% 
  mutate(SR = "Strait of Georgia")

# plot data and predictions
ggplot() +
  geom_line(aes(temp, .fitted), d_preds_SoG, col = 'blue') +
  geom_point(aes(temp, rate), SoG_fr, size = 2, alpha = 0.5) +
  theme_bw(base_size = 12) +
  labs(x = 'Temperature (ºC)',
       y = 'Feeding rate',
       title = 'Strait of Georgia')

#SoG_fr: quadratic: refit model using nlsLM----
fit_nlsLM <- minpack.lm::nlsLM(rate~quadratic_2008(temp = temp, a, b, c),
                               data = SoG_fr,
                               start = coef(d_fit$quadratic[[1]]),
                               lower = get_lower_lims(SoG_fr$temp, SoG_fr$rate, model_name = 'quadratic_2008'),
                               upper = get_upper_lims(SoG_fr$temp, SoG_fr$rate, model_name = 'quadratic_2008'),
                               weights = rep(1, times = nrow(SoG_fr)))

# bootstrap using case resampling
boot1 <- Boot(fit_nlsLM, method = 'case')

# look at the data
head(boot1$t)

hist(boot1, layout = c(2,2))

#SoG_fr: quadratic: Now plot the bootstrapped models----
#create predictions of each bootstrapped model
boot1_preds <- boot1$t %>%
  as.data.frame() %>%
  drop_na() %>%
  mutate(iter = 1:n()) %>%
  group_by_all() %>%
  do(data.frame(temp = seq(min(SoG_fr$temp), max(SoG_fr$temp), length.out = 100))) %>%
  ungroup() %>%
  mutate(pred = quadratic_2008(temp = temp, a, b, c))

# calculate bootstrapped confidence intervals
boot1_conf_preds_SoG <- group_by(boot1_preds, temp) %>%
  summarise(conf_lower = quantile(pred, 0.025),
            conf_upper = quantile(pred, 0.975)) %>%
  ungroup() %>% 
  mutate(SR = "Strait of Georgia")

# plot bootstrapped CIs
ggplot() +
  geom_line(aes(temp, .fitted), d_preds_SoG, col = 'blue') +
  geom_ribbon(aes(temp, ymin = conf_lower, ymax = conf_upper), boot1_conf_preds_SoG, fill = 'blue', alpha = 0.3) +
  geom_point(aes(temp, rate), SoG_fr, size = 2, alpha = 0.5) +
  theme_bw(base_size = 12) +
  labs(x = 'Temperature (ºC)',
       y = 'Feeding rate',
       title = 'Quadratic: SoG')

#SoG_fr: quadratic: Estimate parameters & CI intervals ----
extra_params <- calc_params(fit_nlsLM) %>%
  pivot_longer(everything(), names_to =  'param', values_to = 'estimate')

ci_extra_params <- Boot(fit_nlsLM, f = function(x){unlist(calc_params(x))}, labels = names(calc_params(fit_nlsLM)), R = 200, method = 'case') %>%
  confint(., method = 'bca') %>%
  as.data.frame() %>%
  rename(conf_lower = 1, conf_upper = 2) %>%
  rownames_to_column(., var = 'param') %>%
  mutate(method = 'case bootstrap')

ci_extra_params <- left_join(ci_extra_params, extra_params)

ci_params_select_SoG_fr <- ci_extra_params %>% 
  filter(param == "ctmax" | param == "topt") %>% 
  mutate(SR = "Strait of Georgia",
         RV = "fr",
         model = "quadratic")

ggplot(ci_params_select_SoG_fr, aes(param, estimate)) +
  geom_point(size = 4) +
  geom_linerange(aes(ymin = conf_lower, ymax = conf_upper)) +
  theme_bw() +
  facet_wrap(~param, scales = 'free') +
  scale_x_discrete('') +
  labs(title = 'Quadratic - SoG')

#Create combined fr plot----
preds_all <- d_preds_CC %>% 
  rbind(d_preds_SoG) %>% 
  mutate(SR = as.factor(SR))

boot_conf_all <- boot1_conf_preds_CC %>% 
  rbind(boot1_conf_preds_SoG) %>% 
  mutate(SR = as.factor(SR))

CC_fr_2 <- CC_fr %>% 
  mutate(SR = "Central Coast")
SoG_fr_2 <- SoG_fr %>% 
  mutate(SR = "Strait of Georgia")

fr_all <- CC_fr_2 %>% 
  rbind(SoG_fr_2)

# plot data and model fit
pfr_quadratic <- ggplot() +
  stat_summary(aes(y = rate, x = temp, col = SR), data = fr_all, fun=mean, geom="point", size = 3) +
  stat_summary(aes(y = rate, x = temp, col = SR), data = fr_all, fun.data = "mean_se", geom = "errorbar", width = 0.2, size = 0.5) +
  geom_line(aes(temp, .fitted, col = SR), preds_all, linewidth = 1) +
  geom_ribbon(aes(temp, ymin = conf_lower, ymax = conf_upper, fill = SR), boot_conf_all,  alpha = 0.3) +
  scale_colour_manual(values = c("skyblue", "coral")) +
  scale_fill_manual(values = c("skyblue3", "coral3")) +
  labs(x = 'Temperature (ºC)',
       y = 'Per capita weekly feeding rate',
       col = "Source Region",
       fill = "Source Region") + 
  theme_cowplot(16) + 
  scale_x_continuous(breaks = c(12, 14, 16, 18, 20, 22, 24)) +
  expand_limits(x = c(12, 24.7)) +
  ylim(0, 1.4) +
  geom_vline(xintercept = 16.6, linetype = "dotted", colour = "skyblue", size = 1) +
  geom_vline(xintercept = 19.2, linetype = "dotted", colour = "coral", size = 1) +
  geom_vline(xintercept = 24.66, linetype = "dashed", colour = "skyblue", size = 1) +
  geom_vline(xintercept = 23.03, linetype = "dashed", colour = "coral", size = 1)

pfr_quadratic

#Create collated TPC figure----
pfr_quadratic_2 <- pfr_quadratic + draw_plot_label("(a)", 12.1, 1.4, fontface = "plain")
pTiW_quadratic_2 <- pTiW_quadratic + draw_plot_label("(b)", 12.1, 1.25, fontface = "plain")
pl_quadratic_2 <- pl_quadratic + draw_plot_label("(c)", 12.1, 10.3, fontface = "plain")
pShW_quadratic_2 <- pShW_quadratic + draw_plot_label("(d)", 12.1, 0.63, fontface = "plain")

p_comb_TPC <- plot_grid(pfr_quadratic_2 + theme(axis.text.x = element_blank(), axis.title.x = element_blank(), legend.position = "none"),
                        pTiW_quadratic_2 + theme(axis.text.x = element_blank(), axis.title.x = element_blank()),
                        get_legend(pfr_quadratic),
                        pl_quadratic_2 + theme(axis.title.x = element_blank()),
                        pShW_quadratic_2 + theme(axis.title.x = element_blank()), NULL,
                        ncol = 3, rel_widths = c(1,1,0.4), axis = "lb", align = "hv")

xaxistitle_treat <- ggdraw() + draw_label("Temperature °C", fontface = "plain", x = 0.4, hjust = 0, size = 16)
p_comb_TPC_title <- plot_grid(p_comb_TPC, xaxistitle_treat, ncol = 1, rel_heights = c(1, 0.05))

ggsave(p_comb_TPC_title, file = "plots/TPC/Fig3_TPC.pdf", height = 7, width = 12, dpi = 300)

#Create collated parameters table----
params_bootstrapped <- ci_params_select_CC_TiW %>% 
  rbind(ci_params_select_SoG_TiW, 
        ci_params_select_CC_ShW, ci_params_select_SoG_ShW,
        ci_params_select_CC_l, ci_params_select_SoG_l,
        ci_params_select_CC_fr, ci_params_select_SoG_fr) %>% 
  select(SR, RV, param, estimate, conf_lower, conf_upper, model, method) %>% 
  arrange(desc(param)) %>% 
  mutate(RV = ifelse(RV == "fr", "FR",
                     ifelse(RV == "l", "L", RV)),
         RV = factor(RV, level = c("FR", "TiW", "ShW", "L")),
         param = ifelse(param == "topt", "Topt", "CTmax"),
         param = factor(param, level = c("Topt", "CTmax"))) %>% 
  group_by(param, SR) %>% 
  mutate(average_est = mean(estimate),
         mean_CI_low = mean(conf_lower),
         mean_CI_high = mean(conf_upper))

params_bootstrapped_CTmax <- params_bootstrapped %>% 
  filter(param == "CTmax")

#Visualize these in a ggplot
level_order = c("TiW", "ShW", "L", "FR")
name_RV = c("Tissue weight", "Shell weight", "Shell length", "Feeding rate")

pparams_TPC <- ggplot(params_bootstrapped_CTmax, aes(RV, estimate, colour = SR)) +
  geom_point(aes(fill = SR), size = 4, alpha = 0.5, position=position_dodge(0.8)) + 
  geom_errorbar(aes(ymin=conf_lower, ymax=conf_upper), width=.1, position=position_dodge(0.8)) +
  labs(x = "Response variable", y = "CTmax", colour = "Source region",
       fill = "Source region") +
  scale_x_discrete(limits = level_order, labels = name_RV) +
  scale_colour_manual(values = c("skyblue", "coral"))+ 
  theme_cowplot(16) 

ggsave(pparams_TPC, file = "plots/supp_figs/FigS?_CTmax.pdf", height = 6, width = 9, dpi = 300)
