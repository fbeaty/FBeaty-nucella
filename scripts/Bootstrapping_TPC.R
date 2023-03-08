#Bootstrapping my models! Based on Padfield code
#https://padpadpadpad.github.io/rTPC/articles/bootstrapping_models.html

#Here we want to work with the following dataframes:
#meso_subset
#CC_TiW
#SoG_TiW

# load packages----
library(boot)
library(car)
library(rTPC)
library(nls.multstart)
library(broom)
library(tidyverse)
library(patchwork)
library(minpack.lm)


#CC: Load the data----
d <- CC_TiW %>% 
  rename(rate = TiW) %>% 
  select(temp, rate)

#Visualize the data first
ggplot(d, aes(temp, rate)) +
  geom_point(size = 2, alpha = 0.5) +
  theme_bw(base_size = 12) +
  labs(x = 'Temperature (ºC)',
       y = 'Tissue growth rate',
       title = 'Growth rate across temperatures')

#CC: Gaussian: Fit data----
# fit with Gaussian model
d_fit <- nest(d, data = c(temp, rate)) %>%
  mutate(gaussian = map(data, ~nls_multstart(rate~gaussian_1987(temp = temp, rmax, topt, a),
                                             data = .x,
                                             iter = c(4,4,4),
                                             start_lower = get_start_vals(.x$temp, .x$rate, model_name = 'gaussian_1987') - 10,
                                             start_upper = get_start_vals(.x$temp, .x$rate, model_name = 'gaussian_1987') + 10,
                                             lower = get_lower_lims(.x$temp, .x$rate, model_name = 'gaussian_1987'),
                                             upper = get_upper_lims(.x$temp, .x$rate, model_name = 'gaussian_1987'),
                                             supp_errors = 'Y',
                                             convergence_count = FALSE)),
         # create new temperature data
         new_data = map(data, ~tibble(temp = seq(min(.x$temp), max(.x$temp), length.out = 100))),
         # predict over that data,
         preds =  map2(gaussian, new_data, ~augment(.x, newdata = .y)))

# unnest predictions
d_preds <- select(d_fit, preds) %>%
  unnest(preds)

# plot data and predictions
ggplot() +
  geom_line(aes(temp, .fitted), d_preds, col = 'blue') +
  geom_point(aes(temp, rate), d, size = 2, alpha = 0.5) +
  theme_bw(base_size = 12) +
  labs(x = 'Temperature (ºC)',
       y = 'Growth rate',
       title = 'Strait of Georgia')

#CC: Gaussian: refit model using nlsLM----
fit_nlsLM <- minpack.lm::nlsLM(rate~gaussian_1987(temp = temp, rmax, topt, a),
                               data = d,
                               start = coef(d_fit$gaussian[[1]]),
                               lower = get_lower_lims(d$temp, d$rate, model_name = 'gaussian_1987'),
                               upper = get_upper_lims(d$temp, d$rate, model_name = 'gaussian_1987'),
                               weights = rep(1, times = nrow(d)))

# bootstrap using case resampling
boot1 <- Boot(fit_nlsLM, method = 'case')

# look at the data
head(boot1$t)

hist(boot1, layout = c(2,2))

#CC: Gaussian: Now plot the bootstrapped models----
#create predictions of each bootstrapped model
boot1_preds <- boot1$t %>%
  as.data.frame() %>%
  drop_na() %>%
  mutate(iter = 1:n()) %>%
  group_by_all() %>%
  do(data.frame(temp = seq(min(d$temp), max(d$temp), length.out = 100))) %>%
  ungroup() %>%
  mutate(pred = gaussian_1987(temp = temp, rmax, topt, a))

# calculate bootstrapped confidence intervals
boot1_conf_preds <- group_by(boot1_preds, temp) %>%
  summarise(conf_lower = quantile(pred, 0.025),
            conf_upper = quantile(pred, 0.975)) %>%
  ungroup()

# plot bootstrapped CIs
p_gaussian_sog <- ggplot() +
  geom_line(aes(temp, .fitted), d_preds, col = 'blue') +
  geom_ribbon(aes(temp, ymin = conf_lower, ymax = conf_upper), boot1_conf_preds, fill = 'blue', alpha = 0.3) +
  geom_point(aes(temp, rate), d, size = 2, alpha = 0.5) +
  theme_bw(base_size = 12) +
  labs(x = 'Temperature (ºC)',
       y = 'Tissue weight growth',
       title = 'Gaussian: SoG')

p_gaussian_sog

#CC: Gaussian: Estimate parameters & CI intervals ----
extra_params <- calc_params(fit_nlsLM) %>%
  pivot_longer(everything(), names_to =  'param', values_to = 'estimate')

ci_extra_params <- Boot(fit_nlsLM, f = function(x){unlist(calc_params(x))}, labels = names(calc_params(fit_nlsLM)), R = 200, method = 'case') %>%
  confint(., method = 'bca') %>%
  as.data.frame() %>%
  rename(conf_lower = 1, conf_upper = 2) %>%
  rownames_to_column(., var = 'param') %>%
  mutate(method = 'case bootstrap')

ci_extra_params <- left_join(ci_extra_params, extra_params)

ci_params_select <- ci_extra_params %>% 
  filter(param == "ctmax" | param == "topt")

param_gaussian_cc <- ggplot(ci_params_select, aes(param, estimate)) +
  geom_point(size = 4) +
  geom_linerange(aes(ymin = conf_lower, ymax = conf_upper)) +
  theme_bw() +
  facet_wrap(~param, scales = 'free') +
  scale_x_discrete('') +
  labs(title = 'Gaussian - CC')

param_gaussian_cc

#CC: Briere: Fit data----
# fit with Gaussian model
d_fit <- nest(d, data = c(temp, rate)) %>%
  mutate(briere = map(data, ~nls_multstart(rate~briere2_1999(temp, tmin, tmax, a, b),
                                             data = .x,
                                             iter = c(4,4,4,4),
                                             start_lower = get_start_vals(.x$temp, .x$rate, model_name = 'briere2_1999') - 10,
                                             start_upper = get_start_vals(.x$temp, .x$rate, model_name = 'briere2_1999') + 10,
                                             lower = get_lower_lims(.x$temp, .x$rate, model_name = 'briere2_1999'),
                                             upper = get_upper_lims(.x$temp, .x$rate, model_name = 'briere2_1999'),
                                             supp_errors = 'Y',
                                             convergence_count = FALSE)),
         # create new temperature data
         new_data = map(data, ~tibble(temp = seq(min(.x$temp), max(.x$temp), length.out = 100))),
         # predict over that data,
         preds =  map2(briere, new_data, ~augment(.x, newdata = .y)))

# unnest predictions
d_preds <- select(d_fit, preds) %>%
  unnest(preds)

# plot data and predictions
ggplot() +
  geom_line(aes(temp, .fitted), d_preds, col = 'blue') +
  geom_point(aes(temp, rate), d, size = 2, alpha = 0.5) +
  theme_bw(base_size = 12) +
  labs(x = 'Temperature (ºC)',
       y = 'Growth rate',
       title = 'Strait of Georgia')

#CC: Briere: refit model using nlsLM----
fit_nlsLM <- minpack.lm::nlsLM(rate~briere2_1999(temp, tmin, tmax, a, b),
                               data = d,
                               start = coef(d_fit$briere[[1]]),
                               lower = get_lower_lims(d$temp, d$rate, model_name = 'briere2_1999'),
                               upper = get_upper_lims(d$temp, d$rate, model_name = 'briere2_1999'),
                               weights = rep(1, times = nrow(d)))

# bootstrap using case resampling
boot1 <- Boot(fit_nlsLM, method = 'case')

# look at the data
head(boot1$t)

hist(boot1, layout = c(2,2))

#CC: Briere: Now plot the bootstrapped models----
#create predictions of each bootstrapped model
boot1_preds <- boot1$t %>%
  as.data.frame() %>%
  drop_na() %>%
  mutate(iter = 1:n()) %>%
  group_by_all() %>%
  do(data.frame(temp = seq(min(d$temp), max(d$temp), length.out = 100))) %>%
  ungroup() %>%
  mutate(pred = briere2_1999(temp, tmin, tmax, a, b))

# calculate bootstrapped confidence intervals
boot1_conf_preds <- group_by(boot1_preds, temp) %>%
  summarise(conf_lower = quantile(pred, 0.025),
            conf_upper = quantile(pred, 0.975)) %>%
  ungroup()

# plot bootstrapped CIs
p_briere_sog <- ggplot() +
  geom_line(aes(temp, .fitted), d_preds, col = 'blue') +
  geom_ribbon(aes(temp, ymin = conf_lower, ymax = conf_upper), boot1_conf_preds, fill = 'blue', alpha = 0.3) +
  geom_point(aes(temp, rate), d, size = 2, alpha = 0.5) +
  theme_bw(base_size = 12) +
  labs(x = 'Temperature (ºC)',
       y = 'Tissue weight growth',
       title = 'Briere: SoG')

p_briere_sog

#CC: Briere: Estimate parameters & CI intervals ----
extra_params <- calc_params(fit_nlsLM) %>%
  pivot_longer(everything(), names_to =  'param', values_to = 'estimate')

ci_extra_params <- Boot(fit_nlsLM, f = function(x){unlist(calc_params(x))}, labels = names(calc_params(fit_nlsLM)), R = 200, method = 'case') %>%
  confint(., method = 'bca') %>%
  as.data.frame() %>%
  rename(conf_lower = 1, conf_upper = 2) %>%
  rownames_to_column(., var = 'param') %>%
  mutate(method = 'case bootstrap')

ci_extra_params <- left_join(ci_extra_params, extra_params)

ci_params_select <- ci_extra_params %>% 
  filter(param == "ctmax" | param == "topt" | param == "tmax")

param_briere_cc <- ggplot(ci_params_select, aes(param, estimate)) +
  geom_point(size = 4) +
  geom_linerange(aes(ymin = conf_lower, ymax = conf_upper)) +
  theme_bw() +
  facet_wrap(~param, scales = 'free') +
  scale_x_discrete('') +
  labs(title = 'Briere - CC')

param_briere_cc

#SoG: Load the data----
d <- SoG_TiW %>% 
  rename(rate = TiW) %>% 
  select(temp, rate)

#Visualize the data first
ggplot(d, aes(temp, rate)) +
  geom_point(size = 2, alpha = 0.5) +
  theme_bw(base_size = 12) +
  labs(x = 'Temperature (ºC)',
       y = 'Tissue growth rate',
       title = 'Growth rate across temperatures')

#SoG: Gaussian: Fit data----
# fit with Gaussian model
d_fit <- nest(d, data = c(temp, rate)) %>%
  mutate(gaussian = map(data, ~nls_multstart(rate~gaussian_1987(temp = temp, rmax, topt, a),
                                             data = .x,
                                             iter = c(4,4,4),
                                             start_lower = get_start_vals(.x$temp, .x$rate, model_name = 'gaussian_1987') - 10,
                                             start_upper = get_start_vals(.x$temp, .x$rate, model_name = 'gaussian_1987') + 10,
                                             lower = get_lower_lims(.x$temp, .x$rate, model_name = 'gaussian_1987'),
                                             upper = get_upper_lims(.x$temp, .x$rate, model_name = 'gaussian_1987'),
                                             supp_errors = 'Y',
                                             convergence_count = FALSE)),
         # create new temperature data
         new_data = map(data, ~tibble(temp = seq(min(.x$temp), max(.x$temp), length.out = 100))),
         # predict over that data,
         preds =  map2(gaussian, new_data, ~augment(.x, newdata = .y)))

# unnest predictions
d_preds <- select(d_fit, preds) %>%
  unnest(preds)

# plot data and predictions
ggplot() +
  geom_line(aes(temp, .fitted), d_preds, col = 'blue') +
  geom_point(aes(temp, rate), d, size = 2, alpha = 0.5) +
  theme_bw(base_size = 12) +
  labs(x = 'Temperature (ºC)',
       y = 'Growth rate',
       title = 'Strait of Georgia')

#SoG: Gaussian: refit model using nlsLM----
fit_nlsLM <- minpack.lm::nlsLM(rate~gaussian_1987(temp = temp, rmax, topt, a),
                               data = d,
                               start = coef(d_fit$gaussian[[1]]),
                               lower = get_lower_lims(d$temp, d$rate, model_name = 'gaussian_1987'),
                               upper = get_upper_lims(d$temp, d$rate, model_name = 'gaussian_1987'),
                               weights = rep(1, times = nrow(d)))

# bootstrap using case resampling
boot1 <- Boot(fit_nlsLM, method = 'case')

# look at the data
head(boot1$t)

hist(boot1, layout = c(2,2))

#SoG: Gaussian: Now plot the bootstrapped models----
#create predictions of each bootstrapped model
boot1_preds <- boot1$t %>%
  as.data.frame() %>%
  drop_na() %>%
  mutate(iter = 1:n()) %>%
  group_by_all() %>%
  do(data.frame(temp = seq(min(d$temp), max(d$temp), length.out = 100))) %>%
  ungroup() %>%
  mutate(pred = gaussian_1987(temp = temp, rmax, topt, a))

# calculate bootstrapped confidence intervals
boot1_conf_preds <- group_by(boot1_preds, temp) %>%
  summarise(conf_lower = quantile(pred, 0.025),
            conf_upper = quantile(pred, 0.975)) %>%
  ungroup()

# plot bootstrapped CIs
p_gaussian_sog <- ggplot() +
  geom_line(aes(temp, .fitted), d_preds, col = 'blue') +
  geom_ribbon(aes(temp, ymin = conf_lower, ymax = conf_upper), boot1_conf_preds, fill = 'blue', alpha = 0.3) +
  geom_point(aes(temp, rate), d, size = 2, alpha = 0.5) +
  theme_bw(base_size = 12) +
  labs(x = 'Temperature (ºC)',
       y = 'Tissue weight growth',
       title = 'Gaussian: SoG')

p_gaussian_sog

#SoG: Gaussian: Estimate parameters & CI intervals ----
extra_params <- calc_params(fit_nlsLM) %>%
  pivot_longer(everything(), names_to =  'param', values_to = 'estimate')

ci_extra_params <- Boot(fit_nlsLM, f = function(x){unlist(calc_params(x))}, labels = names(calc_params(fit_nlsLM)), R = 200, method = 'case') %>%
  confint(., method = 'bca') %>%
  as.data.frame() %>%
  rename(conf_lower = 1, conf_upper = 2) %>%
  rownames_to_column(., var = 'param') %>%
  mutate(method = 'case bootstrap')

ci_extra_params <- left_join(ci_extra_params, extra_params)

ci_params_select <- ci_extra_params %>% 
  filter(param == "ctmax" | param == "topt")

param_gaussian_sog <- ggplot(ci_params_select, aes(param, estimate)) +
  geom_point(size = 4) +
  geom_linerange(aes(ymin = conf_lower, ymax = conf_upper)) +
  theme_bw() +
  facet_wrap(~param, scales = 'free') +
  scale_x_discrete('') +
  labs(title = 'Gaussian - SoG')



#Visualize them together----
param_sharpe_sog + param_gaussian_sog



#Sharpe: Fit data----
# fit Sharpe-Schoolfield model
d_fit <- nest(d, data = c(temp, rate)) %>%
  mutate(sharpeschoolhigh = map(data, ~nls_multstart(rate~sharpeschoolhigh_1981(temp = temp, r_tref,e,eh,th, tref = 15),
                                                     data = .x,
                                                     iter = c(3,3,3,3),
                                                     start_lower = get_start_vals(.x$temp, .x$rate, model_name = 'sharpeschoolhigh_1981') - 10,
                                                     start_upper = get_start_vals(.x$temp, .x$rate, model_name = 'sharpeschoolhigh_1981') + 10,
                                                     lower = get_lower_lims(.x$temp, .x$rate, model_name = 'sharpeschoolhigh_1981'),
                                                     upper = get_upper_lims(.x$temp, .x$rate, model_name = 'sharpeschoolhigh_1981'),
                                                     supp_errors = 'Y',
                                                     convergence_count = FALSE)),
         # create new temperature data
         new_data = map(data, ~tibble(temp = seq(min(.x$temp), max(.x$temp), length.out = 100))),
         # predict over that data,
         preds =  map2(sharpeschoolhigh, new_data, ~augment(.x, newdata = .y)))

# unnest predictions
d_preds <- select(d_fit, preds) %>%
  unnest(preds)

# plot data and predictions
ggplot() +
  geom_line(aes(temp, .fitted), d_preds, col = 'blue') +
  geom_point(aes(temp, rate), d, size = 2, alpha = 0.5) +
  theme_bw(base_size = 12) +
  labs(x = 'Temperature (ºC)',
       y = 'Growth rate',
       title = 'Growth rate across temperatures')

#Sharpe: refit model using nlsLN----
# refit model using nlsLM
fit_nlsLM <- minpack.lm::nlsLM(rate~sharpeschoolhigh_1981(temp = temp, r_tref,e,eh,th, tref = 15),
                               data = d,
                               start = coef(d_fit$sharpeschoolhigh[[1]]),
                               lower = get_lower_lims(d$temp, d$rate, model_name = 'sharpeschoolhigh_1981'),
                               upper = get_upper_lims(d$temp, d$rate, model_name = 'sharpeschoolhigh_1981'),
                               weights = rep(1, times = nrow(d)))

# bootstrap using case resampling
boot1 <- Boot(fit_nlsLM, method = 'case')

# look at the data
head(boot1$t)

hist(boot1, layout = c(2,2))

#Sharpe: Now plot bootstrapped model----
# create predictions of each bootstrapped model
boot1_preds <- boot1$t %>%
  as.data.frame() %>%
  drop_na() %>%
  mutate(iter = 1:n()) %>%
  group_by_all() %>%
  do(data.frame(temp = seq(min(d$temp), max(d$temp), length.out = 100))) %>%
  ungroup() %>%
  mutate(pred = sharpeschoolhigh_1981(temp, r_tref, e, eh, th, tref = 15))

# calculate bootstrapped confidence intervals
boot1_conf_preds <- group_by(boot1_preds, temp) %>%
  summarise(conf_lower = quantile(pred, 0.025),
            conf_upper = quantile(pred, 0.975)) %>%
  ungroup()

# plot bootstrapped CIs
p_sharpe_SoG <- ggplot() +
  geom_line(aes(temp, .fitted), d_preds, col = 'blue') +
  geom_ribbon(aes(temp, ymin = conf_lower, ymax = conf_upper), boot1_conf_preds, fill = 'blue', alpha = 0.3) +
  geom_point(aes(temp, rate), d, size = 2, alpha = 0.5) +
  theme_bw(base_size = 12) +
  labs(x = 'Temperature (ºC)',
       y = 'Tissue Growth rate',
       title = 'SoG: Sharpe-Schoolfield')

p_sharpe_SoG

#Sharpe: Estimate parameters & CI intervals ----
extra_params <- calc_params(fit_nlsLM) %>%
  pivot_longer(everything(), names_to =  'param', values_to = 'estimate')

ci_extra_params <- Boot(fit_nlsLM, f = function(x){unlist(calc_params(x))}, labels = names(calc_params(fit_nlsLM)), R = 200, method = 'case') %>%
  confint(., method = 'bca') %>%
  as.data.frame() %>%
  rename(conf_lower = 1, conf_upper = 2) %>%
  rownames_to_column(., var = 'param') %>%
  mutate(method = 'case bootstrap')

ci_extra_params <- left_join(ci_extra_params, extra_params)

ci_params_select <- ci_extra_params %>% 
  filter(param == "ctmax" | param == "topt")

param_sharpe_sog <- ggplot(ci_params_select, aes(param, estimate)) +
  geom_point(size = 4) +
  geom_linerange(aes(ymin = conf_lower, ymax = conf_upper)) +
  theme_bw() +
  facet_wrap(~param, scales = 'free') +
  scale_x_discrete('') +
  labs(title = 'Sharpe-Schoolfield - SoG')
