#Now that you have visualized the differnt models, go through AIC to selecte the best fitting one
#Based on code provided in https://padpadpadpad.github.io/rTPC/articles/model_averaging_selection.html

#You can only run this code after running the TPC_meso code (first section)

#The main objects you want include: 
#meso_subset
#CC_TiW
#SoG_TiW

#Decision for tissue weight: the AICs for Briere, Gaussian, Pawal, and Sharpe fit pretty well, with Gaussian being the best fit
#BUT: I think I will go with Briere as my final model because it aligns very strongly with Gaussian but includes Tmax in its parameters, which is close to CTmax, which I am interested in

#The models you narrowed the search down to include: 
#briere, gaussian, modified gaussian, pawar, and sharpe-schoolfield low

#Code results: Gaussian is the best fitting model for both CC and SoG

#load any new libraries----
library("ggrepel")
library("MuMIn")

#CC: Copied from padfield code----
# fit five chosen model formulations in rTPC
d <- CC_TiW %>% 
  rename(rate = TiW) %>% 
  arrange(temp)%>% 
  select(temp, rate)

d_fits <- nest(d, data = c(temp, rate)) %>%
  mutate(briere = map(data, ~nls_multstart(rate~briere2_1999(temp, tmin, tmax, a, b),
                                           data = .x,
                                           iter = c(4,4,4,4),
                                           start_lower = get_start_vals(.x$temp, .x$rate, model_name = 'briere2_1999') - 10,
                                           start_upper = get_start_vals(.x$temp, .x$rate, model_name = 'briere2_1999') + 10,
                                           lower = get_lower_lims(.x$temp, .x$rate, model_name = 'briere2_1999'),
                                           upper = get_upper_lims(.x$temp, .x$rate, model_name = 'briere2_1999'),
                                           supp_errors = 'Y',
                                           convergence_count = FALSE)),
         gaussian = map(data, ~nls_multstart(rate~gaussian_1987(temp = temp, rmax, topt, a),
                                             data = .x,
                                             iter = c(4,4,4),
                                             start_lower = get_start_vals(.x$temp, .x$rate, model_name = 'gaussian_1987') - 10,
                                             start_upper = get_start_vals(.x$temp, .x$rate, model_name = 'gaussian_1987') + 10,
                                             lower = get_lower_lims(.x$temp, .x$rate, model_name = 'gaussian_1987'),
                                             upper = get_upper_lims(.x$temp, .x$rate, model_name = 'gaussian_1987'),
                                             supp_errors = 'Y',
                                             convergence_count = FALSE)),
         pawar = map(data, ~nls_multstart(rate~pawar_2018(temp, r_tref,e,eh, th, tref = 12),
                                          data = .x,
                                          iter = c(4,4,4,4),
                                          start_lower = get_start_vals(.x$temp, .x$rate, model_name = 'pawar_2018') - 0.5,
                                          start_upper = get_start_vals(.x$temp, .x$rate, model_name = 'pawar_2018') + 0.5,
                                          lower = get_lower_lims(.x$temp, .x$rate, model_name = 'pawar_2018'),
                                          upper = get_upper_lims(.x$temp, .x$rate, model_name = 'pawar_2018'),
                                          supp_errors = 'Y',
                                          convergence_count = FALSE)),
         sharpeschoolhigh = map(data, ~nls_multstart(rate~sharpeschoolhigh_1981(temp = temp, r_tref,e,eh,th, tref = 15),
                                                     data = .x,
                                                     iter = c(4,4,4,4),
                                                     start_lower = get_start_vals(.x$temp, .x$rate, model_name = 'sharpeschoolhigh_1981') - 10,
                                                     start_upper = get_start_vals(.x$temp, .x$rate, model_name = 'sharpeschoolhigh_1981') + 10,
                                                     lower = get_lower_lims(.x$temp, .x$rate, model_name = 'sharpeschoolhigh_1981'),
                                                     upper = get_upper_lims(.x$temp, .x$rate, model_name = 'sharpeschoolhigh_1981'),
                                                     supp_errors = 'Y',
                                                     convergence_count = FALSE)))

# stack models
d_stack <- select(d_fits, -data) %>%
  pivot_longer(., names_to = 'model_name', values_to = 'fit', briere:sharpeschoolhigh)

# get predictions using augment
newdata <- tibble(temp = seq(min(d$temp), max(d$temp), length.out = 100))
d_preds <- d_stack %>%
  mutate(., preds = map(fit, augment, newdata = newdata)) %>%
  select(-fit) %>%
  unnest(preds)

# take a random point from each model for labelling
d_labs <- filter(d_preds, temp < 30) %>%
  group_by(., model_name) %>%
  sample_n(., 1) %>%
  ungroup()

# plot
ggplot(d_preds, aes(temp, .fitted)) +
  geom_line(aes(col = model_name)) +
  geom_label_repel(aes(temp, .fitted, label = model_name, col = model_name), fill = 'white', nudge_y = 0.8, segment.size = 0.2, segment.colour = 'grey50', d_labs) +
  geom_point(aes(temp, rate), d) +
  theme_bw(base_size = 12) +
  theme(legend.position = 'none') +
  labs(x = 'Temperature (ºC)',
       y = 'Tissue weight growth',
       title = 'Respiration across temperatures') +
  geom_hline(aes(yintercept = 0), linetype = 2) +
  scale_color_brewer(type = 'qual', palette = 2)

#CC: Now start the AIC process----
d_ic <- d_stack %>%
  mutate(., info = map(fit, glance),
         AICc =  map_dbl(fit, MuMIn::AICc)) %>%
  select(-fit) %>%
  unnest(info) %>%
  select(model_name, sigma, AIC, AICc, BIC, df.residual)

d_ic

# filter for best model
best_model = filter(d_ic, AICc == min(AICc)) %>% pull(model_name)
best_model

# get colour code
col_best_mod = RColorBrewer::brewer.pal(n = 6, name = "Dark2")[6]

# plot
cc_best <- ggplot(d_preds, aes(temp, .fitted)) +
  geom_line(aes(group = model_name), col = 'grey50', alpha = 0.5) +
  geom_line(data = filter(d_preds, model_name == best_model), col = col_best_mod) +
  geom_label_repel(aes(temp, .fitted, label = model_name), fill = 'white', nudge_y = 0.8, segment.size = 0.2, segment.colour = 'grey50', data = filter(d_labs, model_name == best_model), col = col_best_mod) +
  geom_point(aes(temp, rate), d) +
  theme_bw(base_size = 12) +
  theme(legend.position = 'none') +
  labs(x = 'Temperature (ºC)',
       y = 'Tissue weight growth',
       title = 'Central Coast') +
  geom_hline(aes(yintercept = 0), linetype = 2) 

cc_best

#SoG: Copied from padfield code----
# fit five chosen model formulations in rTPC

#I'm not sure why, but to get the code below to work properly I had to adjust the temp
#values slightly so that they weren't identical... 
d <- SoG_TiW %>% 
  rename(rate = TiW) %>% 
  arrange(temp) %>% 
  mutate(temp = c(12.0, 12.0002, 12.0001, 12.0003,
                  15.0, 15.0002, 15.0004, 15.0006, 15.0008,
                  15.0001, 15.0003, 15.0005, 15.0007, 15.0008,
                  19.0, 19.0002, 19.0004, 19.0001, 19.0003, 19.0005,
                  22.0, 22.0002, 22.0004, 22.0001, 22.0003, 22.0005)) %>% 
  select(temp, rate)
  
d_fits <- nest(d, data = c(temp, rate)) %>%
  mutate(briere = map(data, ~nls_multstart(rate~briere2_1999(temp, tmin, tmax, a, b),
                                           data = .x,
                                           iter = c(4,4,4,4),
                                           start_lower = get_start_vals(.x$temp, .x$rate, model_name = 'briere2_1999') - 10,
                                           start_upper = get_start_vals(.x$temp, .x$rate, model_name = 'briere2_1999') + 10,
                                           lower = get_lower_lims(.x$temp, .x$rate, model_name = 'briere2_1999'),
                                           upper = get_upper_lims(.x$temp, .x$rate, model_name = 'briere2_1999'),
                                           supp_errors = 'Y',
                                           convergence_count = FALSE)),
         gaussian = map(data, ~nls_multstart(rate~gaussian_1987(temp = temp, rmax, topt, a),
                                             data = .x,
                                             iter = c(4,4,4),
                                             start_lower = get_start_vals(.x$temp, .x$rate, model_name = 'gaussian_1987') - 10,
                                             start_upper = get_start_vals(.x$temp, .x$rate, model_name = 'gaussian_1987') + 10,
                                             lower = get_lower_lims(.x$temp, .x$rate, model_name = 'gaussian_1987'),
                                             upper = get_upper_lims(.x$temp, .x$rate, model_name = 'gaussian_1987'),
                                             supp_errors = 'Y',
                                             convergence_count = FALSE)),
        pawar = map(data, ~nls_multstart(rate~pawar_2018(temp, r_tref,e,eh, th, tref = 12),
                                          data = .x,
                                          iter = c(4,4,4,4),
                                          start_lower = get_start_vals(.x$temp, .x$rate, model_name = 'pawar_2018') - 0.5,
                                          start_upper = get_start_vals(.x$temp, .x$rate, model_name = 'pawar_2018') + 0.5,
                                          lower = get_lower_lims(.x$temp, .x$rate, model_name = 'pawar_2018'),
                                          upper = get_upper_lims(.x$temp, .x$rate, model_name = 'pawar_2018'),
                                          supp_errors = 'Y',
                                          convergence_count = FALSE)),
         sharpeschoolhigh = map(data, ~nls_multstart(rate~sharpeschoolhigh_1981(temp = temp, r_tref,e,eh,th, tref = 15),
                                                     data = .x,
                                                     iter = c(4,4,4,4),
                                                     start_lower = get_start_vals(.x$temp, .x$rate, model_name = 'sharpeschoolhigh_1981') - 10,
                                                     start_upper = get_start_vals(.x$temp, .x$rate, model_name = 'sharpeschoolhigh_1981') + 10,
                                                     lower = get_lower_lims(.x$temp, .x$rate, model_name = 'sharpeschoolhigh_1981'),
                                                     upper = get_upper_lims(.x$temp, .x$rate, model_name = 'sharpeschoolhigh_1981'),
                                                     supp_errors = 'Y',
                                                     convergence_count = FALSE)))

# stack models
d_stack <- select(d_fits, -data) %>%
  pivot_longer(., names_to = 'model_name', values_to = 'fit', briere:sharpeschoolhigh)

# get predictions using augment
newdata <- tibble(temp = seq(min(d$temp), max(d$temp), length.out = 100))
d_preds <- d_stack %>%
  mutate(., preds = map(fit, augment, newdata = newdata)) %>%
  select(-fit) %>%
  unnest(preds)

# take a random point from each model for labelling
d_labs <- filter(d_preds, temp < 30) %>%
  group_by(., model_name) %>%
  sample_n(., 1) %>%
  ungroup()

# plot
ggplot(d_preds, aes(temp, .fitted)) +
  geom_line(aes(col = model_name)) +
  geom_label_repel(aes(temp, .fitted, label = model_name, col = model_name), fill = 'white', nudge_y = 0.8, segment.size = 0.2, segment.colour = 'grey50', d_labs) +
  geom_point(aes(temp, rate), d) +
  theme_bw(base_size = 12) +
  theme(legend.position = 'none') +
  labs(x = 'Temperature (ºC)',
       y = 'Tissue weight growth',
       title = 'Respiration across temperatures') +
  geom_hline(aes(yintercept = 0), linetype = 2) +
  scale_color_brewer(type = 'qual', palette = 2)

#SoG: Now start the AIC process----
d_ic <- d_stack %>%
  mutate(., info = map(fit, glance),
         AICc =  map_dbl(fit, MuMIn::AICc)) %>%
  select(-fit) %>%
  unnest(info) %>%
  select(model_name, sigma, AIC, AICc, BIC, df.residual)

d_ic

# filter for best model
best_model = filter(d_ic, AICc == min(AICc)) %>% pull(model_name)
best_model

# get colour code
col_best_mod = RColorBrewer::brewer.pal(n = 6, name = "Dark2")[6]

# plot
sog_best <- ggplot(d_preds, aes(temp, .fitted)) +
  geom_line(aes(group = model_name), col = 'grey50', alpha = 0.5) +
  geom_line(data = filter(d_preds, model_name == best_model), col = col_best_mod) +
  geom_label_repel(aes(temp, .fitted, label = model_name), fill = 'white', nudge_y = 0.8, segment.size = 0.2, segment.colour = 'grey50', data = filter(d_labs, model_name == best_model), col = col_best_mod) +
  geom_point(aes(temp, rate), d) +
  theme_bw(base_size = 12) +
  theme(legend.position = 'none') +
  labs(x = 'Temperature (ºC)',
       y = 'Tissue weight growth',
       title = 'Strait of Georgia') +
  geom_hline(aes(yintercept = 0), linetype = 2) 

#SoG: get model weights----
# filtering on AIC score is hashtagged out in this example
d_ic <- d_ic %>%
  #filter(d_ic, aic - min(aic) <= 2) %>%
  mutate(., weight = MuMIn::Weights(AICc))

select(d_ic, model_name, weight) %>%
  arrange(., desc(weight))

# calculate average prediction
ave_preds <- left_join(d_preds, select(d_ic, model_name, weight)) %>%
  group_by(temp) %>%
  summarise(., .fitted = sum(.fitted*weight)) %>%
  ungroup() %>%
  mutate(model_name = 'model average')

# create label for averaged predictions
d_labs <- filter(ave_preds, temp < 30) %>% sample_n(., 1)

# plot these
ggplot(d_preds, aes(temp, .fitted)) +
  geom_line(aes(col = model_name), alpha = 0.3) +
  geom_line(data = ave_preds, col = 'blue') +
  geom_label_repel(aes(label = model_name), fill = 'white', nudge_y = 0.8, segment.size = 0.2, segment.colour = 'grey50', data = d_labs, col = 'blue') +
  geom_point(aes(temp, rate), d) +
  theme_bw(base_size = 12) +
  theme(legend.position = 'none') +
  labs(x = 'Temperature (ºC)',
       y = 'Tissue weight growth',
       title = 'Strait of Georgia') +
  geom_hline(aes(yintercept = 0), linetype = 2) +
  scale_color_brewer(type = 'qual', palette = 2)

#SoG: calculate estimated parameters----
params <- d_stack %>%
  mutate(., params = map(fit, calc_params)) %>%
  select(-fit) %>%
  unnest(params)

# get averaged parameters based on model weights
ave_params <- left_join(params, select(d_ic, model_name, weight)) %>%
  summarise(., across(rmax:skewness, function(x){sum(x*.$weight)})) %>%
  mutate(model_name = 'model average')

# show them
bind_rows(select(params, model_name, rmax:skewness), ave_params) %>%
  mutate_if(is.numeric, round, 2)
