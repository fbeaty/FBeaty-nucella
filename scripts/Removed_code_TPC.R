#Removed code from TPC_meso

#Making dataframes----
meso_subset <- meso_lm_block %>% 
  select(SR, SP, Treat, Tank, meandiff_TiW) %>% 
  rename(TiW = meandiff_TiW,
         temp = Treat) %>% 
  mutate(temp = as.character(temp),
         temp = as.numeric(temp))

SoG_TiW <- meso_subset %>% 
  filter(SR == "Strait of Georgia")

kwak_TiW <- meso_subset %>% 
  filter(SP == "Kwak") 

pruth_TiW <- meso_subset %>% 
  filter(SP == "Pruth") 

cedar_TiW <- meso_subset %>% 
  filter(SP == "Cedar") 

heron_TiW <- meso_subset %>% 
  filter(SP == "Heron") 

#biere2_1999 SR----
start_vals <- get_start_vals(CC_TiW$temp, CC_TiW$TiW, model_name = 'briere2_1999')
low_lims <- get_lower_lims(CC_TiW$temp, CC_TiW$TiW, model_name = 'briere2_1999')
upper_lims <- get_upper_lims(CC_TiW$temp, CC_TiW$TiW, model_name = 'briere2_1999')

fit <- nls_multstart(TiW~briere2_1999(temp, tmin, tmax, a, b),
                     data = CC_TiW,
                     iter = 500,
                     start_lower = start_vals - 10,
                     start_upper = start_vals + 10,
                     lower = low_lims,
                     upper = upper_lims,
                     supp_errors = 'Y')

#Calculate parameters
calc_params(fit) %>%
  mutate_all(round, 2)

# predict new data
new_data <- data.frame(temp = seq(min(CC_TiW$temp), max(CC_TiW$temp), 0.5))
preds_CC <- augment(fit, newdata = new_data) %>% 
  mutate(SR = "Central Coast")

# plot data and model fit
ggplot(CC_TiW, aes(temp, TiW)) +
  geom_point() +
  geom_line(aes(temp, .fitted), preds_CC, col = 'blue') +
  theme_bw(base_size = 12) +
  labs(x = 'Temperature (ºC)',
       y = 'TiW growth',
       title = "Flinn")

#Strait of Georgia
start_vals <- get_start_vals(SoG_TiW$temp, SoG_TiW$TiW, model_name = 'briere2_1999')
low_lims <- get_lower_lims(SoG_TiW$temp, SoG_TiW$TiW, model_name = 'briere2_1999')
upper_lims <- get_upper_lims(SoG_TiW$temp, SoG_TiW$TiW, model_name = 'briere2_1999')

fit <- nls_multstart(TiW~briere2_1999(temp, tmin, tmax, a, b),
                     data = SoG_TiW,
                     iter = 500,
                     start_lower = start_vals - 10,
                     start_upper = start_vals + 10,
                     lower = low_lims,
                     upper = upper_lims,
                     supp_errors = 'Y')

#Calculate parameters
calc_params(fit) %>%
  mutate_all(round, 2)

# predict new data
new_data <- data.frame(temp = seq(min(SoG_TiW$temp), max(SoG_TiW$temp), 0.5))
preds_SoG <- augment(fit, newdata = new_data) %>% 
  mutate(SR = "Strait of Georgia")

# plot data and model fit
ggplot(SoG_TiW, aes(temp, TiW)) +
  geom_point() +
  geom_line(aes(temp, .fitted), preds_SoG, col = 'blue') +
  theme_bw(base_size = 12) +
  labs(x = 'Temperature (ºC)',
       y = 'TiW growth')

#Combine all the predictions into one df SR----
preds_all <- preds_CC %>% 
  rbind(preds_SoG) %>% 
  mutate(SR = as.factor(SR))

# plot data and model fit
p_briere_SR <- ggplot(meso_subset, aes(temp, TiW)) +
  geom_point(aes(fill = SR), size = 3, alpha = 0.5, shape = 21, stroke = 0) + 
  geom_line(aes(temp, .fitted, col = SR), preds_all, linewidth = 1) +
  theme_bw(base_size = 12) +
  scale_colour_manual(values = c("skyblue", "coral")) +
  scale_fill_manual(values = c("skyblue3", "coral3")) +
  labs(x = 'Temperature (ºC)',
       y = 'Tissue weight growth',
       title = "Briere",
       col = "Source Region",
       fill = "Source Region") + 
  theme_cowplot(16) + 
  theme(legend.position = "none")
#theme(legend.position = c(0.7, 0.9))

p_briere_SR

#flinn_1991 SR----
start_vals <- get_start_vals(CC_TiW$temp, CC_TiW$TiW, model_name = 'flinn_1991')
low_lims <- get_lower_lims(CC_TiW$temp, CC_TiW$TiW, model_name = 'flinn_1991')
upper_lims <- get_upper_lims(CC_TiW$temp, CC_TiW$TiW, model_name = 'flinn_1991')

fit <- nls_multstart(TiW~flinn_1991(temp, a, b, c),
                     data = CC_TiW,
                     iter = 500,
                     start_lower = start_vals - 10,
                     start_upper = start_vals + 10,
                     lower = low_lims,
                     upper = upper_lims,
                     supp_errors = 'Y')

#Calculate parameters
calc_params(fit) %>%
  mutate_all(round, 2)

# predict new data
new_data <- data.frame(temp = seq(min(CC_TiW$temp), max(CC_TiW$temp), 0.5))
preds_CC <- augment(fit, newdata = new_data) %>% 
  mutate(SR = "Central Coast")

# plot data and model fit
ggplot(CC_TiW, aes(temp, TiW)) +
  geom_point() +
  geom_line(aes(temp, .fitted), preds_CC, col = 'blue') +
  theme_bw(base_size = 12) +
  labs(x = 'Temperature (ºC)',
       y = 'TiW growth',
       title = "Flinn")

#Strait of Georgia
start_vals <- get_start_vals(SoG_TiW$temp, SoG_TiW$TiW, model_name = 'flinn_1991')
low_lims <- get_lower_lims(SoG_TiW$temp, SoG_TiW$TiW, model_name = 'flinn_1991')
upper_lims <- get_upper_lims(SoG_TiW$temp, SoG_TiW$TiW, model_name = 'flinn_1991')

fit <- nls_multstart(TiW~flinn_1991(temp, a, b, c),
                     data = SoG_TiW,
                     iter = 500,
                     start_lower = start_vals - 10,
                     start_upper = start_vals + 10,
                     lower = low_lims,
                     upper = upper_lims,
                     supp_errors = 'Y')

#Calculate parameters
calc_params(fit) %>%
  mutate_all(round, 2)

# predict new data
new_data <- data.frame(temp = seq(min(SoG_TiW$temp), max(SoG_TiW$temp), 0.5))
preds_SoG <- augment(fit, newdata = new_data) %>% 
  mutate(SR = "Strait of Georgia")

# plot data and model fit
ggplot(SoG_TiW, aes(temp, TiW)) +
  geom_point() +
  geom_line(aes(temp, .fitted), preds_SoG, col = 'blue') +
  theme_bw(base_size = 12) +
  labs(x = 'Temperature (ºC)',
       y = 'TiW growth')

#Combine all the predictions into one df SR----
preds_all <- preds_CC %>% 
  rbind(preds_SoG) %>% 
  mutate(SR = as.factor(SR))

# plot data and model fit
p_flinn_SR <- ggplot(meso_subset, aes(temp, TiW)) +
  geom_point(aes(fill = SR), size = 3, alpha = 0.5, shape = 21, stroke = 0) + 
  geom_line(aes(temp, .fitted, col = SR), preds_all, linewidth = 1) +
  theme_bw(base_size = 12) +
  scale_colour_manual(values = c("skyblue", "coral")) +
  scale_fill_manual(values = c("skyblue3", "coral3")) +
  labs(x = 'Temperature (ºC)',
       y = 'Tissue weight growth',
       title = "Flinn",
       col = "Source Region",
       fill = "Source Region") + 
  theme_cowplot(16) + 
  theme(legend.position = "none")
#theme(legend.position = c(0.7, 0.9))

p_flinn_SR

#Gaussian_1987 SR----
start_vals <- get_start_vals(CC_TiW$temp, CC_TiW$rate, model_name = 'gaussian_1987')
low_lims <- get_lower_lims(CC_TiW$temp, CC_TiW$rate, model_name = 'gaussian_1987')
upper_lims <- get_upper_lims(CC_TiW$temp, CC_TiW$rate, model_name = 'gaussian_1987')

fit <- nls_multstart(rate~gaussian_1987(temp, rmax, topt, a),
                     data = CC_TiW,
                     iter = 500,
                     start_lower = start_vals - 10,
                     start_upper = start_vals + 10,
                     lower = low_lims,
                     upper = upper_lims,
                     supp_errors = 'Y')

#Calculate parameters
calc_params(fit) %>%
  mutate_all(round, 2)

# predict new data
new_data <- data.frame(temp = seq(min(CC_TiW$temp), max(CC_TiW$temp), 0.5))
preds_CC <- augment(fit, newdata = new_data) %>% 
  mutate(SR = "Central Coast")

# plot data and model fit
ggplot(CC_TiW, aes(temp, TiW)) +
  geom_point() +
  geom_line(aes(temp, .fitted), preds_CC, col = 'blue') +
  theme_bw(base_size = 12) +
  labs(x = 'Temperature (ºC)',
       y = 'TiW growth',
       title = "Flinn")

#Strait of Georgia
start_vals <- get_start_vals(SoG_TiW$temp, SoG_TiW$TiW, model_name = 'gaussian_1987')
low_lims <- get_lower_lims(SoG_TiW$temp, SoG_TiW$TiW, model_name = 'gaussian_1987')
upper_lims <- get_upper_lims(SoG_TiW$temp, SoG_TiW$TiW, model_name = 'gaussian_1987')

fit <- nls_multstart(TiW~gaussian_1987(temp, rmax, topt, a),
                     data = SoG_TiW,
                     iter = 500,
                     start_lower = start_vals - 10,
                     start_upper = start_vals + 10,
                     lower = low_lims,
                     upper = upper_lims,
                     supp_errors = 'Y')

#Calculate parameters
calc_params(fit) %>%
  mutate_all(round, 2)

# predict new data
new_data <- data.frame(temp = seq(min(SoG_TiW$temp), max(SoG_TiW$temp), 0.5))
preds_SoG <- augment(fit, newdata = new_data) %>% 
  mutate(SR = "Strait of Georgia")

# plot data and model fit
ggplot(SoG_TiW, aes(temp, TiW)) +
  geom_point() +
  geom_line(aes(temp, .fitted), preds_SoG, col = 'blue') +
  theme_bw(base_size = 12) +
  labs(x = 'Temperature (ºC)',
       y = 'TiW growth')

#Combine all the predictions into one df SR----
preds_all <- preds_CC %>% 
  rbind(preds_SoG) %>% 
  mutate(SR = as.factor(SR))

# plot data and model fit
p_gaussian_SR <- ggplot(meso_subset, aes(temp, TiW)) +
  geom_point(aes(fill = SR), size = 3, alpha = 0.5, shape = 21, stroke = 0) + 
  geom_line(aes(temp, .fitted, col = SR), preds_all, linewidth = 1) +
  theme_bw(base_size = 12) +
  scale_colour_manual(values = c("skyblue", "coral")) +
  scale_fill_manual(values = c("skyblue3", "coral3")) +
  labs(x = 'Temperature (ºC)',
       y = 'Tissue weight growth',
       title = "Gaussian",
       col = "Source Region",
       fill = "Source Region") + 
  theme_cowplot(16) + 
  theme(legend.position = "none")
#theme(legend.position = c(0.7, 0.9))

p_gaussian_SR

#modifiedgaussian_2006 SR----
start_vals <- get_start_vals(CC_TiW$temp, CC_TiW$TiW, model_name = 'modifiedgaussian_2006')
low_lims <- get_lower_lims(CC_TiW$temp, CC_TiW$TiW, model_name = 'modifiedgaussian_2006')
upper_lims <- get_upper_lims(CC_TiW$temp, CC_TiW$TiW, model_name = 'modifiedgaussian_2006')

fit <- nls_multstart(TiW~modifiedgaussian_2006(temp, rmax, topt, a, b),
                     data = CC_TiW,
                     iter = 500,
                     start_lower = start_vals - 10,
                     start_upper = start_vals + 10,
                     lower = low_lims,
                     upper = upper_lims,
                     supp_errors = 'Y')

#Calculate parameters
calc_params(fit) %>%
  mutate_all(round, 2)

# predict new data
new_data <- data.frame(temp = seq(min(CC_TiW$temp), max(CC_TiW$temp), 0.5))
preds_CC <- augment(fit, newdata = new_data) %>% 
  mutate(SR = "Central Coast")

# plot data and model fit
ggplot(CC_TiW, aes(temp, TiW)) +
  geom_point() +
  geom_line(aes(temp, .fitted), preds_CC, col = 'blue') +
  theme_bw(base_size = 12) +
  labs(x = 'Temperature (ºC)',
       y = 'TiW growth')

#Strait of Georgia
start_vals <- get_start_vals(SoG_TiW$temp, SoG_TiW$TiW, model_name = 'modifiedgaussian_2006')
low_lims <- get_lower_lims(SoG_TiW$temp, SoG_TiW$TiW, model_name = 'modifiedgaussian_2006')
upper_lims <- get_upper_lims(SoG_TiW$temp, SoG_TiW$TiW, model_name = 'modifiedgaussian_2006')

fit <- nls_multstart(TiW~modifiedgaussian_2006(temp, rmax, topt, a, b),
                     data = SoG_TiW,
                     iter = 500,
                     start_lower = start_vals - 10,
                     start_upper = start_vals + 10,
                     lower = low_lims,
                     upper = upper_lims,
                     supp_errors = 'Y')

#Calculate parameters
calc_params(fit) %>%
  mutate_all(round, 2)

# predict new data
new_data <- data.frame(temp = seq(min(SoG_TiW$temp), max(SoG_TiW$temp), 0.5))
preds_SoG <- augment(fit, newdata = new_data) %>% 
  mutate(SR = "Strait of Georgia")

# plot data and model fit
ggplot(SoG_TiW, aes(temp, TiW)) +
  geom_point() +
  geom_line(aes(temp, .fitted), preds_SoG, col = 'blue') +
  theme_bw(base_size = 12) +
  labs(x = 'Temperature (ºC)',
       y = 'TiW growth')

#Combine all the predictions into one df SR----
preds_all <- preds_CC %>% 
  rbind(preds_SoG) %>% 
  mutate(SR = as.factor(SR))

# plot data and model fit
p_mod_gaussian_SR <- ggplot(meso_subset, aes(temp, TiW)) +
  geom_point(aes(fill = SR), size = 3, alpha = 0.5, shape = 21, stroke = 0) + 
  geom_line(aes(temp, .fitted, col = SR), preds_all, linewidth = 1) +
  theme_bw(base_size = 12) +
  scale_colour_manual(values = c("skyblue", "coral")) +
  scale_fill_manual(values = c("skyblue3", "coral3")) +
  labs(x = 'Temperature (ºC)',
       y = 'Tissue weight growth',
       title = "Modified Gaussian",
       col = "Source Region",
       fill = "Source Region") + 
  theme_cowplot(16) + 
  theme(legend.position = "none")
#theme(legend.position = c(0.7, 0.9))

p_mod_gaussian_SR

#oneill_1972 SR----
start_vals <- get_start_vals(CC_TiW$temp, CC_TiW$TiW, model_name = 'oneill_1972')
low_lims <- get_lower_lims(CC_TiW$temp, CC_TiW$TiW, model_name = 'oneill_1972')
upper_lims <- get_upper_lims(CC_TiW$temp, CC_TiW$TiW, model_name = 'oneill_1972')

fit <- nls_multstart(TiW~oneill_1972(temp, rmax,ctmax,topt, q10),
                     data = CC_TiW,
                     iter = 500,
                     start_lower = start_vals - 10,
                     start_upper = start_vals + 10,
                     lower = low_lims,
                     upper = upper_lims,
                     supp_errors = 'Y')

#Calculate parameters
calc_params(fit) %>%
  mutate_all(round, 2)

# predict new data
new_data <- data.frame(temp = seq(min(CC_TiW$temp), max(CC_TiW$temp), 0.5))
preds_CC <- augment(fit, newdata = new_data) %>% 
  mutate(SR = "Central Coast")

# plot data and model fit
ggplot(CC_TiW, aes(temp, TiW)) +
  geom_point() +
  geom_line(aes(temp, .fitted), preds_CC, col = 'blue') +
  theme_bw(base_size = 12) +
  labs(x = 'Temperature (ºC)',
       y = 'TiW growth')

#Strait of Georgia
start_vals <- get_start_vals(SoG_TiW$temp, SoG_TiW$TiW, model_name = 'oneill_1972')
low_lims <- get_lower_lims(SoG_TiW$temp, SoG_TiW$TiW, model_name = 'oneill_1972')
upper_lims <- get_upper_lims(SoG_TiW$temp, SoG_TiW$TiW, model_name = 'oneill_1972')

fit <- nls_multstart(TiW~oneill_1972(temp, rmax,ctmax,topt, q10),
                     data = SoG_TiW,
                     iter = 500,
                     start_lower = start_vals - 10,
                     start_upper = start_vals + 10,
                     lower = low_lims,
                     upper = upper_lims,
                     supp_errors = 'Y')

#Calculate parameters
calc_params(fit) %>%
  mutate_all(round, 2)

# predict new data
new_data <- data.frame(temp = seq(min(SoG_TiW$temp), max(SoG_TiW$temp), 0.5))
preds_SoG <- augment(fit, newdata = new_data) %>% 
  mutate(SR = "Strait of Georgia")

# plot data and model fit
ggplot(SoG_TiW, aes(temp, TiW)) +
  geom_point() +
  geom_line(aes(temp, .fitted), preds_SoG, col = 'blue') +
  theme_bw(base_size = 12) +
  labs(x = 'Temperature (ºC)',
       y = 'TiW growth')

#Combine all the predictions into one df SR----
preds_all <- preds_CC %>% 
  rbind(preds_SoG) %>% 
  mutate(SR = as.factor(SR))

# plot data and model fit
p_oneill_SR <- ggplot(meso_subset, aes(temp, TiW)) +
  geom_point(aes(fill = SR), size = 3, alpha = 0.5, shape = 21, stroke = 0) + 
  geom_line(aes(temp, .fitted, col = SR), preds_all, linewidth = 1) +
  theme_bw(base_size = 12) +
  scale_colour_manual(values = c("skyblue", "coral")) +
  scale_fill_manual(values = c("skyblue3", "coral3")) +
  labs(x = 'Temperature (ºC)',
       y = 'Tissue weight growth',
       title = "O'Neill",
       col = "Source Region",
       fill = "Source Region") + 
  theme_cowplot(16) + 
  theme(legend.position = "none")
#theme(legend.position = c(0.7, 0.9))

p_oneill_SR

#Pawar by SR----
#Start with CC
start_vals <- get_start_vals(CC_TiW$temp, CC_TiW$TiW, model_name = 'pawar_2018')
low_lims <- get_lower_lims(CC_TiW$temp, CC_TiW$TiW, model_name = 'pawar_2018')
upper_lims <- get_upper_lims(CC_TiW$temp, CC_TiW$TiW, model_name = 'pawar_2018')

fit <- nls_multstart(TiW~pawar_2018(temp, r_tref,e,eh, th, tref = 12),
                     data = CC_TiW,
                     iter = 500,
                     start_lower = start_vals - 10,
                     start_upper = start_vals + 10,
                     lower = low_lims,
                     upper = upper_lims,
                     supp_errors = 'Y')

#Calculate parameters
calc_params(fit) %>%
  mutate_all(round, 2)

# predict new data
new_data <- data.frame(temp = seq(min(CC_TiW$temp), max(CC_TiW$temp), 0.5))
preds_CC <- augment(fit, newdata = new_data) %>% 
  mutate(SR = "Central Coast")

# plot data and model fit
ggplot(CC_TiW, aes(temp, TiW)) +
  geom_point() +
  geom_line(aes(temp, .fitted), preds_CC, col = 'blue') +
  theme_bw(base_size = 12) +
  labs(x = 'Temperature (ºC)',
       y = 'TiW growth',
       title = "Pawar")

#Now with SoG
start_vals <- get_start_vals(SoG_TiW$temp, SoG_TiW$TiW, model_name = 'pawar_2018')
low_lims <- get_lower_lims(SoG_TiW$temp, SoG_TiW$TiW, model_name = 'pawar_2018')
upper_lims <- get_upper_lims(SoG_TiW$temp, SoG_TiW$TiW, model_name = 'pawar_2018')

fit <- nls_multstart(TiW~pawar_2018(temp, r_tref,e,eh, th, tref = 15),
                     data = SoG_TiW,
                     iter = 500,
                     start_lower = start_vals - 10,
                     start_upper = start_vals + 10,
                     lower = low_lims,
                     upper = upper_lims,
                     supp_errors = 'Y')

#Calculate parameters
calc_params(fit) %>%
  mutate_all(round, 2)

# predict new data
new_data <- data.frame(temp = seq(min(SoG_TiW$temp), max(SoG_TiW$temp), 0.5))
preds_SoG <- augment(fit, newdata = new_data) %>% 
  mutate(SR = "Strait of Georgia")

# plot data and model fit
ggplot(SoG_TiW, aes(temp, TiW)) +
  geom_point() +
  geom_line(aes(temp, .fitted), preds_SoG, col = 'blue') +
  theme_bw(base_size = 12) +
  labs(x = 'Temperature (ºC)',
       y = 'TiW growth',
       title = "Pawar")

#Combine all the predictions into one df SR----
preds_all <- preds_CC %>% 
  rbind(preds_SoG) %>% 
  mutate(SR = as.factor(SR))

# plot data and model fit
p_pawar_SR <- ggplot(meso_subset, aes(temp, TiW)) +
  geom_point(aes(fill = SR), size = 3, alpha = 0.5, shape = 21, stroke = 0) + 
  geom_line(aes(temp, .fitted, col = SR), preds_all, linewidth = 1) +
  theme_bw(base_size = 12) +
  scale_colour_manual(values = c("skyblue", "coral")) +
  scale_fill_manual(values = c("skyblue3", "coral3")) +
  labs(x = 'Temperature (ºC)',
       y = 'Tissue weight growth',
       title = "Pawar",
       col = "Source Region",
       fill = "Source Region") + 
  theme_cowplot(16) + 
  theme(legend.position = "none")
#theme(legend.position = c(0.7, 0.9))

p_pawar_SR

#sharpeschool_high by SR----
#Start with CC
start_vals <- get_start_vals(CC_TiW$temp, CC_TiW$TiW, model_name = 'sharpeschoolhigh_1981')
low_lims <- get_lower_lims(CC_TiW$temp, CC_TiW$TiW, model_name = 'sharpeschoolhigh_1981')
upper_lims <- get_upper_lims(CC_TiW$temp, CC_TiW$TiW, model_name = 'sharpeschoolhigh_1981')

fit <- nls_multstart(TiW~sharpeschoolhigh_1981(temp, r_tref,e,eh, th, tref = 12),
                     data = CC_TiW,
                     iter = 500,
                     start_lower = start_vals - 10,
                     start_upper = start_vals + 10,
                     lower = low_lims,
                     upper = upper_lims,
                     supp_errors = 'Y')

fit

#Calculate parameters
calc_params(fit) %>%
  mutate_all(round, 2)

# predict new data
new_data <- data.frame(temp = seq(min(CC_TiW$temp), max(CC_TiW$temp), 0.5))
preds_CC <- augment(fit, newdata = new_data) %>% 
  mutate(SR = "Central Coast")

# plot data and model fit
ggplot(CC_TiW, aes(temp, TiW)) +
  geom_point() +
  geom_line(aes(temp, .fitted), preds_CC, col = 'blue') +
  theme_bw(base_size = 12) +
  labs(x = 'Temperature (ºC)',
       y = 'TiW growth',
       title = "Sharpe-schoolfield")

#Now with SoG
start_vals <- get_start_vals(SoG_TiW$temp, SoG_TiW$TiW, model_name = 'sharpeschoolhigh_1981')
low_lims <- get_lower_lims(SoG_TiW$temp, SoG_TiW$TiW, model_name = 'sharpeschoolhigh_1981')
upper_lims <- get_upper_lims(SoG_TiW$temp, SoG_TiW$TiW, model_name = 'sharpeschoolhigh_1981')

fit <- nls_multstart(TiW~sharpeschoolhigh_1981(temp, r_tref,e,eh, th, tref = 15),
                     data = SoG_TiW,
                     iter = 500,
                     start_lower = start_vals - 10,
                     start_upper = start_vals + 10,
                     lower = low_lims,
                     upper = upper_lims,
                     supp_errors = 'Y')

fit

#Calculate parameters
calc_params(fit) %>%
  mutate_all(round, 2)

# predict new data
new_data <- data.frame(temp = seq(min(SoG_TiW$temp), max(SoG_TiW$temp), 0.5))
preds_SoG <- augment(fit, newdata = new_data) %>% 
  mutate(SR = "Strait of Georgia")

# plot data and model fit
ggplot(SoG_TiW, aes(temp, TiW)) +
  geom_point() +
  geom_line(aes(temp, .fitted), preds_SoG, col = 'blue') +
  theme_bw(base_size = 12) +
  labs(x = 'Temperature (ºC)',
       y = 'TiW growth',
       title = "Sharpe-schoolfield")
#Combine all the predictions into one df SR----
preds_all <- preds_CC %>% 
  rbind(preds_SoG) %>% 
  mutate(SR = as.factor(SR))

# plot data and model fit
p_sharpe_SR <- ggplot(meso_subset, aes(temp, TiW)) +
  geom_point(aes(fill = SR), size = 3, alpha = 0.5, shape = 21, stroke = 0) + 
  geom_line(aes(temp, .fitted, col = SR), preds_all, linewidth = 1) +
  theme_bw(base_size = 12) +
  scale_colour_manual(values = c("skyblue", "coral")) +
  scale_fill_manual(values = c("skyblue3", "coral3")) +
  labs(x = 'Temperature (ºC)',
       y = 'Tissue weight growth',
       title = "Sharpe-Schoolfield",
       col = "Source Region",
       fill = "Source Region") + 
  theme_cowplot(16) + 
  theme(legend.position = "none")
#theme(legend.position = c(0.7, 0.9))

p_sharpe_SR

#weibull by SR----
#Start with CC
start_vals <- get_start_vals(CC_TiW$temp, CC_TiW$TiW, model_name = 'weibull_1995')
low_lims <- get_lower_lims(CC_TiW$temp, CC_TiW$TiW, model_name = 'weibull_1995')
upper_lims <- get_upper_lims(CC_TiW$temp, CC_TiW$TiW, model_name = 'weibull_1995')

fit <- nls_multstart(TiW~weibull_1995(temp, a, topt, b, c),
                     data = CC_TiW,
                     iter = 500,
                     start_lower = start_vals - 10,
                     start_upper = start_vals + 10,
                     lower = low_lims,
                     upper = upper_lims,
                     supp_errors = 'Y')

#Calculate parameters
calc_params(fit) %>%
  mutate_all(round, 2)

# predict new data
new_data <- data.frame(temp = seq(min(CC_TiW$temp), max(CC_TiW$temp), 0.5))
preds_CC <- augment(fit, newdata = new_data) %>% 
  mutate(SR = "Central Coast")

# plot data and model fit
ggplot(CC_TiW, aes(temp, TiW)) +
  geom_point() +
  geom_line(aes(temp, .fitted), preds_CC, col = 'blue') +
  theme_bw(base_size = 12) +
  labs(x = 'Temperature (ºC)',
       y = 'TiW growth',
       title = "Weibull")

#Now with SoG
start_vals <- get_start_vals(SoG_TiW$temp, SoG_TiW$TiW, model_name = 'weibull_1995')
low_lims <- get_lower_lims(SoG_TiW$temp, SoG_TiW$TiW, model_name = 'weibull_1995')
upper_lims <- get_upper_lims(SoG_TiW$temp, SoG_TiW$TiW, model_name = 'weibull_1995')

fit <- nls_multstart(TiW~weibull_1995(temp, a, topt, b, c),
                     data = SoG_TiW,
                     iter = 500,
                     start_lower = start_vals - 10,
                     start_upper = start_vals + 10,
                     lower = low_lims,
                     upper = upper_lims,
                     supp_errors = 'Y')

#Calculate parameters
calc_params(fit) %>%
  mutate_all(round, 2)

# predict new data
new_data <- data.frame(temp = seq(min(SoG_TiW$temp), max(SoG_TiW$temp), 0.5))
preds_SoG <- augment(fit, newdata = new_data) %>% 
  mutate(SR = "Strait of Georgia")

# plot data and model fit
ggplot(SoG_TiW, aes(temp, TiW)) +
  geom_point() +
  geom_line(aes(temp, .fitted), preds_SoG, col = 'blue') +
  theme_bw(base_size = 12) +
  labs(x = 'Temperature (ºC)',
       y = 'TiW growth',
       title = "Weibull")
#Combine all the predictions into one df SR----
preds_all <- preds_CC %>% 
  rbind(preds_SoG) %>% 
  mutate(SR = as.factor(SR))

# plot data and model fit
p_weibull_SR <- ggplot(meso_subset, aes(temp, TiW)) +
  geom_point(aes(fill = SR), size = 3, alpha = 0.5, shape = 21, stroke = 0) + 
  geom_line(aes(temp, .fitted, col = SR), preds_all, linewidth = 1) +
  theme_bw(base_size = 12) +
  scale_colour_manual(values = c("skyblue", "coral")) +
  scale_fill_manual(values = c("skyblue3", "coral3")) +
  labs(x = 'Temperature (ºC)',
       y = 'Tissue weight growth',
       title = "Weibull",
       col = "Source Region",
       fill = "Source Region") + 
  theme_cowplot(16) + 
  theme(legend.position = "none")
#theme(legend.position = c(0.7, 0.9))

p_weibull_SR

#Combine all SR plots into one----
pcomb_SR <- plot_grid(p_briere_SR, p_flinn_SR, p_gaussian_SR, p_mod_gaussian_SR,
                      p_oneill_SR, p_pawar_SR, p_sharpe_SR, p_weibull_SR,
                      ncol = 4)


#####Now run model selection process----

#Combine all the plots into one----
pcomb <- plot_grid(p_briere, p_flinn, p_gaussian, p_hinshelwood, p_johnsonlewin,
                   p_lactin, p_modgaus, p_oneill, p_pawar, p_quadratic, 
                   p_ratkowsky, p_rezende, p_sharpeschool,  p_sharpeschool_low, p_spain,
                   p_thomas, p_weibull,
                   ncol = 5)

#Ones to exclude: 
#Sharpe-schoolfield low b/c estimating the lower parameters
#Hinehslwood & Johnsonlewin b/c estimating activation & deactivation energies but not limits
#Rezende, Spain and Thomas b/c don't estimate limits and don't fit well
#Also remove Ratkowsky, Quadratic, Lactin

pcomb <- plot_grid(p_briere, p_flinn, p_gaussian,
                   p_modgaus, p_oneill, p_pawar, 
                   p_sharpeschool, p_weibull,
                   ncol = 4)
pcomb

#Models with just the Cedar or SP code----
#biere2_1999 by SP----
start_vals <- get_start_vals(kwak_TiW$temp, kwak_TiW$TiW, model_name = 'briere2_1999')
low_lims <- get_lower_lims(kwak_TiW$temp, kwak_TiW$TiW, model_name = 'briere2_1999')
upper_lims <- get_upper_lims(kwak_TiW$temp, kwak_TiW$TiW, model_name = 'briere2_1999')

fit <- nls_multstart(TiW~briere2_1999(temp, tmin, tmax, a, b),
                     data = kwak_TiW,
                     iter = 500,
                     start_lower = start_vals - 10,
                     start_upper = start_vals + 10,
                     lower = low_lims,
                     upper = upper_lims,
                     supp_errors = 'Y')

#Calculate parameters
calc_params(fit) %>%
  mutate_all(round, 2)

# predict new data
new_data <- data.frame(temp = seq(min(kwak_TiW$temp), max(kwak_TiW$temp), 0.5))
preds_k <- augment(fit, newdata = new_data) %>% 
  mutate(SP = "Kwak")

# plot data and model fit
ggplot(kwak_TiW, aes(temp, TiW)) +
  geom_point() +
  geom_line(aes(temp, .fitted), preds_k, col = 'blue') +
  theme_bw(base_size = 12) +
  labs(x = 'Temperature (ºC)',
       y = 'TiW growth',
       title = "Flinn")

#Start with the cedar df b/c no NA values in that one (maybehave to remove the NA values to run the models)
start_vals <- get_start_vals(pruth_TiW$temp, pruth_TiW$TiW, model_name = 'briere2_1999')
low_lims <- get_lower_lims(pruth_TiW$temp, pruth_TiW$TiW, model_name = 'briere2_1999')
upper_lims <- get_upper_lims(pruth_TiW$temp, pruth_TiW$TiW, model_name = 'briere2_1999')

fit <- nls_multstart(TiW~briere2_1999(temp, tmin, tmax, a, b),
                     data = pruth_TiW,
                     iter = 500,
                     start_lower = start_vals - 10,
                     start_upper = start_vals + 10,
                     lower = low_lims,
                     upper = upper_lims,
                     supp_errors = 'Y')

#Calculate parameters
calc_params(fit) %>%
  mutate_all(round, 2)

# predict new data
new_data <- data.frame(temp = seq(min(pruth_TiW$temp), max(pruth_TiW$temp), 0.5))
preds_p <- augment(fit, newdata = new_data) %>% 
  mutate(SP = "Pruth")

# plot data and model fit
ggplot(pruth_TiW, aes(temp, TiW)) +
  geom_point() +
  geom_line(aes(temp, .fitted), preds_p, col = 'blue') +
  theme_bw(base_size = 12) +
  labs(x = 'Temperature (ºC)',
       y = 'TiW growth',
       title = "Flinn")

start_vals <- get_start_vals(cedar_TiW$temp, cedar_TiW$TiW, model_name = 'briere2_1999')
low_lims <- get_lower_lims(cedar_TiW$temp, cedar_TiW$TiW, model_name = 'briere2_1999')
upper_lims <- get_upper_lims(cedar_TiW$temp, cedar_TiW$TiW, model_name = 'briere2_1999')

fit <- nls_multstart(TiW~briere2_1999(temp, tmin, tmax, a, b),
                     data = cedar_TiW,
                     iter = 500,
                     start_lower = start_vals - 10,
                     start_upper = start_vals + 10,
                     lower = low_lims,
                     upper = upper_lims,
                     supp_errors = 'Y')

#Calculate parameters
calc_params(fit) %>%
  mutate_all(round, 2)

# predict new data
new_data <- data.frame(temp = seq(min(cedar_TiW$temp), max(cedar_TiW$temp), 0.5))
preds_c <- augment(fit, newdata = new_data) %>% 
  mutate(SP = "Cedar")

#Now Heron
start_vals <- get_start_vals(heron_TiW$temp, heron_TiW$TiW, model_name = 'briere2_1999')
low_lims <- get_lower_lims(heron_TiW$temp, heron_TiW$TiW, model_name = 'briere2_1999')
upper_lims <- get_upper_lims(heron_TiW$temp, heron_TiW$TiW, model_name = 'briere2_1999')

fit <- nls_multstart(TiW~briere2_1999(temp, tmin, tmax, a, b),
                     data = heron_TiW,
                     iter = 500,
                     start_lower = start_vals - 10,
                     start_upper = start_vals + 10,
                     lower = low_lims,
                     upper = upper_lims,
                     supp_errors = 'Y')

#Calculate parameters
calc_params(fit) %>%
  mutate_all(round, 2)

# predict new data
new_data <- data.frame(temp = seq(min(heron_TiW$temp), max(heron_TiW$temp), 0.5))
preds_h <- augment(fit, newdata = new_data) %>% 
  mutate(SP = "Heron")

#Combine all the predictions into one df----
preds_all <- preds_k %>% 
  rbind(preds_p, preds_c, preds_h) %>% 
  mutate(SP = as.factor(SP))

both <- kwak_TiW %>% 
  rbind(pruth_TiW, cedar_TiW, heron_TiW) 

# plot data and model fit
p_briere <- ggplot(both, aes(temp, TiW), col = SP) +
  geom_point(aes(col = SP)) +
  geom_line(aes(temp, .fitted, col = SP), preds_all) +
  theme_bw(base_size = 12) +
  scale_colour_manual(values = c("coral", "coral3", "skyblue3", "skyblue")) +
  labs(x = 'Temperature (ºC)',
       y = 'Tissue weight growth',
       title = "Briere") + 
  theme_cowplot(16)

p_briere

plot_temp_box <- function(df, x, y, grp, fill.values, clr.values, lbl.y){
  ggplot(df, aes({{x}}, {{y}}, fill = {{grp}}, colour = {{grp}})) + 
    geom_boxplot(colour = "black", varwidth = TRUE, alpha = 0.8) +
    geom_point(size = 3, alpha=0.5, position = position_jitterdodge(dodge.width = 0.6, jitter.width=0.3))  +
    scale_fill_manual(values = fill.values) +
    scale_colour_manual(values = clr.values) +
    labs(y = lbl.y) +
    theme_cowplot(16)
}
p_briere

#flinn_1991----
#Start with the cedar df b/c no NA values in that one (maybehave to remove the NA values to run the models)
start_vals <- get_start_vals(cedar_TiW$temp, cedar_TiW$TiW, model_name = 'flinn_1991')
low_lims <- get_lower_lims(cedar_TiW$temp, cedar_TiW$TiW, model_name = 'flinn_1991')
upper_lims <- get_upper_lims(cedar_TiW$temp, cedar_TiW$TiW, model_name = 'flinn_1991')

?flinn_1991()

fit <- nls_multstart(TiW~flinn_1991(temp, a, b, c),
                     data = cedar_TiW,
                     iter = 500,
                     start_lower = start_vals - 10,
                     start_upper = start_vals + 10,
                     lower = low_lims,
                     upper = upper_lims,
                     supp_errors = 'Y')

fit

#Calculate parameters
calc_params(fit) %>%
  mutate_all(round, 2)

# predict new data
new_data <- data.frame(temp = seq(min(cedar_TiW$temp), max(cedar_TiW$temp), 0.5))
preds <- augment(fit, newdata = new_data)

# plot data and model fit
p_flinn <- ggplot(cedar_TiW, aes(temp, TiW)) +
  geom_point() +
  geom_line(aes(temp, .fitted), preds, col = 'blue') +
  theme_bw(base_size = 12) +
  labs(x = 'Temperature (ºC)',
       y = 'TiW growth',
       title = "Flinn")


#gaussian_1987----
#Start with the cedar df b/c no NA values in that one (maybehave to remove the NA values to run the models)
start_vals <- get_start_vals(cedar_TiW$temp, cedar_TiW$TiW, model_name = 'gaussian_1987')
low_lims <- get_lower_lims(cedar_TiW$temp, cedar_TiW$TiW, model_name = 'gaussian_1987')
upper_lims <- get_upper_lims(cedar_TiW$temp, cedar_TiW$TiW, model_name = 'gaussian_1987')

?gaussian_1987()

fit <- nls_multstart(TiW~gaussian_1987(temp, rmax, topt, a),
                     data = cedar_TiW,
                     iter = 500,
                     start_lower = start_vals - 10,
                     start_upper = start_vals + 10,
                     lower = low_lims,
                     upper = upper_lims,
                     supp_errors = 'Y')

fit

#Calculate parameters
calc_params(fit) %>%
  mutate_all(round, 2)

# predict new data
new_data <- data.frame(temp = seq(min(cedar_TiW$temp), max(cedar_TiW$temp), 0.5))
preds <- augment(fit, newdata = new_data)

# plot data and model fit
p_gaussian <- ggplot(cedar_TiW, aes(temp, TiW)) +
  geom_point() +
  geom_line(aes(temp, .fitted), preds, col = 'blue') +
  theme_bw(base_size = 12) +
  labs(x = 'Temperature (ºC)',
       y = 'TiW growth',
       title = "Gaussian")

#modified gaussian----
#Start with the cedar df b/c no NA values in that one (maybehave to remove the NA values to run the models)
start_vals <- get_start_vals(cedar_TiW$temp, cedar_TiW$TiW, model_name = 'modifiedgaussian_2006')
low_lims <- get_lower_lims(cedar_TiW$temp, cedar_TiW$TiW, model_name = 'modifiedgaussian_2006')
upper_lims <- get_upper_lims(cedar_TiW$temp, cedar_TiW$TiW, model_name = 'modifiedgaussian_2006')

?modifiedgaussian_2006()

fit <- nls_multstart(TiW~modifiedgaussian_2006(temp, rmax, topt, a, b),
                     data = cedar_TiW,
                     iter = 500,
                     start_lower = start_vals - 10,
                     start_upper = start_vals + 10,
                     lower = low_lims,
                     upper = upper_lims,
                     supp_errors = 'Y')

fit

#Calculate parameters
calc_params(fit) %>%
  mutate_all(round, 2)

# predict new data
new_data <- data.frame(temp = seq(min(cedar_TiW$temp), max(cedar_TiW$temp), 0.5))
preds <- augment(fit, newdata = new_data)

# plot data and model fit
p_modgaus <- ggplot(cedar_TiW, aes(temp, TiW)) +
  geom_point() +
  geom_line(aes(temp, .fitted), preds, col = 'blue') +
  theme_bw(base_size = 12) +
  labs(x = 'Temperature (ºC)',
       y = 'TiW growth',
       title = "Modified gaussian")

#oneill----
#Start with the cedar df b/c no NA values in that one (maybehave to remove the NA values to run the models)
start_vals <- get_start_vals(cedar_TiW$temp, cedar_TiW$TiW, model_name = 'oneill_1972')
low_lims <- get_lower_lims(cedar_TiW$temp, cedar_TiW$TiW, model_name = 'oneill_1972')
upper_lims <- get_upper_lims(cedar_TiW$temp, cedar_TiW$TiW, model_name = 'oneill_1972')

fit <- nls_multstart(TiW~oneill_1972(temp, rmax,ctmax,topt, q10),
                     data = cedar_TiW,
                     iter = 500,
                     start_lower = start_vals - 10,
                     start_upper = start_vals + 10,
                     lower = low_lims,
                     upper = upper_lims,
                     supp_errors = 'Y')

fit

#Calculate parameters
calc_params(fit) %>%
  mutate_all(round, 2)

# predict new data
new_data <- data.frame(temp = seq(min(cedar_TiW$temp), max(cedar_TiW$temp), 0.5))
preds <- augment(fit, newdata = new_data)

# plot data and model fit
p_oneill <- ggplot(cedar_TiW, aes(temp, TiW)) +
  geom_point() +
  geom_line(aes(temp, .fitted), preds, col = 'blue') +
  theme_bw(base_size = 12) +
  labs(x = 'Temperature (ºC)',
       y = 'TiW growth',
       title = "O'Neill")


#Pawar----
#Start with pawar_2018() & the cedar df b/c no NA values in that one (maybehave to remove the NA values to run the models)
start_vals <- get_start_vals(cedar_TiW$temp, cedar_TiW$TiW, model_name = 'pawar_2018')
low_lims <- get_lower_lims(cedar_TiW$temp, cedar_TiW$TiW, model_name = 'pawar_2018')
upper_lims <- get_upper_lims(cedar_TiW$temp, cedar_TiW$TiW, model_name = 'pawar_2018')

fit <- nls_multstart(TiW~pawar_2018(temp, r_tref,e,eh,topt, tref = 15),
                     data = cedar_TiW,
                     iter = 500,
                     start_lower = start_vals - 10,
                     start_upper = start_vals + 10,
                     lower = low_lims,
                     upper = upper_lims,
                     supp_errors = 'Y')

#Calculate parameters
calc_params(fit) %>%
  mutate_all(round, 2)

# predict new data
new_data <- data.frame(temp = seq(min(cedar_TiW$temp), max(cedar_TiW$temp), 0.5))
preds <- augment(fit, newdata = new_data)

# plot data and model fit
p_pawar <- ggplot(cedar_TiW, aes(temp, TiW)) +
  geom_point() +
  geom_line(aes(temp, .fitted), preds, col = 'blue') +
  theme_bw(base_size = 12) +
  labs(x = 'Temperature (ºC)',
       y = 'TiW growth',
       title = "Pawar")

#sharpeschool_high----
#Start with the cedar df b/c no NA values in that one (maybehave to remove the NA values to run the models)
start_vals <- get_start_vals(cedar_TiW$temp, cedar_TiW$TiW, model_name = 'sharpeschoolhigh_1981')
low_lims <- get_lower_lims(cedar_TiW$temp, cedar_TiW$TiW, model_name = 'sharpeschoolhigh_1981')
upper_lims <- get_upper_lims(cedar_TiW$temp, cedar_TiW$TiW, model_name = 'sharpeschoolhigh_1981')

fit <- nls_multstart(TiW~sharpeschoolhigh_1981(temp, r_tref,e,eh, th, tref = 15),
                     data = cedar_TiW,
                     iter = 500,
                     start_lower = start_vals - 10,
                     start_upper = start_vals + 10,
                     lower = low_lims,
                     upper = upper_lims,
                     supp_errors = 'Y')

fit

#Calculate parameters
calc_params(fit) %>%
  mutate_all(round, 2)

# predict new data
new_data <- data.frame(temp = seq(min(cedar_TiW$temp), max(cedar_TiW$temp), 0.5))
preds <- augment(fit, newdata = new_data)

# plot data and model fit
p_sharpeschool <- ggplot(cedar_TiW, aes(temp, TiW)) +
  geom_point() +
  geom_line(aes(temp, .fitted), preds, col = 'blue') +
  theme_bw(base_size = 12) +
  labs(x = 'Temperature (ºC)',
       y = 'TiW growth',
       title = "Sharpe-schoolfield")




#weibull----
#Start with the cedar df b/c no NA values in that one (maybehave to remove the NA values to run the models)
start_vals <- get_start_vals(cedar_TiW$temp, cedar_TiW$TiW, model_name = 'weibull_1995')
low_lims <- get_lower_lims(cedar_TiW$temp, cedar_TiW$TiW, model_name = 'weibull_1995')
upper_lims <- get_upper_lims(cedar_TiW$temp, cedar_TiW$TiW, model_name = 'weibull_1995')

?weibull_1995()

fit <- nls_multstart(TiW~weibull_1995(temp, a, topt, b, c),
                     data = cedar_TiW,
                     iter = 500,
                     start_lower = start_vals - 10,
                     start_upper = start_vals + 10,
                     lower = low_lims,
                     upper = upper_lims,
                     supp_errors = 'Y')

fit

#Calculate parameters
calc_params(fit) %>%
  mutate_all(round, 2)

# predict new data
new_data <- data.frame(temp = seq(min(cedar_TiW$temp), max(cedar_TiW$temp), 0.5))
preds <- augment(fit, newdata = new_data)

# plot data and model fit
p_weibull <- ggplot(cedar_TiW, aes(temp, TiW)) +
  geom_point() +
  geom_line(aes(temp, .fitted), preds, col = 'blue') +
  theme_bw(base_size = 12) +
  labs(x = 'Temperature (ºC)',
       y = 'TiW growth',
       title = "Weibull")


#Models that you decided not to include----
#hinshelwood_1947----
#Start with the cedar df b/c no NA values in that one (maybehave to remove the NA values to run the models)
start_vals <- get_start_vals(cedar_TiW$temp, cedar_TiW$TiW, model_name = 'hinshelwood_1947')
low_lims <- get_lower_lims(cedar_TiW$temp, cedar_TiW$TiW, model_name = 'hinshelwood_1947')
upper_lims <- get_upper_lims(cedar_TiW$temp, cedar_TiW$TiW, model_name = 'hinshelwood_1947')

?hinshelwood_1947()

fit <- nls_multstart(TiW~hinshelwood_1947(temp, a, e, b, eh),
                     data = cedar_TiW,
                     iter = 500,
                     start_lower = start_vals - 10,
                     start_upper = start_vals + 10,
                     lower = low_lims,
                     upper = upper_lims,
                     supp_errors = 'Y')

fit

#Calculate parameters
calc_params(fit) %>%
  mutate_all(round, 2)

# predict new data
new_data <- data.frame(temp = seq(min(cedar_TiW$temp), max(cedar_TiW$temp), 0.5))
preds <- augment(fit, newdata = new_data)

# plot data and model fit
p_hinshelwood <- ggplot(cedar_TiW, aes(temp, TiW)) +
  geom_point() +
  geom_line(aes(temp, .fitted), preds, col = 'blue') +
  theme_bw(base_size = 12) +
  labs(x = 'Temperature (ºC)',
       y = 'TiW growth',
       title = "Hinshelwood")

#johnsonlewin_1946----
#Start with the cedar df b/c no NA values in that one (maybehave to remove the NA values to run the models)
start_vals <- get_start_vals(cedar_TiW$temp, cedar_TiW$TiW, model_name = 'johnsonlewin_1946')
low_lims <- get_lower_lims(cedar_TiW$temp, cedar_TiW$TiW, model_name = 'johnsonlewin_1946')
upper_lims <- get_upper_lims(cedar_TiW$temp, cedar_TiW$TiW, model_name = 'johnsonlewin_1946')

?johnsonlewin_1946()

fit <- nls_multstart(TiW~johnsonlewin_1946(temp, r0, e, eh, topt),
                     data = cedar_TiW,
                     iter = 500,
                     start_lower = start_vals - 10,
                     start_upper = start_vals + 10,
                     lower = low_lims,
                     upper = upper_lims,
                     supp_errors = 'Y')

fit

#Calculate parameters
calc_params(fit) %>%
  mutate_all(round, 2)

# predict new data
new_data <- data.frame(temp = seq(min(cedar_TiW$temp), max(cedar_TiW$temp), 0.5))
preds <- augment(fit, newdata = new_data)

# plot data and model fit
p_johnsonlewin <- ggplot(cedar_TiW, aes(temp, TiW)) +
  geom_point() +
  geom_line(aes(temp, .fitted), preds, col = 'blue') +
  theme_bw(base_size = 12) +
  labs(x = 'Temperature (ºC)',
       y = 'TiW growth',
       title = "Johnsonlewin")

#lactin2_1995----
#Start with the cedar df b/c no NA values in that one (maybehave to remove the NA values to run the models)
start_vals <- get_start_vals(cedar_TiW$temp, cedar_TiW$TiW, model_name = 'lactin2_1995')
low_lims <- get_lower_lims(cedar_TiW$temp, cedar_TiW$TiW, model_name = 'lactin2_1995')
upper_lims <- get_upper_lims(cedar_TiW$temp, cedar_TiW$TiW, model_name = 'lactin2_1995')

?lactin2_1995()

fit <- nls_multstart(TiW~lactin2_1995(temp, a, b, tmax, delta_t),
                     data = cedar_TiW,
                     iter = 500,
                     start_lower = start_vals - 10,
                     start_upper = start_vals + 10,
                     lower = low_lims,
                     upper = upper_lims,
                     supp_errors = 'Y')

fit

#Calculate parameters
calc_params(fit) %>%
  mutate_all(round, 2)

# predict new data
new_data <- data.frame(temp = seq(min(cedar_TiW$temp), max(cedar_TiW$temp), 0.5))
preds <- augment(fit, newdata = new_data)

# plot data and model fit
p_lactin <- ggplot(cedar_TiW, aes(temp, TiW)) +
  geom_point() +
  geom_line(aes(temp, .fitted), preds, col = 'blue') +
  theme_bw(base_size = 12) +
  labs(x = 'Temperature (ºC)',
       y = 'TiW growth',
       title = "Lactin")


#quadratic 2008----
#Start with the cedar df b/c no NA values in that one (maybehave to remove the NA values to run the models)
start_vals <- get_start_vals(cedar_TiW$temp, cedar_TiW$TiW, model_name = 'quadratic_2008')
low_lims <- get_lower_lims(cedar_TiW$temp, cedar_TiW$TiW, model_name = 'quadratic_2008')
upper_lims <- get_upper_lims(cedar_TiW$temp, cedar_TiW$TiW, model_name = 'quadratic_2008')

?quadratic_2008()

fit <- nls_multstart(TiW~quadratic_2008(temp, a, b, c),
                     data = cedar_TiW,
                     iter = 500,
                     start_lower = start_vals - 10,
                     start_upper = start_vals + 10,
                     lower = low_lims,
                     upper = upper_lims,
                     supp_errors = 'Y')

fit

#Calculate parameters
calc_params(fit) %>%
  mutate_all(round, 2)

# predict new data
new_data <- data.frame(temp = seq(min(cedar_TiW$temp), max(cedar_TiW$temp), 0.5))
preds <- augment(fit, newdata = new_data)

# plot data and model fit
p_quadratic <- ggplot(cedar_TiW, aes(temp, TiW)) +
  geom_point() +
  geom_line(aes(temp, .fitted), preds, col = 'blue') +
  theme_bw(base_size = 12) +
  labs(x = 'Temperature (ºC)',
       y = 'TiW growth',
       title = "Quadratic")

#Ratkowsky 1983----
#Start with the cedar df b/c no NA values in that one (maybehave to remove the NA values to run the models)
start_vals <- get_start_vals(cedar_TiW$temp, cedar_TiW$TiW, model_name = 'ratkowsky_1983')
low_lims <- get_lower_lims(cedar_TiW$temp, cedar_TiW$TiW, model_name = 'ratkowsky_1983')
upper_lims <- get_upper_lims(cedar_TiW$temp, cedar_TiW$TiW, model_name = 'ratkowsky_1983')

?ratkowsky_1983()

fit <- nls_multstart(TiW~ratkowsky_1983(temp, tmin, tmax, a, b),
                     data = cedar_TiW,
                     iter = 500,
                     start_lower = start_vals - 10,
                     start_upper = start_vals + 10,
                     lower = low_lims,
                     upper = upper_lims,
                     supp_errors = 'Y')

fit

#Calculate parameters
calc_params(fit) %>%
  mutate_all(round, 2)

# predict new data
new_data <- data.frame(temp = seq(min(cedar_TiW$temp), max(cedar_TiW$temp), 0.5))
preds <- augment(fit, newdata = new_data)

# plot data and model fit
p_ratkowsky <- ggplot(cedar_TiW, aes(temp, TiW)) +
  geom_point() +
  geom_line(aes(temp, .fitted), preds, col = 'blue') +
  theme_bw(base_size = 12) +
  labs(x = 'Temperature (ºC)',
       y = 'TiW growth',
       title = "Ratkowsky")

#Rezende 2019----
#Start with the cedar df b/c no NA values in that one (maybehave to remove the NA values to run the models)
start_vals <- get_start_vals(cedar_TiW$temp, cedar_TiW$TiW, model_name = 'rezende_2019')
low_lims <- get_lower_lims(cedar_TiW$temp, cedar_TiW$TiW, model_name = 'rezende_2019')
upper_lims <- get_upper_lims(cedar_TiW$temp, cedar_TiW$TiW, model_name = 'rezende_2019')

?rezende_2019()

fit <- nls_multstart(TiW~rezende_2019(temp, q10, a, b, c),
                     data = cedar_TiW,
                     iter = 500,
                     start_lower = start_vals - 10,
                     start_upper = start_vals + 10,
                     lower = low_lims,
                     upper = upper_lims,
                     supp_errors = 'Y')

fit

#Calculate parameters
calc_params(fit) %>%
  mutate_all(round, 2)

# predict new data
new_data <- data.frame(temp = seq(min(cedar_TiW$temp), max(cedar_TiW$temp), 0.5))
preds <- augment(fit, newdata = new_data)

# plot data and model fit
p_rezende <- ggplot(cedar_TiW, aes(temp, TiW)) +
  geom_point() +
  geom_line(aes(temp, .fitted), preds, col = 'blue') +
  theme_bw(base_size = 12) +
  labs(x = 'Temperature (ºC)',
       y = 'TiW growth',
       title = "Rezende")
#sharpeschool_low----
#Start with the cedar df b/c no NA values in that one (maybehave to remove the NA values to run the models)
start_vals <- get_start_vals(cedar_TiW$temp, cedar_TiW$TiW, model_name = 'sharpeschoollow_1981')
low_lims <- get_lower_lims(cedar_TiW$temp, cedar_TiW$TiW, model_name = 'sharpeschoollow_1981')
upper_lims <- get_upper_lims(cedar_TiW$temp, cedar_TiW$TiW, model_name = 'sharpeschoollow_1981')

?sharpeschoollow_1981()

fit <- nls_multstart(TiW~sharpeschoollow_1981(temp, r_tref,e,el, tl, tref = 15),
                     data = cedar_TiW,
                     iter = 500,
                     start_lower = start_vals - 10,
                     start_upper = start_vals + 10,
                     lower = low_lims,
                     upper = upper_lims,
                     supp_errors = 'Y')

fit

#Calculate parameters
calc_params(fit) %>%
  mutate_all(round, 2)

# predict new data
new_data <- data.frame(temp = seq(min(cedar_TiW$temp), max(cedar_TiW$temp), 0.5))
preds <- augment(fit, newdata = new_data)

# plot data and model fit
p_sharpeschool_low <- ggplot(cedar_TiW, aes(temp, TiW)) +
  geom_point() +
  geom_line(aes(temp, .fitted), preds, col = 'blue') +
  theme_bw(base_size = 12) +
  labs(x = 'Temperature (ºC)',
       y = 'TiW growth',
       title = "Sharpe-schoolfield Low")



#spain----
#Start with the cedar df b/c no NA values in that one (maybehave to remove the NA values to run the models)
start_vals <- get_start_vals(cedar_TiW$temp, cedar_TiW$TiW, model_name = 'spain_1982')
low_lims <- get_lower_lims(cedar_TiW$temp, cedar_TiW$TiW, model_name = 'spain_1982')
upper_lims <- get_upper_lims(cedar_TiW$temp, cedar_TiW$TiW, model_name = 'spain_1982')

?spain_1982()

fit <- nls_multstart(TiW~spain_1982(temp, a, b, c, r0),
                     data = cedar_TiW,
                     iter = 500,
                     start_lower = start_vals - 10,
                     start_upper = start_vals + 10,
                     lower = low_lims,
                     upper = upper_lims,
                     supp_errors = 'Y')

fit

#Calculate parameters
calc_params(fit) %>%
  mutate_all(round, 2)

# predict new data
new_data <- data.frame(temp = seq(min(cedar_TiW$temp), max(cedar_TiW$temp), 0.5))
preds <- augment(fit, newdata = new_data)

# plot data and model fit
p_spain <- ggplot(cedar_TiW, aes(temp, TiW)) +
  geom_point() +
  geom_line(aes(temp, .fitted), preds, col = 'blue') +
  theme_bw(base_size = 12) +
  labs(x = 'Temperature (ºC)',
       y = 'TiW growth',
       title = "Spain")




#thomas----
#Start with the cedar df b/c no NA values in that one (maybehave to remove the NA values to run the models)
start_vals <- get_start_vals(cedar_TiW$temp, cedar_TiW$TiW, model_name = 'thomas_2012')
low_lims <- get_lower_lims(cedar_TiW$temp, cedar_TiW$TiW, model_name = 'thomas_2012')
upper_lims <- get_upper_lims(cedar_TiW$temp, cedar_TiW$TiW, model_name = 'thomas_2012')

?thomas_2012()

fit <- nls_multstart(TiW~thomas_2012(temp, a, b, c, topt),
                     data = cedar_TiW,
                     iter = 500,
                     start_lower = start_vals - 10,
                     start_upper = start_vals + 10,
                     lower = low_lims,
                     upper = upper_lims,
                     supp_errors = 'Y')

fit

#Calculate parameters
calc_params(fit) %>%
  mutate_all(round, 2)

# predict new data
new_data <- data.frame(temp = seq(min(cedar_TiW$temp), max(cedar_TiW$temp), 0.5))
preds <- augment(fit, newdata = new_data)

# plot data and model fit
p_thomas <- ggplot(cedar_TiW, aes(temp, TiW)) +
  geom_point() +
  geom_line(aes(temp, .fitted), preds, col = 'blue') +
  theme_bw(base_size = 12) +
  labs(x = 'Temperature (ºC)',
       y = 'TiW growth',
       title = "Thomas")




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


#CC_TiW: Briere: Fit data----
# fit with Gaussian model
d_fit <- nest(CC_TiW, data = c(temp, rate)) %>%
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
  geom_point(aes(temp, rate), CC_TiW, size = 2, alpha = 0.5) +
  theme_bw(base_size = 12) +
  labs(x = 'Temperature (ºC)',
       y = 'Tissue growth rate',
       title = 'Central Coast')

#CC_TiW: Briere: refit model using nlsLM----
fit_nlsLM <- minpack.lm::nlsLM(rate~briere2_1999(temp, tmin, tmax, a, b),
                               data = CC_TiW,
                               start = coef(d_fit$briere[[1]]),
                               lower = get_lower_lims(CC_TiW$temp, CC_TiW$rate, model_name = 'briere2_1999'),
                               upper = get_upper_lims(CC_TiW$temp, CC_TiW$rate, model_name = 'briere2_1999'),
                               weights = rep(1, times = nrow(CC_TiW)))

# bootstrap using case resampling
boot1 <- Boot(fit_nlsLM, method = 'case')

# look at the data
head(boot1$t)

hist(boot1, layout = c(2,2))

#CC_TiW: Briere: Now plot the bootstrapped models----
#create predictions of each bootstrapped model
boot1_preds <- boot1$t %>%
  as.data.frame() %>%
  drop_na() %>%
  mutate(iter = 1:n()) %>%
  group_by_all() %>%
  do(data.frame(temp = seq(min(CC_TiW$temp), max(CC_TiW$temp), length.out = 100))) %>%
  ungroup() %>%
  mutate(pred = briere2_1999(temp, tmin, tmax, a, b))

# calculate bootstrapped confidence intervals
boot1_conf_preds <- group_by(boot1_preds, temp) %>%
  summarise(conf_lower = quantile(pred, 0.025),
            conf_upper = quantile(pred, 0.975)) %>%
  ungroup()

# plot bootstrapped CIs
p_briere_CC <- ggplot() +
  geom_line(aes(temp, .fitted), d_preds, col = 'blue') +
  geom_ribbon(aes(temp, ymin = conf_lower, ymax = conf_upper), boot1_conf_preds, fill = 'blue', alpha = 0.3) +
  geom_point(aes(temp, rate), CC_TiW, size = 2, alpha = 0.5) +
  theme_bw(base_size = 12) +
  labs(x = 'Temperature (ºC)',
       y = 'Tissue weight growth',
       title = 'Briere: CC')

p_briere_CC

#CC_TiW: Briere: Estimate parameters & CI intervals ----
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
         RV = "TiW")

ggplot(ci_params_select, aes(param, estimate)) +
  geom_point(size = 4) +
  geom_linerange(aes(ymin = conf_lower, ymax = conf_upper)) +
  theme_bw() +
  facet_wrap(~param, scales = 'free') +
  scale_x_discrete('') +
  labs(title = 'Briere - CC')


#SoG_TiW: Briere: Fit data----
# fit with Gaussian model
d_fit <- nest(SoG_TiW, data = c(temp, rate)) %>%
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
  geom_point(aes(temp, rate), SoG_TiW, size = 2, alpha = 0.5) +
  theme_bw(base_size = 12) +
  labs(x = 'Temperature (ºC)',
       y = 'Tissue growth rate',
       title = 'Strait of Georgia')

#SoG_TiW: Briere: refit model using nlsLM----
fit_nlsLM <- minpack.lm::nlsLM(rate~briere2_1999(temp, tmin, tmax, a, b),
                               data = SoG_TiW,
                               start = coef(d_fit$briere[[1]]),
                               lower = get_lower_lims(SoG_TiW$temp, SoG_TiW$rate, model_name = 'briere2_1999'),
                               upper = get_upper_lims(SoG_TiW$temp, SoG_TiW$rate, model_name = 'briere2_1999'),
                               weights = rep(1, times = nrow(SoG_TiW)))

# bootstrap using case resampling
boot1 <- Boot(fit_nlsLM, method = 'case')

# look at the data
head(boot1$t)

hist(boot1, layout = c(2,2))

#SoG_TiW: Briere: Now plot the bootstrapped models----
#create predictions of each bootstrapped model
boot1_preds <- boot1$t %>%
  as.data.frame() %>%
  drop_na() %>%
  mutate(iter = 1:n()) %>%
  group_by_all() %>%
  do(data.frame(temp = seq(min(SoG_TiW$temp), max(SoG_TiW$temp), length.out = 100))) %>%
  ungroup() %>%
  mutate(pred = briere2_1999(temp, tmin, tmax, a, b))

# calculate bootstrapped confidence intervals
boot1_conf_preds <- group_by(boot1_preds, temp) %>%
  summarise(conf_lower = quantile(pred, 0.025),
            conf_upper = quantile(pred, 0.975)) %>%
  ungroup()

# plot bootstrapped CIs
p_briere_SoG <- ggplot() +
  geom_line(aes(temp, .fitted), d_preds, col = 'blue') +
  geom_ribbon(aes(temp, ymin = conf_lower, ymax = conf_upper), boot1_conf_preds, fill = 'blue', alpha = 0.3) +
  geom_point(aes(temp, rate), SoG_TiW, size = 2, alpha = 0.5) +
  theme_bw(base_size = 12) +
  labs(x = 'Temperature (ºC)',
       y = 'Tissue weight growth',
       title = 'Briere: SoG')

p_briere_SoG

#SoG_TiW: Briere: Estimate parameters & CI intervals ----
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
         RV = "TiW")

ggplot(ci_params_select, aes(param, estimate)) +
  geom_point(size = 4) +
  geom_linerange(aes(ymin = conf_lower, ymax = conf_upper)) +
  theme_bw() +
  facet_wrap(~param, scales = 'free') +
  scale_x_discrete('') +
  labs(title = 'Briere - SoG')



#CC_ShW: Briere: Fit data----
# fit with Gaussian model
d_fit <- nest(CC_ShW, data = c(temp, rate)) %>%
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
  geom_point(aes(temp, rate), CC_ShW, size = 2, alpha = 0.5) +
  theme_bw(base_size = 12) +
  labs(x = 'Temperature (ºC)',
       y = 'Shell growth rate',
       title = 'Central Coast')

#CC_ShW: Briere: refit model using nlsLM----
fit_nlsLM <- minpack.lm::nlsLM(rate~briere2_1999(temp, tmin, tmax, a, b),
                               data = CC_ShW,
                               start = coef(d_fit$briere[[1]]),
                               lower = get_lower_lims(CC_ShW$temp, CC_ShW$rate, model_name = 'briere2_1999'),
                               upper = get_upper_lims(CC_ShW$temp, CC_ShW$rate, model_name = 'briere2_1999'),
                               weights = rep(1, times = nrow(CC_ShW)))

# bootstrap using case resampling
boot1 <- Boot(fit_nlsLM, method = 'case')

# look at the data
head(boot1$t)

hist(boot1, layout = c(2,2))

#CC_ShW: Briere: Now plot the bootstrapped models----
#create predictions of each bootstrapped model
boot1_preds <- boot1$t %>%
  as.data.frame() %>%
  drop_na() %>%
  mutate(iter = 1:n()) %>%
  group_by_all() %>%
  do(data.frame(temp = seq(min(CC_ShW$temp), max(CC_ShW$temp), length.out = 100))) %>%
  ungroup() %>%
  mutate(pred = briere2_1999(temp, tmin, tmax, a, b))

# calculate bootstrapped confidence intervals
boot1_conf_preds <- group_by(boot1_preds, temp) %>%
  summarise(conf_lower = quantile(pred, 0.025),
            conf_upper = quantile(pred, 0.975)) %>%
  ungroup()

# plot bootstrapped CIs
p_briere_CC <- ggplot() +
  geom_line(aes(temp, .fitted), d_preds, col = 'blue') +
  geom_ribbon(aes(temp, ymin = conf_lower, ymax = conf_upper), boot1_conf_preds, fill = 'blue', alpha = 0.3) +
  geom_point(aes(temp, rate), CC_ShW, size = 2, alpha = 0.5) +
  theme_bw(base_size = 12) +
  labs(x = 'Temperature (ºC)',
       y = 'Shell weight growth',
       title = 'Briere: CC')

p_briere_CC

#CC_ShW: Briere: Estimate parameters & CI intervals ----
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
         RV = "ShW")

ggplot(ci_params_select, aes(param, estimate)) +
  geom_point(size = 4) +
  geom_linerange(aes(ymin = conf_lower, ymax = conf_upper)) +
  theme_bw() +
  facet_wrap(~param, scales = 'free') +
  scale_x_discrete('') +
  labs(title = 'Briere - CC')

#SoG_ShW: Briere: Fit data----
# fit with Gaussian model
d_fit <- nest(SoG_ShW, data = c(temp, rate)) %>%
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
  geom_point(aes(temp, rate), SoG_ShW, size = 2, alpha = 0.5) +
  theme_bw(base_size = 12) +
  labs(x = 'Temperature (ºC)',
       y = 'Shell growth rate',
       title = 'Strait of Georgia')

#SoG_ShW: Briere: refit model using nlsLM----
fit_nlsLM <- minpack.lm::nlsLM(rate~briere2_1999(temp, tmin, tmax, a, b),
                               data = SoG_ShW,
                               start = coef(d_fit$briere[[1]]),
                               lower = get_lower_lims(SoG_ShW$temp, SoG_ShW$rate, model_name = 'briere2_1999'),
                               upper = get_upper_lims(SoG_ShW$temp, SoG_ShW$rate, model_name = 'briere2_1999'),
                               weights = rep(1, times = nrow(SoG_ShW)))

# bootstrap using case resampling
boot1 <- Boot(fit_nlsLM, method = 'case')

# look at the data
head(boot1$t)

hist(boot1, layout = c(2,2))

#SoG_ShW: Briere: Now plot the bootstrapped models----
#create predictions of each bootstrapped model
boot1_preds <- boot1$t %>%
  as.data.frame() %>%
  drop_na() %>%
  mutate(iter = 1:n()) %>%
  group_by_all() %>%
  do(data.frame(temp = seq(min(SoG_ShW$temp), max(SoG_ShW$temp), length.out = 100))) %>%
  ungroup() %>%
  mutate(pred = briere2_1999(temp, tmin, tmax, a, b))

# calculate bootstrapped confidence intervals
boot1_conf_preds <- group_by(boot1_preds, temp) %>%
  summarise(conf_lower = quantile(pred, 0.025),
            conf_upper = quantile(pred, 0.975)) %>%
  ungroup()

# plot bootstrapped CIs
p_briere_SoG <- ggplot() +
  geom_line(aes(temp, .fitted), d_preds, col = 'blue') +
  geom_ribbon(aes(temp, ymin = conf_lower, ymax = conf_upper), boot1_conf_preds, fill = 'blue', alpha = 0.3) +
  geom_point(aes(temp, rate), SoG_ShW, size = 2, alpha = 0.5) +
  theme_bw(base_size = 12) +
  labs(x = 'Temperature (ºC)',
       y = 'Shell weight growth',
       title = 'Briere: SoG')

p_briere_SoG

#SoG_ShW: Briere: Estimate parameters & CI intervals ----
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
         RV = "ShW")

param_briere_SoG <- ggplot(ci_params_select, aes(param, estimate)) +
  geom_point(size = 4) +
  geom_linerange(aes(ymin = conf_lower, ymax = conf_upper)) +
  theme_bw() +
  facet_wrap(~param, scales = 'free') +
  scale_x_discrete('') +
  labs(title = 'Briere - SoG')



#CC_l: Briere: Fit data----
# fit with Gaussian model
d_fit <- nest(CC_l, data = c(temp, rate)) %>%
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
  geom_point(aes(temp, rate), CC_l, size = 2, alpha = 0.5) +
  theme_bw(base_size = 12) +
  labs(x = 'Temperature (ºC)',
       y = 'Shell growth rate',
       title = 'Central Coast')

#CC_l: Briere: refit model using nlsLM----
fit_nlsLM <- minpack.lm::nlsLM(rate~briere2_1999(temp, tmin, tmax, a, b),
                               data = CC_l,
                               start = coef(d_fit$briere[[1]]),
                               lower = get_lower_lims(CC_l$temp, CC_l$rate, model_name = 'briere2_1999'),
                               upper = get_upper_lims(CC_l$temp, CC_l$rate, model_name = 'briere2_1999'),
                               weights = rep(1, times = nrow(CC_l)))

# bootstrap using case resampling
boot1 <- Boot(fit_nlsLM, method = 'case')

# look at the data
head(boot1$t)

hist(boot1, layout = c(2,2))

#CC_l: Briere: Now plot the bootstrapped models----
#create predictions of each bootstrapped model
boot1_preds <- boot1$t %>%
  as.data.frame() %>%
  drop_na() %>%
  mutate(iter = 1:n()) %>%
  group_by_all() %>%
  do(data.frame(temp = seq(min(CC_l$temp), max(CC_l$temp), length.out = 100))) %>%
  ungroup() %>%
  mutate(pred = briere2_1999(temp, tmin, tmax, a, b))

# calculate bootstrapped confidence intervals
boot1_conf_preds <- group_by(boot1_preds, temp) %>%
  summarise(conf_lower = quantile(pred, 0.025),
            conf_upper = quantile(pred, 0.975)) %>%
  ungroup()

# plot bootstrapped CIs
p_briere_CC <- ggplot() +
  geom_line(aes(temp, .fitted), d_preds, col = 'blue') +
  geom_ribbon(aes(temp, ymin = conf_lower, ymax = conf_upper), boot1_conf_preds, fill = 'blue', alpha = 0.3) +
  geom_point(aes(temp, rate), CC_l, size = 2, alpha = 0.5) +
  theme_bw(base_size = 12) +
  labs(x = 'Temperature (ºC)',
       y = 'Length growth',
       title = 'Briere: CC')

p_briere_CC

#CC_l: Briere: Estimate parameters & CI intervals ----
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
         RV = "l")

ggplot(ci_params_select, aes(param, estimate)) +
  geom_point(size = 4) +
  geom_linerange(aes(ymin = conf_lower, ymax = conf_upper)) +
  theme_bw() +
  facet_wrap(~param, scales = 'free') +
  scale_x_discrete('') +
  labs(title = 'Briere - CC')

#SoG_l: Briere: Fit data----
# fit with Gaussian model
d_fit <- nest(SoG_l, data = c(temp, rate)) %>%
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
  geom_point(aes(temp, rate), SoG_l, size = 2, alpha = 0.5) +
  theme_bw(base_size = 12) +
  labs(x = 'Temperature (ºC)',
       y = 'Shell growth rate',
       title = 'Strait of Georgia')

#SoG_l: Briere: refit model using nlsLM----
fit_nlsLM <- minpack.lm::nlsLM(rate~briere2_1999(temp, tmin, tmax, a, b),
                               data = SoG_l,
                               start = coef(d_fit$briere[[1]]),
                               lower = get_lower_lims(SoG_l$temp, SoG_l$rate, model_name = 'briere2_1999'),
                               upper = get_upper_lims(SoG_l$temp, SoG_l$rate, model_name = 'briere2_1999'),
                               weights = rep(1, times = nrow(SoG_l)))

# bootstrap using case resampling
boot1 <- Boot(fit_nlsLM, method = 'case')

# look at the data
head(boot1$t)

hist(boot1, layout = c(2,2))

#SoG_l: Briere: Now plot the bootstrapped models----
#create predictions of each bootstrapped model
boot1_preds <- boot1$t %>%
  as.data.frame() %>%
  drop_na() %>%
  mutate(iter = 1:n()) %>%
  group_by_all() %>%
  do(data.frame(temp = seq(min(SoG_l$temp), max(SoG_l$temp), length.out = 100))) %>%
  ungroup() %>%
  mutate(pred = briere2_1999(temp, tmin, tmax, a, b))

# calculate bootstrapped confidence intervals
boot1_conf_preds <- group_by(boot1_preds, temp) %>%
  summarise(conf_lower = quantile(pred, 0.025),
            conf_upper = quantile(pred, 0.975)) %>%
  ungroup()

# plot bootstrapped CIs
p_briere_SoG <- ggplot() +
  geom_line(aes(temp, .fitted), d_preds, col = 'blue') +
  geom_ribbon(aes(temp, ymin = conf_lower, ymax = conf_upper), boot1_conf_preds, fill = 'blue', alpha = 0.3) +
  geom_point(aes(temp, rate), SoG_l, size = 2, alpha = 0.5) +
  theme_bw(base_size = 12) +
  labs(x = 'Temperature (ºC)',
       y = 'length',
       title = 'Briere: SoG')

p_briere_SoG

#SoG_l: Briere: Estimate parameters & CI intervals ----
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
         RV = "l")

ggplot(ci_params_select, aes(param, estimate)) +
  geom_point(size = 4) +
  geom_linerange(aes(ymin = conf_lower, ymax = conf_upper)) +
  theme_bw() +
  facet_wrap(~param, scales = 'free') +
  scale_x_discrete('') +
  labs(title = 'Briere - SoG')


#CC_fr: Briere: Fit data----
# fit with Gaussian model
d_fit <- nest(CC_fr, data = c(temp, rate)) %>%
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
  geom_point(aes(temp, rate), CC_fr, size = 2, alpha = 0.5) +
  theme_bw(base_size = 12) +
  labs(x = 'Temperature (ºC)',
       y = 'Feeding rate',
       title = 'Central Coast')

#CC_fr: Briere: refit model using nlsLM----
fit_nlsLM <- minpack.lm::nlsLM(rate~briere2_1999(temp, tmin, tmax, a, b),
                               data = CC_fr,
                               start = coef(d_fit$briere[[1]]),
                               lower = get_lower_lims(CC_fr$temp, CC_fr$rate, model_name = 'briere2_1999'),
                               upper = get_upper_lims(CC_fr$temp, CC_fr$rate, model_name = 'briere2_1999'),
                               weights = rep(1, times = nrow(CC_fr)))

# bootstrap using case resampling
boot1 <- Boot(fit_nlsLM, method = 'case')

# look at the data
head(boot1$t)

hist(boot1, layout = c(2,2))

#CC_fr: Briere: Now plot the bootstrapped models----
#create predictions of each bootstrapped model
boot1_preds <- boot1$t %>%
  as.data.frame() %>%
  drop_na() %>%
  mutate(iter = 1:n()) %>%
  group_by_all() %>%
  do(data.frame(temp = seq(min(CC_fr$temp), max(CC_fr$temp), length.out = 100))) %>%
  ungroup() %>%
  mutate(pred = briere2_1999(temp, tmin, tmax, a, b))

# calculate bootstrapped confidence intervals
boot1_conf_preds <- group_by(boot1_preds, temp) %>%
  summarise(conf_lower = quantile(pred, 0.025),
            conf_upper = quantile(pred, 0.975)) %>%
  ungroup()

# plot bootstrapped CIs
ggplot() +
  geom_line(aes(temp, .fitted), d_preds, col = 'blue') +
  geom_ribbon(aes(temp, ymin = conf_lower, ymax = conf_upper), boot1_conf_preds, fill = 'blue', alpha = 0.3) +
  geom_point(aes(temp, rate), CC_fr, size = 2, alpha = 0.5) +
  theme_bw(base_size = 12) +
  labs(x = 'Temperature (ºC)',
       y = 'Feeding rate',
       title = 'Briere: CC')

#CC_fr: Briere: Estimate parameters & CI intervals ----
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
         RV = "fr")

ggplot(ci_params_select, aes(param, estimate)) +
  geom_point(size = 4) +
  geom_linerange(aes(ymin = conf_lower, ymax = conf_upper)) +
  theme_bw() +
  facet_wrap(~param, scales = 'free') +
  scale_x_discrete('') +
  labs(title = 'Briere - CC')

#SoG_fr: Briere: Fit data----
# fit with Gaussian model
d_fit <- nest(SoG_fr, data = c(temp, rate)) %>%
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
  geom_point(aes(temp, rate), SoG_fr, size = 2, alpha = 0.5) +
  theme_bw(base_size = 12) +
  labs(x = 'Temperature (ºC)',
       y = 'Shell growth rate',
       title = 'Strait of Georgia')

#SoG_fr: Briere: refit model using nlsLM----
fit_nlsLM <- minpack.lm::nlsLM(rate~briere2_1999(temp, tmin, tmax, a, b),
                               data = SoG_fr,
                               start = coef(d_fit$briere[[1]]),
                               lower = get_lower_lims(SoG_fr$temp, SoG_fr$rate, model_name = 'briere2_1999'),
                               upper = get_upper_lims(SoG_fr$temp, SoG_fr$rate, model_name = 'briere2_1999'),
                               weights = rep(1, times = nrow(SoG_fr)))

# bootstrap using case resampling
boot1 <- Boot(fit_nlsLM, method = 'case')

# look at the data
head(boot1$t)

hist(boot1, layout = c(2,2))

#SoG_fr: Briere: Now plot the bootstrapped models----
#create predictions of each bootstrapped model
boot1_preds <- boot1$t %>%
  as.data.frame() %>%
  drop_na() %>%
  mutate(iter = 1:n()) %>%
  group_by_all() %>%
  do(data.frame(temp = seq(min(SoG_fr$temp), max(SoG_fr$temp), length.out = 100))) %>%
  ungroup() %>%
  mutate(pred = briere2_1999(temp, tmin, tmax, a, b))

# calculate bootstrapped confidence intervals
boot1_conf_preds <- group_by(boot1_preds, temp) %>%
  summarise(conf_lower = quantile(pred, 0.025),
            conf_upper = quantile(pred, 0.975)) %>%
  ungroup()

# plot bootstrapped CIs
ggplot() +
  geom_line(aes(temp, .fitted), d_preds, col = 'blue') +
  geom_ribbon(aes(temp, ymin = conf_lower, ymax = conf_upper), boot1_conf_preds, fill = 'blue', alpha = 0.3) +
  geom_point(aes(temp, rate), SoG_fr, size = 2, alpha = 0.5) +
  theme_bw(base_size = 12) +
  labs(x = 'Temperature (ºC)',
       y = 'feeding rate',
       title = 'Briere: SoG')

#SoG_fr: Briere: Estimate parameters & CI intervals ----
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
         RV = "fr")

ggplot(ci_params_select, aes(param, estimate)) +
  geom_point(size = 4) +
  geom_linerange(aes(ymin = conf_lower, ymax = conf_upper)) +
  theme_bw() +
  facet_wrap(~param, scales = 'free') +
  scale_x_discrete('') +
  labs(title = 'Briere - SoG')
