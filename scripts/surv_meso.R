#Survival analysis on mesocosm snails
#Code taken from your 'Extra_ch2_script.R' file (outside of project)

#MESO: Clean the survival data for analysis----
#Remove the failed tanks (3, 5, 6) and assign the correct tank treatments, separate treat into temp & pH columns
meso_survival <- read.csv("data/snail_RVs/meso_collated_survival.csv")
tanks_remove <- c(3, 5, 6, 17, 20, 21, 24, 18, 19, 22, 23)

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

#Select only necessary columns for analysis
meso_surv_1 <- meso_surv_1 %>% 
  select(!pH) %>% 
  mutate(Temp = as.numeric(as.character(Temp)),
         SR = ifelse(SR == "Nanaimo", "Strait of Georgia", "Central Coast")) %>% 
  rename(status = Dead,
         time = days_diff)

#Try running survival analysis based on ----
#https://www.emilyzabor.com/tutorials/survival_analysis_in_r_tutorial.html

Surv(meso_surv_1$time, meso_surv_1$status) [1:10]

s1 <- survfit(Surv(time, status) ~ SR, data = meso_surv_1)
str(s1)

ggsurvplot(s1, legend.title = "Source Region", xlab = "Time, days", 
           conf.int = TRUE, 
           facet.by = "Temp",
           data = meso_surv_1) 


#Create a new datasframe that duplicates this one 49 times----
meso_surv_2 <- meso_surv_1 %>% 
  slice(rep(1:n(), each = 49)) %>% 
  arrange(ID)

meso_surv_2 <- meso_surv_2 %>% 
  group_by(ID) %>% 
  mutate(day_exp = c(1:49)) %>% 
  mutate(dead_repeat = ifelse(days_diff == 49, 0,
                              ifelse(day_exp >= days_diff, 1, 0)))

meso_surv_temp <- meso_surv_2 %>% 
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


#MESO: Remove this at the end of your process once you're decided to not run the survival analysis for anything, since data violate assumptions----
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


