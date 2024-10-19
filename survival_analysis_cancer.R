#######################################################################
########################  SURVIVAL ANALYSIS  ##########################
#######################################################################

# Load libraries
library(survival);library(survminer);library(ggplot2);library(dplyr)

#setwd("/Users/emilybates/Documents/Documents - Emily’s MacBook Air/MSDA - Semester 2/Data Driven Decision Making and Design")
cancer <- read.csv("gbsg.csv")

##
## Data Exploration & Manipulation
##
head(cancer)
str(cancer)
sum(is.na(cancer)) #no missing values, yay!

# Define age groups variable
cancer <- cancer %>%
  mutate(age_group = case_when(
    age >= 20 & age < 40 ~ "20-39",
    age >= 40 & age < 60 ~ "40-59",
    age >= 60 & age < 110 ~ "60+",
    TRUE ~ NA_character_  
  ))

head(cancer) # check result

# Plot for hormone status by outcome
ggplot(data = as.data.frame(table(cancer$status, cancer$hormon)), aes(x = Var2, y = Freq, fill = Var1)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Hormone Status by Outcome", x = "Hormone Status", y = "Count") +
  theme_minimal() +
  scale_fill_discrete(name = "Outcome")

# Plot for age group by outcome
ggplot(data = as.data.frame(table(cancer$status, cancer$age_group)), aes(x = Var2, y = Freq, fill = Var1)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Age Group by Outcome", x = "Age Group", y = "Count") +
  theme_minimal() +
  scale_fill_discrete(name = "Outcome")

# Plot for tumor grade by outcome
ggplot(data = as.data.frame(table(cancer$status, cancer$grade)), aes(x = Var2, y = Freq, fill = Var1)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Tumor Grade by Outcome", x = "Tumor Grade", y = "Count") +
  theme_minimal() +
  scale_fill_discrete(name = "Outcome")

##
##survival analysis by age group
##

survfit_obj <- survfit(Surv(rfstime, status) ~ age_group, data = cancer)

summary(survfit_obj) #this shows survival tables for each group

# Plot survival curves
ggsurvplot(survfit_obj, data = cancer, pval = FALSE, conf.int = FALSE)

# Test for overall effect of drugs on survival (log rank test)
survdiff(Surv(rfstime, status) ~ age_group, data = cancer)
# p-value is 0.03
#The log rank test shows that there is at least one difference between the three survival curves.

# Compare survival curves for each drug with each other
pairwise_survdiff(Surv(rfstime, status) ~ age_group, data = cancer)
# The p-value of 0.028 indicates that we can reject the null hypothesis that there isn't a difference 
#between age groups 20-39 and 40-59. The p-value of 0.037 between age groups 20-39 and 60+ indicates 
#that we can also reject the null hypothesis that there isn't a difference between them. The p-value of 
#0.656 between age group 40-59 and 60+ indicates that we cannot reject the null hypothesis - there isn't 
#a difference between these two age groups.

##
##survival analysis by hormonal therapy
##
survfit_obj2 <- survfit(Surv(rfstime, status) ~ hormon, data = cancer)

summary(survfit_obj2) #this shows survival tables for each group

# Plot survival curves
ggsurvplot(survfit_obj2, data = cancer, pval = FALSE, conf.int = FALSE)

# Test for overall effect of drugs on survival (log rank test)
survdiff(Surv(rfstime, status) ~ hormon, data = cancer)
# p-value is 0.003
#The log rank test shows that there is a difference between those who did hormone therapy and those who did not.

##
##survival analysis by tumor grade
##
survfit_obj3 <- survfit(Surv(rfstime, status) ~ grade, data = cancer)

summary(survfit_obj3) #this shows survival tables for each group

# Plot survival curves
ggsurvplot(survfit_obj3, data = cancer, pval = FALSE, conf.int = FALSE)

# Test for overall effect of drugs on survival (log rank test)
survdiff(Surv(rfstime, status) ~ grade, data = cancer)
# p-value is 3e-05

pairwise_survdiff(Surv(rfstime, status) ~ grade, data = cancer)



##
## Analysis with Cox proportional hazards regression
##
# Fit a survival model using Cox proportional hazards regression
surv_model <- coxph(Surv(rfstime, status) ~ age + meno + size + grade + nodes + pgr + er + hormon, data = cancer)

# Summary of the survival model
summary(surv_model)

# Plot the survival curves
# Example: Plot survival curves for age < 50 and age >= 50
# You can adjust the age thresholds based on your data
plot(survfit(surv_model, newdata = data.frame(age = 50, meno = 0, size = 30, grade = 2, nodes = 5, pgr = 0, er = 0, hormon = 0)), col = "blue", lty = 1, lwd = 2, main = "Survival Curve")
lines(survfit(surv_model, newdata = data.frame(age = 70, meno = 0, size = 30, grade = 2, nodes = 5, pgr = 0, er = 0, hormon = 0)), col = "red", lty = 2, lwd = 2)
legend("topright", legend = c("Age < 50", "Age >= 50"), col = c("blue", "red"), lty = 1:2, lwd = 2)



