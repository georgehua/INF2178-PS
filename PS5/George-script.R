library(skimr)
library(ggplot2)
library(rddensity)
library(rdrobust)
library(broom)
library(tidyverse)
library(ggpubr)

#########################
# Import and cleaning data
#########################
dt_raw <- read_csv("CIT_2019_Cambridge_education.csv")
dt <- dt %>%
  filter(X >= -1.6 & X <= 1.6)

#########################
# Summary stats
#########################
skim(dt_raw)

# how many total data points
nrow(dt)

# how many unique values
length(unique(dt$X))

# frequency distribution of the running variable
ggplot(dt, aes(x=X)) +
  geom_histogram(breaks=seq(-1.3, 0, by = 0.1), col="black", fill="pink", alpha = 1)+
  geom_histogram(breaks=seq(0, 1.3, by = 0.1), col="black", fill="orange", alpha = 1)+
  geom_vline(aes(xintercept=0),   # Ignore NA values for mean
             color="red", linetype="dashed", size=1) +
  ggtitle("Frequency distribution of the running variable (distance from GPA cutoff)") +
  theme_bw()

#########################
# Assumptions
#########################

# Running variable continuous at the cutoff

# robust test
density_check <- rddensity(dt$X)
summary(density_check)

# plot test
rdplotdensity(density_check, dt$X, xlabel = "Distance to the GPA cut-off", ylabel = "Density", title = "Density plot of X (Distance to the GPA cut-off)")


# Continuous for other revelent variables
summary(rdrobust(dt$hsgrade_pct, dt$X))
summary(rdrobust(dt$totcredits_year1, dt$X))
summary(rdrobust(dt$age_at_entry, dt$X))
summary(rdrobust(dt$male, dt$X))
summary(rdrobust(dt$bpl_north_america, dt$X))
summary(rdrobust(dt$english, dt$X))
summary(rdrobust(dt$loc_campus1, dt$X))
summary(rdrobust(dt$loc_campus2, dt$X))
summary(rdrobust(dt$loc_campus3, dt$X))

#########################
# Model
#########################
# Probation on Immediate decision, by gender

female_rdd <- dt %>% filter(male == 0)
rdplot(female_rdd$left_school, female_rdd$X,  x.label = "Distance to GPA cut-off", y.label = "Left University Voluntariliy", title="Effect of Academic Probation on Attrition, Female", col.dots="red")
summary(rdrobust(female_rdd$left_school, female_rdd$X))

male_rdd <- dt %>% filter(male == 1)
rdplot(male_rdd$left_school, male_rdd$X,  x.label = "Distance to GPA cut-off", y.label = "Left University Voluntariliy", title="Effect of Academic Probation on Attrition, Male", col.dots="red")
summary(rdrobust(male_rdd$left_school, male_rdd$X))

# Probation on Immediate decision, by native-language
eng_rdd <- dt %>% filter(english == 1)
rdplot(eng_rdd$left_school, eng_rdd$X,  x.label = "Distance to GPA cut-off", y.label = "Left University Voluntariliy", title="Effect of Native Language on Attrition, English", col.dots="red")
summary(rdrobust(eng_rdd$left_school, eng_rdd$X))

non_eng_rdd <- dt %>% filter(english == 0)
rdplot(non_eng_rdd$left_school, non_eng_rdd$X,  x.label = "Distance to GPA cut-off", y.label = "Left University Voluntariliy", title="Effect of Native Language on Attrition, Non-English", col.dots="red")
summary(rdrobust(non_eng_rdd$left_school, non_eng_rdd$X))






