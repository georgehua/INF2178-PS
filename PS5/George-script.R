library(sjPlot)
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
dt <- read_csv("CIT_2019_Cambridge_education.csv")
dt <- dt %>%
  filter(X >= -1.2 & X <= 1.2)

#########################
# Summary stats
#########################
skim(dt)

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
# test whether the density of the score is continuous at the cutoff
#########################
density_check <- rddensity(dt$X)
summary(density_check)

# rdplot(dt$hsgrade_pct, dt$X,
#        x.label = "Distance to GPA Cut-off",
#        y.label = "Frequency",
#        title="Frequency distribution on unique distance to GPA Cut-off",
#        col.dots="red")

gghistogram(dt, x = "X", bins = 30,
            add = "mean", rug = TRUE)

#########################
# Model
#########################

# Probation on Immediate decision, by gender
male_rdd <- dt %>% filter(male == 1)
female_rdd <- dt %>% filter(male == 0)

rdplot(female_rdd$left_school, female_rdd$X,  x.label = "Distance to GPA cut-off", y.label = "Left University Voluntariliy", title="Effect of Academic Probation on Attrition, Female", col.dots="red")

rdplot(male_rdd$left_school, male_rdd$X,  x.label = "Distance to GPA cut-off", y.label = "Left University Voluntariliy", title="Effect of Academic Probation on Attrition, Male", col.dots="red")


# Probation on Immediate decision, by native-language
eng_rdd <- dt %>% filter(english == 1)
non_eng_rdd <- dt %>% filter(english == 0)

rdplot(eng_rdd$left_school, eng_rdd$X,  x.label = "Distance to GPA cut-off", y.label = "Left University Voluntariliy", title="Effect of Native Language on Attrition, English", col.dots="red")

rdplot(non_eng_rdd$left_school, non_eng_rdd$X,  x.label = "Distance to GPA cut-off", y.label = "Left University Voluntariliy", title="Effect of Native Language on Attrition, Non-English", col.dots="red")







