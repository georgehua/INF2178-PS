## UofT iSchool INF2178 PS4
## Author: Zhaotian Li, Yonghao Li, Hongtianxu Hua
## Date: 20 Mar 2020

library(tidyverse)
library(ggplot2)
library(performance)

# import dataset 
eitc <- read.csv("https://raw.githubusercontent.com/georgehua/INF2178-PS/master/PS4/eitc.csv")

# sanity check
skim(eitc, work, year, children)

# drop na if present
eitc <- eitc %>% drop_na()
eitc %>% summarise_all(.funs = funs(sum(is.na(.))))

# the EITC went into effect in the year 1994
# The EITC only affects women with at least one child, so the
# treatment group will be all women with children.
eitc <- eitc %>%
  filter(age >= 25 & age <= 64 & earn <= 11610) %>%
  mutate(post93 = year >= 1994, 
         anykids = children >= 1)

# plot average workforce paticipation by year
ggplot(eitc, aes(year, work, color = anykids)) +
  stat_summary(geom = 'line') +
  geom_vline(xintercept = 1994) +
  ggtitle("Average workforce paticipation on year for unmarried women") +
  theme_minimal()

# build model
model = lm(work ~ anykids*post93, data = eitc)

summary(model)

# check performance
check_model(model)

model_performance(model)


# verify
# Compute the four data points needed in the DID calculation
control_before = sapply(subset(eitc, post93 == 0 & anykids == 0, select=work), mean)
treatment_before = sapply(subset(eitc, post93 == 0 & anykids == 1, select=work), mean)
control_after = sapply(subset(eitc, post93 == 1 & anykids == 0, select=work), mean)
treatment_after = sapply(subset(eitc, post93 == 1 & anykids == 1, select=work), mean)

# Compute the effect of the EITC 93 extension on the employment of women with children:
(treatment_after-control_after)-(treatment_before-control_before)
