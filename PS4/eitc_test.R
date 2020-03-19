library(tidyverse)
library(broom)
library(ggplot2)

# Create two additional dummy variables to indicate before/after
# and treatment/control groups using Tarzan's code

# the EITC went into effect in the year 1994
eitc$post93 = as.numeric(eitc$year >= 1994)

# The EITC only affects women with at least one child, so the
# treatment group will be all women with children.
eitc$anykids = as.numeric(eitc$children >= 1)

# Take average value of 'work' by year, conditional on anykids using Tarzan's code
minfo = aggregate(eitc$work, list(eitc$year, eitc$anykids == 1), mean)

# rename column headings (variables)
names(minfo) = c("YR","Treatment","LFPR")

# Attach a new column with labels
minfo$anykids[1:6] = 0
minfo$anykids[7:12] = 1
minfo

# making graph using Tarzan's code

qplot(YR, LFPR, data=minfo, geom=c("point","line"), colour=Group,
      xlab="Year", ylab="Labor Force Participation Rate")

minfo$anykids <- as.factor(minfo$anykids)
minfo$Treatment <- as.factor(minfo$Treatment)

# make graph using Rohan's code, also works

minfo %>%
  ggplot(aes(x = YR,
             y = LFPR,
             color = Treatment)) + 
  geom_point() +
  geom_line(aes(group = anykids)) +
  theme_minimal() +
  labs(x = "employment",
       y = "year of survey",
       color = "after 93 extension") +
  scale_color_brewer(palette = "Set1")


# Compute the four data points needed in the DID calculation using Tarzan's code:
control_before = sapply(subset(eitc, post93 == 0 & anykids == 0, select=work), mean)
treatment_before = sapply(subset(eitc, post93 == 0 & anykids == 1, select=work), mean)
control_after = sapply(subset(eitc, post93 == 1 & anykids == 0, select=work), mean)
treatment_after = sapply(subset(eitc, post93 == 1 & anykids == 1, select=work), mean)

# Compute the effect of the EITC 93 extension on the employment of women with children:
(treatment_after-control_after)-(treatment_before-control_before)

# computing the same thing (I guess) with Rohan's code, does not work
average_differences <- 
  minfo %>% 
  pivot_wider(names_from = Treatment,
              values_from = LFPR,
              names_prefix = "anykids_") %>% 
  mutate(difference = anykids_1 - anykids_0) %>% 
  group_by(Treatment) %>% 
  summarise(average_difference = mean(difference))

# also Rohan's code with the original dataset, does not work either
average_differences <- 
  eitc %>% 
  pivot_wider(names_from = anykids,
              values_fn = list(work = list),
              names_prefix = "anykids_") %>% 
  mutate(difference = anykids_1 - anykids_0) %>% 
  group_by(post93) %>% 
  summarise(average_difference = mean(difference))

#regression with Tarzan's code

eitc$p93kids.interaction = eitc$post93*eitc$anykids
reg1 = lm(work ~ post93 + anykids + p93kids.interaction, data = eitc)
summary(reg1)

#regression with Rohan's code
reg2 = lm(work ~ post93*anykids , data = eitc)
summary(reg2)


