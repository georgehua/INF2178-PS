library(ggplot2)
library(rddensity)
library(rdrobust)
library(broom)
library(tidyverse)

testdata = read_csv("CIT_2019_Cambridge_education.csv")

#remove null values in nextGPA or nextGPA_nonorm
testdata <- testdata %>% filter(!is.na(nextGPA))

#draw diagram using Rohan's code
testdata %>% 
  ggplot(aes(x = X,
             y = nextGPA)) +
  geom_point(alpha = 0.2) +
  geom_smooth(data = testdata %>% filter(X < 0), 
              method='lm',
              color = "black") +
  geom_smooth(data = testdata %>% filter(X >= 0), 
              method='lm',
              color = "black") +
  theme_minimal() +
  labs(x = "X",
       y = "next year GPA")

#draw histogram using the code from the book

testdata %>%
  ggplot(aes(X))+
  geom_histogram(breaks=seq(-2.8, 0, by = 0.1), col="black", fill="blue", alpha = 1)+
  geom_histogram(breaks=seq(0, 1.6, by = 0.1), col="black", fill="red", alpha = 1)+
  labs(x="Score", y="Number of Observations")+geom_vline(xintercept=0, color="black")+
  theme_bw()

#test whether the density of the score is continuous at the cutoff
#Rohan's code
robust1 = rdrobust(y = testdata$nextGPA_nonorm, x = testdata$X, c = 0, h = 2, all = TRUE) %>% summary()

#code from the book
robust2 = rddensity(testdata$X)
summary(robust2)

#run regression using Rohan's code
testdata <- 
  testdata %>% 
  mutate(score_below_zero = if_else(X > 0, 0, 1))

result = lm(nextGPA_nonorm ~ X + score_below_zero, data = testdata)

summary(result)

#use the code from the book

rdplot(testdata$nextGPA_nonorm, testdata$X,  binselect = 'esmv', x.label = 'Score', y.label = 'Outcome', title = '')


