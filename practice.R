library(tidyverse)
library(mlbench)
data("PimaIndiansDiabetes")
df <- (PimaIndiansDiabetes)
df
df %>% glimpse
df %>% group_by(diabetes) %>% summarise_all(mean)
df %>% group_by(diabetes) %>% summarise_all(list(Mean=mean,SD=sd,Median=median,Min=min,Max=max)) %>% t()
pos <- df %>% filter(diabetes=="pos")
pos
neg <- df %>% filter(diabetes=="neg")
neg
cor_pos <- pos %>% select(-diabetes) %>% cor()
cor_pos
cor_neg <- neg %>% select(-diabetes) %>% cor()
cor_neg
t.test (glucose~diabetes,data=df)
t.test (pressure~diabetes,data=df)
t.test (triceps~diabetes,data=df)
boxplot (glucose~diabetes,data=df)
boxplot (insulin~diabetes,data=df)

# Prevalence
events <- df$diabetes
events
Prevalence <- mean(events=="pos")
#long method to calculate prevalence
total <- nrow(df)
table(df$diabetes)
diabetic_cases <- sum(df$diabetes=="pos",na.rm=TRUE)
prevalence <- diabetic_cases/total
prevalence <- diabetic_cases/total
