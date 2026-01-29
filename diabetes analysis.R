library(tidyverse)
library(mlbench)
data("PimaIndiansDiabetes")
df <- (PimaIndiansDiabetes)
df %>% glimpse


# what is the average blood glucose for diabetes patient and average blood glucose for non diabetes patient.
df %>% group_by(diabetes) %>% 
  summarise_all(mean)
df %>% group_by(diabetes) %>% 
summarise_all(list(Mean=mean,Sd=sd,Median=median,Min=min,MAx=max)) %>% t()
#Correlation
pos <- df %>% filter(diabetes=="pos")
neg <- df %>% filter(diabetes=="neg")
cor_pos <- pos %>% select(-diabetes) %>% cor()
cor_neg <- neg %>% select(-diabetes) %>% cor()
# to conduct t test
t.test(glucose~diabetes,data=df)
t.test(pos$glucose,neg$glucose)
#visuals
boxplot(glucose~diabetes,data=df)
boxplot(pressure~diabetes,data=df)
boxplot(insulin~diabetes,data=df)
boxplot(mass~diabetes,data=df)
boxplot(pedigree~diabetes,data=df)
boxplot(triceps~diabetes,data=df)
