library(tidyverse)
install.packages("readxl")
library(readxl)
s <- c("this", "is", "fart")
m <- matrix(1:40, nrow = 8, ncol=5)
b <- FALSE
class(b)
l <- list(s,m,b)
l <- list(string = s, matrix = m, bool = b)
l
l[["matrix"]][2:4, 3:5]
file <- file.choose()
file
file2 <- file.choose()
d2 <- read_csv(file2, col_names = TRUE)
d <-read_excel(file,sheet = 1, col_names = TRUE) 
d
install.packages("ggplot2")
library(ggplot2)
d2$population
d2$area
d2$density <- d2$population / d2$area
d2 <- d2 %>%
  arrange(desc(density))
##########
d3 <- read_csv("/Users/juneburke/Downloads/KamilarAndCooperData.csv", col_names = TRUE)
d3
names(d3)
detach(d3)
mean(d3$Brain_Size_Species_Mean, na.rm = TRUE)
attach(d3) #allows you to not have to specify which df you're talking about
mean(Brain_Size_Female_Mean)
detach(d3)
with(d3,
     mean(Body_mass_female_mean, na.rm = TRUE),
     ) #with is a function that allows you to not specify which df but w/out attached
summary (d3)
install.packages("skimr")
library(skimr)
skim(d3)
library(ggplot2)
boxplot(log(d3$Body_mass_female_mean))
stripchart(log(d3$Body_mass_female_mean),
           method = "jitter",
           col = "blue",
           vertical = TRUE,
           add = TRUE)
boxplot(log(d3$Body_mass_female_mean) ~ d3$Family)
stripchart(log(d3$Body_mass_female_mean) ~ d3$Family,
           method = "jitter",
           col = "blue",
           vertical = TRUE,
           add = TRUE)
p <- ggplot(data = d3,
       aes(x = d3$Family, y = log(d3$Body_mass_female_mean))) + 
  geom_boxplot(na.rm = TRUE) + 
  geom_jitter( color = "blue", width = 0.1)
p


       