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
