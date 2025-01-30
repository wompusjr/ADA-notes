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
file2 <- "https://raw.githubusercontent.com/difiore/ada-datasets/refs/heads/main/CPDS-1960-2014-reduced.csv"
d2 <- read_csv(file2, col_names = TRUE)
d3 <-read_excel(file,sheet = 1, col_names = TRUE)
d3
install.packages("ggplot2")
library(ggplot2)

