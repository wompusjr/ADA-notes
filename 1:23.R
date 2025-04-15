library(tidyverse)
f <- file.choose()
d <- read_csv(f, col_names = TRUE)
fbm <- d$Body_mass_female_mean
bs <- d$Brain_Size_Species_Mean
plot (fbm, bs)
plot(log(fbm), log(bs))
##########
45-35
10+15
range(10,25)
