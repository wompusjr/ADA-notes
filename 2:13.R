##data wrangling challenge
library(tidyverse)
install.packages("oce")
library(oce)
library(dplyr)
gps <- read_csv("https://raw.githubusercontent.com/difiore/ada-datasets/refs/heads/main/sample_gps_data.csv", col_names = TRUE)
beh <- read_csv("https://raw.githubusercontent.com/difiore/ada-datasets/refs/heads/main/sample_behavioral_data.csv", col_names = TRUE)
beh <- beh |> mutate(Year = substr(beh$Date.Time, 1, 4))
beh <- filter(beh, Year == (2012:2014))
inner <-inner_join(beh, gps, by = c("Date.Time" = "Date.Time", "Observer" = "Observer"))
inner2 <- inner |>
  rowwise() |>
  mutate(
    easting = lonlat2utm(`Mean.Longitude`,`Mean.Latitude`)$easting,
    northing = lonlat2utm (`Mean.Longitude`, `Mean.Latitude`)$northing + 
      10000000
  )
poto <- filter(inner2, Focal.Animal == ("Poto"))
library(ggplot2)
potoplot <- ggplot(data = poto, aes(x = easting,
                           y = northing)) + 
    geom_point(na.rm = TRUE) 
potoplot
##statistics
library(mosaic)
library(radiant)
#challenge popvar and sampvar
x <- c(10, 100, 1000, 2000)
pop_var <- function (x) {
  v <- sum((x - mean(x))^2)/(length(x))
  return(v)
}
sample_var <- function(x) {
  v <- sum((x - mean(x))^2)/(length(x) - 1)
  return(v)
}
(a <- pop_var(x))
(b <- sample_var(x))
(c <- var(x))
