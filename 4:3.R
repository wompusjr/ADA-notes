library(tidyverse)
library(mosaic)
library(car)
library(jtools)
z <- read_csv("https://raw.githubusercontent.com/difiore/ada-datasets/refs/heads/main/zombies.csv", col_names = TRUE)
attach(z)
m <- lm(height ~ weight + age + gender)
summary(m)
a <- read_csv("https://raw.githubusercontent.com/difiore/ada-datasets/refs/heads/main/AVONETdataset1.csv", col_names = TRUE)
a <- a |>
  filter(Order1 == "Accipitriformes")
mm <- lm(log(a$Range.Size) ~ log(a$Mass) + a$Primary.Lifestyle) #mu
summary(mm)
 predictheight <- 33.3+0.14*132+0.66*29+1.6*1
(ci <- predict(m,
               newdata = data.frame(age =29, gender = "Male", weight = 132), 
               interval = "prediction",
               level = 0.95)) #spits out prediction intervals
#comparing models 
library(MASS)
library(MuMIn)
library(AICcmodavg) 
a <- read_csv("https://raw.githubusercontent.com/difiore/ada-datasets/refs/heads/main/AVONETdataset1.csv", col_names = TRUE)
a <- a |> dplyr::select(Species1,Family1, Order1, Beak.Width, Beak.Depth, Tarsus.Length, Wing.Length, Tail.Length, Mass, Habitat, Migration, Trophic.Level,Trophic.Niche,Min.Latitude,Max.Latitude,Centroid.Latitude,Range.Size)
m1 <- lm(data = a, log(Beak.Width*Beak.Depth)~ log(Range.Size)*Migration)
summary(m1)
