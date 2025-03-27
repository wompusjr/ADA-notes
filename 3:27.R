library(tidyverse)
library(mosaic)
d <- read_csv("https://raw.githubusercontent.com/difiore/ada-datasets/refs/heads/main/AVONETdataset1.csv", col_names = TRUE)
d <- d |>
  select(Species1,Family1, Order1, Beak.Width, Beak.Depth, Tarsus.Length, Wing.Length, Tail.Length, Mass, Habitat, Migration, Trophic.Level,Trophic.Niche,Min.Latitude,Max.Latitude,Centroid.Latitude,Range.Size)
glimpse(d)
table(d$Habitat)
nrow(d)
(p1 <- ggplot(data = d |> drop_na(Trophic.Level),
       aes(x = Trophic.Level, y = log(Mass))) +
  geom_boxplot() +
  geom_jitter())
(p2 <- ggplot(data = d |> drop_na(Migration),
              aes(x = Migration, y = log(Mass))) +
    geom_boxplot() +
    geom_jitter())
(p3 <- ggplot(data = d |> drop_na(Habitat),
                                aes(x = Migration, y = log(Mass))) +
                     geom_boxplot() +
                     geom_jitter())
d <- d |> mutate(Migration = as.factor(Migration))
(m1 <- lm(log(Mass) ~ Trophic.Level, data = d)) #intercept is default-set to alphabetical (in this case, carnivores) #intercept is the variable that's being compared to the other ones
d <- d |> mutate(Trophic.Level = relevel(as.factor(Trophic.Level), ref = "Herbivore")) #how to switch intecept
summary(m1)
(m2 <- lm(log(Mass) ~ Migration, data = d))
(pairwise.t.test(log(d$Mass), d$Trophic.Level, p.adj = "bonferroni"))
#permutation approach for ANOVA
original.F <- aov(log(Mass) ~ Trophic.Level, data = d) |>
  broom::tidy() |>
  filter(term == "Trophic.Level")
library(infer)
d <- d |> mutate(logMass = log(Mass))
permuted.F <- d |>
  specify(logMass ~ Trophic.Level) |>
  hypothesize(null = "independence") |>
  generate(reps = 1000, type = "permute") |>
  calculate(stat = "F")
visualize(permuted.F) +
  shade_p_value(obs_stat = original.F$statistic, direction = "greater")
p.value <- permuted.F |> get_p_value(obs_stat = original.F$statistic, direction = "greater")  
#gotime
d <- d |>
  select(Species1,Family1, Order1, Beak.Width, Beak.Depth, Beak.Length_Culmen, Tarsus.Length, Wing.Length, Tail.Length, Mass, Habitat, Migration, Trophic.Level,Trophic.Niche, Primary.Lifestyle, Min.Latitude,Max.Latitude,Centroid.Latitude,Range.Size)
glimpse(d)
mbeak <- lm(log(Beak.Length_Culmen) ~ log(Mass), data = d)
mtarsus <- lm(log(Tarsus.Length) ~ log(Mass), data = d)
d <- d |> mutate(relative.beak.length = mbeak$residuals) 
d <- d |> mutate(relative.tarsus.length = mtarsus$residuals) 
(p01 <- ggplot(data = d |> drop_na(Primary.Lifestyle),
              aes(x = Primary.Lifestyle, y = relative.tarsus.length)) +
    geom_boxplot() 
    )
(p02 <- ggplot(data = d |> drop_na(Trophic.Niche),
               aes(x = Trophic.Niche, y = relative.beak.length)) +
    geom_boxplot() 
    )
aov(log(d$Range.Size) ~ d$Migration)
