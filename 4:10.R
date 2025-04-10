library(tidyverse)
library(tidyr)
t <- read_csv("https://raw.githubusercontent.com/difiore/ada-datasets/refs/heads/main/titanic_train.csv", col_names = TRUE)
t <- t |> mutate(Sex = factor(Sex),
                 Pclass = factor(Pclass),
                 Embarked = factor(Embarked))
t <- t |> select(-c(PassengerId, Ticket, Cabin))
(p <- ggplot(data = t, aes(x = Age, y = Survived)) +
  geom_point())
(p2 <- ggplot(data = t, aes(x = Sex, y = Survived)) +
    geom_violin())
m <- glm(Survived ~ Age, data = t, family = "binomial")
summary(m)
coefs <- broom::tidy(m) |> select(estimate)
logOR_female_survival <- coefs$estimate[1] + coefs$estimate[2]*0
logOR_male_survival <- coefs$estimate[1] + coefs$estimate[2]*1
OR_female_survival <- exp(logOR_female_survival)
OR_male_survival <- exp(logOR_male_survival)
PR_male_survival <- OR_male_survival/(1 + OR_male_survival)
x <- data.frame(Sex = c("male", "female"))
(logOR <- predict(m, newdata = x))
(OR <- exp(logOR))
y <- predict(m, newdata = x, se.fit = TRUE)

x