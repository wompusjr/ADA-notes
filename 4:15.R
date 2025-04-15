library(tidyverse)
t <- read_csv("https://raw.githubusercontent.com/difiore/ada-datasets/refs/heads/main/titanic_train.csv", col_names = TRUE)
t <- t |> mutate(Sex = factor(Sex),
                 Pclass = factor(Pclass),
                 Embarked = factor(Embarked))
t <- t |> select(-c(PassengerId, Ticket, Cabin))

m <- glm(Survived ~ Pclass, data = t, family = "binomial")
summary(m)
log(odds) = 0.5306 + (-0.6394)class2 + (-1.6704)class3 #equation for results
#take the exponent to unlog it to get the actual odds
#probability = ODDS/1-ODDS
#use predict to generate a predicted outcome given the data
x <- data.frame(Pclass = c("1", "2", "3"), Sex = c("Male", "Female"))
logOR <- predict(m, newdata = x)                
OR <- exp(logOR)
PrS_C1 <- OR[1]/(1+OR[1])
PrS_C2 <- OR[2]/(1+OR[2])
PrS_C3 <- OR[3]/(1+OR[3])

PrS<- predict(m, newdata = x, type = "response")   #adding response changes it to probabilites  
summary(PrS)

m2 <- glm(Survived ~ Pclass + Sex, data = t, family = "binomial")
summary(m2)
logODD = -2.6419 + (-0.8380)*0 + (-1.9055)*1
odds <- exp(logODD)
prob <- odds/(odds + 1)
#or
x <- data.frame(Pclass = c("1", "1", "2", "2", "3", "3"), Sex = c("male", "female", "male", "female", "male", "female"))
PrS <- predict(m2, newdata = x, type = "response")

##getting deviance instead of variance
install.packages("lmtest")
library(lmtest)
lrtest(m, m2)
#OR
anova(m, m2, test = "Chisq")

##poisson regression - poisson is a count - not everything has to be even instances
w <- read_csv("https://raw.githubusercontent.com/difiore/ada-datasets/refs/heads/main/woollydata.csv", col_names = TRUE)

(p <- ggplot(data = w, aes(x = age, y = success)) +
    geom_point())
mw <- glm(success ~ age, data = w, family = "poisson")
mw2 <- glm(success ~ 1, data = w, family = "poisson") #intercept only model
summary(mw)  
lrtest(mw2 ,mw)
