library(tidyverse)
library(ggplot2)
library(lme4)
d <- read_csv("https://raw.githubusercontent.com/difiore/ada-datasets/refs/heads/main/chimpgrooming.csv", col_names = TRUE)
(p1 <- ggplot(data = d, mapping = aes(x = subject, y = duration)) +
  geom_boxplot())
(p2 <- ggplot(data = d, mapping = aes(x = reprocondition, y = duration)) +
    geom_boxplot())
(p3 <- ggplot(data = d, mapping = aes(x = reprocondition, y = duration, fill = parity)) +
    geom_boxplot())
(p4 <- ggplot(data = d, mapping = aes(x = reprocondition, y = duration, fill = subject)) +
    geom_boxplot())
mfuller <- lmer(data = d, duration ~ reprocondition + parity + (1|subject), REML = FALSE)
msimple <- lmer(data = d, duration ~ parity + (1|subject), REML = FALSE)
anova(msimple, mfuller, test = "Chisq")

mrandom <- lmer(data = d, duration ~ reprocondition + parity + (1 + reprocondition|subject) + (1 + parity|subject), REML = FALSE)
summary(mrandom)

full <- lmer(data = d, duration ~ reprocondition + parity + (1 + reprocondition|subject) + (1 + parity|subject), REML = FALSE)
minusRC <- lmer(data = d, duration ~ parity + (1 + reprocondition|subject) + (1 + parity|subject), REML = FALSE)
minusP <- lmer(data = d, duration ~ reprocondition + (1 + reprocondition|subject) + (1 + parity|subject), REML = FALSE)
anova(minusRC, full, tect = "Chisq")
anova(minusP, full, test = "Chisq")
#using AIC for inference

null <- lmer(data = d, duration ~ (1 + reprocondition|subject) + (1 + parity|subject), REML = FALSE)
library(AICcmodavg)
(aic_table <- aictab(list(full, minusRC, minusP, null), modnames = c("full", "minusRC", "minusP", "null")))
