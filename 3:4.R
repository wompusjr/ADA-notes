library(tidyverse)
f<- read_csv("https://raw.githubusercontent.com/difiore/ada-datasets/refs/heads/main/tbs-2006-2008-ranges.csv", col_names = TRUE)
dmale <- d |> filter(sex == "M")
dfemale <- d |> filter(sex == "F")
library(infer)
d <- d |> specify(formula = kernel95 ~ sex)
d <- d |> hypothesize(null = "independence")
perm <- d |> generate(reps = 10000, type = "permute")
perm <- perm |> calculate(stat = "diff in means", order = c("M", "F"))
perm
visualize (perm, bins = 20) +
  shade_p_value(obs_stat = obs, direction = "both")
get_p_value(perm, obs_stat = obs, direction = "both")
obs <- d |>
  specify(formula = kernel95 ~ sex) |>
  calculate(stat = "diff in means", order = c("M", "F"))
obs

##regression
install.packages("lmodel2")
install.packages("sjPlot")
install.packages("broom")
library(broom)

f <- read_csv("https://raw.githubusercontent.com/difiore/ada-datasets/refs/heads/main/zombies.csv", col_names = TRUE)

cov <- sum((f$weight - mean(f$weight))*(f$height-mean(f$height)))/(length(f$height)-1)
cov(f$height, f$weight)
cor <- cov/(sd(f$height)*sd(f$weight))
cor(f$height, f$weight)
plot(f$height, f$weight)

f <- mutate(f, centered_height = height - mean(height))
f <- mutate(f, centered_weight = weight - mean(weight))

slope.test <- function(beta1, data) {
  g <- ggplot(data = data, aes(x = centered_weight, y = centered_height))
  g <- g + geom_point()
  g <- g + geom_abline(intercept = 0, slope = beta1, size = 1, colour = "blue",
                       alpha = 1/2)
  ols <- sum((data$centered_height - beta1 * data$centered_weight)^2)
  g <- g + ggtitle(paste("Slope = ", beta1, "\nSum of Squared Deviations = ", round(ols,
                                                                                    3)))
  g
}
library(manipulate)
manipulate(slope.test(beta1, data = f), beta1 = slider(-1, 1, initial = 0, step = 0.005))
var <- var(f$centered_height)
beta1 <- cov/var(f$weight)
beta0 <- mean(f$height) - beta1*mean(f$weight)

lm(height~weight, data = f)
             