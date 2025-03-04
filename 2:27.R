library(tidyverse)
d <- read_csv("https://raw.githubusercontent.com/difiore/ada-datasets/refs/heads/main/tbs-2006-2008-ranges.csv", col_names = TRUE)
n_boot <- 10000
dmale <- d |> filter(sex == "M")
dfemale <- d |> filter(sex == "F")
##male
mboot <- vector(length = n_boot)
n_male <- length(dmale$kernel95)
for (i in 1:n_boot) {
  mboot[[i]] <- mean(sample(dmale$kernel95, n_male, replace = TRUE))
}
(ci_mboot <- quantile(mboot, probs = c(0.025, 0.975)))
##female
fboot <- vector(length = n_boot)
n_female <- length(dfemale$kernel95)
for (i in 1:n_boot) {
  fboot[[i]] <- mean(sample(dfemale$kernel95, n_female, replace = TRUE))
}
(ci_fboot <- quantile(fboot, probs = c(0.025, 0.975)))

mboot <- as.data.frame(mboot)
fboot <- as.data.frame(fboot)
(malep <- ggplot(mboot, aes(x = mboot)) + 
                  geom_histogram(color="lightblue", fill = "steelblue")
    )
(femalep <- ggplot(fboot, aes(x = fboot)) + 
    geom_histogram(color="lightblue", fill = "steelblue")
)
ci_f <- mean(dfemale$kernel95) + qnorm(c(0.025,0.975))*se_f

se_f <- sd(dfemale$kernel95)/sqrt(length(dfemale$kernel95))

ci_m <- mean(dmale$kernel95) + qnorm(c(0.025,0.975))*se_m

se_m <- sd(dmale$kernel95)/sqrt(length(dmale$kernel95))

##two-sample t-test
numerator <- mean(dfemale$kernel95) - mean(dmale$kernel95) - 0
sfemale <- (length(dfemale$kernel95)-1)*sd(dfemale$kernel95)^2
smale <- (length(dmale$kernel95)-1)*sd(dmale$kernel95)^2
base <- (length(dfemale$kernel95))+(length(dmale$kernel95))-2
sp2 <- sfemale + smale/base
denominator <- sqrt(sp2*(1/length(dfemale)+1/length(dmale)))
t <- numerator/denominator        
###easy way vvvv
t.test(x = dmale$kernel95, y = dfemale$kernel95, alternative = "two.sided")

##two sample permutation test
d <- d |> 
  select (id, sex, kernel95)
summary <- d |> 
  group_by(sex) |>
  summarise(mean = mean(kernel95))
obs <- filter(summary, sex =="F") |> pull(mean) - 
  filter(summary, sex == "M") |> pull(mean)
reps <- 1000
perm <- vector(length = reps)

for (i in 1:reps){
  temp <- d
  temp$sex <- sample(temp$sex)
  summary <- temp |>
    group_by(sex) |>
    summarize(mean = mean(kernel95))
  perm[[i]] <- filter(summary, sex =="F") |> pull(mean) - filter(summary, sex == "M") |> pull(mean)
}
histogram(perm)
#not working v
p <- sum(perm < -1 * abs(obs) | perm > abs(obs))/reps
p
#^^^
install.packages("infer")
library(infer)
d <- d |> specify(formula = kernel95 ~ sex)
d <- d |> hypothesize(null = "independence")
perm <- d |> generate(reps = 10000, type = "permute")
perm <- perm |> calculate(stat = "diff in means", order = c("M", "F"))
perm
visualize (perm, bins = 20)