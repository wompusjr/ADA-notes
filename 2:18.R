library(mosaic)
mu <- 10
sigma <- 2
plotDist("norm",mean=mu, sd=sigma, xlab = "x", ylab = "Frequency")
##visualizing a normal distribution ^
##drawing out of the normal distribution v 
s1 <- rnorm(n = 10, mean = 10, sd = 2)
mean(s1)
sd(s1)

reps <- 500
samp_dist_mean <-
  do(reps) * mean(rnorm(n=10, mean=10, sd =2))
str(samp_dist_mean)
histogram(samp_dist_mean$mean)
samp_dist_median <-
  do(reps) * median(rnorm(n=10, mean=10, sd =2))
histogram(samp_dist_median$median)
mean(samp_dist_mean$mean)

se_mean <- sd(samp_dist_mean$mean)
#estimating from a single variable
se_estimate <- sd(s1)/sqrt(length(s1))

##challenge
sample <- rnorm(n = 100, mean = 2, sd = 4)
mean(sample) #what is the mean? 1.706305
sd(sample) #what is the standard deviation? 4.374248
(se1 <- sd(sample/sqrt(length(sample)))) #what is the standard error based on the sample? 0.4374248
reps <- 1000
sample_dist_mean <-
  do(reps) * mean(rnorm(n = 100, mean = 2, sd = 4))
(se2 <- sd(sample_dist_mean$mean)) #se for the sample distribution? 0.3893194
plotDist("t", df=10, xlab = "x", ylab = "Frequency", col = "cyan")
reps <- 1000
sample_dist_t <-
  do(reps) * mean(rt(n = 100, df = 99, ncp = 2))
(mean(sample_dist_t$mean))
plotDist("beta", shape1=0.3, shape2=4, xlab = "x", ylab = "Frequency", col = "cyan")