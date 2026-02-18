# The purpose of this Hacky Hours is to get an introduction to bootstrapping while
# practicing our for loop skills. The main idea of bootstrapping is to approximate a 
# sampling distribution that we might not have an analytical solution for. But honestly, with
# a little simulation and a little bootstrapping, you can practice statistics without
# knowing very much statistical theory.

# let's remind ourselves of sampling distributions and looping by approximating the sampling distribution
# of the mean of a population that is normally distributed with mean 0 and standard deviation 10,
# from which we will take 100 samples.

n_ind <- 100
samps <- rnorm(n_ind, 0, 10)

samps
mean(samps)

# CHALLENGE 1: approximate the sampling distribution of the mean by sampling from the
# population and calculating the mean 10000 times. Store the means in a vector called mu.





# plot the histogram (i.e. our approximation of the sampling distribution of the mean)

hist(mu)

# let's plot it as a density and compare to the analytical sampling distribution

plot(density(mu), lwd = 3)

# now add the known sampling distribution. Remember if the standard deviation of the population
# is known, then the standard deviation of the sampling distribution of the mean is sd/sqrt(n)

curve(dnorm(x, 0, 10/sqrt(n_ind)), -4, 4, col = "red", lwd = 3, add = T)

# it looks like our approximation is pretty close to the true sampling distribution!
# we run into a problem here though because we probably can't sample from the population
# 10000 times! Enter the bootstrap! We make--a pretty big--assumption that our sample represents the 
# population and resample from it.

# start by taking a sample from the population
set.seed(12042014)
my_samp <- rnorm(n_ind, 0, 10)

mean(my_samp)

# our sample has a mean of -.73, we might be interested, for whatever reason if the population mean
# is actually different from 0, or if our sample is consistent with a population that has mean 0. 
# we can use bootstrapping to test this. All we have to do is resample--with replacement and the same
# size as our data--many times from our sample and see if our population deviates from 0 (or any othe # value of interest)

# start by taking a bootstrap sample and calculating the mean
my_boot <- sample(my_samp, size = length(my_samp), replace = T)

mean(my_boot)


# CHALLENGE 2: construct the distribution by repeating this process many times 
# (let's say 10000) and store the means in a vector called mu_boot







# plot the histogram (an approximation of the sampling distribution)
hist(mu_boot)

# we can get the 95% confidence interval by taking the .025 and .975 quantiles of our distribution!
# add the interval values as vertical bars on the histogram
abline(v = quantile(mu_boot, c(.025, .975)), col = "red", lwd = 2)

# the interval spans 0, indicating we don't have any evidence that our population has a mean
# that is different from 0. (n.b. from a p-value perspective, this is the same thing as saying that
# p > .05. p = .05 would happen if one of the intervals was exactly 0).


# let's try an example that is a bit trickier, bootstrapping the coefficients for logistic regression.

# here's some survival data from some plants that I grew a while back

survival <- read.csv("boots_2_loops/drought_short.csv")

# let's try to predict survival as a function of the plant's size at the start of a 
# terminal drought experiment.

# to make things clearer let's pull out survival and seedling area and then we will standardize
# seedling area (often a good idea to standardize the predictors)

alive <- survival$survive
z_size <- (survival$seedling_area - mean(survival$seedling_area))/sd(survival$seedling_area)

# now we do a logistic regression
fit <- glm(alive ~ z_size, family = "binomial")

summary(fit)

# for this example, we can get the p-values from the summary (hint: to understand where it comes from
# see what estimat/sd.err is, and then run pnorm(est/std.err) + pnorm(est/std.err, lower.tail = F))
# BUT we are here for boots and loops

# first extract the coefficients

intercept <- as.numeric(fit$coefficients[1])
slope <- as.numeric(fit$coefficients[2])

# CHALLENGE 3: get 10000 bootstrap estimates of the intercept and slope (HINT: sample complete cases, don't sample alive and z_size separately)








# plot the histograms of the slope and intercept as well as the 95% confidence interval

hist(intercept)
abline(v = quantile(intercept, c(.025, .975)), col = "red")

hist(slope)
abline(v = quantile(slope, c(.025, .975)), col = "red")

# looks like p < .05 for the slope but not the intercept (remember we centered the predictor so
# the intercept is log-odds of survival at the mean size)

# let's plot the fit!
# first make a vector to get the expected values over
z <- seq(min(z_size), max(z_size), l = 100)

plot(NULL, ylim = c(0, 1), xlim = c(-2,3), xlab = "Standardized Size", ylab = "Prob. Survive")
points(alive ~ z_size)

#CHALLENGE 4: Add 100 lines using the bootstrapped slopes and intercepts (and a for loop!! =)

# HINT, if I want to add the first bootstrapped sample:
lines(plogis(intercept[1] + slope[1]*z) ~ z, col = "grey")


