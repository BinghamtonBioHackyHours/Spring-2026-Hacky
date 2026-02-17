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

mu <- c()
for(i in 1:1e4){
  samps <- rnorm(n_ind, 0, 10)
  mu[i] <- mean(samps)
}