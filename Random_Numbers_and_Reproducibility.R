library(doParallel)
library(parallel)
cl.sock <- makeCluster(1, type = "PSOCK")
registerDoParallel(cl.sock)
cl.fork <- makeCluster(1, type = "FORK")
#Is the following code reproducible and why?
set.seed(100)
foreach(i = 1:2) %dopar% rnorm(3)

#No, because it sets the RNG (Random Number Generator) seed only on the master process.


# Register the SOCK cluster
registerDoParallel(cl.sock)

replicate(
  # Use 2 replicates
  n = 2, 
  expr = {
    # Set the seed to 100
    set.seed(100)
    # Run two iterations in parallel, bound by rows
    foreach(i = 1:2, .combine = rbind) %dopar% rnorm(3)
  }, 
  simplify = FALSE
)


# Change this to register the FORK cluster
registerDoParallel(cl.fork)

# Run this again and look at the output!
replicate(
  n = 2, 
  expr = {
    set.seed(100)
    foreach(i = 1:2, .combine = rbind) %dopar% rnorm(3)
  }, 
  simplify = FALSE
)


# Create a cluster
cl5 <- makeCluster(2)

# Check RNGkind on workers
clusterCall(cl5, RNGkind)

# Set the RNG seed on workers
clusterSetRNGStream(cl5, 100)

# Check RNGkind on workers
clusterCall(cl5, RNGkind)


n_vec <- rep(1000, 5)
cl6 <- makeCluster(2, type = "PSOCK")

mean_of_rnorm <- function(n) {
  # Generate normally distributed random numbers
  random_numbers <- rnorm(n)
  # Calculate the mean of the random numbers
  return(mean(random_numbers))
}

t(replicate(
  # Use 3 replicates
  n = 3,
  expr = {
    # Spread across cl, apply mean_of_rnorm() to n_vec
    clusterApply(cl6, n_vec, mean_of_rnorm)
  }
))


t(replicate(
  n = 3,
  expr = {
    # Set the cluster's RNG stream seed to 1234
    clusterSetRNGStream(cl, iseed = 1234)
    clusterApply(cl6, n_vec, mean_of_rnorm)
  }
))


# Make a cluster of size 2
cl7 <- makeCluster(2)

# Set the cluster's RNG stream seed to 1234
clusterSetRNGStream(cl7, iseed = 1234)

# Spread across the cluster, apply mean_of_rnorm() to n_vec
unlist(clusterApply(cl7, n_vec, mean_of_rnorm))


# Do the same, with 4 nodes
cl8 <- makeCluster(4)
clusterSetRNGStream(cl8, iseed = 1234)
unlist(clusterApply(cl8, n_vec, mean_of_rnorm))

install.packages("doRNG")
library(doRNG)
seed <- 123

ar1_block_of_trajectories <- function(id, rate0 = 0.015, traj_len = 15, block_size = 10) {
  trajectories <- matrix(NA, nrow = block_size, ncol = traj_len)
  for (i in seq_len(block_size)) 
    trajectories[i,] <- ar1_one_trajectory(unlist(ar1est[id, ]), 
                                           rate0 = rate0, len = traj_len)
  trajectories
}

# Register doParallel and doRNG
registerDoParallel(cores = 2)
registerDoRNG(seed)

# Call ar1_block_of_trajectories via foreach
mpar <- foreach(r = 1:5) %dopar% ar1_block_of_trajectories(r)

# Register sequential backend, set seed and run foreach
registerDoSEQ()
set.seed(seed)
mseq <- foreach(r = 1:5) %dorng% ar1_block_of_trajectories(r)

# Check if results identical
identical(mpar, mseq)
install.packages("future.apply")
library(future.apply)
seed2 <- 999

# Set multiprocess plan 
plan(multiprocess, workers = 2)

# Call ar1_block_of_trajectories via future_lapply
mfpar <- future_lapply(1:5, FUN = ar1_block_of_trajectories, future.seed = seed)

# Set sequential plan and repeat future_lapply
plan(sequential)
mfseq <- future_lapply(1:5, FUN = ar1_block_of_trajectories, future.seed = seed)

# Check if results are identical
identical(mfpar, mfseq)
