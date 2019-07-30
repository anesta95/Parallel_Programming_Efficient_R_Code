install.packages("microbenchmark")
library(microbenchmark)
library(janeaustenr)
library(ggplot2)
# Load the parallel package
library(parallel)

# Make a cluster with 4 nodes
cl <- makeCluster(spec = 4)

# Investigate the structure of cl
str(cl)

# What is the process ID of the workers?
clusterCall(cl, Sys.getpid)

# Stop the cluster
stopCluster(cl)

# A global variable and a print function are defined
a_global_var <- "before"
print_global_var <- function() print(a_global_var)

# Create a socket cluster with 2 nodes
cl_sock <- makeCluster(2, type = "PSOCK")

# Evaluate the print function on each node
clusterCall(cl_sock, print_global_var)

# Stop the cluster
stopCluster(cl_sock)


# Create a fork cluster with 2 nodes
cl_fork <- makeCluster(2, type = "FORK")

# Evaluate the print function on each node
clusterCall(cl_fork, print_global_var)

# Change the global var to "after"
a_global_var <- "after"

# Evaluate the print fun on each node again
clusterCall(cl_fork, print_global_var)

# Stop the cluster
stopCluster(cl_fork)

cl <- makeCluster(2, type = "PSOCK")

mean_of_rnorm <- function(n) {
  # Generate normally distributed random numbers
  random_numbers <- rnorm(n)
  # Calculate the mean of the random numbers
  return(mean(random_numbers))
}

# Wrap this code into a function
mean_of_rnorm_sequentially <- function(n_numbers_per_replicate, n_replicates) {
  n <- rep(n_numbers_per_replicate, n_replicates)
  lapply(n, mean_of_rnorm)
}

# Call it to try it
mean_of_rnorm_sequentially(1000, 5)

# Wrap this code into a function
mean_of_rnorm_in_parallel <- function(n_numbers_per_replicate, n_replicates) {
  n <- rep(n_numbers_per_replicate, n_replicates)
  clusterApply(cl, n, mean_of_rnorm) 
}

# Call it to try it
mean_of_rnorm_in_parallel(1000, 5)

# Set numbers per replicate to 5 million
n_numbers_per_replicate <- 5e6

# Set number of replicates to 4
n_replicates <- 4

# Run a microbenchmark
microbenchmark(
  # Call mean_of_rnorm_sequentially()
  mean_of_rnorm_sequentially(n_numbers_per_replicate, n_replicates), 
  # Call mean_of_rnorm_in_parallel()
  mean_of_rnorm_in_parallel(n_numbers_per_replicate, n_replicates),
  times = 1, 
  unit = "s"
)

# Change the numbers per replicate to 100
n_numbers_per_replicate <- 100

# Change number of replicates to 100
n_replicates <- 100

# Rerun the microbenchmark
microbenchmark(
  mean_of_rnorm_sequentially(n_numbers_per_replicate, n_replicates), 
  mean_of_rnorm_in_parallel(n_numbers_per_replicate, n_replicates),
  times = 1, 
  unit = "s"
)

# Stop the cluster
stopCluster(cl)


cl4 <- makeCluster(4, type = "PSOCK")


# Pre-defined myrdnorm 
myrdnorm <- function(n, mean = 0, sd = 1) {
  rdnorm(n, mean = mean, sd = sd)
}
# Parameters
n_numbers_per_replicate <- 1000
n_replicates <- 20

# Repeat n_numbers_per_replicate, n_replicates times
n <- rep(n_numbers_per_replicate, n_replicates)

# Load extraDistr on master
install.packages("extraDistr")
library(extraDistr)

# Run myrdnorm in parallel. This should fail!
res <- clusterApply(cl4, n, myrdnorm)

# From previous step
myrdnorm <- function(n, mean = 0, sd = 1) 
  rdnorm(n, mean = mean, sd = sd)
n_numbers_per_replicate <- 1000
n_replicates <- 20
n <- rep(n_numbers_per_replicate, n_replicates)

# Load extraDistr on master
library(extraDistr)

# Load extraDistr on all workers
clusterEvalQ(cl, library(extraDistr))

# Run myrdnorm in parallel. It should work now!
res <- clusterApply(cl4, n, myrdnorm)


# Plot the result
plot(table(unlist(res)))


# rdnorm(), but using global variables
myrdnorm2 <- function(n) {
  rdnorm(n, mean = mean, sd = sd)
}

# Set mean to 10, globally
mean <- 10

# Set sd to 5, globally
sd <- 5

# Generate 1000 numbers with myrdnorm()
myrdnorm2(1000)

# From previous step
myrdnorm <- function(n) {
  rdnorm(n, mean = mean, sd = sd)
}

# Set number of numbers to generate
n <- rep(1000, 20)

# Run an expression on each worker
clusterEvalQ(
  cl4, {
    # Load extraDistr
    library(extraDistr)
    # Set mean to 10
    mean <- 10
    # Set sd to 5
    sd <- 5
  })

# Run myrdnorm in parallel
res <- clusterApply(cl4, n, myrdnorm2)

# Plot the results
plot(table(unlist(res)))

# Set global objects on master: mean to 20, sd to 10
mean <- 20
sd <- 10

# Load extraDistr on workers
clusterEvalQ(cl4, library(extraDistr))

# Export global objects to workers
clusterExport(cl4, c("mean", "sd"))

# Run myrdnorm in parallel
res <- clusterApply(cl4, n, myrdnorm2)

# Plot the results
plot(table(unlist(res)))


select_words <- function(words, letter, min_length = 1) {
  min_length_words <- words[nchar(words) >= min_length]
  grep(paste0("^", letter), min_length_words, value = TRUE)
}

janeausten_words <- function(x) {
  austen_books$text <- str_to_lower(austen_books$text)
}

words <- janeausten_words()


# Select words beginning with "v", at least 10 letters long
words_v10 <- select_words(words, "v", min_length = 10)

# Get the unique words
print(unique(words_v10))

# Generate 2 random groups
groups <- sample(2, length(words), replace = TRUE)

# See those groups
head(groups, 20)

# Split words into groups
split_words <- split(words, groups)

# Apply select_words() to each element of split_words in parallel
res <- clusterApply(cl4, split_words, select_words, letter = "v", min_length = 10)

# Flatten the result
words_v10 <- unlist(res)

# Get the unique words
print(unique(words_v10))


ar1_block_of_trajectories <- function(id, rate0 = 0.015, traj_len = 15, block_size = 10) {
  trajectories <- matrix(NA, nrow = block_size, ncol = traj_len)
  for (i in seq_len(block_size)) 
    trajectories[i,] <- ar1_one_trajectory(unlist(ar1est[id, ]), 
                                           rate0 = rate0, len = traj_len)
  trajectories
}

ar1_one_trajectory <- function(est, rate0, len = 15) {
  trajectory <- rep(NA, len)
  rate <- rate0
  for (time in seq_len(len)) {
    trajectory[time] <- ar1_one_value(est, r = rate)
    rate <- trajectory[time]
  }
  trajectory
}

ar1_one_value <- function(est, r) {
  est['mu'] + est['phi'] * (r - est['mu']) + 
    rnorm(1, sd = est['sigma'])
}


show_migration <- function(trajs) {
  df <- data.frame(time = seq(2020, by = 5, len = ncol(trajs)),
                   migration_rate = apply(trajs, 2, median),
                   lower = apply(trajs, 2, quantile, 0.1),
                   upper = apply(trajs, 2, quantile, 0.9)
  )
  g <- ggplot(df, aes(x = time, y = migration_rate)) + 
    geom_ribbon(aes(ymin = lower, ymax = upper), fill = "grey70") + 
    geom_line()
  print(g)
}

ar1est <- read.csv("ar1est.csv")


# Export data and functions
clusterExport(cl4, c("ar1est", "ar1_block_of_trajectories", "ar1_one_trajectory"))

# Process ar1_multiple_blocks_of_trajectories in parallel
res <- clusterApply(cl4, 1:nrow(ar1est), ar1_multiple_blocks_of_trajectories)


# Combine results into a matrix and show results        
trajs <- do.call(rbind, res)
show_migration(trajs)


# Split task into 5 chunks
ind <- splitIndices(nrow(ar1est), 5)

# Process ar1_multiple_blocks_of_trajectories in parallel
res_new <- clusterApply(cl, ind, ar1_multiple_blocks_of_trajectories)

# Compare the structure of the results 
str(res)
str(res_new)


stopCluster(cl4)
