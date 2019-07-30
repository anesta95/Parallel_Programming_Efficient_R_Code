install.packages("janeaustenr")
install.packages("stringr")
install.packages("ggplot2")
install.packages("parallel")
library(ggplot2)
library(janeaustenr)
library(stringr)
library(parallel)
austen_books <- austen_books()

janeausten_words <- function(x) {
  austen_books$text <- str_to_lower(austen_books$text)
}

max_frequency <- function(letter, words, min_length = 1) {
  w <- select_words(letter, words = words, min_length = min_length)
  frequency <- table(w)    
  frequency[which.max(frequency)]
}

select_words <- function(letter, words, min_length = 1) {
  min_length_words <- words[nchar(words) >= min_length]
  grep(paste0("^", letter), min_length_words, value = TRUE)
}

# Vector of words from all six books
words <- janeausten_words()
View(words)

?lapply

# Most frequent "a"-word that is at least 5 chars long
max_frequency(letter = "a", words = words, min_length = 5)

# Partitioning
result <- lapply(letters, max_frequency,
                 words = words, min_length = 5) %>% unlist()

# barplot of result
barplot(result, las = 2)

# Complete the function definition
mean_of_rnorm <- function(n) {
  # Generate normally distributed random numbers
  random_numbers <- rnorm(n)
  # Calculate the mean of the random numbers
  return(mean(random_numbers))
}

# Try it out
mean_of_rnorm(n = 100)


# From previous step
mean_of_rnorm <- function(n) {
  random_numbers <- rnorm(n)
  mean(random_numbers)
}

n_replicates <- 50
n_numbers_per_replicate <- 10000

# Create a vector to store the results
result <- c(rep(NA, n_replicates))

# Set the random seed to 123
set.seed(123)

# Set up a for loop with iter from 1 to n_replicates
for (iter in seq_len(n_replicates)) {
  # Call mean_of_rnorm with n_numbers_per_replicate
  result[iter] <- mean_of_rnorm(n_numbers_per_replicate)
}

# View the result
hist(result)

# Repeat n_numbers_per_replicate, n_replicates times
n <- c(rep(n_numbers_per_replicate, n_replicates))

# Call mean_of_rnorm() repeatedly using sapply()
result <- sapply(
  # The vectorized argument to pass
  n, 
  # The function to call
  mean_of_rnorm
)

# View the results
hist(result)

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

# Function definition of ar1_multiple_blocks_of_trajectories()
ar1_multiple_blocks_of_trajectories <- function(ids, ...) {
  # Call ar1_block_of_trajectories() for each ids
  trajectories_by_block <- lapply(ids, ar1_block_of_trajectories, ...)
  
  # rbind results
  do.call(rbind, trajectories_by_block)
}

# Create a sequence from 1 to number of blocks
traj_ids <- seq_len(nrow(ar1est))

# Generate trajectories for all rows of the estimation dataset
trajs <- ar1_multiple_blocks_of_trajectories(
  ids = traj_ids, rate0 = 0.015,
  block_size = 10, traj_len = 15
)

# Show results
show_migration(trajs)



# How many physical cores are available?
ncores <- detectCores(logical = FALSE)

# How many random numbers to generate
n <- seq(ncores:1)



# Use lapply to call rnorm for each n,
# setting mean to 10 and sd to 2 
lapply(n, rnorm, mean = 10 , sd = 2)


# Create a cluster
cl <- makeCluster(ncores)

# Use clusterApply to call rnorm for each n in parallel,
# again setting mean to 10 and sd to 2 
clusterApply(cl, n, rnorm, mean = 10, sd = 2)

# Stop the cluster
stopCluster(cl)


# Evaluate partial sums in parallel
part_sums <- clusterApply(cl, x = c(1, 51),
                          fun = function(x) sum(x:(x + 49)))
# Total sum
total <- sum(unlist(part_sums))

# Check for correctness
total == sum(1:100)
stopCluster(cl)


# Create a cluster and set parameters
cl2 <- makeCluster(2)
n_replicates <- 50
n_numbers_per_replicate <- 10000

# Parallel evaluation
means <- clusterApply(cl2, 
                      x = rep(n_numbers_per_replicate, n_replicates), 
                      fun = mean_of_rnorm)

# View results as histogram
hist(unlist(means))
stopCluster(cl2)
