install.packages("parallel")
install.packages("tidyverse")
library(tidyverse)
# Load the parallel package
library(parallel)


# Store the number of cores in the object no_of_cores
no_of_cores <- detectCores()

# Print no_of_cores
print(no_of_cores)

total <- no_of_rolls <- 0 # Initialize
while(total < 10) {
  total <- total + sample(1:6, 1)
  
  if(total %% 2 == 0) total <- 0  # If even. Reset to 0
  
  no_of_rolls <- no_of_rolls + 1
}
no_of_rolls


play <- function() {
  total <- no_of_rolls <- 0
  while(total < 10) {
    total <- total + sample(1:6, 1)
    
    # If even. Reset to 0
    if(total %% 2 == 0) total <- 0 
    no_of_rolls <- no_of_rolls + 1
  }
  no_of_rolls
}


results <- numeric(100)
for(i in seq_along(results)) 
  results[i] <- play()

# Create a cluster via makeCluster (2 cores)
cl <- makeCluster(2)

# Export the play() function to the cluster
clusterExport(cl, "play")

# Re-write sapply as parSapply
res <- parSapply(cl, 1:100, function(i) play())

# Stop the cluster
stopCluster(cl)

# Set the number of games to play
no_of_games <- 1e5

## Time serial version
system.time(serial <- sapply(1:no_of_games, function(i) play()))

## Set up cluster
cl <- makeCluster(4)
clusterExport(cl, "play")

## Time parallel version
system.time(par <- parSapply(cl, 1:no_of_games, function(i) play()))

## Stop cluster
stopCluster(cl)
