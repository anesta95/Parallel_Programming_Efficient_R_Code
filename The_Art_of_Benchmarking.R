install.packages("microbenchmark")
install.packages("benchmarkme")
library(benchmarkme)
library(microbenchmark)
# Print the R version details using version
print(version)

# Assign the variable major to the major component
major <- version$major

# Assign the variable minor to the minor component
minor <- version$minor


movies.rds <- (readRDS("movies.rds"))
write.csv(movies.rds, "movies.csv")


# How long does it take to read movies from CSV?
system.time(read.csv("movies.csv"))

# How long does it take to read movies from RDS?
system.time(readRDS("movies.rds"))

# Assign the variable ram to the amount of RAM on this machine
ram <- get_ram()
ram

# Assign the variable cpu to the cpu specs
cpu <- get_cpu()
cpu


# Compare the two functions
compare <- microbenchmark(read.csv("movies.csv"), 
                          readRDS("movies.rds"), 
                          times = 10)

# Print compare
print(compare)

# Run the io benchmark
res <- benchmark_io(runs = 1, size = 5)

# Plot the results
plot(res)
