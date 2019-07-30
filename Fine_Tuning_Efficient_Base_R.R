library(microbenchmark)
#First rule of R club: Never, ever grow a vector

growing <- function(n) {
  x <- NULL
  for(i in 1:n)
    x <- c(x, rnorm(1))
  x
}
#Preallocate them instead
system.time(res_grow <- growing(30000))
n <- 30000
pre_allocate <- function(n) {
  x <- numeric(n) # Pre-allocate
  for(i in 1:n) 
    x[i] <- rnorm(1)
  x
}
system.time(res_allocate <- pre_allocate(n))

#Second rule of R club: use a vectorized solution wherever possible

x <- rnorm(10)
#Instead of:
x2 <- numeric(length(x))
for(i in 1:10)
  x2[i] <- x[i] * x[i]

#Use:
x2_imp <- x^2
#or: 
x2_imp2 <- x * x

#Vectorized code: calculating a log-sum
total <- 0
for(i in seq_along(x))
  total <- total + log(x[i])

#Significantly improved by:
?runif

# Initial code
n <- 100
total <- 0
x <- runif(n)
for(i in 1:n) 
  total <- total + log(x[i])

# Rewrite in a single line. Store the result in log_sum
log_sum <- sum(log(x))

#The third rule of R club: Use a matrix (not a data frame) whenever appropriate (whenever
#you have all data of the same type)

# Which is faster, mat[, 1] or df[, 1]? 
microbenchmark(mat[, 1], df[, 1])


# Which is faster, mat[1, ] or df[1, ]? 
microbenchmark(mat[1,], df[1,])

