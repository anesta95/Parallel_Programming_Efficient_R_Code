install.packages("foreach")
library(foreach)
library(janeaustenr)
set.seed(1)
n.seq <- sample(1:1000, 10)
res <- foreach(n = n.seq, .combine = c) %do% rnorm(n)
length(res)

max_frequency <- function(letter, words, min_length = 1) {
  w <- select_words(letter, words = words, min_length = min_length)
  frequency <- table(w)    
  frequency[which.max(frequency)]
}

janeausten_words <- function(x) {
  austen_books$text <- str_to_lower(austen_books$text)
}

words <- janeausten_words()

# foreach() %do% construct
result <- foreach(let = letters, .combine = c) %do% 
  max_frequency(let, words, min_length = 5)

# Plot results 
barplot(result, las = 2)

# foreach()%do% construct with 2 iterators
result2 <- foreach(let = letters, n = c(rep(2, 13), rep(6, 13)), .combine = c) %do%
  max_frequency(let, words, min_length = n)

# Plot results
barplot(result2, las = 2)

myrdnorm <- function(n, mean = 0, sd = 1) {
  rdnorm(n, mean = mean, sd = sd)
}

install.packages("doParallel")
library(doParallel)
library(extraDistr)
library(stringr)
# Register doParallel with 3 cores
registerDoParallel(cores = 3)

# foreach()%dopar% loop
res3 <- foreach(r = rep(1000, 100), .combine = rbind, 
               .packages = "extraDistr") %do% myrdnorm(r)

# Dimensions of res
dim_res <- dim(res3)


freq_seq <- myrdnorm <- function(n, mean = 0, sd = 1) {
  rdnorm(n, mean = mean, sd = sd)
}
chars <- letters


select_words <- function(letter, words, min_length = 1) {
  min_length_words <- words[nchar(words) >= min_length]
  grep(paste0("^", letter), min_length_words, value = TRUE)
}

# Function for doParallel foreach
freq_doPar <- function(cores, min_length = 5) {
  # Register a cluster of size cores
  registerDoParallel(cores = cores)
  
  # foreach loop
  foreach(let = chars, .combine = c, 
          .export = c("max_frequency", "select_words", "words"),
          .packages = c("janeaustenr", "stringr")) %dopar%
    max_frequency(let, words = words, min_length = min_length)
}

# Run on 2 cores
freq_doPar(2)
install.packages("doFuture")
library(doFuture)

# Function for doFuture foreach
freq_doFut <- function(cores, min_length = 5) {
  # Register and set plan
  registerDoFuture()
  plan(cluster, workers = cores)
  
  # foreach loop
  foreach(let = chars, .combine = c) %dopar% 
    max_frequency(let, words = words, min_length = min_length)
}
library(microbenchmark)
min_length <- 6
cores <- 2

# Benchmark
microbenchmark(freq_seq(min_length), 
               freq_doPar(cores, min_length), 
               freq_doFut(cores, min_length),
               times = 1)

extract_words_from_text <- function(text) {
  str_extract_all(text, boundary("word")) %>% unlist %>% tolower
}

# Main function
freq_fapply <- function(words, chars = letters, min_length = 5) {
  unlist(
    future_lapply(chars, max_frequency, words = words, 
                  min_length = min_length)
  )
}

# Extract words
words <- extract_words_from_text(obama_speech)

# Call the main function
res <- freq_fapply(words)

# Plot results
barplot(res, las = 2)

# multicore function
fapply_mc <- function(cores = 2, ...) {
  # future plan
  plan(multicore, workers = cores)
  freq_fapply(words, chars, ...)
}

# cluster function
fapply_cl <- function(cores = NULL, ...) {
  # default value for cores
  if(is.null(cores)){
    cores <- rep(c("oisin", "oscar"), each = 16)
  }
  # future plan
  plan(cluster, workers = cores)
  freq_fapply(words, chars, ...)
}

# Microbenchmark
microbenchmark(fapply_seq = fapply_seq(),
               fapply_mc_2 = fapply_mc(cores = 2), 
               fapply_mc_10 = fapply_mc(cores = 10),
               fapply_cl = fapply_cl(cores = 2), 
               times = 1)

# Benchmark clusterApply and clusterApplyLB
microbenchmark(
  clusterApply(cl, tasktime, Sys.sleep),
  clusterApplyLB(cl, tasktime, Sys.sleep),
  times = 1
)

# Plot cluster usage
plot_cluster_apply(cl, tasktime, Sys.sleep)
plot_cluster_applyLB(cl, tasktime, Sys.sleep)


# Plot cluster usage for parSapply
plot_parSapply(cl, tasktime, Sys.sleep)

# Microbenchmark
microbenchmark(
  clusterApplyLB(cl, bias_tasktime, Sys.sleep),
  parSapply(cl, bias_tasktime, Sys.sleep),
  times = 1
)

# Plot cluster usage for parSapply and clusterApplyLB
plot_parSapply(cl, bias_tasktime, Sys.sleep)
plot_cluster_applyLB(cl, bias_tasktime, Sys.sleep)