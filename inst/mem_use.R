library(tidyverse)
library(qs)
library(future)
plan(multisession)
library(progressr)
handlers(global = TRUE)

x <- qread("/Users/dwachsmuth/Documents/Academic/Code/global-file-import/temp_sigs.qs",
           nthreads = 32)
y <- x
method = "grey"
compare_ar = TRUE
stretch = 1.2
mem_scale = 0.2
quiet = FALSE
par_check <- TRUE

install.packages("memuse")
library(memuse)
memuse::Sys.meminfo()

lengths(x_list)

i <- 5
x_matrix <- matrix(unlist(field(x_list[[i]], "signature")), 
                   ncol = vec_size(x_list[[i]]))
y_matrix <- matrix(unlist(field(y_list[[i]], "signature")), 
                   ncol = vec_size(y_list[[i]]))




vec_size <- c(100, 200, 500, 750, 1000, 1500, 2000, 2500)
mem_results <- vector("numeric", length(vec_size))
i <- 1

for (n in vec_size) {
  start <- gc(verbose = FALSE, reset = TRUE)
  result <- stats::cor(x_matrix[, 1:n], y_matrix[, 1:n])
  end <- gc(verbose = FALSE, reset = FALSE)
  mem_results[[i]] <- end[2,7] - start[2,7]
  i <- i + 1
  }

mem_results

plot(vec_size^2, mem_results)

lm(mem ~ vec, data = tibble(vec = vec_size ^ 2, mem = mem_results))


mem_results


# Estimate memory usage ---------------------------------------------------

map2_dbl(lengths(x_list), lengths(y_list), ~{7.872e-06 * .x * .y})







# Try lists ---------------------------------------------------------------

start <- gc(verbose = FALSE, reset = TRUE)
result <- stats::cor(x_matrix[,seq_len(round(length(x_list[[5]]) * 0.2))], y_matrix)
end <- gc(verbose = FALSE, reset = FALSE)
end[2,7] - start[2,7]

# list 5 * 0.2 used 34.6 GB and didn't pressure memory at all













# Test df -----------------------------------------------------------------

i <- 5

test_results <- tibble(
  list_n = integer(),
  scale = numeric(),
  cores = integer(),
  time_cor = numeric(),
  mem_cor = numeric(),
  time_rbind = numeric(),
  mem_rbind = numeric()
)

test_results %>% 
  add_row(
    list_n = 5,
    scale = 0.2,
    cores = 32,
    time_cor = 1.974207 * 60,
    mem_cor = 34576.9,
    time_rbind = 3.98954 * 60,
    mem_rbind = 34573.8
  ) %>% 
  add_row(
    list_n = 5,
    scale = 0.2,
    cores = 16,
    time_cor = 2.063705 * 60,
    mem_cor = 34583.1,
    time_rbind = 3.466335 * 60,
    mem_rbind = 34572.3
  ) %>% 
  add_row(
    list_n = 5,
    scale = 0.2,
    cores = 1,
    time_cor = 14.40796 * 60,
    mem_cor = 34581.8,
    time_rbind = 0,
    mem_rbind = 0
  ) %>% 
  add_row(
    list_n = 5,
    scale = 0.4,
    cores = 32,
    time_cor = 3.659896 * 60,
    mem_cor = 69143.9,
    time_rbind = 7.557382 * 60,
    mem_rbind = 69143
  ) %>% 
  add_row(
    list_n = 5,
    scale = 0.4,
    cores = 16,
    time_cor = 3.992154 * 60,
    mem_cor = 69144.2,
    time_rbind = 8.255597 * 60,
    mem_rbind = 69143
  ) %>% 
  add_row(
    list_n = 5,
    scale = 0.6,
    cores = 16,
    time_cor = 5.787758 * 60,
    mem_cor = 103715.8,
    time_rbind = 13.40151 * 60,
    mem_rbind = 103715.2
  ) %>% 
  add_row(
    list_n = 5,
    scale = 1,
    cores = 32,
    time_cor = 11.60139 * 60,
    mem_cor = 172858.7,
    time_rbind = 13.40151 * 60,
    mem_rbind = 103715.2
  )


plan(multisession, workers = 32)
x_matrix <- chunk(x_list[[5]][1:round(length(x_list[[5]]) * 1)], n_threads() * 10)
x_matrix <- lapply(x_matrix, function(x) {
  matrix(unlist(field(x, "signature")), ncol = vec_size(x))})
y_matrix <- matrix(unlist(field(y_list[[i]], "signature")), 
                   ncol = vec_size(y_list[[i]]))

start_time <- Sys.time()
start <- gc(verbose = FALSE, reset = TRUE)
result <- suppressWarnings(par_lapply(x_matrix, stats::cor, y_matrix))
end <- gc(verbose = FALSE, reset = FALSE)
Sys.time() - start_time
end[2,7] - start[2,7]

start_time <- Sys.time()
start <- gc(verbose = FALSE, reset = TRUE)
result <- do.call(rbind, result)
end <- gc(verbose = FALSE, reset = FALSE)
Sys.time() - start_time
end[2,7] - start[2,7]



# Single core
plan(sequential)
x_matrix <- matrix(unlist(field(x_list[[i]], "signature")), ncol = vec_size(x_list[[i]]))
y_matrix <- matrix(unlist(field(y_list[[i]], "signature")), ncol = vec_size(y_list[[i]]))

start_time <- Sys.time()
start <- gc(verbose = FALSE, reset = TRUE)
result <- suppressWarnings(stats::cor(x_matrix[,1:round(length(x_list[[5]]) * 0.2)], y_matrix))
end <- gc(verbose = FALSE, reset = FALSE)
Sys.time() - start_time
end[2,7] - start[2,7]


max_mem / 1e6

dim(x_matrix)
lengths(x_list)

# List 8 sent MBP into red memory, with slightly more memory used than total system memory

Sys.meminfo()


rm(result)
gc()
