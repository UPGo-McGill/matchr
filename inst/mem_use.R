library(tidyverse)
library(qs)
library(future)
plan(multisession)
library(progressr)
handlers(global = TRUE)

qload("inst/hash_test_final.qsm", nthreads = 32)
x <- sigs_high
class(x) <- c("matchr_signature", "vctrs_rcrd", "vctrs_vctr")
y <- x
distance <- ~nearest + bilinear
compare_ar = TRUE
stretch = 1.2
mem_scale = 0.2
quiet = FALSE
par_check <- TRUE
output <- ms_prep(x, y, compare_ar, stretch, mem_scale, mem_override)
x <- output[[1]]
y <- output[[2]]
x_na <- output[[3]]
y_na <- output[[4]]
x_list <- output[[5]]
y_list <- output[[6]]
x_sig <- output[[7]]
y_sig <- output[[8]]
rm(output)


# install.packages("memuse")
library(memuse)
memuse::Sys.meminfo()

lengths(x_list)

i <- 2
x_matrix <- matrix(unlist(get_hash(x_list[[i]])), ncol = vec_size(x_list[[i]]))
y_matrix <- matrix(unlist(get_hash(y_list[[i]])), ncol = vec_size(y_list[[i]]))  
len <- nrow(x_matrix) / 2

vec_sizes <- c(100, 200, 500, 750, 1000, 1500, 2000, 2500)
mem_results <- vector("numeric", length(vec_sizes))
i <- 1

for (n in vec_sizes) {
  start <- gc(verbose = FALSE, reset = TRUE)
  x_near <- matrix(x_matrix[seq_len(len),], len)[, 1:n]
  y_near <- matrix(y_matrix[seq_len(len),], len)[, 1:n]
  nearest <- hamming(x_near, y_near)
  x_bi <- matrix(x_matrix[seq_len(len) + len,], len)[, 1:n]
  y_bi <- matrix(y_matrix[seq_len(len) + len,], len)[, 1:n]
  bilinear <- hamming(x_bi, y_bi)
  result <- nearest * bilinear
  end <- gc(verbose = FALSE, reset = FALSE)
  mem_results[[i]] <- end[2,7] - start[2,7]
  i <- i + 1
  }

mem_results

plot(vec_sizes^2, mem_results)

lm(mem ~ vec, data = tibble(vec = vec_sizes ^ 2, mem = mem_results)) |> summary()



# Get memory usage for entire vector --------------------------------------

mem_results_full <- vector("numeric", length(x_list))

result <- vector("list", length(x_list))
for (i in seq_along(x_list)) {
  start <- gc(verbose = FALSE, reset = TRUE)
  result[[i]] <- ms_internal(x_list[[i]], y_list[[i]], distance)
  end <- gc(verbose = FALSE, reset = FALSE)
  mem_results_full[[i]] <- end[2,7] - start[2,7]
}

mem_results_full


# Estimate memory usage ---------------------------------------------------

ram_size <- ceiling(as.numeric(lengths(x_list)) * 
                      as.numeric(lengths(y_list)) * 2.4e-05)
start_mem_2 <- gc(verbose = FALSE, reset = TRUE)
out_2 <- match_signatures(x)
end_mem_2 <- gc(verbose = FALSE, reset = FALSE)
end_mem_2[2,7] - start_mem_2[2,7]
# Seems to be a 20:12 relationship between ram_size and final memory

start_mem_002 <- gc(verbose = FALSE, reset = TRUE)
out_002 <- match_signatures(x, mem_scale = 0.002)
end_mem_002 <- gc(verbose = FALSE, reset = FALSE)
end_mem_002[2,7] - start_mem_002[2,7]
end_mem_002[2,2] - start_mem_002[2,2]
ram_size <- c(450, 8958, 210, 245, 9220, 1309, 2)

start_mem_0002 <- gc(verbose = FALSE, reset = TRUE)
out_0002 <- match_signatures(x, mem_scale = 0.0002)
end_mem_0002 <- gc(verbose = FALSE, reset = FALSE)
end_mem_0002[2,7] - start_mem_0002[2,7]
ram_size <- c(450, 8958, 210, 245, 9220, 1309, 2)




object.size(out_002) / 1024 ^ 2

7415207656 / 1024 ^ 2


sum(map2_dbl(lengths(x_list), lengths(y_list), ~{2.4e-05 * .x * .y}))
sum(mem_results_full)
start_mem_nc <- gc(verbose = FALSE, reset = TRUE)
out_nc <- match_signatures(x, compare_ar = FALSE)
end_mem_nc <- gc(verbose = FALSE, reset = FALSE)
end_mem_nc[2,7] - start_mem_nc[2,7]

length(x) * length(y) * 2.4e-05
sum(lengths(x_list) * lengths(y_list)) * 2.4e-05



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
