#### BENCHMARKING ##############################################################

library(dplyr)
library(tidyr)
library(ggplot2)
library(future)
library(progressr)
handlers(global = TRUE)


# Load data ---------------------------------------------------------------

paths_low <- list.files("/Volumes/Data 2/Scrape photos/vancouver/kj", 
                        full.names = TRUE)

paths_high <- list.files("/Volumes/Data 2/Scrape photos/vancouver/ab", 
                         full.names = TRUE)


# Benchmark load_image ----------------------------------------------------

withr::with_options(list(matchr.force_parallel = TRUE), {
  li_bench <- bench::press(
    length = c(100, 1000, 10000),
    workers = c(1, 2, 4, 8, 16, 32),
    {
      if (workers == 1) plan(sequential) else plan(multisession, 
                                                   workers = workers)
      bench::mark(low = load_image(paths_low[1:length], quiet = TRUE),
                  high = {if (length <= 1000) load_image(paths_high[1:length], 
                                                         quiet = TRUE)},
                  iterations = 20, check = FALSE)
      })
  })

li_bench <- 
  li_bench %>% 
  select(-result, -memory) %>% 
  mutate(expression = as.character(expression))

save(li_bench, file = "inst/li_bench.Rdata")


# Benchmark create_signature ----------------------------------------------

withr::with_options(list(matchr.force_parallel = TRUE), {
  cs_bench <- bench::press(
    length = c(1000, 10000),
    workers = c(1, 2, 4, 8, 16, 32),
    {
      if (workers == 1) plan(sequential) else 
        plan(multisession, workers = workers)
      bench::mark(low = create_signature(paths_low[1:length], backup = FALSE, 
                                         quiet = TRUE),
                  high = create_signature(paths_high[1:length], backup = FALSE, 
                                          quiet = TRUE),
                  iterations = 20, check = FALSE)
      })
  })
    
cs_bench <- 
  cs_bench %>% 
  select(-result, -memory) %>% 
  mutate(expression = as.character(expression))

save(cs_bench, file = "inst/cs_bench.Rdata")


# Benchmark match_signatures ----------------------------------------------

sig_high <- create_signature(paths_high[1:30000])
sig_low <- create_signature(paths_low[1:20000])

withr::with_options(list(matchr.force_parallel = TRUE), {
  ms_bench <- bench::press(
    workers = c("1", "2", "4", "8", "32", "BLAS"),
    compare = c(TRUE, FALSE), {
      if (workers != "BLAS") {
        if (workers == "1") plan(sequential) else 
          plan(multisession, workers = as.numeric(workers))
        BLAS_opt <- options(matchr.blas = FALSE)
      }
      
      res <- bench::mark(
        small = match_signatures(sig_high[1:1000], sig_low[1:1000], 
                                 compare_ar = compare, quiet = TRUE),
        medium = match_signatures(sig_high, sig_low, 
                                  compare_ar = compare, quiet = TRUE),
        large = match_signatures(rep(sig_high, 3), rep(sig_low, 3),
                                 compare_ar = compare, quiet = TRUE),
        iterations = 20, check = FALSE)
      
      if (workers != "BLAS") options(BLAS_opt)
      res
    })
})

ms_bench <-
  ms_bench %>% 
  select(-result, -memory) %>% 
  mutate(expression = as.character(expression))

save(ms_bench, file = "inst/ms_bench.Rdata")
