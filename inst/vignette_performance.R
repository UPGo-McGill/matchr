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


# Load signatures for BLAS comparison -------------------------------------

plan(multisession)
sig_high <- create_signature(paths_high)
sig_low <- create_signature(paths_low)

# Save temp data
qs::qsavem(sig_low, sig_high, file = "inst/temp_vignette_performance.qsm",
           nthreads = 32)


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
  li_bench |> 
  select(-result, -memory) |>
  mutate(expression = as.character(expression)) |> 
  filter(expression == "low" | length < 10000)

li_bench_graph <- 
  li_bench |> 
  select(expression:workers, time, gc) |> 
  rowwise() |> 
  mutate(gc = list(if (nrow(gc != 0)) gc else tibble(
    level0 = rep(0L, 20),
    level1 = level0,
    level2 = level1))) |> 
  ungroup() |> 
  unnest(c(time, gc)) |> 
  select(expression:time)

save(li_bench, li_bench_graph, file = "vignettes/li_bench.Rdata")
# load("vignettes/li_bench.Rdata")


# Benchmark create_signature ----------------------------------------------

# Do this in chunks because it takes too long
withr::with_options(list(matchr.force_parallel = TRUE), {
  cs_bench_1 <- bench::press(
    length = c(1000, 10000), workers = c(16, 32), {
      plan(multisession, workers = workers)
      bench::mark(low = create_signature(paths_low[1:length], backup = FALSE, 
                                         quiet = TRUE),
                  high = create_signature(paths_high[1:length], backup = FALSE, 
                                          quiet = TRUE),
                  iterations = 20, check = FALSE)
      })
  })

qs::qsavem(cs_bench_1, file = "inst/temp_vignette_cs.qsm", nthreads = 32)

withr::with_options(list(matchr.force_parallel = TRUE), {
  cs_bench_2 <- bench::press(
    length = c(1000, 10000), workers = c(4, 8), {
      plan(multisession, workers = workers)
      bench::mark(low = create_signature(paths_low[1:length], backup = FALSE, 
                                         quiet = TRUE),
                  high = create_signature(paths_high[1:length], backup = FALSE, 
                                          quiet = TRUE),
                  iterations = 20, check = FALSE)
    })
})
qs::qsavem(cs_bench_2, cs_bench_1, file = "inst/temp_vignette_cs.qsm", 
           nthreads = 32)

withr::with_options(list(matchr.force_parallel = TRUE), {
  cs_bench_3 <- bench::press(
    length = c(1000, 10000), workers = c(1, 2), {
      if (workers == 1) plan(sequential) else 
        plan(multisession, workers = workers)
      bench::mark(low = create_signature(paths_low[1:length], backup = FALSE, 
                                         quiet = TRUE),
                  high = create_signature(paths_high[1:length], backup = FALSE, 
                                          quiet = TRUE),
                  iterations = 20, check = FALSE)
    })
})

qs::qsavem(cs_bench_3, cs_bench_2, cs_bench_1, 
           file = "inst/temp_vignette_cs.qsm", nthreads = 32)


cs_bench <- 
  bind_rows(
    mutate(cs_bench_3, expression = as.character(expression)),
    mutate(cs_bench_2, expression = as.character(expression)),
    mutate(cs_bench_1, expression = as.character(expression))
    ) |> 
  select(-result, -memory)

cs_bench_graph <- 
  cs_bench |> 
  select(expression:workers, time, gc) |> 
  rowwise() |> 
  mutate(gc = list(if (nrow(gc != 0)) gc else tibble(
    level0 = rep(0L, 20),
    level1 = level0,
    level2 = level1))) |> 
  ungroup() |> 
  unnest(c(time, gc)) |> 
  select(expression:time)

save(cs_bench, cs_bench_graph, file = "vignettes/cs_bench.Rdata")
file.remove("inst/temp_vignette_cs.qsm")
# load("vignettes/cs_bench.Rdata")


# Benchmark match_signatures ----------------------------------------------

qs::qload("inst/temp_vignette_performance.qsm", nthreads = 32)
shs <- sig_high[1:1000]
sls <- sig_low[1:1000]
shm <- sig_high[1:20000]
slm <- sig_low[1:20000]
shl <- rep(sig_high, 2)
sll <- rep(sig_low, 2)

# Need to run this twice, once with vecLib and once with reference BLAS
ms_bench_blas <- bench::press(
  compare = c(TRUE, FALSE), BLAS = TRUE, bench::mark(
    small = match_signatures(shs, sls, compare_ar = compare, quiet = TRUE),
    medium = match_signatures(shm, slm, compare_ar = compare, quiet = TRUE),
    large = match_signatures(shl, sll, compare_ar = compare, quiet = TRUE),
    iterations = 20, check = FALSE))

qs::qsave(ms_bench_blas, file = "inst/temp_ms_blas.qs", nthreads = 32)

ms_bench_no_blas <- bench::press(
  compare = c(TRUE, FALSE), BLAS = FALSE, bench::mark(
    small = match_signatures(shs, sls, compare_ar = compare, quiet = TRUE),
    medium = match_signatures(shm, slm, compare_ar = compare, quiet = TRUE),
    large = match_signatures(shl, sll, compare_ar = compare, quiet = TRUE),
    iterations = 15, check = FALSE))

ms_bench_blas <- qs::qread("inst/temp_ms_blas.qs", nthreads = 32)

ms_bench <- 
  bind_rows(
    mutate(ms_bench_blas, expression = as.character(expression)),
    mutate(ms_bench_no_blas, expression = as.character(expression))) |> 
  select(-result, -memory)
  
ms_bench_graph <- 
  ms_bench |> 
  unnest(c(time, gc)) |> 
  select(expression:time)

save(ms_bench, ms_bench_graph, file = "vignettes/ms_bench.Rdata")
file.remove("inst/temp_ms_blas.qs")
file.remove("inst/temp_vignette_performance.qsm")
# load("vignettes/ms_bench.Rdata")
