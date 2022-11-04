#### HASH MATCHING #############################################################

# Setup -------------------------------------------------------------------

library(tidyverse)
library(future)
plan(multisession)
library(progressr)
handlers(global = TRUE)

qs::qload("~/Desktop/new_hash.qsm", nthreads = 32)
qs::qload("~/Desktop/hash_match_test.qsm", nthreads = 32)

paths_low <- list.files("/Volumes/Data 2/Scrape photos/vancouver/kj", 
                        full.names = TRUE)
paths_high <- list.files("/Volumes/Data 2/Scrape photos/vancouver/ab", 
                         full.names = TRUE)


# Calculate hashes and get matches ----------------------------------------

x <- create_signature_2(paths_high)
y <- create_signature_2(paths_low)
matches <- match_signatures_2(x, y)
result <- identify_matches_2(matches, threshold = 300)

x_bi <- create_signature_2(get_path(x), resize = "bilinear")
y_bi <- create_signature_2(get_path(y), resize = "bilinear")
matches_bi <- match_signatures_2(x_bi, y_bi)
result_bi <- identify_matches_2(matches_bi, threshold = 18)


# Split results by distance for easier wrangling --------------------------

result_0 <- result |> filter(distance == 0)
result_2 <- result |> filter(distance == 2)
result_4 <- result |> filter(distance == 4)
result_6 <- result |> filter(distance == 6)
result_8 <- result |> filter(distance == 8)
result_10 <- result |> filter(distance == 10)
result_12 <- result |> filter(distance == 12)
result_14 <- result |> filter(distance == 14)
result_16 <- result |> filter(distance %in% 15:16)
result_18 <- result |> filter(distance %in% 17:18)

qs::qsavem(sigs_new_high, sigs_new_low, x_ahash, y_ahash, matches, result, 
           result_0, result_2, result_4, result_6, result_8, result_10, 
           result_12, result_14, result_16, result_18, 
           file = "~/Desktop/hash_match_test.qsm", nthreads = 32)


# Split bilinear results --------------------------------------------------

result_bi_0 <- result_bi |> filter(distance == 0)
result_bi_2 <- result_bi |> filter(distance == 2)
result_bi_4 <- result_bi |> filter(distance == 4)
result_bi_6 <- result_bi |> filter(distance == 6)
result_bi_8 <- result_bi |> filter(distance == 8)
result_bi_10 <- result_bi |> filter(distance == 10)
result_bi_12 <- result_bi |> filter(distance == 12)
result_bi_14 <- result_bi |> filter(distance == 14)
result_bi_16 <- result_bi |> filter(distance %in% 15:16)
result_bi_18 <- result_bi |> filter(distance %in% 17:18)


# Check matches with Shiny app --------------------------------------------

changes_0 <- 
  result_0 |> 
  confirm_matches_2(remove_duplicates = FALSE, 
                    thresholds = c(-1, 8, 12, 15))

changes_2 <- 
  result_2 |> 
  confirm_matches_2(remove_duplicates = FALSE, 
                    thresholds = c(-1, 8, 12, 15))

changes_4 <- 
  result_4 |> 
  confirm_matches_2(remove_duplicates = FALSE, 
                    thresholds = c(-1, 8, 12, 15))

changes_6 <- 
  result_6 |> 
  confirm_matches_2(remove_duplicates = FALSE)

changes_8 <- 
  result_8 |> 
  confirm_matches_2(remove_duplicates = FALSE)

changes_10 <- 
  result_10 |> 
  confirm_matches_2(remove_duplicates = FALSE)

changes_12 <- 
  result_12 |> 
  confirm_matches_2(remove_duplicates = FALSE,
                    thresholds = c(-1, 8, 10, 15))

changes_14_01 <- 
  result_14 |>
  slice(1:2000) |> 
  confirm_matches_2(remove_duplicates = FALSE)

changes_14_02 <- 
  result_14 |> 
  slice(2001:4000) |> 
  confirm_matches_2(remove_duplicates = FALSE)

changes_14_03 <- 
  result_14 |> 
  slice(4001:6000) |> 
  confirm_matches_2(remove_duplicates = FALSE)

changes_14_04 <- 
  result_14 |> 
  slice(6001:8000) |> 
  confirm_matches_2(remove_duplicates = FALSE)

changes_14_05 <- 
  result_14 |> 
  slice(8001:10000) |> 
  confirm_matches_2()

changes_14_06 <- 
  result_14 |> 
  slice(10001:14000) |> 
  confirm_matches_2()

changes_14_07 <- 
  result_14 |> 
  slice(14001:18000) |> 
  confirm_matches_2()


qs::qsavem(x, y, matches, result, result_0, result_2, result_4, result_6, 
           result_8, result_10, result_12, result_14, result_16, result_18, 
           changes_0, changes_2, changes_4, changes_6, changes_8, changes_10, 
           changes_12, changes_14_01, changes_14_02, changes_14_03, 
           changes_14_04, changes_14_05, changes_14_06, test_out, test_out_12,
           file = "~/Desktop/hash_match_test.qsm", nthreads = 32)


# Transport results to bilinear matches -----------------------------------

all_changes <- bind_rows(
  changes_0, changes_2, changes_4, changes_6, changes_8, changes_10, 
  changes_12, changes_14_01, changes_14_02, changes_14_03, 
  changes_14_04, changes_14_05, changes_14_06)

result_bi_0 |> 
  filter()




# Test hash results -------------------------------------------------------

changes_14 <- bind_rows(changes_14_01, changes_14_02, changes_14_03, 
                        changes_14_04, changes_14_05, changes_14_06)

result_to_test <- 
  map2(list(result_0, result_2, result_4, result_6, result_8, result_10, 
            result_12, result_14[1:14000,]),
       list(changes_0, changes_2, changes_4, changes_6, changes_8, changes_10, 
            changes_12, changes_14), ~{
       .x$match <- .y$new_match_status
       .x
     }) |> 
  bind_rows()


# Add different experimental hashes ---------------------------------------

result_to_test <- 
  result_to_test |> 
  mutate(x_sig_bi = create_signature_2(get_path(x_sig), resize = "bilinear"),
         y_sig_bi = create_signature_2(get_path(y_sig), resize = "bilinear"))

result_to_test <- 
  result_to_test |> 
  mutate(dist_bi = match_signatures_2_pairwise(x_sig_bi, y_sig_bi),
         dd_bi = dist_bi * match_signatures_2_pairwise(x_sig_bi, y_sig_bi, hash = "ahash"))

result_to_test |> 
  group_by(distance, match) |> 
  summarize(across(c(dd, dist_bi, dd_bi), mean, na.rm = TRUE), .groups = "drop")

result_to_test |> 
  group_by(dist_bi, match) |> 
  summarize(across(c(dd, distance, dd_bi), mean, na.rm = TRUE), .groups = "drop")

result_to_test |> 
  count(distance, match) |> 
  group_by(distance) |> 
  summarize(match_pct = n[match] / sum(n))

result_to_test |> 
  count(dist_bi, match) |> 
  group_by(dist_bi) |> 
  summarize(match_pct = n[match] / sum(n))


result_to_test |> 
  mutate(dist_prod = distance * dist_a) |> 
  ggplot(aes(distance, dist_a, colour = match)) +
  geom_jitter(width = 0.2) +
  facet_wrap(~match)

result_to_test |> 
  mutate(dist_prod = distance * dist_a) |> 
  # group_by(match) |> 
  # summarize(range = range(dist_prod))
  ggplot(aes(dist_prod, fill = match)) +
  geom_histogram(alpha = 0.5) #+
facet_wrap(~match)

result_to_test |> 
  count(dist_prod, match)

result_to_test |> 
  ggplot(aes(dist_prod, match)) +
  geom_violin()

# Find a good cutoff for dist_prod?
dist_prop <- 
  tibble(
    n = 0:200 * 2,
    n_true = map_int(n, ~{
      result_to_test |> 
        filter(match, dist_prod <= .x) |> 
        nrow()}),
    n_false = map_int(n, ~{
      result_to_test |> 
        filter(!match, dist_prod <= .x) |> 
        nrow()}),
    pct_true = n_true / sum(result_to_test$match),
    pct_false = n_false / sum(result_to_test$match == FALSE)
  )

dist_prop_8 <- 
  tibble(
    n = 0:200 * 2,
    new_true = map_int(n, ~{
      result_to_test |> 
        filter(match, distance >= 8, dist_prod == .x) |> 
        nrow()}),
    new_false = map_int(n, ~{
      result_to_test |> 
        filter(!match, distance >= 8, dist_prod == .x) |> 
        nrow()}),
    n_true = map_int(n, ~{
      result_to_test |> 
        filter(match, distance >= 8, dist_prod <= .x) |> 
        nrow()}),
    n_false = map_int(n, ~{
      result_to_test |> 
        filter(!match, distance >= 8, dist_prod <= .x) |> 
        nrow()}),
    pct_true = n_true / sum(result_to_test[result_to_test$distance >= 8, ,]$match),
    pct_false = n_false / sum(result_to_test[result_to_test$distance >= 8,]$match == FALSE)
  )


dist_prop |> 
  # mutate(new_true = slider::slide_int(n_true, ~{.x[2] - .x[1]}, .before = 1))
  ggplot() +
  geom_line(aes(n, pct_true), colour = "green") +
  geom_line(aes(n, pct_false), colour = "red")

dist_prop_8 |> 
  ggplot() +
  geom_line(aes(n, pct_true), colour = "green") +
  geom_line(aes(n, pct_false), colour = "red")

dist_prop_8 |> 
  ggplot() +
  geom_line(aes(n, n_true), colour = "green") +
  geom_line(aes(n, n_false), colour = "red")

dist_prop_8 |> 
  group_by(tens = round(n, -1)) |> 
  summarize(across(c(new_true, new_false), sum)) |> 
  mutate(pct_true = new_true / (new_true + new_false)) |> 
  # filter(new_true > 0 | new_false > 0) |> 
  ggplot() +
  # geom_line(aes(tens, pct_true))
  geom_point(aes(tens, new_true), colour = "green") +
  geom_point(aes(tens, new_false), colour = "red")

