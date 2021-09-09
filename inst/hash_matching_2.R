#### HASH MATCHING 2 ###########################################################

result_old_to_join <- 
  result_to_test |> 
  mutate(x_name = get_path(x_sig),
         y_name = get_path(y_sig)) |> 
  select(x_name, y_name, match, distance, dd)
  
result_test_new <- 
  result_bi |> 
  arrange(distance) |> 
  mutate(x_name = get_path(x_sig),
         y_name = get_path(y_sig)) |> 
  left_join(result_old_to_join, by = c("x_name", "y_name"))
  
result_test_new |> 
  group_by(distance.x) |> 
  summarize(mean(distance.y, na.rm = TRUE))

result_test_new |> 
  filter(!is.na(match)) |> 
  count(distance.x, match)

test_out <- 
  result_test_new |> 
  filter(is.na(match)) |> 
  filter(distance.x <= 10) |> 
  select(index, x_sig, y_sig, distance = distance.x, dd = dd.x) |> 
  confirm_matches_2(remove_duplicates = FALSE)

result_test_new |> 
  filter(distance.x <= 10) |> 
  left_join(test_out, by = "index") |> 
  mutate(match = coalesce(new_match_status, match)) |> 
  count(distance.x, match) |> 
  group_by(distance.x) |> 
  summarize(pct = n[match] / sum(n))


result_test_new |> 
  filter(distance.x == 10) |> 
  left_join(test_out, by = "index") |> 
  mutate(match = coalesce(new_match_status, match)) |> 
  select(index, x_sig, y_sig, distance = distance.x, dd = dd.x) |> 
  confirm_matches_2(remove_duplicates = FALSE)

test_out_12 <- 
  result_test_new |> 
  filter(distance.x == 12) |> 
  select(index, x_sig, y_sig, distance = distance.x, dd = dd.x) |> 
  confirm_matches_2()

result_test_new <- 
  result_test_new |> 
  filter(distance.x <= 12 | distance.y <= 12) |> 
  left_join(test_out, by = "index") |> 
  mutate(match = coalesce(new_match_status, match)) |> 
  select(-new_match_status, -new_highlight) |> 
  left_join(test_out_12, by = "index") |> 
  mutate(match = coalesce(new_match_status, match)) |> 
  select(-new_match_status, -new_highlight)

result_test_new <- 
  result_test_new |> 
  select(-x_name, -y_name) |> 
  relocate(match, .after = y_sig) |> 
  rename(dist_bi = distance.x, dd_bi = dd.x, dist_nn = distance.y, dd_nn = dd.y)

result_test_new |> 
  filter(is.na(dist_nn)) |> 
  left_join(result, by = "index") |> 
  select(index, match:dd_nn, distance, dd)

result_test_new <- 
  result_test_new |> 
  rowwise() |> 
  mutate(dist_nn = if_else(is.na(dist_nn), 
                           get_array(matches)[[index[[1]]]][index[[2]], index[[3]]],
                           dist_nn)) |> 
  ungroup() |> 
  mutate(dd_nn = coalesce(dd_nn, dist_nn * match_signatures_2_pairwise(x_sig, y_sig, "ahash")))

result_test_new <- 
  result_test_new |> 
  mutate(dd_both = dist_bi * dist_nn,
         dd_new = dist_bi + dist_nn)

result_test_new |> 
  count(dd_both, match) |> 
  group_by(tens = round(dd_both, -1)) |>
  summarize(match_n = sum(n[match]),
            no_match_n = sum(n[!match]),
            match_pct = sum(n[match]) / sum(n)) |> 
  as.data.frame()

#' dd_both < 120 is almost entirely matches, 
#' dd_both >= 120 & < 170 has a reasonable number of matches

result_test_new |> 
  count(dd_nn, match) |> 
  group_by(tens = round(dd_nn, -1)) |>
  summarize(match_n = sum(n[match]),
            no_match_n = sum(n[!match]),
            match_pct = sum(n[match]) / sum(n)) |> 
  as.data.frame()

#' dd_nn < 70 is almost entirely matches,
#' dd_nn >= 70 & < 130 has a reasonable number of matches

nn_1 <- 70
nn_2 <- 130
both_1 <- 120
both_2 <- 150
new_1 <- 20
new_2 <- 25

result_test_new |>
  summarize(
    method = c("nn", "both", "new"),
    match_1 = c(
      sum(match[dd_nn < nn_1]), 
      sum(match[dd_both < both_1]), 
      sum(match[dd_new < new_1])),
    no_match_1 = c(
      sum(!match[dd_nn < nn_1]), 
      sum(!match[dd_both < both_1]), 
      sum(!match[dd_new < new_1])),
    pct_1 = c(
      match_1[1] / sum(dd_nn < nn_1), 
      match_1[2] / sum(dd_both < both_1), 
      match_1[3] / sum(dd_new < new_1)),
    match_2 = c(
      sum(match[dd_nn >= nn_1 & dd_nn < nn_2]), 
      sum(match[dd_both >= both_1 & dd_both < both_2]), 
      sum(match[dd_new >= new_1 & dd_new < new_2])),
    no_match_2 = c(
      sum(!match[dd_nn >= nn_1 & dd_nn < nn_2]), 
      sum(!match[dd_both >= both_1 & dd_both < both_2]),
      sum(!match[dd_new >= new_1 & dd_new < new_2])),
    pct_2 = c(
      match_2[1] / sum(dd_nn >= nn_1 & dd_nn < nn_2), 
      match_2[2] / sum(dd_both >= both_1 & dd_both < both_2),
      match_2[3] / sum(dd_new >= new_1 & dd_new < new_2)),
    missing = c(
      sum(match[dd_nn >= nn_2]), 
      sum(match[dd_both >= both_2]),
      sum(match[dd_new >= new_2])))

result_test_new |> 
  summarize(max_nn = max(dd_nn[match]),
            max_both = max(dd_both[match]),
            max_new = max(dd_new[match]))

result_test_new |> 
  summarize(false_nn = sum(!match[dd_nn <= 520]),
            false_both = sum(!match[dd_both <= 200]),
            false_new = sum(!match[dd_new <= 30]))

result_test_new |> 
  filter(dd_both <= 200) |> 
  count(match)

result_test_new |> 
  summarize(max_nn = max(dist_nn[match]))

#' In summary:
#' All matches are found with dd_both <= 200, with 48.5% false positives,
#' 96% of matches are found with dd_both < 120, with only 1% false positives

result_test_new |> 
  count(dist_nn, match, dd_1 = dd_both < 120, dd_2 = dd_both < 170) |> 
  group_by(dist_nn, dd_1, dd_2) |> 
  summarize(match_n = sum(n[match]),
            no_match_n = sum(n[!match]),
            match_pct = sum(n[match]) / sum(n)) |> 
  as.data.frame()
  
#' Final algorithm:
#' dist_nn <= 6, 

result_test_new |> 
  ggplot(aes(dist_nn, dd_both, colour = match)) +
  geom_jitter()

result_test_new |> 
  ggplot(aes(dist_nn, dd_nn, colour = match)) +
  geom_jitter()

result_test_new |> 
  filter(dd_both < 70) |> 
  count(dist_nn, dd_both) |> 
  as.data.frame()


result_test_new |> 
  count(distance.y, match)