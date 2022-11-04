#### Hashing tests #############################################################

# Load packages -----------------------------------------------------------

library(matchr)
library(future)
plan(multisession)
progressr::handlers(global = TRUE)


# Load data ---------------------------------------------------------------

paths_low <- list.files("/Volumes/Data 2/Scrape photos/vancouver/kj", 
                        full.names = TRUE)
paths_high <- list.files("/Volumes/Data 2/Scrape photos/vancouver/ab", 
                         full.names = TRUE)

sigs_old_low <- create_signature(paths_low)
sigs_old_high <- create_signature(paths_high)
sigs_new_low <- create_signature_2(paths_low)
sigs_new_high <- create_signature_2(paths_high)

matches <- match_signatures(sigs_old_low, sigs_old_high)
matches <- identify_matches(matches)
confirmed <- compare_images(matches, remove_duplicates = FALSE, corr_thresh = 1)
final_matches <- integrate_changes(matches, confirmed)


# Make convenience functions ----------------------------------------------

hasher <- function(x, hash_fun = phash, MODE = "hash") {
  out <- readImage(x)
  out <- rgb_2gray(out)
  out <- hash_fun(out, MODE = MODE)
  out <- as.vector(out)
  out
}

hash_vec <- function(x, hash_fun = phash, MODE = "hash") {
  out <- lapply(x, hasher, hash_fun = hash_fun, MODE = MODE)
  names(out) <- x
  out
}

hamming <- function(x, y) {
  diff <- x - y
  out <- sum(diff != 0)
  out
}

hamming_vec <- function(x, y) {
  mapply(hamming, x, y, SIMPLIFY = TRUE, USE.NAMES = FALSE)
}


# Get high correlation candidates -----------------------------------------

paths_1 <- get_path(final_matches$x_sig)
paths_2 <- get_path(final_matches$y_sig)


# Get different hashes ----------------------------------------------------

ahash_1 <- hash_vec(paths_1[-c(10, 168, 172, 215, 322:324, 745, 972)], average_hash, "binary")
ahash_2 <- hash_vec(paths_2[-c(10, 168, 172, 215, 322:324, 745, 972)], average_hash, "binary")
phash_1 <- hash_vec(paths_1[-c(10, 168, 172, 215, 322:324, 745, 972)], phash, "binary")
phash_2 <- hash_vec(paths_2[-c(10, 168, 172, 215, 322:324, 745, 972)], phash, "binary")
# dhash_1 <- hash_vec(paths_1[-c(10, 168, 172, 215, 322:324, 745, 972)], dhash, "binary")
# dhash_2 <- hash_vec(paths_2[-c(10, 168, 172, 190, 215, 322:324, 745, 972)][1:200], dhash, "binary")


# Compare -----------------------------------------------------------------

compare_a <- hamming_vec(ahash_1, ahash_2)
compare_p <- hamming_vec(phash_1, phash_2)
# compare_d <- hamming_vec(dhash_1, dhash_2)


# Integrate results -------------------------------------------------------

results <- 
  final_matches[-c(10, 168, 172, 215, 322:324, 745, 972),] |> 
  select(x_sig, y_sig, correlation, match) |> 
  mutate(ahash = compare_a,
         phash = compare_p)

results |> 
  count(match)

results |> 
  filter(phash <= 16)

results |> 
  filter(match == "match") |> 
  slice(5) |> 
  pull(y_sig) |> 
  get_path() |> 
  load_image() |> 
  plot()

results |> 
  group_by(match) |> 
  summarize(across(c(ahash, phash), 
                   .fns = list(min = min, max = max, mean = mean)))

#' Results:
#' ahash is all over the map, phash seems to have zero overlap with a large gap 
#' (10-20), and dhash has a small overlap between 17-19


# Benchmark hashes --------------------------------------------------------

#' Including load time, phash is 2x for small images, and ~ same for large, 
#' while matchr does well for small and far better for large
bench::mark(
  ahash_1 = hash_vec(paths_1, average_hash, "binary"),
  ahash_2 = hash_vec(paths_2, average_hash, "binary"),
  phash_1 = hash_vec(paths_1, phash, "binary"),
  phash_2 = hash_vec(paths_2, phash, "binary"),
  dhash_1 = hash_vec(paths_1, dhash, "binary"),
  dhash_2 = hash_vec(paths_2, dhash, "binary"),
  matchr_1 = create_signature(paths_1, quiet = TRUE),
  matchr_2 = create_signature(paths_2, quiet = TRUE),
  check = FALSE
)

# phash far slower with images already loaded, but far faster than create_signature
imgs_1 <- lapply(paths_1, readImage)
imgs_1 <- lapply(imgs_1, rgb_2gray)
imgs_2 <- lapply(paths_2, readImage)
imgs_2 <- lapply(imgs_2, rgb_2gray)
matchr_imgs_1 <- load_image(paths_1)
matchr_imgs_2 <- load_image(paths_2)

bench::mark(
  ahash_1 = lapply(imgs_1, average_hash, MODE = "binary"),
  ahash_2 = lapply(imgs_2, average_hash, MODE = "binary"),
  phash_1 = lapply(imgs_1, phash, MODE = "binary"),
  phash_2 = lapply(imgs_2, phash, MODE = "binary"),
  dhash_1 = lapply(imgs_1, dhash, MODE = "binary"),
  dhash_2 = lapply(imgs_2, dhash, MODE = "binary"),
  matchr_1 = create_signature(matchr_imgs_1, quiet = TRUE),
  matchr_2 = create_signature(matchr_imgs_2, quiet = TRUE),
  check = FALSE
)


# Run hashes on matchr_image data structure -------------------------------

matchr_imgs_1 |> 
  get_array() |> 
  pluck(1) |> 
  rgb_2gray() |> 
  phash()

test_img <- 
  matchr_imgs_1 |> 
  get_array() |> 
  pluck(1)

bench::mark(
# test_mean_1 = apply(test_img, c(1, 2), mean),
test_mean_2 = rgb_2gray(test_img),
test_mean_3 = rowMeans(test_img, dims = 2),
check = FALSE)



# Simple create_signature replacement -------------------------------------

create_signature_new <- function(img) {
  
  arrays <- get_array(img)
  
  hashes <- lapply(arrays, \(x) {
    x |> 
      rowMeans(dims = 2) |> 
      phash(MODE = "binary") |> 
      as.vector()
  })
  
  ar <- lapply(arrays, dim)
  ar <- sapply(ar, \(x)  ifelse(length(x) == 2, x[[2]][2] / x[[2]][1], 
                                NA_real_))
  new_rcrd(
    fields = list(hash = hashes, path = get_path(img), ar = ar),
    class = "matchr_signature_new"
  )
}

format.matchr_signature_new <- function(x, ...) {
  hashes <- field(x, "hash")
  hashes <- sapply(hashes, \(x) {
    string <- vector("character", 4)
    for (i in 1:4) {
      string[[i]] <-
        x[((i - 1) * 16 + 1):(i * 16)] |> 
        paste0(collapse = "") |> 
        strtoi(2) |> 
        as.hexmode() |> 
        as.character()
    }
    string <- paste0(string, collapse = "")
    string <- paste0(rep("0", 16 - nchar(string)), string)
  })
  hashes
}

new_sig_1 <- create_signature_new(matchr_imgs_1)
new_sig_2 <- create_signature_new(matchr_imgs_2)

bench::mark(
  old_1 = create_signature(matchr_imgs_1, quiet = TRUE, rm_black_bars = TRUE),
  old_2 = create_signature(matchr_imgs_2, quiet = TRUE, rm_black_bars = TRUE),
  new_1 = create_signature_new(matchr_imgs_1),
  new_2 = create_signature_new(matchr_imgs_2),
  check = FALSE
)


# Evaluate correlation options --------------------------------------------

hamming_binary <- function(x, y = NULL) {
  x_matrix <- matrix(unlist(field(x, "hash")), ncol = vec_size(x))
  
  if (is.null(y)) {
    D <- t(1 - x_matrix) %*% x_matrix
    D + t(D)
  } else {
    y_matrix <- matrix(unlist(field(y, "hash")), ncol = vec_size(y))
    t(1 - x_matrix) %*% y_matrix + t(x_matrix) %*% (1 - y_matrix)
  }
}

x_matrix <- matrix(unlist(field(new_sig_1, "hash")), ncol = vec_size(new_sig_1))
y_matrix <- matrix(unlist(field(new_sig_2, "hash")), ncol = vec_size(new_sig_2))

test_matrix_1 <- matchr:::fast_cor(x_matrix, y_matrix)
test_matrix_2 <- cor(x_matrix, y_matrix)
test_matrix_3 <- 
  lapply(field(new_sig_1, "hash"), \(x) {
    sapply(field(new_sig_2, "hash"), \(y) hamming(x, y))
  })
test_matrix_4 <- hamming_binary(x_matrix, y_matrix)

bench::mark(
  fast = matchr:::fast_cor(x_matrix, y_matrix),
  base = cor(x_matrix, y_matrix),
  hamming = lapply(field(new_sig_1, "hash"), \(x) {
      sapply(field(new_sig_2, "hash"), \(y) hamming(x, y))
    }),
  hamming_binary = hamming_binary(new_sig_1, new_sig_2),
  check = FALSE
)

which(test_matrix_1 > 0.9)
which(test_matrix_2 > 0.9)
which(test_matrix_4 < 4, arr.ind = TRUE)


# Compare Hamming results with old match_signatures -----------------------

x_img <- 
  results |> 
  pull(x_sig) |> 
  unique() |> 
  get_path() |> 
  load_image()

y_img <- 
  results |> 
  pull(y_sig) |> 
  unique() |> 
  get_path() |> 
  load_image()

x_sig <- create_signature(x_img)
y_sig <- create_signature(y_img)
x_sig_2 <- create_signature_new(x_img)
y_sig_2 <- create_signature_new(y_img)

match_old <- match_signatures(x_sig, y_sig)
match_old <- identify_matches(match_old)

x_matrix <- matrix(unlist(field(x_sig_2, "hash")), ncol = vec_size(x_sig_2))
y_matrix <- matrix(unlist(field(y_sig_2, "hash")), ncol = vec_size(y_sig_2))
match <- hamming_binary(x_matrix, y_matrix)
match_index <- which(match <= 17, arr.ind = TRUE)
dimnames(match_index)[[2]] <- c("x_index", "y_index")
match_new <- dplyr::as_tibble(match_index)
match_new <- 
  match_new |> 
  mutate(matrix = 1,
         x_sig = x_sig[x_index],
         y_sig = y_sig[y_index],
         dist = match[match_index],
         correlation = 0.9,
         match = case_when(
           dist <= 10 ~ "match",
           dist <= 13 ~ "likely match",
           dist <= 15 ~ "possible match",
           TRUE ~ "no match"
         ))

confirm_old <- compare_images(match_old, remove_duplicates = FALSE, corr_thresh = 1)
confirm_new <- compare_images(match_new, remove_duplicates = FALSE)

result_old <- 
  integrate_changes(match_old, confirm_old) |> 
  mutate(pair_id = paste(get_path(x_sig), get_path(y_sig))) |> 
  filter(match == "match")

result_new <- 
  integrate_changes(match_new, confirm_new) |> 
  mutate(pair_id = paste(get_path(x_sig), get_path(y_sig))) |> 
  filter(match == "match")

extra_x <- 
  result_old |> 
  anti_join(result_new, by = "pair_id") |> 
  pull(x_sig) |> 
  get_path() |> 
  load_image()

extra_y <- 
  result_old |> 
  anti_join(result_new, by = "pair_id") |> 
  pull(y_sig) |> 
  get_path() |> 
  load_image()

hamming_binary(create_signature_new(extra_x), create_signature_new(extra_y))


result_new |> anti_join(result_old, by = "pair_id")

integrate_changes(match_new, confirm_new) |> 
  group_by(match) |> 
  summarize(min = min(dist), max = max(dist), mean = mean(dist))

#' Preliminary findings:
#' All distances 11 or under are matches
#' All distances 19 or over are not matches
#' 12-18 is ambiguous zone, but only one match above 16

