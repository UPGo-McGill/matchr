### MATCHR TEST SETUP ##########################################################

# Create objects that will be used multiple times -------------------------

# load_image
test_img <- load_image(test_urls[3])
black_image <- 
  new_image(list(array(rep(0.002, 4800), dim = c(40, 40, 3))), "black_image")
test_na <- suppressWarnings(load_image(test_urls[4]))

# create_signature
test_sig <- create_signature(c(test_img, test_img))
test_long_sig <- suppressWarnings(create_signature(test_urls))

# match_signatures
test_match <- match_signatures(test_long_sig)

# identify_matches
test_identify <- identify_matches(test_match, confirm = FALSE)

# confirm_matches
test_confirm <- confirm_matches(test_identify)

# integrate_changes
test_changes <- test_confirm
test_changes[6,]$match <- "no match"
test_changes$new_match_status <- test_changes$match
test_changes$match <- test_changes$x_sig <- test_changes$y_sig <- NULL
test_changes$new_highlight <- FALSE
test_integrate <- integrate_changes(test_confirm, test_changes)
test_confirm_2 <- test_confirm
test_confirm_2$confirmed <- c(TRUE, FALSE, FALSE, FALSE, FALSE, FALSE)
test_changes_2 <- test_changes[2:6,]
test_integrate_2 <- integrate_changes(test_confirm_2, test_changes_2)


# Data to be loaded for tests ---------------------------------------------

load("test-get_clusters.Rdata")
