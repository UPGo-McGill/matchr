### MATCHR TEST SETUP ##########################################################

# Create objects that will be used multiple times -------------------------

# test_urls
test_urls <- 
  paste0(test_path("resources"), "/", list.files(test_path("resources")))
test_urls <- test_urls[1:15]

# load_image
test_img <- load_image("https://upgo.lab.mcgill.ca/resources/img_1_small")
test_long_img <- suppressWarnings(load_image(test_urls))
black_image <- 
  new_image(list(array(rep(0.002, 4800), dim = c(40, 40, 3))), "black_image")
test_na <- suppressWarnings(load_image(test_urls[4]))

# create_signature
test_sig <- create_signature(c(test_img, test_img))
test_long_sig <- suppressWarnings(create_signature(test_long_img))

# match_signatures
test_match <- match_signatures(test_long_sig)

# identify_matches
test_identify <- identify_matches(test_match, confirm = FALSE)

# integrate_changes
test_changes <- test_identify["index"]
test_changes$new_match_status <- NA
test_changes$new_highlight <- NA
test_changes[1:3,]$new_match_status <- TRUE
test_changes[1:3,]$new_highlight <- FALSE
test_integrate <- integrate_changes(test_identify, test_changes)
test_changes_2 <- test_changes
test_changes_2[4,]$new_match_status <- FALSE
test_changes_2[4,]$new_highlight <- TRUE
test_integrate_2 <- integrate_changes(test_integrate, test_changes_2)


# Data to be loaded for tests ---------------------------------------------

load(test_path("test-get_clusters.Rdata"))
