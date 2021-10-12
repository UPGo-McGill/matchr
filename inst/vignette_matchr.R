#### GENERATE DATA FOR MATCHR VIGNETTE #########################################

all_matches <- match_images(example_urls, compare = FALSE)
all_matches_xy <- match_images(example_urls[1:7], 
                               example_urls[8:15], compare = FALSE)

save(all_matches, all_matches_xy, file = "inst/vignette_matchr.Rdata")
