# library(future)
# plan(multiprocess)
# library(future.apply)
#
#
# # Helpers -----------------------------------------------------------------
#
# can_be_merged <- function(x, y) {
#   length(intersect(x, y)) > 0
# }
#
# merge_fun <- function(x, y) {
#   sort(union(x, y))
# }
#
#
# # Get matches to test -----------------------------------------------------
#
# test <- identify_matches(mtl_cl_matrix)
# test <- confirm_matches(test)
#
#
# # Identify x images with correlation ~= 1 ---------------------------------
#
# x_name <- unique(test$x_name)
# x_sig <- identify_image(x_name)
# x_cor <- match_signatures(x_sig)
# x_matches <- identify_matches(x_cor)
# x_matches <- x_matches[x_matches$correlation >= 0.9999 &
#                          x_matches$x_name != x_matches$y_name,]
#
#
# # Group x images together by correlation ----------------------------------
#
# x_list <- mapply(function(x, y) {c(x, y)}, x_matches$x_name, x_matches$y_name,
#                  SIMPLIFY = FALSE)
#
# x_list_unique <-
#   Reduce(function(acc, curr) {
#
#     curr_vec <- curr[[1]]
#
#     to_merge_id_x <- Position(f = function(x) can_be_merged(x, curr_vec), acc)
#
#     if(is.na(to_merge_id_x)) {
#       acc[[length(acc) + 1]] <- curr_vec
#     } else {
#
#       acc[[to_merge_id_x]] <- merge_fun(acc[[to_merge_id_x]], curr_vec)
#     }
#
#     return(acc)
#   }, Map(list, x_list))
#
#
# # Create x table ----------------------------------------------------------
#
# x_table <-
#   lapply(seq_along(x_list_unique), function(n) {
#     tibble(x_id = n, x_name = x_list_unique[[n]])
#   })
#
# x_table <- bind_rows(x_table)
#
#
# # Identify y images with correlation ~= 1 ---------------------------------
#
# y_name <- unique(test$y_name)
# y_sig <- identify_image(y_name)
# y_cor <- match_signatures(y_sig)
# y_matches <- identify_matches(y_cor)
# y_matches <- y_matches[y_matches$correlation >= 0.9999 &
#                          y_matches$x_name != y_matches$y_name,]
#
#
# # Group y images together by correlation ----------------------------------
#
# y_list <- mapply(function(x, y) {c(x, y)}, y_matches$x_name, y_matches$y_name,
#                  SIMPLIFY = FALSE)
#
# y_list_unique <-
#   Reduce(function(acc, curr) {
#
#     curr_vec <- curr[[1]]
#
#     to_merge_id_x <- Position(f = function(x) can_be_merged(x, curr_vec), acc)
#
#     if (is.na(to_merge_id_x)) {
#       acc[[length(acc) + 1]] <- curr_vec
#     } else {
#
#       acc[[to_merge_id_x]] <- merge_fun(acc[[to_merge_id_x]], curr_vec)
#     }
#
#     return(acc)
#   }, Map(list, y_list))
#
#
# # Create y table ----------------------------------------------------------
#
# y_table <-
#   lapply(seq_along(y_list_unique), function(n) {
#     tibble(y_id = n, y_name = y_list_unique[[n]])
#   })
#
# y_table <- bind_rows(y_table)
#
#
# # Join IDs to main table --------------------------------------------------
#
# test_join <-
#   test %>%
#   left_join(x_table) %>%
#   left_join(y_table)
#
# test_unique <-
#   test_join %>%
#   filter(!is.na(x_id) | !is.na(y_id)) %>%
#   distinct(x_id, y_id, .keep_all = TRUE) %>%
#   bind_rows(filter(test_join, is.na(x_id), is.na(y_id)))
#
# compare_images(test_unique)
