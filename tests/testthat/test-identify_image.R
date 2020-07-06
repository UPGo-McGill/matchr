#### TESTS FOR identify_image ##################################################

### Setup ######################################################################

context("identify_image tests")


### Tests ######################################################################

# test_that("a single cimg works", {
#   expect_equal(
#     ceiling(sum(identify_image(load_image(
#       "https://upgo.lab.mcgill.ca/img/UPGo_logo.png")))), 24)
#   })
#
# test_that("a list of cimg objects works", {
#   expect_equal(
#     ceiling(
#       sum(
#         unlist(
#           identify_image(
#             load_image(
#               c("https://upgo.lab.mcgill.ca/img/UPGo_logo.png",
#                 "https://upgo.lab.mcgill.ca/img/mcgill.png"
#               )))))), 34)
#   })
#
# test_that("a single path works", {
#   expect_equal(
#     ceiling(
#       sum(
#         unlist(
#           identify_image(
#             "https://upgo.lab.mcgill.ca/img/UPGo_logo.png")))), 24)
#   })
#
# test_that("a vector of path works", {
#   expect_equal(
#     ceiling(
#       sum(
#         unlist(
#           identify_image(
#             c("https://upgo.lab.mcgill.ca/img/UPGo_logo.png",
#               "https://upgo.lab.mcgill.ca/img/mcgill.png"
#               ))))), 34)
#   })
#
# test_that("a list of paths works", {
#   expect_equal(
#     ceiling(
#       sum(
#         unlist(
#           identify_image(
#             list(
#               "https://upgo.lab.mcgill.ca/img/UPGo_logo.png",
#               "https://upgo.lab.mcgill.ca/img/mcgill.png"
#               ))))), 34)
#   })
#
