# matchr 0.3

## Major changes
* Image signatures have been re-implemented as perceptual hashes from the OpenImageR package. These are faster and more reliable than the previous image colour bands. (#25)
* The compare_images Shiny app has been re-purposed to be a more flexible, general-purpose image comparison tool. confirm_changes takes the place of the old compare_images in the main image matching workflow.
* remove_black_bars has been rewritten in C++ to be much faster, and now is a generic, with methods for `matchr_image` vectors, pixel arrays, or lists of pixel arrays.
* find_duplicates has been broken out as a standalone function that identifies pairwise duplicates in a table of image matches. The internals have been rewritten in C++ and are now approximately 15x as fast.

## Big fixes and minor updates
* TKTK bug fixes
* Added a `NEWS.md` file to track changes to the package.
