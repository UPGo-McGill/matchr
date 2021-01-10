match_signatures_pairwise <- function(x, y, method = "colour", quiet = FALSE) {

  # Prepare method
  if (method %in% c("grey", "gray", "greyscale", "grayscale")) {
    x <- trim_signature(x, 1:(sig_length(x) / 4)) 
    y <- trim_signature(y, 1:(sig_length(y) / 4)) 
  }
  
  if (method %in% c("colour", "color", "rgb", "RGB")) {
    x <- trim_signature(x, (sig_length(x) / 4 + 1):sig_length(x))
    y <- trim_signature(y, (sig_length(y) / 4 + 1):sig_length(y)) 
  }
  
  mapply(stats::cor, field(x, "signature"), field(y, "signature"), 
         SIMPLIFY = TRUE)
}
