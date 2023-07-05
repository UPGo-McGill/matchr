// [[Rcpp::plugins(cpp11)]]
// [[Rcpp::depends(RcppArmadillo)]]
# include <RcppArmadillo.h>
using namespace Rcpp;

////////////////////////////////////////////////////////////////////////////////

//[[Rcpp::export]]
arma::vec row_means(arma::cube x) {
  arma::mat out = mean(x, 1);
  return(mean(out, 1));
}


////////////////////////////////////////////////////////////////////////////////

//[[Rcpp::export]]
arma::cube rm_bb_c(arma::cube x) {
  
  // Start by getting row means
  arma::vec rm = row_means(x);
  
  // Exit if the image is all black
  if (mean(rm) < 0.01) {
    return(x);
  }
  
  // Get black_strips
  arma::uvec start_strips = find(rm < 0.02);
  int n = start_strips.size();

  // Exit if the image has no black bars
  if (n == 0) {
    return(x);
  }
  
  // Remove any rows with reasonably high pixel values
  arma::uvec black_strips(n);
  int j = 0;
  
  // Loop through black_strips
  for (int i = 0; i < n; i++) {
    arma::uvec bright = find(x.row(start_strips[i]) > 0.1, 1);
    if (bright.size() == 0) {
      black_strips(j++) = start_strips[i];
    }
  }
  black_strips.set_size(j);
  
  // Exit if the image has no black bars
  if (black_strips.size() == 0) {
    return(x);
  }
  
  // Remove the bottom bar, if it exists
  if (black_strips(j - 1) == rm.size() - 1) {
    
    // Get smallest index position which is in a continuous sequence with j
    arma::uword bottom_bound = rm.size() - 1;
    for (int i = j - 1; i > 0; --i) {
      if (black_strips(i) == black_strips(i - 1) + 1) {
        bottom_bound -= 1;
      } else {
        break;
      }
    }
    
    // Drop corresponding rows
    arma::uword end(x.n_rows - 1);
    x.shed_rows(bottom_bound, end);
  }
  
  // Remove the top bar, if it exists
  if (black_strips(0) == 0) {
    
    // Get largest index position which is in a continuous sequence with 1
    arma::uword top_bound = 0;
    for (int i = 0; i < (j - 1); ++i) {
      if (black_strips[i + 1] == black_strips[i] + 1) {
        top_bound += 1;
      } else {
        break;
      }
    }
    
    // Drop corresponding rows
    arma::uword start(0);
    x.shed_rows(start, top_bound);
    
  }
  
  return(x);
}


////////////////////////////////////////////////////////////////////////////////

//[[Rcpp::export]]
arma::mat rm_bb_g(arma::mat x) {
  
  // Start by getting row means
  arma::vec rm = mean(x, 1);
  
  // Exit if the image is all black
  if (mean(rm) < 0.005) {
    return(x);
  }
  
  // Get black_strips
  arma::uvec start_strips = find(rm < 0.02);
  int n = start_strips.size();

  // Exit if the image has no black bars
  if (n == 0) {
    return(x);
  }
  
  // Remove any rows with reasonably high pixel values
  arma::uvec black_strips(n);
  int j = 0;
  
  // Loop through black_strips
  for (int i = 0; i < n; i++) {
    arma::uvec bright = find(x.row(start_strips[i]) > 0.1, 1);
    if (bright.size() == 0) {
      black_strips(j++) = start_strips[i];
    }
  }
  black_strips.set_size(j);
  
  // Exit if the image has no black bars
  if (black_strips.size() == 0) {
    return(x);
  }
  
  // Remove the bottom bar, if it exists
  if (black_strips(j - 1) == rm.size() - 1) {
    
    // Get smallest index position which is in a continuous sequence with j
    arma::uword bottom_bound = rm.size() - 1;
    for (int i = j - 1; i > 0; --i) {
      if (black_strips(i) == black_strips(i - 1) + 1) {
        bottom_bound -= 1;
      } else {
        break;
      }
    }
    
    // Drop corresponding rows
    arma::uword end(x.n_rows - 1);
    x.shed_rows(bottom_bound, end);
    
  }
  
  // Remove the top bar, if it exists
  if (black_strips(0) == 0) {
    
    // Get largest index position which is in a continuous sequence with 1
    arma::uword top_bound = 0;
    for (int i = 0; i < (j - 1); ++i) {
      if (black_strips[i + 1] == black_strips[i] + 1) {
        top_bound += 1;
      } else {
        break;
      }
    }
    
    // Drop corresponding rows
    arma::uword start(0);
    x.shed_rows(start, top_bound);
    
  }
  
  return(x);
}

////////////////////////////////////////////////////////////////////////////////

// [[Rcpp::export]]
std::vector<arma::cube> rm_bb_i_c(std::vector<arma::cube> x) {
  
  int n = x.size();
  for (int i = 0; i < n; i++) {
    x[i] = rm_bb_c(x[i]);
  }
  
  return(x);
}


////////////////////////////////////////////////////////////////////////////////

// [[Rcpp::export]]
std::vector<arma::mat> rm_bb_i_g(std::vector<arma::mat> x) {
  
  int n = x.size();
  for (int i = 0; i < n; i++) {
    x[i] = rm_bb_g(x[i]);
  }
  
  return(x);
}
