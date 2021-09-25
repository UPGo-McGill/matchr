// [[Rcpp::plugins(cpp11)]]
#include <Rcpp.h>
using namespace Rcpp;

bool any_in_int(std::unordered_set<int> s1, std::vector<int> x) {
  int n = x.size();
  int i = 0;
  bool new_val = false;
  LogicalVector out(n);
  while (i < n && new_val == false) {
    new_val = !(s1.find(x[i]) == s1.end());
    ++i;
  }
  
  return new_val;
  
}

////////////////////////////////////////////////////////////////////////////////

bool any_in_string(std::unordered_set<std::string> s1, std::vector<std::string> x) {
  int n = x.size();
  int i = 0;
  bool new_val = false;
  LogicalVector out(n);
  while (i < n && new_val == false) {
    new_val = !(s1.find(x[i]) == s1.end());
    ++i;
  }
  
  return new_val;
  
}

////////////////////////////////////////////////////////////////////////////////

// [[Rcpp::export]]
std::vector<std::unordered_set<int>> reduce_int(List x) {
  
  // Initialize output
  std::vector<std::unordered_set<int>> output;
  
  // Loop through x
  List::iterator x_it;
  List::iterator x_it_start = x.begin();
  List::iterator x_it_end = x.end();
  for(x_it = x_it_start; x_it != x_it_end; ++x_it) {
    
    // Initialize bucket and match_vector   
    std::unordered_set<int> bucket;
    std::vector<int> add_x = *x_it;
    bucket.insert(add_x.begin(), add_x.end());

    // Loop throughout output to find matches and accumulate values to bucket
    std::vector<std::unordered_set<int>>::const_iterator out_it = output.cbegin();
    while (out_it != output.cend()) {
      if (any_in_int(*out_it, *x_it)) {
        std::unordered_set<int> add_out = *out_it;
        bucket.insert(add_out.begin(), add_out.end());
        out_it = output.erase(out_it);
      } else {
        ++out_it;
      }
    }
    
    // Finally, add bucket to the end of output
    output.push_back(bucket);
  }  
  
  return output;
}
