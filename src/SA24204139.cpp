#include <Rcpp.h>
using namespace Rcpp;

// [[Rcpp::export]]
DataFrame complete_design(List varlist, bool numbering = true, int size = 1) {
  int nf = varlist.size(); // Number of factors
  std::vector<int> nlev(nf); // Number of levels per factor
  std::vector<std::string> tf(nf); // Type of each factor ("factor" or "df")
  std::vector<CharacterVector> levels_list(nf); // Stores levels for factors
  std::vector<DataFrame> df_list(nf); // Stores data for data frames
  
  // Parse varlist to process each factor
  for (int i = 0; i < nf; ++i) {
    SEXP item = varlist[i];
    
    if (Rf_isInteger(item)) { // Integer input, treated as a sequence of levels
      IntegerVector iv = as<IntegerVector>(item);
      if (iv.size() == 1 && iv[0] > 0) {
        nlev[i] = iv[0];
        levels_list[i] = CharacterVector(iv[0]);
        for (int j = 0; j < iv[0]; ++j) levels_list[i][j] = std::to_string(j + 1);
        tf[i] = "factor";
      } else {
        stop("Invalid input: integers must be positive and have size 1.");
      }
    } else if (Rf_isString(item)) { // Character input, treated as factor levels
      CharacterVector cv = as<CharacterVector>(item);
      if (is_true(any(duplicated(cv)))) {
        stop("Invalid input: factor levels cannot have duplicates.");
      }
      nlev[i] = cv.size();
      levels_list[i] = cv;
      tf[i] = "factor";
    } else if (Rf_isFrame(item)) { // Data frame input
      DataFrame df = as<DataFrame>(item);
      nlev[i] = df.nrows();
      df_list[i] = df;
      tf[i] = "df";
    } else {
      stop("Invalid input type.");
    }
  }
  
  // Calculate total number of experiments
  int ne = size;
  for (int i = 0; i < nf; ++i) {
    ne *= nlev[i];
  }
  
  // Generate combinations for each factor
  List result_list(nf);
  for (int i = 0; i < nf; ++i) {
    IntegerVector ids = rep_each(seq_len(nlev[i]), ne / nlev[i]);
    if (tf[i] == "factor") {
      CharacterVector factor_levels = levels_list[i];
      result_list[i] = factor_levels[ids - 1];
    } else {
      DataFrame df = df_list[i];
      result_list[i] = df[ids - 1];
    }
  }
  
  // Create final data frame
  DataFrame result = DataFrame::create(result_list);
  
  // Add experiment numbering if needed
  if (numbering) {
    IntegerVector exp_number = seq_len(ne / size);
    exp_number = rep_each(exp_number, size);
    result.push_back(exp_number, "expNumber");
  }
  
  return result;
}

/*** R
# Test cases in R
test1 <- function() {
  complete_design(list(f1 = 3, f2 = c("a", "b"), f3 = 4))
}
print(head(test1()))

test2 <- function() {
  complete_design(list(f1 = 3, f2 = c("a", "b"), f3 = 4), true, 2)
}
print(head(test2()))

test3 <- function() {
  complete_design(list(
    f1 = 3,
    f2 = c("a", "b"),
    f3 = data.frame(
      fa = c(0.01, 0.05, 0.1),
      fb = c("x", "x", "y")
    )
  ), true, 2)
}
print(head(test3()))
*/