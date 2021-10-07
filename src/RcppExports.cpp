// Generated by using Rcpp::compileAttributes() -> do not edit by hand
// Generator token: 10BE3573-1514-4C36-9D1C-5A225CD40393

#include <RcppArmadillo.h>
#include <Rcpp.h>

using namespace Rcpp;

#ifdef RCPP_USE_GLOBAL_ROSTREAM
Rcpp::Rostream<true>&  Rcpp::Rcout = Rcpp::Rcpp_cout_get();
Rcpp::Rostream<false>& Rcpp::Rcerr = Rcpp::Rcpp_cerr_get();
#endif

// row_means
arma::vec row_means(arma::cube x);
RcppExport SEXP _matchr_row_means(SEXP xSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< arma::cube >::type x(xSEXP);
    rcpp_result_gen = Rcpp::wrap(row_means(x));
    return rcpp_result_gen;
END_RCPP
}
// rm_bb_c
arma::cube rm_bb_c(arma::cube x);
RcppExport SEXP _matchr_rm_bb_c(SEXP xSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< arma::cube >::type x(xSEXP);
    rcpp_result_gen = Rcpp::wrap(rm_bb_c(x));
    return rcpp_result_gen;
END_RCPP
}
// rm_bb_g
arma::mat rm_bb_g(arma::mat x);
RcppExport SEXP _matchr_rm_bb_g(SEXP xSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< arma::mat >::type x(xSEXP);
    rcpp_result_gen = Rcpp::wrap(rm_bb_g(x));
    return rcpp_result_gen;
END_RCPP
}
// rm_bb_i_c
std::vector<arma::cube> rm_bb_i_c(std::vector<arma::cube> x);
RcppExport SEXP _matchr_rm_bb_i_c(SEXP xSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< std::vector<arma::cube> >::type x(xSEXP);
    rcpp_result_gen = Rcpp::wrap(rm_bb_i_c(x));
    return rcpp_result_gen;
END_RCPP
}
// rm_bb_i_g
std::vector<arma::mat> rm_bb_i_g(std::vector<arma::mat> x);
RcppExport SEXP _matchr_rm_bb_i_g(SEXP xSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< std::vector<arma::mat> >::type x(xSEXP);
    rcpp_result_gen = Rcpp::wrap(rm_bb_i_g(x));
    return rcpp_result_gen;
END_RCPP
}
// hamming
arma::mat hamming(arma::mat x, arma::mat y);
RcppExport SEXP _matchr_hamming(SEXP xSEXP, SEXP ySEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< arma::mat >::type x(xSEXP);
    Rcpp::traits::input_parameter< arma::mat >::type y(ySEXP);
    rcpp_result_gen = Rcpp::wrap(hamming(x, y));
    return rcpp_result_gen;
END_RCPP
}
// reduce_int
std::vector<std::set<int>> reduce_int(List x);
RcppExport SEXP _matchr_reduce_int(SEXP xSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< List >::type x(xSEXP);
    rcpp_result_gen = Rcpp::wrap(reduce_int(x));
    return rcpp_result_gen;
END_RCPP
}
// rgb_to_grey
arma::mat rgb_to_grey(arma::cube x);
RcppExport SEXP _matchr_rgb_to_grey(SEXP xSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< arma::cube >::type x(xSEXP);
    rcpp_result_gen = Rcpp::wrap(rgb_to_grey(x));
    return rcpp_result_gen;
END_RCPP
}

static const R_CallMethodDef CallEntries[] = {
    {"_matchr_row_means", (DL_FUNC) &_matchr_row_means, 1},
    {"_matchr_rm_bb_c", (DL_FUNC) &_matchr_rm_bb_c, 1},
    {"_matchr_rm_bb_g", (DL_FUNC) &_matchr_rm_bb_g, 1},
    {"_matchr_rm_bb_i_c", (DL_FUNC) &_matchr_rm_bb_i_c, 1},
    {"_matchr_rm_bb_i_g", (DL_FUNC) &_matchr_rm_bb_i_g, 1},
    {"_matchr_hamming", (DL_FUNC) &_matchr_hamming, 2},
    {"_matchr_reduce_int", (DL_FUNC) &_matchr_reduce_int, 1},
    {"_matchr_rgb_to_grey", (DL_FUNC) &_matchr_rgb_to_grey, 1},
    {NULL, NULL, 0}
};

RcppExport void R_init_matchr(DllInfo *dll) {
    R_registerRoutines(dll, NULL, CallEntries, NULL, NULL);
    R_useDynamicSymbols(dll, FALSE);
}
