// Generated by using Rcpp::compileAttributes() -> do not edit by hand
// Generator token: 10BE3573-1514-4C36-9D1C-5A225CD40393

#include <Rcpp.h>

using namespace Rcpp;

// mod
float mod(float x, int base);
RcppExport SEXP _minVL_mod(SEXP xSEXP, SEXP baseSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< float >::type x(xSEXP);
    Rcpp::traits::input_parameter< int >::type base(baseSEXP);
    rcpp_result_gen = Rcpp::wrap(mod(x, base));
    return rcpp_result_gen;
END_RCPP
}
// get_vl_elt_distance
double get_vl_elt_distance(double e1, double e2, String elt_type);
RcppExport SEXP _minVL_get_vl_elt_distance(SEXP e1SEXP, SEXP e2SEXP, SEXP elt_typeSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< double >::type e1(e1SEXP);
    Rcpp::traits::input_parameter< double >::type e2(e2SEXP);
    Rcpp::traits::input_parameter< String >::type elt_type(elt_typeSEXP);
    rcpp_result_gen = Rcpp::wrap(get_vl_elt_distance(e1, e2, elt_type));
    return rcpp_result_gen;
END_RCPP
}
// get_vl_ascending_distance
std::vector<double> get_vl_ascending_distance(double e1, std::vector<double> e2, String elt_type);
RcppExport SEXP _minVL_get_vl_ascending_distance(SEXP e1SEXP, SEXP e2SEXP, SEXP elt_typeSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< double >::type e1(e1SEXP);
    Rcpp::traits::input_parameter< std::vector<double> >::type e2(e2SEXP);
    Rcpp::traits::input_parameter< String >::type elt_type(elt_typeSEXP);
    rcpp_result_gen = Rcpp::wrap(get_vl_ascending_distance(e1, e2, elt_type));
    return rcpp_result_gen;
END_RCPP
}
// vl_dist
double vl_dist(std::vector<double> s1, std::vector<double> s2, String elt_type, String norm);
RcppExport SEXP _minVL_vl_dist(SEXP s1SEXP, SEXP s2SEXP, SEXP elt_typeSEXP, SEXP normSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< std::vector<double> >::type s1(s1SEXP);
    Rcpp::traits::input_parameter< std::vector<double> >::type s2(s2SEXP);
    Rcpp::traits::input_parameter< String >::type elt_type(elt_typeSEXP);
    Rcpp::traits::input_parameter< String >::type norm(normSEXP);
    rcpp_result_gen = Rcpp::wrap(vl_dist(s1, s2, elt_type, norm));
    return rcpp_result_gen;
END_RCPP
}
// order_by
std::vector<double> order_by(std::vector<double>& data, std::vector<double> by);
RcppExport SEXP _minVL_order_by(SEXP dataSEXP, SEXP bySEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< std::vector<double>& >::type data(dataSEXP);
    Rcpp::traits::input_parameter< std::vector<double> >::type by(bySEXP);
    rcpp_result_gen = Rcpp::wrap(order_by(data, by));
    return rcpp_result_gen;
END_RCPP
}
// add_dist
double add_dist(double prev_dist, double new_pair_dist, String norm);
RcppExport SEXP _minVL_add_dist(SEXP prev_distSEXP, SEXP new_pair_distSEXP, SEXP normSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< double >::type prev_dist(prev_distSEXP);
    Rcpp::traits::input_parameter< double >::type new_pair_dist(new_pair_distSEXP);
    Rcpp::traits::input_parameter< String >::type norm(normSEXP);
    rcpp_result_gen = Rcpp::wrap(add_dist(prev_dist, new_pair_dist, norm));
    return rcpp_result_gen;
END_RCPP
}
// min_vl
List min_vl(NumericVector s1, NumericVector s2, String elt_type, String norm);
RcppExport SEXP _minVL_min_vl(SEXP s1SEXP, SEXP s2SEXP, SEXP elt_typeSEXP, SEXP normSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< NumericVector >::type s1(s1SEXP);
    Rcpp::traits::input_parameter< NumericVector >::type s2(s2SEXP);
    Rcpp::traits::input_parameter< String >::type elt_type(elt_typeSEXP);
    Rcpp::traits::input_parameter< String >::type norm(normSEXP);
    rcpp_result_gen = Rcpp::wrap(min_vl(s1, s2, elt_type, norm));
    return rcpp_result_gen;
END_RCPP
}
// min_vl_dist
double min_vl_dist(NumericVector s1, NumericVector s2, String elt_type, String norm);
RcppExport SEXP _minVL_min_vl_dist(SEXP s1SEXP, SEXP s2SEXP, SEXP elt_typeSEXP, SEXP normSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< NumericVector >::type s1(s1SEXP);
    Rcpp::traits::input_parameter< NumericVector >::type s2(s2SEXP);
    Rcpp::traits::input_parameter< String >::type elt_type(elt_typeSEXP);
    Rcpp::traits::input_parameter< String >::type norm(normSEXP);
    rcpp_result_gen = Rcpp::wrap(min_vl_dist(s1, s2, elt_type, norm));
    return rcpp_result_gen;
END_RCPP
}
// min_vl_dists
NumericMatrix min_vl_dists(List s1_list, List s2_list, String elt_type, String norm);
RcppExport SEXP _minVL_min_vl_dists(SEXP s1_listSEXP, SEXP s2_listSEXP, SEXP elt_typeSEXP, SEXP normSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< List >::type s1_list(s1_listSEXP);
    Rcpp::traits::input_parameter< List >::type s2_list(s2_listSEXP);
    Rcpp::traits::input_parameter< String >::type elt_type(elt_typeSEXP);
    Rcpp::traits::input_parameter< String >::type norm(normSEXP);
    rcpp_result_gen = Rcpp::wrap(min_vl_dists(s1_list, s2_list, elt_type, norm));
    return rcpp_result_gen;
END_RCPP
}
// min_vls
std::vector<List> min_vls(List s1_list, NumericVector s2, String elt_type, String norm);
RcppExport SEXP _minVL_min_vls(SEXP s1_listSEXP, SEXP s2SEXP, SEXP elt_typeSEXP, SEXP normSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< List >::type s1_list(s1_listSEXP);
    Rcpp::traits::input_parameter< NumericVector >::type s2(s2SEXP);
    Rcpp::traits::input_parameter< String >::type elt_type(elt_typeSEXP);
    Rcpp::traits::input_parameter< String >::type norm(normSEXP);
    rcpp_result_gen = Rcpp::wrap(min_vls(s1_list, s2, elt_type, norm));
    return rcpp_result_gen;
END_RCPP
}

static const R_CallMethodDef CallEntries[] = {
    {"_minVL_mod", (DL_FUNC) &_minVL_mod, 2},
    {"_minVL_get_vl_elt_distance", (DL_FUNC) &_minVL_get_vl_elt_distance, 3},
    {"_minVL_get_vl_ascending_distance", (DL_FUNC) &_minVL_get_vl_ascending_distance, 3},
    {"_minVL_vl_dist", (DL_FUNC) &_minVL_vl_dist, 4},
    {"_minVL_order_by", (DL_FUNC) &_minVL_order_by, 2},
    {"_minVL_add_dist", (DL_FUNC) &_minVL_add_dist, 3},
    {"_minVL_min_vl", (DL_FUNC) &_minVL_min_vl, 4},
    {"_minVL_min_vl_dist", (DL_FUNC) &_minVL_min_vl_dist, 4},
    {"_minVL_min_vl_dists", (DL_FUNC) &_minVL_min_vl_dists, 4},
    {"_minVL_min_vls", (DL_FUNC) &_minVL_min_vls, 4},
    {NULL, NULL, 0}
};

RcppExport void R_init_minVL(DllInfo *dll) {
    R_registerRoutines(dll, NULL, CallEntries, NULL, NULL);
    R_useDynamicSymbols(dll, FALSE);
}
