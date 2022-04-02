#include <R.h>
#include <Rinternals.h>
#include <stdlib.h> // for NULL
#include <R_ext/Rdynload.h>

/* FIXME: 
   Check these declarations against the C/Fortran source code.
*/

/* .Call calls */
extern SEXP C_abs_diff(SEXP, SEXP, SEXP, SEXP);
extern SEXP C_and_raw(SEXP, SEXP, SEXP);
extern SEXP C_character2integer(SEXP, SEXP, SEXP, SEXP);
extern SEXP C_comma(SEXP, SEXP, SEXP);
extern SEXP C_empty(SEXP, SEXP, SEXP, SEXP);
extern SEXP C_FLIP(SEXP);
extern SEXP C_hausdorffEuclid(SEXP, SEXP);
extern SEXP C_haversineDistance(SEXP, SEXP, SEXP, SEXP, SEXP);
extern SEXP C_match_min_Haversine(SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP);
extern SEXP C_Mode(SEXP, SEXP, SEXP);
extern SEXP C_op2M(SEXP);
extern SEXP C_or_raw(SEXP, SEXP, SEXP);
extern SEXP C_theEmptiestQuarters(SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP);
extern SEXP C_theEuclidDistance(SEXP, SEXP, SEXP, SEXP, SEXP);
extern SEXP C_which_first_lgl1(SEXP, SEXP, SEXP, SEXP);
extern SEXP C_which_min_HaversineDistance(SEXP, SEXP, SEXP, SEXP, SEXP);
extern SEXP C_which_raw(SEXP, SEXP);
extern SEXP Callocate_with_root(SEXP, SEXP, SEXP, SEXP, SEXP, SEXP);
extern SEXP Callocate0_dbl(SEXP, SEXP);
extern SEXP Callocate0_except(SEXP, SEXP, SEXP, SEXP);
extern SEXP Callocate0_int(SEXP, SEXP);
extern SEXP CallocateConstants(SEXP, SEXP);
extern SEXP Cand3(SEXP, SEXP, SEXP);
extern SEXP Cands(SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP);
extern SEXP CAnyCharMatch(SEXP, SEXP, SEXP);
extern SEXP CanyOutside(SEXP, SEXP, SEXP, SEXP, SEXP);
extern SEXP CBetween(SEXP, SEXP, SEXP, SEXP, SEXP);
extern SEXP Ccoalesce0(SEXP, SEXP);
extern SEXP Ccount_logical(SEXP, SEXP);
extern SEXP Ccumsum_reset(SEXP, SEXP);
extern SEXP Ccumsum_reset_sorted_int(SEXP);
extern SEXP Ccumsum_reset_where(SEXP, SEXP, SEXP, SEXP);
extern SEXP Cdiagnose_omp(SEXP);
extern SEXP Cdivisible(SEXP, SEXP, SEXP);
extern SEXP Cdivisible16(SEXP, SEXP);
extern SEXP Cdivisible2(SEXP, SEXP, SEXP);
extern SEXP CEmptiestQuarter(SEXP, SEXP, SEXP, SEXP, SEXP, SEXP);
extern SEXP Cevery_int32(SEXP, SEXP);
extern SEXP CextractMandatory(SEXP, SEXP, SEXP);
extern SEXP CfirstNonNegativeRadix(SEXP, SEXP, SEXP, SEXP);
extern SEXP Cforce_as_integer(SEXP, SEXP);
extern SEXP Chas_openmp();
extern SEXP CImplies(SEXP, SEXP, SEXP, SEXP);
extern SEXP Cis_altrep(SEXP);
extern SEXP Cis_constant(SEXP, SEXP);
extern SEXP Cis_safe2int(SEXP);
extern SEXP Cis_seq(SEXP);
extern SEXP Cis_sorted(SEXP, SEXP);
extern SEXP Cisnt_constant(SEXP);
extern SEXP Cisnt_sorted(SEXP, SEXP);
extern SEXP Clgl2raw(SEXP, SEXP, SEXP);
extern SEXP Cminmax(SEXP, SEXP, SEXP);
extern SEXP Cna_and(SEXP);
extern SEXP Cor3(SEXP, SEXP, SEXP);
extern SEXP Cors(SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP);
extern SEXP Cpar_in_int(SEXP, SEXP, SEXP);
extern SEXP Cpmax(SEXP, SEXP, SEXP, SEXP);
extern SEXP Cpmax0_bitwise(SEXP, SEXP, SEXP);
extern SEXP Cpmax0_radix_sorted_dbl(SEXP, SEXP, SEXP);
extern SEXP Cpmax0_radix_sorted_int(SEXP, SEXP, SEXP);
extern SEXP CpmaxC_in_place(SEXP, SEXP, SEXP, SEXP);
extern SEXP Cpmin(SEXP, SEXP, SEXP, SEXP);
extern SEXP Cpmin0_bitwise(SEXP, SEXP, SEXP);
extern SEXP Cpmin0_radix_sorted_dbl(SEXP, SEXP, SEXP);
extern SEXP Cpmin0_radix_sorted_int(SEXP, SEXP, SEXP);
extern SEXP CpminC_in_place(SEXP, SEXP, SEXP, SEXP);
extern SEXP CPrintChars(SEXP);
extern SEXP Crange(SEXP);
extern SEXP Craw2lgl(SEXP, SEXP);
extern SEXP CSquish(SEXP, SEXP);
extern SEXP CStringEqual(SEXP, SEXP);
extern SEXP Csum_isna(SEXP, SEXP);
extern SEXP Csum_raw(SEXP, SEXP);
extern SEXP Csummary3(SEXP, SEXP, SEXP, SEXP, SEXP);
extern SEXP Cuncoalesce0(SEXP);
extern SEXP Cunique_fmatch(SEXP, SEXP, SEXP);
extern SEXP CuniqueN_fmatch(SEXP, SEXP);
extern SEXP Cwhere_square_bracket_opens(SEXP, SEXP);
extern SEXP Cwhich_16(SEXP, SEXP, SEXP, SEXP);
extern SEXP Cwhich_even(SEXP);
extern SEXP Cwhich_first(SEXP);
extern SEXP Cwhich_first__(SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP);
extern SEXP Cwhich_first_false(SEXP);
extern SEXP Cwhich_first_in_lgl(SEXP, SEXP, SEXP, SEXP);
extern SEXP Cwhich_first_lgl_lgl_op(SEXP, SEXP, SEXP, SEXP);
extern SEXP Cwhich_firstNA(SEXP);
extern SEXP Cwhich_isna(SEXP, SEXP, SEXP);
extern SEXP Cwhich_isnt_integerish(SEXP);
extern SEXP Cwhich_last(SEXP);
extern SEXP Cwhich_last__(SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP);
extern SEXP Cwhich_last_false(SEXP);
extern SEXP Cwhich_last_in_lgl(SEXP, SEXP, SEXP, SEXP);
extern SEXP Cwhich_last_notFALSE(SEXP);
extern SEXP Cwhich_last_notTRUE(SEXP);
extern SEXP Cwhich_lastNA(SEXP);
extern SEXP Cwhich_true_onwards(SEXP);
extern SEXP Cwhich3(SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP);
extern SEXP Cwhich3_mem(SEXP, SEXP, SEXP, SEXP);
extern SEXP fmatch(SEXP, SEXP, SEXP, SEXP, SEXP, SEXP);
extern SEXP fmatchp_lgl(SEXP, SEXP, SEXP, SEXP);

static const R_CallMethodDef CallEntries[] = {
    {"C_abs_diff",                    (DL_FUNC) &C_abs_diff,                     4},
    {"C_and_raw",                     (DL_FUNC) &C_and_raw,                      3},
    {"C_character2integer",           (DL_FUNC) &C_character2integer,            4},
    {"C_comma",                       (DL_FUNC) &C_comma,                        3},
    {"C_empty",                       (DL_FUNC) &C_empty,                        4},
    {"C_FLIP",                        (DL_FUNC) &C_FLIP,                         1},
    {"C_hausdorffEuclid",             (DL_FUNC) &C_hausdorffEuclid,              2},
    {"C_haversineDistance",           (DL_FUNC) &C_haversineDistance,            5},
    {"C_match_min_Haversine",         (DL_FUNC) &C_match_min_Haversine,         11},
    {"C_Mode",                        (DL_FUNC) &C_Mode,                         3},
    {"C_op2M",                        (DL_FUNC) &C_op2M,                         1},
    {"C_or_raw",                      (DL_FUNC) &C_or_raw,                       3},
    {"C_theEmptiestQuarters",         (DL_FUNC) &C_theEmptiestQuarters,          7},
    {"C_theEuclidDistance",           (DL_FUNC) &C_theEuclidDistance,            5},
    {"C_which_first_lgl1",            (DL_FUNC) &C_which_first_lgl1,             4},
    {"C_which_min_HaversineDistance", (DL_FUNC) &C_which_min_HaversineDistance,  5},
    {"C_which_raw",                   (DL_FUNC) &C_which_raw,                    2},
    {"Callocate_with_root",           (DL_FUNC) &Callocate_with_root,            6},
    {"Callocate0_dbl",                (DL_FUNC) &Callocate0_dbl,                 2},
    {"Callocate0_except",             (DL_FUNC) &Callocate0_except,              4},
    {"Callocate0_int",                (DL_FUNC) &Callocate0_int,                 2},
    {"CallocateConstants",            (DL_FUNC) &CallocateConstants,             2},
    {"Cand3",                         (DL_FUNC) &Cand3,                          3},
    {"Cands",                         (DL_FUNC) &Cands,                          7},
    {"CAnyCharMatch",                 (DL_FUNC) &CAnyCharMatch,                  3},
    {"CanyOutside",                   (DL_FUNC) &CanyOutside,                    5},
    {"CBetween",                      (DL_FUNC) &CBetween,                       5},
    {"Ccoalesce0",                    (DL_FUNC) &Ccoalesce0,                     2},
    {"Ccount_logical",                (DL_FUNC) &Ccount_logical,                 2},
    {"Ccumsum_reset",                 (DL_FUNC) &Ccumsum_reset,                  2},
    {"Ccumsum_reset_sorted_int",      (DL_FUNC) &Ccumsum_reset_sorted_int,       1},
    {"Ccumsum_reset_where",           (DL_FUNC) &Ccumsum_reset_where,            4},
    {"Cdiagnose_omp",                 (DL_FUNC) &Cdiagnose_omp,                  1},
    {"Cdivisible",                    (DL_FUNC) &Cdivisible,                     3},
    {"Cdivisible16",                  (DL_FUNC) &Cdivisible16,                   2},
    {"Cdivisible2",                   (DL_FUNC) &Cdivisible2,                    3},
    {"CEmptiestQuarter",              (DL_FUNC) &CEmptiestQuarter,               6},
    {"Cevery_int32",                  (DL_FUNC) &Cevery_int32,                   2},
    {"CextractMandatory",             (DL_FUNC) &CextractMandatory,              3},
    {"CfirstNonNegativeRadix",        (DL_FUNC) &CfirstNonNegativeRadix,         4},
    {"Cforce_as_integer",             (DL_FUNC) &Cforce_as_integer,              2},
    {"Chas_openmp",                   (DL_FUNC) &Chas_openmp,                    0},
    {"CImplies",                      (DL_FUNC) &CImplies,                       4},
    {"Cis_altrep",                    (DL_FUNC) &Cis_altrep,                     1},
    {"Cis_constant",                  (DL_FUNC) &Cis_constant,                   2},
    {"Cis_safe2int",                  (DL_FUNC) &Cis_safe2int,                   1},
    {"Cis_seq",                       (DL_FUNC) &Cis_seq,                        1},
    {"Cis_sorted",                    (DL_FUNC) &Cis_sorted,                     2},
    {"Cisnt_constant",                (DL_FUNC) &Cisnt_constant,                 1},
    {"Cisnt_sorted",                  (DL_FUNC) &Cisnt_sorted,                   2},
    {"Clgl2raw",                      (DL_FUNC) &Clgl2raw,                       3},
    {"Cminmax",                       (DL_FUNC) &Cminmax,                        3},
    {"Cna_and",                       (DL_FUNC) &Cna_and,                        1},
    {"Cor3",                          (DL_FUNC) &Cor3,                           3},
    {"Cors",                          (DL_FUNC) &Cors,                           7},
    {"Cpar_in_int",                   (DL_FUNC) &Cpar_in_int,                    3},
    {"Cpmax",                         (DL_FUNC) &Cpmax,                          4},
    {"Cpmax0_bitwise",                (DL_FUNC) &Cpmax0_bitwise,                 3},
    {"Cpmax0_radix_sorted_dbl",       (DL_FUNC) &Cpmax0_radix_sorted_dbl,        3},
    {"Cpmax0_radix_sorted_int",       (DL_FUNC) &Cpmax0_radix_sorted_int,        3},
    {"CpmaxC_in_place",               (DL_FUNC) &CpmaxC_in_place,                4},
    {"Cpmin",                         (DL_FUNC) &Cpmin,                          4},
    {"Cpmin0_bitwise",                (DL_FUNC) &Cpmin0_bitwise,                 3},
    {"Cpmin0_radix_sorted_dbl",       (DL_FUNC) &Cpmin0_radix_sorted_dbl,        3},
    {"Cpmin0_radix_sorted_int",       (DL_FUNC) &Cpmin0_radix_sorted_int,        3},
    {"CpminC_in_place",               (DL_FUNC) &CpminC_in_place,                4},
    {"CPrintChars",                   (DL_FUNC) &CPrintChars,                    1},
    {"Crange",                        (DL_FUNC) &Crange,                         1},
    {"Craw2lgl",                      (DL_FUNC) &Craw2lgl,                       2},
    {"CSquish",                       (DL_FUNC) &CSquish,                        2},
    {"CStringEqual",                  (DL_FUNC) &CStringEqual,                   2},
    {"Csum_isna",                     (DL_FUNC) &Csum_isna,                      2},
    {"Csum_raw",                      (DL_FUNC) &Csum_raw,                       2},
    {"Csummary3",                     (DL_FUNC) &Csummary3,                      5},
    {"Cuncoalesce0",                  (DL_FUNC) &Cuncoalesce0,                   1},
    {"Cunique_fmatch",                (DL_FUNC) &Cunique_fmatch,                 3},
    {"CuniqueN_fmatch",               (DL_FUNC) &CuniqueN_fmatch,                2},
    {"Cwhere_square_bracket_opens",   (DL_FUNC) &Cwhere_square_bracket_opens,    2},
    {"Cwhich_16",                     (DL_FUNC) &Cwhich_16,                      4},
    {"Cwhich_even",                   (DL_FUNC) &Cwhich_even,                    1},
    {"Cwhich_first",                  (DL_FUNC) &Cwhich_first,                   1},
    {"Cwhich_first__",                (DL_FUNC) &Cwhich_first__,                 8},
    {"Cwhich_first_false",            (DL_FUNC) &Cwhich_first_false,             1},
    {"Cwhich_first_in_lgl",           (DL_FUNC) &Cwhich_first_in_lgl,            4},
    {"Cwhich_first_lgl_lgl_op",       (DL_FUNC) &Cwhich_first_lgl_lgl_op,        4},
    {"Cwhich_firstNA",                (DL_FUNC) &Cwhich_firstNA,                 1},
    {"Cwhich_isna",                   (DL_FUNC) &Cwhich_isna,                    3},
    {"Cwhich_isnt_integerish",        (DL_FUNC) &Cwhich_isnt_integerish,         1},
    {"Cwhich_last",                   (DL_FUNC) &Cwhich_last,                    1},
    {"Cwhich_last__",                 (DL_FUNC) &Cwhich_last__,                  8},
    {"Cwhich_last_false",             (DL_FUNC) &Cwhich_last_false,              1},
    {"Cwhich_last_in_lgl",            (DL_FUNC) &Cwhich_last_in_lgl,             4},
    {"Cwhich_last_notFALSE",          (DL_FUNC) &Cwhich_last_notFALSE,           1},
    {"Cwhich_last_notTRUE",           (DL_FUNC) &Cwhich_last_notTRUE,            1},
    {"Cwhich_lastNA",                 (DL_FUNC) &Cwhich_lastNA,                  1},
    {"Cwhich_true_onwards",           (DL_FUNC) &Cwhich_true_onwards,            1},
    {"Cwhich3",                       (DL_FUNC) &Cwhich3,                        7},
    {"Cwhich3_mem",                   (DL_FUNC) &Cwhich3_mem,                    4},
    {"fmatch",                        (DL_FUNC) &fmatch,                         6},
    {"fmatchp_lgl",                   (DL_FUNC) &fmatchp_lgl,                    4},
    {NULL, NULL, 0}
};

void R_init_hutilscpp(DllInfo *dll)
{
    R_registerRoutines(dll, NULL, CallEntries, NULL, NULL);
    R_useDynamicSymbols(dll, FALSE);
}
