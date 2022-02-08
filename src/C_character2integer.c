#include "hutilscpp.h"

unsigned int UCHAR2INT[256] = {0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 0};
unsigned int UCHAR2TEN[256] = {9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 0};
const char BIGMARKS[] = ",, '_~\"/";


static const char i2bigmark(int x) {
  switch(x) {
  case 1:
    return ',';
  case 2:
    return ' ';
  case 3:
    return '\'';
  case 4:
    return '_';
  case 5:
    return '~';
  case 6:
    return '"';
  case 7:
    return '/';
  }
  return ',';
}


static int firstnws(const char * x, int n) {
  int j = 0;
  while (j < n && isspace(x[j])) {
    ++j;
  }
  return j;
}

static bool is_negative(const char * x, int j_ws, int n) {
  return j_ws < n && x[j_ws] == '-';
}

static int anyDecimal(const char * x, int n) {
  int k = 0;
  for (int j = 0; j < n; ++j) {
    if (x[j] == '.') {
      k = j + 1;
      break;
    }
  }
  return k;
}

static bool trail_dot00(const char * x, int n, int k) {
  if (k && x[k] == '0') {
    while (++k < n) {
      if (x[k] != '0') {
        return false;
      }
    }
    return true;
  }
  return false;
}

int len_characteristic(const char * x, int n) {
  int o = 0;
  for (int j = 0; j < n; ++j) {
    if (x[j] == '.') {
      break;
    }
    o += isdigit(x[j]);
  }
  return o;
}

static int64_t char2int0(const char * x, int nn) {
  int k = anyDecimal(x, nn);
  bool trail00 = trail_dot00(x, nn, k);
  if (k && !trail00) {
    return NA_INTEGER;
  }
  int n = k ? k : nn; // use the location of the decimal
  int j_ws = firstnws(x, n);
  uint64_t o = 0;
  bool x_negative = is_negative(x, j_ws, n);
  if (x_negative) {
    ++j_ws;
  }
  if (j_ws < (n - 11)) {
    for (int j = 0; j < 11; ++j) {
      char x_j = x[j + j_ws];
      if (isdigit(x_j)) {
        o *= 10;
        o += x_j - '0';
      }
    }
    if (o > INT_MAX) {
      return NA_INTEGER;
    }
    for (int j = 11 + j_ws; j < n; ++j) {
      char x_j = x[j];
      if (isdigit(x_j)) {
        o *= 10;
        o += x_j - '0';
      }
      if (o > INT_MAX) {
        return NA_INTEGER;
      }
    }
  } else {
    for (int j = j_ws; j < n; ++j) {
      char x_j = x[j];
      if (isdigit(x_j)) {
        o *= 10;
        o += x_j - '0';
      }
    }
  }
  return x_negative ? -o : o;
}

static int char2int1(const char * x, int n) {
  int j_ws = firstnws(x, n);
  uint64_t o = 0;
  bool x_negative = is_negative(x, j_ws, n);
  if (x_negative) {
    ++j_ws;
  }
  const unsigned char ucZ = '0';
  for (int j = j_ws; j < n; ++j) {
    unsigned char xj = x[j];
    if (xj == '.') {
      break;
    }
    unsigned int c_rel_0 = ((unsigned char)(xj - ucZ));
    unsigned int m10 = UCHAR2TEN[c_rel_0] + 1;
    o *= m10;
    unsigned int v10 = UCHAR2INT[c_rel_0];
    o += v10;
  }
  if (o > INT_MAX) {
    return NA_INTEGER;
  }
  return x_negative ? -o : o;
}

static double char2double0(const char * x, int n) {
  int j_ws = firstnws(x, n);
  double o = 0;
  bool x_negative = is_negative(x, j_ws, n);
  if (x_negative) {
    ++j_ws;
  }
  const unsigned char ucZ = '0';
  int j = j_ws;
  for (; j < n; ++j) {
    unsigned char xj = x[j];
    if (xj == '.') {
      break;
    }
    unsigned int c_rel_0 = ((unsigned char)(xj - ucZ));
    unsigned int m10 = UCHAR2TEN[c_rel_0] + 1;
    o *= m10;
    unsigned int v10 = UCHAR2INT[c_rel_0];
    o += v10;
  }
  double ten = 0.1;
  while (j < n) {
    unsigned char xj = x[j];
    unsigned int c_rel_0 = ((unsigned char)(xj - ucZ));
    unsigned int m10 = UCHAR2TEN[c_rel_0] + 1;
    ten /= m10;
    unsigned int v10 = UCHAR2INT[c_rel_0];
    o += v10 * ten;
  }
  return o;
}

static bool is_NA0(const char * x, const char * na_string, int n) {
  for (int j = 0; j < n; ++j) {
    if (x[j] != na_string[j]) {
      return false;
    }
  }
  return true;
}

static bool charNeedsDbl(const char * x, int n) {
  int dec = anyDecimal(x, n);
  int n_digit_charac = dec ? len_characteristic(x, dec) : len_characteristic(x, n);
  if (n_digit_charac > 10) {
    // exceeds INT_MAX
    return true;
  }
  if (n_digit_charac == 10) {
    char DIGIT_INT_MAX[11] = "2147483647";
    char digits[11] = {0};

    for (int j = 0, dj = 0; j < n; ++j) {
      if (dj >= 10) {
        break;
      }
      if (isdigit(x[j])) {
        digits[dj++] = x[j];
      }
    }
    for (int dj = 0; dj < 10; ++dj) {
      if (digits[dj] > DIGIT_INT_MAX[dj]) {
        return true;
      }
      if (digits[dj] < DIGIT_INT_MAX[dj]) {
        break;
      }
    }
  }
  if (dec && !trail_dot00(x, n, dec)) {
    return true;
  }
  return false;
}

static R_xlen_t needsDouble(SEXP x) {
  R_xlen_t N = xlength(x);
  const SEXP * xp = STRING_PTR(x);
  for (R_xlen_t i = 0; i < N; ++i) {
    if (xp[i] == NA_STRING) {
      continue;
    }
    const char * xi = CHAR(xp[i]);
    int n = length(xp[i]);
    if (charNeedsDbl(xi, n)) {
      return i + 1;
    }
  }
  return 0;
}

static SEXP str2int0_(SEXP x) {
  R_xlen_t N = xlength(x);
  SEXP ans = PROTECT(allocVector(INTSXP, N));
  int * restrict ansp = INTEGER(ans);
  const SEXP * xp = STRING_PTR(x);
  for (R_xlen_t i = 0; i < N; ++i) {
    if (xp[i] == NA_STRING) {
      ansp[i] = NA_INTEGER;
      continue;
    }
    const char * xi = CHAR(xp[i]);
    int n = length(xp[i]);
    ansp[i] = char2int1(xi, n);
  }
  UNPROTECT(1);
  return ans;
}

static SEXP str2int0(SEXP x, const char * na_string, int na_len, const int allow_dbl) {
  R_xlen_t N = xlength(x);
  SEXP ans = PROTECT(allocVector(INTSXP, N));
  R_xlen_t first_double = 0;
  int * restrict ansp = INTEGER(ans);
  const SEXP * xp = STRING_PTR(x);

  for (R_xlen_t i = 0; i < N; ++i) {
    if (xp[i] == NA_STRING) {
      ansp[i] = NA_INTEGER;
      continue;
    }
    int n = length(xp[i]);
    const char * xi = CHAR(xp[i]);
    if (n == na_len && is_NA0(xi, na_string, na_len)) {
      ansp[i] = NA_INTEGER;
      continue;
    }
    int anspi = char2int0(xi, n);
    ansp[i] = anspi;
  }
  UNPROTECT(1);
  return ans;
}

double char2dblO(const char * x, int n, int opt) {
  double o = 0;
  int j = 0;
  bool is_negative = x[0] == '-';
  for (; j < n; ++j) {
    unsigned char xj = x[j];
    if (xj == '.') {
      break;
    }
    if (isdigit(xj)) {
      o *= 10;
      o += xj - '0';

    }
  }
  ++j;
  double ten = 0.1;
  for (; j < n; ++j) {
    unsigned char xj = x[j];
    if (isdigit(xj)) {
      o += ten * (xj - '0');
      ten /= 10.0;
    }
  }
  return o;
}

SEXP character2double(SEXP x, SEXP NaStrings, int option) {
  R_xlen_t N = xlength(x);
  SEXP ans = PROTECT(allocVector(REALSXP, N));
  double * restrict ansp = REAL(ans);
  const SEXP * xp = STRING_PTR(x);
  for (R_xlen_t i = 0; i < N; ++i) {
    const char * xi = CHAR(xp[i]);
    int n = length(xp[i]);
    ansp[i] = char2dblO(xi, n, option);
  }
  UNPROTECT(1);
  return ans;
}


SEXP C_character2integer(SEXP x, SEXP NaStrings, SEXP AllowDbl, SEXP Option) {
  const int option = asInteger(Option);
  const int allow_dbl = asInteger(AllowDbl);
  if (!isString(x)) {
    error("`x` was type '%s' but must be type character.", type2char(TYPEOF(x)));
  }
  if (NaStrings != R_NilValue && !isString(NaStrings)) {
    error("NaStrings was type '%s' but must be character (or NULL)", type2char(TYPEOF(NaStrings)));
  }
  R_xlen_t needsDoublex = needsDouble(x);
  if (needsDoublex) {
    if (allow_dbl == 0) {
      error("`allow_double = FALSE` but double is required at position %lld.", needsDoublex);
    }
    if (allow_dbl == 1) {
      return character2double(x, NaStrings, option);
    }
  }


  if (option == 0 && NaStrings == R_NilValue) {
    return str2int0_(x);
  }

  if (length(NaStrings) == 1) {
    const char * na_string = CHAR(STRING_ELT(NaStrings, 0));
    int na_len = length(STRING_ELT(NaStrings, 0));
    return str2int0(x, na_string, na_len, allow_dbl);
  }

  return R_NilValue;

}

static int width_dbl(double x, int d) {
  double ax = fabs(x);
  if (ax < 1) {
    if (d == 0) {
      return 1; // will be 0
    }
    return 2 + d; // '0' + '.' + d
  }
  int w = (x < 0); // '-'

  // if close to zero
  if (ax <= pow(0.1, d)) {
    return w;
  }
  int log10_ax = log10(ax);
  w += 1 + log10_ax + (log10_ax / 3); // front of '.', with bigmarks
  if (d > 0) {
    w += 1; // decimal point (not needed if d <= 0)
    w += d;
  }

  return w;
}

static SEXP dbl2string(double x, int d, const char bigmark) {
  if (ISNAN(x)) {
    char o[3] = {'N', 'A', '\0'};
    const char * oi = (const char *)o;
    return mkCharCE(oi, CE_UTF8);
  }
  if (x == R_PosInf) {
    char o[4] = {'I', 'n', 'f', '\0'};
    const char * oi = (const char *)o;
    return mkCharCE(oi, CE_UTF8);
  }
  if (x == R_NegInf) {
    char o[5] = {'-', 'I', 'n', 'f', '\0'};
    const char * oi = (const char *)o;
    return mkCharCE(oi, CE_UTF8);
  }
  const char digits[10] = "0123456789";
  int n = width_dbl(x, d);
  char o[n + 1];
  int j = 0;
  if (x < 0) {
    o[j++] = '-';
    x = -x;
  }
  if (x < 1) {
    // possibly redundant
    o[j++] = '0';
    if (d > 0) {
      o[j++] = '.';
    }
    double ten = 10;
    for (int k = 0; k < d; ++k) {
      int tenx = ten * x;
      int d0 = tenx % 10;
      o[j++] = digits[d0];
      ten *= 10;
    }
    o[j] = '\0';
    const char * oi = (const char *)o;
    return mkCharCE(oi, CE_UTF8);
  }
  uint64_t x64 = floor(x);
  int log10_ax = log10(x);
  uint64_t ten = pow(10, log10_ax);
  int n_commas = log10_ax / 3;
  int j_comma = (log10_ax % 3) + 1;

  while (ten >= 1) {
    int dd = (x64 / ten) % 10;
    o[j++] = digits[dd];
    x64 %= ten;
    ten /= 10;
    if (n_commas) {
      if (j_comma == j) {
        o[j++] = bigmark;
        --n_commas;
        j_comma += 4; // 3digits plus comma
      }
    }
  }

  if (d > 0) {
    o[j++] = '.';
    double ten = 10;
    for (int k = 0; k < d; ++k) {
      uint64_t tenx = ten * x;
      int d0 = tenx % 10;
      o[j++] = digits[d0];
      ten *= 10;
    }
  }
  o[j] = '\0';
  const char * oi = (const char *)o;
  return mkCharCE(oi, CE_UTF8);
}

SEXP C_comma_dbl(SEXP x, SEXP Digits, SEXP BigMark) {
  int bigmarki = asInteger(BigMark);
  const char bigmark = i2bigmark(bigmarki);
  R_xlen_t N = xlength(x);
  const int d = asInteger(Digits);
  if (d >= 1073741824 || d <= -1073741824) {
    error("digits = %d which is an unlikely high value", d);
  }
  if (!isReal(x)) {
    error("`x` was type '%s' but must be double.", type2char(TYPEOF(x)));
  }
  const double * xp = REAL(x);
  SEXP ans = PROTECT(allocVector(STRSXP, N));
  for (R_xlen_t i = 0; i < N; ++i) {
    SET_STRING_ELT(ans, i, dbl2string(xp[i], d, bigmark));
  }
  UNPROTECT(1);
  return ans;
}

SEXP C_comma(SEXP x, SEXP Digits, SEXP BigMark) {
  if (isReal(x)) {
    return C_comma_dbl(x, Digits, BigMark);
  }
  if (!isInteger(x)) {
    error("`x` must be type integer.");
  }
  R_xlen_t N = xlength(x);
  const int * xp = INTEGER(x);
  SEXP ans = PROTECT(allocVector(STRSXP, N));

  const char digits[10] = "0123456789";
  const char bigmark = i2bigmark(asInteger(BigMark));

  for (R_xlen_t i = 0; i < N; ++i) {
    int xpi = xp[i];
    bool negative = xpi < 0;
    int axpi = negative ? -xpi : xpi;
    if (negative) {
      if (axpi < 10) {
        char oi[3] = {'-', digits[axpi], '\0'};
        const char * coi = (const char *)oi;
        SEXP acoi = mkCharCE(coi, CE_UTF8);
        SET_STRING_ELT(ans, i, acoi);
        continue;
      }
      if (axpi < 100) {
        char oi[4] = {'-', digits[axpi / 10], digits[axpi % 10], '\0'};
        const char * coi = (const char *)oi;
        SEXP acoi = mkCharCE(coi, CE_UTF8);
        SET_STRING_ELT(ans, i, acoi);
        continue;
      }
      if (axpi < 1000) {
        char oi[5] = {'-', digits[axpi / 100], digits[(axpi / 10) % 10], digits[axpi % 10], '\0'};
        const char * coi = (const char *)oi;
        SEXP acoi = mkCharCE(coi, CE_UTF8);
        SET_STRING_ELT(ans, i, acoi);
        continue;
      }
      if (axpi < 10e3) {
        char oi[7] = {'-',
                      digits[axpi / 1000],
                      bigmark,
                      digits[(axpi / 100) % 10],
                            digits[(axpi / 10) % 10],
                                  digits[axpi % 10],
                                        '\0'};
        const char * coi = (const char *)oi;
        SEXP acoi = mkCharCE(coi, CE_UTF8);
        SET_STRING_ELT(ans, i, acoi);
        continue;
      }
      if (axpi < 100e3) {
        char oi[8] = {'-',
                      digits[(axpi / 10000) % 10],
                      digits[(axpi / 1000) % 10],
                            bigmark,
                            digits[(axpi / 100) % 10],
                                  digits[(axpi / 10) % 10],
                                        digits[axpi % 10],
                                              '\0'};
        const char * coi = (const char *)oi;
        SEXP acoi = mkCharCE(coi, CE_UTF8);
        SET_STRING_ELT(ans, i, acoi);
        continue;
      }
      if (axpi < 1e6) {
        char oi[9] = {'-',
                      digits[(axpi / 100000) % 10],
                      digits[(axpi / 10000) % 10],
                            digits[(axpi / 1000) % 10],
                                  bigmark,
                                  digits[(axpi / 100) % 10],
                                        digits[(axpi / 10) % 10],
                                              digits[axpi % 10],
                                                    '\0'};
        const char * coi = (const char *)oi;
        SEXP acoi = mkCharCE(coi, CE_UTF8);
        SET_STRING_ELT(ans, i, acoi);
        continue;
      }
      if (axpi < 1e7) {
        char oi[11] = {'-',
                       digits[(axpi / 1000000) % 10],
                       bigmark,
                       digits[(axpi / 100000) % 10],
                             digits[(axpi / 10000) % 10],
                                   digits[(axpi / 1000) % 10],
                                         bigmark,
                                         digits[(axpi / 100) % 10],
                                               digits[(axpi / 10) % 10],
                                                     digits[axpi % 10],
                                                           '\0'};
        const char * coi = (const char *)oi;
        SEXP acoi = mkCharCE(coi, CE_UTF8);
        SET_STRING_ELT(ans, i, acoi);
        continue;
      }
      if (axpi < 1e8) {
        char oi[12] = {'-',
                       digits[(axpi / 10000000) % 10],
                       digits[(axpi / 1000000) % 10],
                             bigmark,
                             digits[(axpi / 100000) % 10],
                                   digits[(axpi / 10000) % 10],
                                         digits[(axpi / 1000) % 10],
                                               bigmark,
                                               digits[(axpi / 100) % 10],
                                                     digits[(axpi / 10) % 10],
                                                           digits[axpi % 10],
                                                                 '\0'};
        const char * coi = (const char *)oi;
        SEXP acoi = mkCharCE(coi, CE_UTF8);
        SET_STRING_ELT(ans, i, acoi);
        continue;
      }
      if (axpi < 1e9) {
        char oi[13] = {'-',
                       digits[(axpi / 100000000) % 10],
                       digits[(axpi / 10000000) % 10],
                             digits[(axpi / 1000000) % 10],
                                   bigmark,
                                   digits[(axpi / 100000) % 10],
                                         digits[(axpi / 10000) % 10],
                                               digits[(axpi / 1000) % 10],
                                                     bigmark,
                                                     digits[(axpi / 100) % 10],
                                                           digits[(axpi / 10) % 10],
                                                                 digits[axpi % 10],
                                                                       '\0'};
        const char * coi = (const char *)oi;
        SEXP acoi = mkCharCE(coi, CE_UTF8);
        SET_STRING_ELT(ans, i, acoi);
        continue;
      }

      char oi[15] = {'-',
                     digits[(axpi / 1000000000) % 10],
                     bigmark,
                     digits[(axpi / 100000000) % 10],
                           digits[(axpi / 10000000) % 10],
                                 digits[(axpi / 1000000) % 10],
                                       bigmark,
                                       digits[(axpi / 100000) % 10],
                                             digits[(axpi / 10000) % 10],
                                                   digits[(axpi / 1000) % 10],
                                                         bigmark,
                                                         digits[(axpi / 100) % 10],
                                                               digits[(axpi / 10) % 10],
                                                                     digits[axpi % 10],
                                                                           '\0'};
      const char * coi = (const char *)oi;
      SEXP acoi = mkCharCE(coi, CE_UTF8);
      SET_STRING_ELT(ans, i, acoi);
      continue;
    }
    if (axpi < 10) {
      char oi[2] = {digits[axpi], '\0'};
      const char * coi = (const char *)oi;
      SEXP acoi = mkCharCE(coi, CE_UTF8);
      SET_STRING_ELT(ans, i, acoi);
      continue;
    }
    if (axpi < 100) {
      char oi[3] = {digits[axpi / 10], digits[axpi % 10], '\0'};
      const char * coi = (const char *)oi;
      SEXP acoi = mkCharCE(coi, CE_UTF8);
      SET_STRING_ELT(ans, i, acoi);
      continue;
    }
    if (axpi < 1000) {
      char oi[4] = {digits[axpi / 100], digits[(axpi / 10) % 10], digits[axpi % 10], '\0'};
      const char * coi = (const char *)oi;
      SEXP acoi = mkCharCE(coi, CE_UTF8);
      SET_STRING_ELT(ans, i, acoi);
      continue;
    }
    if (axpi < 10e3) {
      char oi[6] = {digits[axpi / 1000],
                    bigmark,
                    digits[(axpi / 100) % 10],
                    digits[(axpi / 10) % 10],
                    digits[axpi % 10],
                    '\0'};
      const char * coi = (const char *)oi;
      SEXP acoi = mkCharCE(coi, CE_UTF8);
      SET_STRING_ELT(ans, i, acoi);
      continue;
    }
    if (axpi < 100e3) {
      char oi[7] = {digits[(axpi / 10000) % 10],
                    digits[(axpi / 1000) % 10],
                    bigmark,
                    digits[(axpi / 100) % 10],
                          digits[(axpi / 10) % 10],
                                digits[axpi % 10],
                                      '\0'};
      const char * coi = (const char *)oi;
      SEXP acoi = mkCharCE(coi, CE_UTF8);
      SET_STRING_ELT(ans, i, acoi);
      continue;
    }
    if (axpi < 1e6) {
      char oi[8] = {digits[(axpi / 100000) % 10],
                    digits[(axpi / 10000) % 10],
                    digits[(axpi / 1000) % 10],
                    bigmark,
                    digits[(axpi / 100) % 10],
                    digits[(axpi / 10) % 10],
                    digits[axpi % 10],
                    '\0'};
      const char * coi = (const char *)oi;
      SEXP acoi = mkCharCE(coi, CE_UTF8);
      SET_STRING_ELT(ans, i, acoi);
      continue;
    }
    if (axpi < 1e7) {
      char oi[10] = {digits[(axpi / 1000000) % 10],
                    bigmark,
                    digits[(axpi / 100000) % 10],
                    digits[(axpi / 10000) % 10],
                    digits[(axpi / 1000) % 10],
                    bigmark,
                    digits[(axpi / 100) % 10],
                    digits[(axpi / 10) % 10],
                    digits[axpi % 10],
                    '\0'};
      const char * coi = (const char *)oi;
      SEXP acoi = mkCharCE(coi, CE_UTF8);
      SET_STRING_ELT(ans, i, acoi);
      continue;
    }
    if (axpi < 1e8) {
      char oi[11] = {digits[(axpi / 10000000) % 10],
                    digits[(axpi / 1000000) % 10],
                    bigmark,
                    digits[(axpi / 100000) % 10],
                    digits[(axpi / 10000) % 10],
                    digits[(axpi / 1000) % 10],
                    bigmark,
                    digits[(axpi / 100) % 10],
                    digits[(axpi / 10) % 10],
                    digits[axpi % 10],
                    '\0'};
      const char * coi = (const char *)oi;
      SEXP acoi = mkCharCE(coi, CE_UTF8);
      SET_STRING_ELT(ans, i, acoi);
      continue;
    }
    if (axpi < 1e9) {
      char oi[12] = {digits[(axpi / 100000000) % 10],
                    digits[(axpi / 10000000) % 10],
                    digits[(axpi / 1000000) % 10],
                    bigmark,
                    digits[(axpi / 100000) % 10],
                    digits[(axpi / 10000) % 10],
                    digits[(axpi / 1000) % 10],
                    bigmark,
                    digits[(axpi / 100) % 10],
                    digits[(axpi / 10) % 10],
                    digits[axpi % 10],
                    '\0'};
      const char * coi = (const char *)oi;
      SEXP acoi = mkCharCE(coi, CE_UTF8);
      SET_STRING_ELT(ans, i, acoi);
      continue;
    }

    char oi[14] = {digits[(axpi / 1000000000) % 10],
                   bigmark,
                   digits[(axpi / 100000000) % 10],
                   digits[(axpi / 10000000) % 10],
                   digits[(axpi / 1000000) % 10],
                    bigmark,
                   digits[(axpi / 100000) % 10],
                   digits[(axpi / 10000) % 10],
                   digits[(axpi / 1000) % 10],
                    bigmark,
                   digits[(axpi / 100) % 10],
                   digits[(axpi / 10) % 10],
                   digits[axpi % 10],
                   '\0'};
    const char * coi = (const char *)oi;
    SEXP acoi = mkCharCE(coi, CE_UTF8);
    SET_STRING_ELT(ans, i, acoi);
  }
  UNPROTECT(1);
  return ans;
}







