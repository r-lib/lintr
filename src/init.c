#include "fixed_regex.h"
#include <R_ext/Rdynload.h>  // for R_registerRoutines and R_CallMethodDef

static const R_CallMethodDef call_methods[] = {
  {"is_not_regex", (DL_FUNC) &is_not_regex, 3},
  {NULL, NULL, 0}
};

void R_init_lintr(DllInfo *info) {
  R_registerRoutines(info,
                     /*.C*/ NULL,
                     /*.Call*/ call_methods,
                     /*.Fortran*/ NULL,
                     /*.External*/ NULL);
  R_useDynamicSymbols(info, FALSE);
  R_forceSymbols(info, TRUE);
}

