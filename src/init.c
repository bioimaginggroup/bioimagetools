#include <R.h>
#include <R_ext/Rdynload.h>
#include "bioimagetools.h"

R_CMethodDef cMethods[] = {
  {"varfilter", (DL_FUNC)&varfilter, 7},
  {"maxfilter", (DL_FUNC)&maxfilter, 7},
  {"minfilter", (DL_FUNC)&minfilter, 7},
  {"segment_cem", (DL_FUNC)&segment_cem, 10},
  {"segment_cem2d", (DL_FUNC)&segment_cem2d, 10},
  {"segment_em", (DL_FUNC)&segment_em, 8},
  {"nearestClassDistancesClass", (DL_FUNC) &nearestClassDistancesClass, 8},
  NULL
};

void R_init_bioimagetools(DllInfo *info) {
  R_registerRoutines(info, cMethods, NULL, NULL, NULL);
  R_useDynamicSymbols(info, TRUE);
}

