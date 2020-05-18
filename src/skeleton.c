#include <R.h>
#include <Rinternals.h>
#include <stdlib.h> // for NULL
#include <Rmath.h>
#include <R_ext/Rdynload.h>

void F77_NAME(s_brook90_f)(double *siteparam, double *climveg, double *param, double *pdur,
    double *soil_materials, double *soil_nodes, double *precdat,
    int *pr, double *output_day, double *output_layer);

extern SEXP s_brook90_c(SEXP siteparam, SEXP climveg, SEXP param, SEXP pdur, SEXP soil_materials,
    SEXP soil_nodes, SEXP precdat, SEXP pr, SEXP n_m, SEXP n_l){

    int n;

    const int n_m_c = INTEGER(n_m)[0];
    const int n_l_c = INTEGER(n_l)[0];

    SEXP output_day = PROTECT( allocMatrix(REALSXP, n_m_c, 39) );

    n = n_m_c * 16 * n_l_c;
    SEXP output_layer = PROTECT( allocVector(REALSXP, n) );
    SEXP dims = PROTECT( allocVector(INTSXP, 3) );

    INTEGER(dims)[0] = n_m_c;
    INTEGER(dims)[1] = 16;
    INTEGER(dims)[2] = n_l_c;
    setAttrib( output_layer, R_DimSymbol, dims);

    F77_CALL(s_brook90_f)(REAL(siteparam), REAL(climveg), REAL(param), REAL(pdur), REAL(soil_materials),
        REAL(soil_nodes), REAL(precdat), LOGICAL(pr), REAL(output_day), REAL(output_layer));

    SEXP out_full = PROTECT( allocVector(VECSXP, 2) );
    SET_VECTOR_ELT(out_full, 0, output_day);
    SET_VECTOR_ELT(out_full, 1, output_layer);

    UNPROTECT(4);

    return out_full;
}

static const R_CallMethodDef CallEntries[] = {
  {"s_brook90_c",   (DL_FUNC) &s_brook90_c,   10},
  {NULL,         NULL,                0}
};

void R_init_LWFBrook90R(DllInfo *dll)
{
    R_registerRoutines(dll, NULL, CallEntries, NULL, NULL);
    R_useDynamicSymbols(dll, FALSE);

    R_RegisterCCallable("LWFBrook90R", "s_brook90_c",  (DL_FUNC) &s_brook90_c);
}
