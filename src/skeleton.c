#include <R.h>
#include <Rinternals.h>
#include <stdlib.h> // for NULL
#include <Rmath.h>
#include <R_ext/Rdynload.h>

void F77_NAME(s_brook90_f)(double *siteparam, double *climveg, double *param, double *pdur,
    double *soil_materials, double *soil_nodes, double *precdat,
    int *pr, int *timer, int *error, double *output, double *output_layer);

extern SEXP s_brook90_c(SEXP siteparam, SEXP climveg, SEXP param, SEXP pdur, SEXP soil_materials,
    SEXP soil_nodes, SEXP precdat, SEXP pr, SEXP timer, SEXP n_days, SEXP n_lays, SEXP n_pint){


    int n; // total length of layer output

    const int n_cols_out = 47; // number of colums for basic output
    const int n_cols_outlay = 14; // number of colums for layer output

    const int n_d_c = INTEGER(n_days)[0];
    const int n_l_c = INTEGER(n_lays)[0];
    const int n_p_c = INTEGER(n_pint)[0];

    SEXP error = PROTECT( allocVector(INTSXP, 1) );
    SEXP output = PROTECT( allocMatrix(REALSXP, n_d_c * n_p_c, n_cols_out) );

    n = n_d_c * 14 * n_l_c * n_p_c;
    SEXP output_layer = PROTECT( allocVector(REALSXP, n) );
    SEXP dims = PROTECT( allocVector(INTSXP, 3) );

    INTEGER(dims)[0] = n_d_c * n_p_c;
    INTEGER(dims)[1] = n_cols_outlay;
    INTEGER(dims)[2] = n_l_c;
    setAttrib( output_layer, R_DimSymbol, dims);

    F77_CALL(s_brook90_f)(REAL(siteparam), REAL(climveg), REAL(param), REAL(pdur), REAL(soil_materials),
        REAL(soil_nodes), REAL(precdat), INTEGER(pr), INTEGER(timer), INTEGER(error), REAL(output), REAL(output_layer));

    SEXP out_full = PROTECT( allocVector(VECSXP, 3) );
    SET_VECTOR_ELT(out_full, 0, error);
    SET_VECTOR_ELT(out_full, 1, output);
    SET_VECTOR_ELT(out_full, 2, output_layer);

    UNPROTECT(5); // 4?

    return out_full;
}

static const R_CallMethodDef CallEntries[] = {
  {"s_brook90_c",   (DL_FUNC) &s_brook90_c,   12},
  {NULL,         NULL,                0}
};

void R_init_LWFBrook90R(DllInfo *dll)
{
    R_registerRoutines(dll, NULL, CallEntries, NULL, NULL);
    R_useDynamicSymbols(dll, FALSE);

    R_RegisterCCallable("LWFBrook90R", "s_brook90_c",  (DL_FUNC) &s_brook90_c);
}
