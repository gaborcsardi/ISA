
#include <Rdefines.h>
#include <R_ext/Rdynload.h>
#include <math.h>
  
SEXP beta_filter_up_vart(SEXP matrix, SEXP threshold) {
  
  SEXP result;
  long int nrow=INTEGER(GET_DIM(matrix))[0];
  long int ncol=INTEGER(GET_DIM(matrix))[1];
  long int i, j;
  double *data, mean, sd, absmax, eff_thr, eff_max;
  double thr;
  
  PROTECT(result=duplicate(matrix));
  
  data=REAL(result);
  for (i=0; i<ncol; i++) {
    
    thr=REAL(threshold)[i];

    /* column dependent thresholds, plus search 
       for (absolute) maximum */

    mean=0.0; sd=0.0; absmax=0.0;
    
    for (j=0; j<nrow; j++) {
      sd += j*pow(*data-mean, 2)/(j+1);
      mean += (*data-mean)/(j+1);
      if (*data > absmax) { absmax=*data; }
      ++data;
    }
    sd=sqrt(sd/(nrow-1));
    eff_thr= mean + thr * sd;
    eff_max = absmax;
  
    /* threshold + norm (1) */
    
    data -= nrow;

    for (j=0; j<nrow; j++) {
      if (*data <= eff_thr) {
	*data=0;
      } else {
	*data /= eff_max;
      }
      data++;
    }
  }
  
  UNPROTECT(1);
  return result;  
}    

SEXP beta_filter_down_vart(SEXP matrix, SEXP threshold) {
  
  SEXP result;
  long int nrow=INTEGER(GET_DIM(matrix))[0];
  long int ncol=INTEGER(GET_DIM(matrix))[1];
  long int i, j;
  double *data, mean, sd, absmax, eff_thr, eff_max;
  double thr, absdata;
  
  PROTECT(result=duplicate(matrix));
  
  data=REAL(result);
  for (i=0; i<ncol; i++) {
    
    thr=REAL(threshold)[i];

    /* column dependent thresholds, plus search 
       for (absolute) maximum */

    mean=0.0; sd=0.0; absmax=0.0;
    
    for (j=0; j<nrow; j++) {
      sd += j*pow(*data-mean, 2)/(j+1);
      mean += (*data-mean)/(j+1);
      absdata=fabs(*data);
      if (absdata > absmax) { absmax=absdata; }
      ++data;
    }
    sd=sqrt(sd/(nrow-1));
    eff_thr= mean - thr * sd;
    eff_max = absmax;
  
    /* threshold + norm (1) */
    
    data -= nrow;

    for (j=0; j<nrow; j++) {
      if (*data >= eff_thr) {
	*data=0;
      } else {
	*data /= eff_max;
      }
      data++;
    }
  }
  
  UNPROTECT(1);
  return result;  
}    

SEXP beta_filter_updown_vart(SEXP matrix, SEXP threshold) {

  SEXP result;
  long int nrow=INTEGER(GET_DIM(matrix))[0];
  long int ncol=INTEGER(GET_DIM(matrix))[1];
  long int i, j;
  double *data, mean, sd, absmax, eff_thr_up, eff_thr_down, tmp;
  double thr;
  
  PROTECT(result=duplicate(matrix));

  data=REAL(result);
  for (i=0; i<ncol; i++) {

    thr=REAL(threshold)[i];

    /* column dependent thresholds */

    mean=0.0; sd=0.0; absmax=0.0;

    for (j=0; j<nrow; j++) {
      sd += j*pow(*data-mean, 2)/(j+1);
      mean += (*data-mean)/(j+1);
      ++data;
    }
    sd=sqrt(sd/(nrow-1));
    eff_thr_up = mean + thr * sd;
    eff_thr_down = mean - thr * sd;

    /* threshold + search for maximum */

    data -= nrow;
    for (j=0; j<nrow; j++) {
      if (*data <= eff_thr_up && *data >= eff_thr_down) {
	*data=0;
      } else {
	tmp=fabs(*data); 
	if (tmp > absmax) { absmax=tmp; }
      }
      data++;
    }
    
    /* norm (1) */
    
    if (absmax != 0) {
      data -= nrow;
      for (j=0; j<nrow; j++) {
	*data /= absmax;
	data++;
      }
    }
  }
  
  UNPROTECT(1);
  return result;  
}  

static const R_CallMethodDef callMethods[]  = {
  {"beta_filter_up_vart", (DL_FUNC) &beta_filter_up_vart, 2},
  {"beta_filter_down_vart", (DL_FUNC) &beta_filter_up_vart, 2},
  {"beta_filter_updown_vart", (DL_FUNC) &beta_filter_up_vart, 2},
  {NULL, NULL, 0}
};

void R_init_isa2(DllInfo *dll) {
  R_registerRoutines(dll, NULL, callMethods, NULL, NULL);
  R_useDynamicSymbols(dll, FALSE);
  R_forceSymbols(dll, TRUE);
}
