
#include <Rdefines.h>
#include <math.h>

/* TODO: handle empty matrix */

SEXP beta_mean_sd(SEXP matrix) {
  
  SEXP result, res_mean, res_std;
  long int nrow=INTEGER(GET_DIM(matrix))[0];
  long int ncol=INTEGER(GET_DIM(matrix))[1];
  long int i, j;
  
  PROTECT(result=NEW_LIST(2));
  SET_VECTOR_ELT(result, 0, NEW_NUMERIC(ncol));
  SET_VECTOR_ELT(result, 1, NEW_NUMERIC(ncol));
  res_mean=VECTOR_ELT(result, 0);
  res_std=VECTOR_ELT(result, 1);

  for (i=0; i<ncol; i++) {
    double *data = REAL(matrix)+i*nrow;
    double mean=data[0], std=0;
    for (j=1; j<nrow; j++) {
      std += j*pow((data[j]-mean), 2)/(j+1);
      mean += (data[j]-mean)/(j+1);
    }
    std = sqrt(std / (nrow-1));
    REAL(res_mean)[i]=mean;
    REAL(res_std)[i]=std;
  }
  
  UNPROTECT(1);
  return result;
}

/*

Test it in R:

R
library(beta)
M <- matrix(runif(1000*1000), 1000, 1000)
system.time(ms <- .Call("beta_mean_sd", M, PACKAGE="beta"))
system.time(m <- apply(M, 2, mean))
system.time(s <- apply(M, 2, sd))
max(abs(ms[[2]] - s ))
max(abs(ms[[1]] - m ))

*/

SEXP beta_threshold_cols_up(SEXP matrix, SEXP thr) {
  
  SEXP result;
  long int nrow=INTEGER(GET_DIM(matrix))[0];
  long int ncol=INTEGER(GET_DIM(matrix))[1];
  long int i, j;
  double *data, threshold;
  
  PROTECT(result=duplicate(matrix));
  data=REAL(result);
  for (i=0; i<ncol; i++) {
    threshold=REAL(thr)[i];
    for (j=0; j<nrow; j++) {
      if (*data <= threshold) {
	*data=0;
      }
      data++;
    }
  }
  
  UNPROTECT(1);
  return result;
}

/* 

Test in R:

R
library(beta)
M <- matrix(runif(10000*1000), 10000, 1000)-0.5
t <- runif(ncol(M))*0.2-0.1
system.time(M2 <- .Call("beta_threshold_cols_up", M, t, PACKAGE="beta"))

f <- function() {
  M3 <- M
  for (i in 1:ncol(M)) {
    M3[,i] <- (M[,i] > t[i]) * M3[,i]
  }
}

system.time(f())

M3 <- M
for (i in 1:ncol(M)) {
  M3[,i] <- (M[,i] > t[i]) * M3[,i]
}

max(abs(M3-M2))

*/

SEXP beta_threshold_cols_updown(SEXP matrix, SEXP up, SEXP down) {
 
  SEXP result;
  long int nrow=INTEGER(GET_DIM(matrix))[0];
  long int ncol=INTEGER(GET_DIM(matrix))[1];
  long int i, j;
  double *data, thr_up, thr_down;
  
  PROTECT(result=duplicate(matrix));
  data=REAL(result);
  for (i=0; i<ncol; i++) {
    thr_up=REAL(up)[i];
    thr_down=REAL(down)[i];
    for (j=0; j<nrow; j++) {
      if (*data <= thr_up && *data >= thr_down) {
	*data=0;
      }
      data++;
    }
  }
  
  UNPROTECT(1);
  return result;
}

/* 

Test in R:

R
library(beta)
M <- matrix(runif(10000*1000), 10000, 1000)-0.5
t <- runif(ncol(M))*0.2-0.1
t2 <- runif(ncol(M))*0.2-0.3 
system.time(M2 <- .Call("beta_threshold_cols_updown", M, t, t2, PACKAGE="beta"))

f <- function() {
  M3 <- M
  for (i in 1:ncol(M)) {
    M3[,i] <- (M[,i] > t[i] | M[,i] < t2[i]) * M3[,i]
  }
}

system.time(f())

M3 <- M
for (i in 1:ncol(M)) {
  M3[,i] <- (M[,i] > t[i] | M[,i] < t2[i]) * M3[,i]
}

max(abs(M3-M2))

*/

SEXP beta_norm_col_inf(SEXP matrix) {
  
  SEXP result;
  long int nrow=INTEGER(GET_DIM(matrix))[0];
  long int ncol=INTEGER(GET_DIM(matrix))[1];
  long int i, j;
  double *data, absmax, tmp;
  
  PROTECT(result=duplicate(matrix));
  data=REAL(result);
  for (i=0; i<ncol; i++) {
    absmax=fabs(*data);
    for (j=0; j<nrow; j++) {
      tmp=fabs(*data);
      if (tmp > absmax) { absmax=tmp; }
      ++data;
    }
    if (absmax != 0) {
      data -= nrow;
      for (j=0; j<nrow; j++) {
	*data /= absmax;
	++data;
      }
    }
  }
  
  UNPROTECT(1);
  return result;
}

/* 

Test in R:

R
library(beta)
M <- matrix(runif(10000*1000), 10000, 1000)
system.time(M2 <- .Call("beta_norm_col_inf", M, PACKAGE="beta"))

norm <- function(x) {
  tmp <- max(abs(x))
  if (tmp != 0) {
    x/tmp
  } else {
    x
  }
}

system.time(M3 <- apply(M, 2, norm))

max(abs(M2-M3))

*/

SEXP beta_norm_col_2(SEXP matrix) {
  
  SEXP result;
  long int nrow=INTEGER(GET_DIM(matrix))[0];
  long int ncol=INTEGER(GET_DIM(matrix))[1];
  long int i, j;
  double *data, sum2, tmp;
  
  PROTECT(result=duplicate(matrix));
  data=REAL(result);
  for (i=0; i<ncol; i++) {
    sum2=0.0;
    for (j=0; j<nrow; j++) {
      sum2 += (*data) * (*data);
      ++data;
    }
    if (sum2 != 0) {
      data -= nrow;
      sum2 = sqrt(sum2);
      for (j=0; j<nrow; j++) {
	*data /= sum2;
	++data;
      }
    }
  }
  
  UNPROTECT(1);
  return result;
}

/* 

Test in R:

R
library(beta)
M <- matrix(runif(10000*1000), 10000, 1000)
system.time(M2 <- .Call("beta_norm_col_2", M, PACKAGE="beta"))

norm2 <- function(x) {
  tmp <- sqrt(sum(x^2))
  if (tmp != 0) {
    x/tmp
  } else {
    x
  }
}

system.time(M3 <- apply(M, 2, norm2))

max(abs(M2-M3))

*/

/* ************************************************ */

SEXP beta_filter_up(SEXP matrix, SEXP threshold) {
  
  SEXP result;
  long int nrow=INTEGER(GET_DIM(matrix))[0];
  long int ncol=INTEGER(GET_DIM(matrix))[1];
  long int i, j;
  double *data, mean, sd, absmax, eff_thr, eff_max;
  double thr=REAL(threshold)[0];
  
  PROTECT(result=duplicate(matrix));
  
  data=REAL(result);
  for (i=0; i<ncol; i++) {

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
  
/*

R 

library(beta)

g <- matrix(runif(10000*1000), 10000, 1000)-0.5
system.time(res <- .Call("beta_filter_up", g, 0.2, PACKAGE="beta"))

  filter <- function(x, t, down) {
    x <- .Call("beta_norm_col_2", x, PACAKGE="beta")
    mx.sdx <- .Call("beta_mean_sd", x, PACKAGE="beta")
    ups <- mx.sdx[[1]] + t*mx.sdx[[2]]
    if (down) {
      dws <- mx.sdx[[1]] - t*mx.sdx[[2]]
      x <- .Call("beta_threshold_cols_updown", x, ups, dws, PACKAGE="beta")
    } else {
      x <- .Call("beta_threshold_cols_up", x, ups, PACKAGE="beta")
    }    
    x <- .Call("beta_norm_col_inf", x, PACKAGE="beta")
    x
  }

system.time(res2 <- filter(g, 0.2, FALSE))

max(abs(res-res2))

*/

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

SEXP beta_filter_updown(SEXP matrix, SEXP threshold) {
  
  SEXP result;
  long int nrow=INTEGER(GET_DIM(matrix))[0];
  long int ncol=INTEGER(GET_DIM(matrix))[1];
  long int i, j;
  double *data, mean, sd, absmax, eff_thr_up, eff_thr_down, tmp;
  double thr=REAL(threshold)[0];
  
  PROTECT(result=duplicate(matrix));

  data=REAL(result);
  for (i=0; i<ncol; i++) {

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

/*

R 

library(beta)

g <- matrix(runif(10000*1000), 10000, 1000)-0.5
system.time(res <- .Call("beta_filter_updown", g, 0.2, PACKAGE="beta"))

  filter <- function(x, t, down) {
#    x <- .Call("beta_norm_col_2", x, PACAKGE="beta")
    mx.sdx <- .Call("beta_mean_sd", x, PACKAGE="beta")
    ups <- mx.sdx[[1]] + t*mx.sdx[[2]]
    if (down) {
      dws <- mx.sdx[[1]] - t*mx.sdx[[2]]
      x <- .Call("beta_threshold_cols_updown", x, ups, dws, PACKAGE="beta")
    } else {
      x <- .Call("beta_threshold_cols_up", x, ups, PACKAGE="beta")
    }    
    x <- .Call("beta_norm_col_inf", x, PACKAGE="beta")
    x
  }

system.time(res2 <- filter(g, 0.2, TRUE))

max(abs(res-res2))

*/

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

SEXP beta_filter_o_up(SEXP matrix, SEXP threshold) {
  
  SEXP result;
  long int nrow=INTEGER(GET_DIM(matrix))[0];
  long int ncol=INTEGER(GET_DIM(matrix))[1];
  long int i, j;
  double *data, amax2;
  double thr=REAL(threshold)[0];
  
  PROTECT(result=duplicate(matrix));
  
  data=REAL(result);
  for (i=0; i<ncol; i++) {
    
    /* threshold, find the maximum, count positive elements */
    
    amax2=0.0;
    for (j=0; j<nrow; j++) {
      if (*data <= thr) {
	*data = 0.0;
      } else { 
	double tmp=fabs(*data);
	if (tmp > amax2) { amax2=tmp; }
      }
      ++data;
    }
    
    /* normalize (1) */
    
    if (amax2 != 0.0 && amax2 != 1.0) {
      data -= nrow;
      for (j=0; j<nrow; j++) {
	*data /= amax2;
	++data;
      }
    }
    
  }
  
  UNPROTECT(1);
  return result;
}

/*

R 

library(beta)

g <- matrix(runif(10000*1000), 10000, 1000)-0.5
system.time(res <- .Call("beta_filter_o_up", g, 0.2, PACKAGE="beta"))

  norm1 <- function(v) {
    am <- max(abs(v))
    if (am != 0 && am != 1.0)
      v/am
    else
      v
  }

  filter <- function(x, t, down) {
      if (down) {
        x[ -abs(t) < x & x < abs(t) ] <- 0
        x <- apply(x, 2, norm1)
      } else {
        x[ x < abs(t) ] <- 0
        x <- apply(x, 2, norm1)
        x <- list(x, apply(x>0, 2, sum))
      }
    x
  }

system.time(res2 <- filter(g, 0.2, FALSE))

max(abs(res[[1]]-res2[[1]]))
max(abs(res[[2]]-res2[[2]]))

*/

SEXP beta_filter_o_updown(SEXP matrix, SEXP threshold) {
  
  SEXP result;
  long int nrow=INTEGER(GET_DIM(matrix))[0];
  long int ncol=INTEGER(GET_DIM(matrix))[1];
  long int i, j;
  double *data, amax2;
  double thr=REAL(threshold)[0];
  
  PROTECT(result=duplicate(matrix));
  
  data=REAL(result);
  for (i=0; i<ncol; i++) {

    /* threshold, find the maximum */
    
    amax2=0.0;
    for (j=0; j<nrow; j++) {
      if (-thr <= *data && *data <= thr) {
	*data = 0.0;
      } else {
	double tmp=fabs(*data);
	if (tmp>amax2) { amax2=tmp; }
      }
      ++data;
    }
    
    /* normalize (1) */
    
    if (amax2 != 0.0 && amax2 != 1.0) {
      data -= nrow;
      for (j=0; j<nrow; j++) {
	*data /= amax2;
	++data;
      }
    }
    
  }
    
  UNPROTECT(1);
  return result;
}

/*

R 

library(beta)

g <- matrix(runif(10000*1000), 10000, 1000)-0.5
system.time(res <- .Call("beta_filter_o_updown", g, 0.2, PACKAGE="beta"))

  norm1 <- function(v) {
    am <- max(abs(v))
    if (am != 0 && am != 1.0)
      v/am
    else
      v
  }

  filter <- function(x, t, down) {
      if (down) {
        x[ -abs(t) < x & x < abs(t) ] <- 0
        x <- apply(x, 2, norm1)
      } else {
        x[ x < abs(t) ] <- 0
        x <- apply(x, 2, norm1)
        x <- list(x, apply(x>0, 2, sum))
      }
    x
  }

system.time(res2 <- filter(g, 0.2, TRUE))

max(abs(res-res2))

*/

SEXP beta_filter_ob_up(SEXP matrix, SEXP threshold) {
  
  SEXP result, res_mat, res_sums;
  long int nrow=INTEGER(GET_DIM(matrix))[0];
  long int ncol=INTEGER(GET_DIM(matrix))[1];
  long int i, j;
  double *data;
  double thr=REAL(threshold)[0];
  
  PROTECT(result=duplicate(matrix));

  data=REAL(result);
  for (i=0; i<ncol; i++) {
    
    /* threshold, count the positive elements */
    
    for (j=0; j<nrow; j++) {
      if (*data <= thr) {
	*data = 0.0;
      } else {
	*data = 1.0;
      }
      ++data;
    }
    
  }
  
  UNPROTECT(1);
  return result;
}

/*

R 

library(beta)

g <- matrix(runif(10000*1000), 10000, 1000)-0.5
system.time(res <- .Call("beta_filter_ob_up", g, 0.2, PACKAGE="beta"))

      filter <- function(x, t, down) {
        if (down) {
          x[ -abs(t) < x & x < abs(t) ] <- 0
          x[] <- sign(x)
        } else {
          x[ x < abs(t) ] <- 0
          x[] <- sign(x)
          x <- list(x, apply(x, 2, sum))
        }
        x
      }

system.time(res2 <- filter(g, 0.2, FALSE))

max(abs(res[[1]]-res2[[1]]))
max(abs(res[[2]]-res2[[2]]))

*/

SEXP beta_filter_ob_updown(SEXP matrix, SEXP threshold) {
  
  SEXP result;
  long int nrow=INTEGER(GET_DIM(matrix))[0];
  long int ncol=INTEGER(GET_DIM(matrix))[1];
  long int nelem=nrow*ncol;
  long int i, j;
  double *data;
  double thr=REAL(threshold)[0];
  
  PROTECT(result=duplicate(matrix));
  
  data=REAL(result);
  for (i=0; i<nelem; i++) {
    if (*data < -thr) {
      *data = -1.0;
    } else if (*data > thr) { 
      *data = 1.0;
    } else {
      *data = 0.0;
    }
    ++data;
  }
  
  UNPROTECT(1);
  return result;
}

/*

R 

library(beta)

g <- matrix(runif(10000*1000), 10000, 1000)-0.5
system.time(res <- .Call("beta_filter_ob_updown", g, 0.2, PACKAGE="beta"))

      filter <- function(x, t, down) {
        if (down) {
          x[ -abs(t) < x & x < abs(t) ] <- 0
          x[] <- sign(x)
        } else {
          x[ x < abs(t) ] <- 0
          x[] <- sign(x)
          x <- list(x, apply(x, 2, sum))
        }
        x
      }

system.time(res2 <- filter(g, 0.2, TRUE))

max(abs(res-res2))

*/

SEXP beta_filter_up_predef(SEXP matrix, SEXP threshold) {
  
  SEXP result;
  long int nrow=INTEGER(GET_DIM(matrix))[0];
  long int ncol=INTEGER(GET_DIM(matrix))[1];
  long int i, j;
  double thr=REAL(threshold)[0];
  
  PROTECT(result=duplicate(matrix));
  
  /* TODO */
 
  UNPROTECT(1);
  return result;
}

SEXP beta_filter_updown_predef(SEXP matrix, SEXP threshold) {

  SEXP result;
  long int nrow=INTEGER(GET_DIM(matrix))[0];
  long int ncol=INTEGER(GET_DIM(matrix))[1];
  long int i, j;
  double thr=REAL(threshold)[0];
  
  PROTECT(result=duplicate(matrix));
  
  /* TODO */
 
  UNPROTECT(1);
  return result;
}

SEXP beta_filter_up_vart_nn(SEXP matrix, SEXP threshold, SEXP pmode) {
  
  SEXP result;
  long int nrow=INTEGER(GET_DIM(matrix))[0];
  long int ncol=INTEGER(GET_DIM(matrix))[1];
  long int i, j;
  double *data, mean, sd, eff_thr, thr;
  int mode=INTEGER(pmode)[0];
  
  PROTECT(result=duplicate(matrix));
  
  data=REAL(result);
  for (i=0; i<ncol; i++) {
    
    thr=REAL(threshold)[i];
    
    /* column dependent threshold */
    
    mean=0.0; sd=0.0;
    
    for (j=0; j<nrow; j++) {
      sd += j*pow(*data-mean, 2)/(j+1);
      mean += (*data-mean)/(j+1);
      ++data;
    }
    sd=sqrt(sd/(nrow-1));
    eff_thr= mean + thr * sd;
    
    /* threshold + divide by sd */
    
    data -= nrow;

    for (j=0; j<nrow; j++) {
      if (*data <= eff_thr) {
	*data=0;
      } else {
	if (mode==0) {
	  *data = (*data-mean)/sd;
	} else if (mode==1) {
	  *data = (*data-eff_thr)/sd;
	}
      }
      data++;
    }
  }
  
  UNPROTECT(1);
  return result;
}

SEXP beta_filter_down_vart_nn(SEXP matrix, SEXP threshold, SEXP pmode) {
  
  SEXP result;
  long int nrow=INTEGER(GET_DIM(matrix))[0];
  long int ncol=INTEGER(GET_DIM(matrix))[1];
  long int i, j;
  double *data, mean, sd, eff_thr, thr;
  int mode=INTEGER(pmode)[0];
  
  PROTECT(result=duplicate(matrix));
  
  data=REAL(result);
  for (i=0; i<ncol; i++) {
    
    thr=REAL(threshold)[i];
    
    /* column dependent threshold */
    
    mean=0.0; sd=0.0;
    
    for (j=0; j<nrow; j++) {
      sd += j*pow(*data-mean, 2)/(j+1);
      mean += (*data-mean)/(j+1);
      ++data;
    }
    sd=sqrt(sd/(nrow-1));
    eff_thr= mean - thr * sd;
    
    /* threshold + divide by sd */
    
    data -= nrow;

    for (j=0; j<nrow; j++) {
      if (*data >= eff_thr) {
	*data=0;
      } else {
	if (mode==0) {
	  *data = (*data-mean)/sd;
	} else if (mode==1) {
	  *data = (*data-eff_thr)/sd;
	}
      }
      data++;
    }
  }
  
  UNPROTECT(1);
  return result;
}

SEXP beta_filter_updown_vart_nn(SEXP matrix, SEXP threshold, SEXP pmode) {

  SEXP result;
  long int nrow=INTEGER(GET_DIM(matrix))[0];
  long int ncol=INTEGER(GET_DIM(matrix))[1];
  long int i, j;
  double *data, mean, sd, eff_thr_up, eff_thr_down;
  double thr;
  int mode=INTEGER(pmode)[0];
  
  PROTECT(result=duplicate(matrix));

  data=REAL(result);
  for (i=0; i<ncol; i++) {

    thr=REAL(threshold)[i];

    /* column dependent thresholds */

    mean=0.0; sd=0.0;

    for (j=0; j<nrow; j++) {
      sd += j*pow(*data-mean, 2)/(j+1);
      mean += (*data-mean)/(j+1);
      ++data;
    }
    sd=sqrt(sd/(nrow-1));
    eff_thr_up = mean + thr * sd;
    eff_thr_down = mean - thr * sd;

    /* threshold, divide by sd */

    data -= nrow;
    for (j=0; j<nrow; j++) {
      if (*data <= eff_thr_up && *data >= eff_thr_down) {
	*data=0;
      } else {
	if (mode==0) {
	  *data = (*data-mean)/sd;
	} else if (mode==1) {
	  if (*data <= eff_thr_down) {
	    *data = (*data-eff_thr_down)/sd;
	  } else {
	    *data = (*data-eff_thr_up)/sd;
	  }
	}
      }
      data++;
    }
  }    

  
  UNPROTECT(1);
  return result;  
}  
  
