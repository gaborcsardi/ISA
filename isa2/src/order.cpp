#include <Rdefines.h>

extern "C" {
	SEXP order(SEXP, SEXP, SEXP);
}

#include <vector>
#include <orderclusters.h>

using namespace std;

SEXP order(SEXP isaresults, SEXP debuglevel, SEXP timelimit) {

	int nrows = INTEGER(GET_DIM(isaresults))[0];
	int ncolumns = INTEGER(GET_DIM(isaresults))[1];
	int debug = (int) REAL(debuglevel)[0];
	int maxtime = (int) REAL(timelimit)[0];
	
	if ( debug > 0 ) {
		printf("dimensions: %d x %d\n", nrows, ncolumns);
	}
	
	vector<vector<int> > data;
	data.resize(nrows);
	
	for ( int row = 0; row < nrows; row++ ) {
		data[row].resize(ncolumns);
		for ( int column = 0; column < ncolumns; column++ ) {
			int temp = (int) REAL(isaresults)[row + nrows * column];
			data[row][column] = temp;
		}
	}
	
	Clusters clusters(data);
	clusters.debug = debug;
	clusters.maxtime = maxtime;
	
	if ( debug > 0 ) { 
		clusters.output();
	}
	
	clusters.simplify();
	if ( debug > 0 ) { 
		clusters.output();
	}

	clusters.prearrange();
	if ( debug > 0 ) { 
		clusters.output();
	}
	
	clusters.arrange();
	if ( debug > 0 ) { 
		clusters.output();
	}

	clusters.complexify();
	if ( debug > 0 ) { 
		clusters.output();
	}
	
		
	int nProtected = 0;
	
	SEXP result;
	PROTECT(result = allocVector(INTSXP, nrows));
	++nProtected;
	// fill map
	for ( int row = 0; row < nrows; row++ ) {
		INTEGER(result)[row] = clusters.order[row] + 1;
	}

	UNPROTECT(nProtected);
	return result;

}    
