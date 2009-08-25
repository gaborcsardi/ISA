#include <Rdefines.h>

extern "C" {
	SEXP orderClusters(SEXP, SEXP, SEXP, SEXP);
}

#include <vector>
#include <orderclusters.h>

using namespace std;

SEXP orderClusters(SEXP _data, SEXP _order, SEXP _debug, SEXP _maxtime) {

	int nrows = INTEGER(GET_DIM(_data))[0];
	int ncolumns = INTEGER(GET_DIM(_data))[1];
	int debug = INTEGER(_debug)[0];
	int maxtime = INTEGER(_maxtime)[0];
	
	if ( debug > 0 ) {
		Rprintf("dimensions: %d x %d\n", nrows, ncolumns);
	}
	
	vector<int> order;
	order.resize(nrows);
		
	vector<vector<int> > data;
	data.resize(nrows);

	for ( int row = 0; row < nrows; row++ ) {
		order[row] = INTEGER(_order)[row] - 1;
		data[row].resize(ncolumns);
		for ( int column = 0; column < ncolumns; column++ ) {
			int temp = INTEGER(_data)[row + nrows * column];
			data[row][column] = temp;
		}
	}
	
	Clusters clusters(data, order);
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
	PROTECT(result = allocVector(INTSXP, nrows + 1));
	++nProtected;
	// fill map
	for ( int row = 0; row < nrows; row++ ) {
		INTEGER(result)[row] = clusters.order[row] + 1;
	}
	INTEGER(result)[nrows] = clusters.status;
	
	UNPROTECT(nProtected);
	return result;

}    
