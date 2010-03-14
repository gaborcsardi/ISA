//     ExpressionView - A package to visualize biclusters
//     Copyright (C) 2009 Computational Biology Group, University of Lausanne
// 
//     This program is free software: you can redistribute it and/or modify
//     it under the terms of the GNU General Public License as published by
//     the Free Software Foundation, either version 3 of the License, or
//     (at your option) any later version.
// 
//     This program is distributed in the hope that it will be useful,
//     but WITHOUT ANY WARRANTY; without even the implied warranty of
//     MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
//     GNU General Public License for more details.
// 
//     You should have received a copy of the GNU General Public License
//     along with this program.  If not, see <http://www.gnu.org/licenses/>.

#include "Rdefines.h"
#include "R_ext/Rdynload.h"

#include <vector>
#include "orderclusters.h"

extern "C" {
	SEXP orderClusters(SEXP, SEXP, SEXP, SEXP);
}

// register functions
R_CallMethodDef callMethods[] = {
    { "orderClusters", (DL_FUNC)&orderClusters, 4 },
    { NULL, NULL, 0 }
};

void R_init_pretty_matrix(DllInfo *dll) {
    R_registerRoutines(dll, NULL, callMethods, NULL, NULL);
}

// ordering function
SEXP orderClusters(SEXP _data, SEXP _order, SEXP _maxtime, SEXP _debug) {

	int nrows = INTEGER(GET_DIM(_data))[0];
	int ncolumns = INTEGER(GET_DIM(_data))[1];
	int maxtime = INTEGER(_maxtime)[0];
	int debug = INTEGER(_debug)[0];
	
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
