#include <vector>
#include <set>
#include <map>
#include <algorithm>
#include <numeric>
#include <string>

#include "orderclusters.h"

using namespace std;

int Clusters::random(int x, int y) {
	return rand() % (y-x+1) + x;
}

int Clusters::elapsedtime() {
	return (clock() - starttime) / CLOCKS_PER_SEC;
}

Clusters::Clusters(vector<vector<int> > _matrix) {
	
	starttime = clock();
	
	matrix = _matrix;
	nSlots = matrix.size();
	nClusters = matrix[0].size();

	order.resize(nSlots);
	for ( int slot = 0; slot < nSlots; slot++ ) {
		order[slot] = slot;
	}
	
	multiplicity.resize(nSlots, 1);
	optimallength.resize(nClusters);
		
};

Clusters::Clusters(int _nClusters, int _nSlots) {

	starttime = clock();

	nClusters = _nClusters;
	nSlots = _nSlots;
	
	matrix.resize(nSlots);
	for ( int slot = 0; slot < nSlots; slot++ ) {
		matrix[slot].resize(nClusters, 0);
	}

	order.resize(nSlots);
	for ( int slot = 0; slot < nSlots; slot++ ) {
		order[slot] = slot;
	}
	
	multiplicity.resize(nSlots, 1);
	optimallength.resize(nClusters);
	
}

void Clusters::orderedsample() {

	if ( debug > 0 ) {
		printf("orderedsample()\n");
	}

	for ( int cluster = 0; cluster < nClusters; cluster++ ) {
		int left = random(0, nSlots-1);
		int right = random(left, nSlots-1);	
		for ( int slot = left; slot <= right; slot++ ) {
			matrix[slot][cluster] = 1;
		}
		optimallength[cluster] = right - left + 1;
	}
	
}

void Clusters::randomsample() {

	if ( debug > 0 ) {
		printf("randomsample()\n");
	}

	for ( int cluster = 0; cluster < nClusters; cluster++ ) {
		for ( int slot = 0; slot < nSlots; slot++ ) {
			if ( random(1,10) > 6 ) {
				matrix[slot][cluster] = 1;
			}
		}
	}
	
};

void Clusters::complexify() {
	
	if ( debug > 0 ) {
		printf("complexify()\n");
	}
	
	vector<int> neworder;
	for ( int slot = 0; slot < nSlots; slot++ ) {
		for ( set<int>::iterator slotp = redundantslots[order[slot]].begin(); slotp != redundantslots[order[slot]].end(); slotp++ ) {
			neworder.push_back(*slotp);
		}
	}
	if ( redundantslots.size() > nSlots ) {
		for ( set<int>::iterator slotp = redundantslots[order[nSlots]].begin(); slotp != redundantslots[order[nSlots]].end(); slotp++ ) {
			neworder.push_back(*slotp);
		}
	}
	
	nSlots = neworder.size();
	order.clear();
	order = neworder;
	
	matrix.clear();
	matrix = initialmatrix;
	
	multiplicity.clear();
	multiplicity.resize(nSlots, 1);
}

void Clusters::simplify() {

	if ( debug > 0 ) {
		printf("simplify()\n");
	}
	
	initialmatrix = matrix;
	vector<vector<int> > newmatrix;
	set<int> discard;
	set<int> zeroes;
	
	multiplicity.clear();
	redundantslots.clear();
	for ( int slot1 = 0; slot1 < nSlots; slot1++ ) {
		if ( discard.find(slot1) != discard.end() ) {
			continue;
		}
		int mult = 1;
		set<int> redundant;
		for ( int slot2 = slot1 + 1; slot2 < nSlots; slot2++ ) {
			if ( matrix[slot1] == matrix[slot2] ) {
				mult++;
				redundant.insert(slot2);
				discard.insert(slot2);
			}
		}
		if ( accumulate(matrix[slot1].begin(), matrix[slot1].end(), 0) == 0 ) {
			zeroes = redundant;
			zeroes.insert(slot1);
			discard.insert(slot1);
		} else {
			newmatrix.push_back(matrix[slot1]);
			multiplicity.push_back(mult);
			redundant.insert(slot1);
			redundantslots.push_back(redundant);
		}
	}
	
	if ( zeroes.size() > 0 ) {
		redundantslots.push_back(zeroes);
	}
	
	matrix.resize(newmatrix.size());
	matrix = newmatrix;
	
	nSlots = matrix.size();
	order.resize(nSlots);
	for ( int slot = 0; slot < nSlots; slot++ ) {
		order[slot] = slot;
	}
	
	for ( int cluster = 0; cluster < nClusters; cluster++ ) {
		int length = 0;
		for ( int slot = 0; slot < nSlots; slot++ ) {
			length += matrix[slot][cluster];
		}
		optimallength[cluster] = length;
	}

}

void Clusters::permute() {

	if ( debug > 0 ) {
		printf("permute()\n");
	}
	
	int slot1 = order.size();
	while ( slot1 > 1 ) {
		slot1--;
		int slot2 = random(0, slot1);
		int temp = order[slot1];
		order[slot1] = order[slot2];
		order[slot2] = temp;
	}
	
}

void Clusters::prearrange() {

	if ( debug > 0 ) {
		printf("prearrange()\n");
	}

	for ( int slot1 = 1; slot1 < nSlots; slot1++ ) {

		double bestfitness = getfitness();

		for ( int slot2 = 0; slot2 <= slot1; slot2++ ) {		
			swap(slot1, slot2);
			double fitness = getfitness();
			if ( fitness <= bestfitness ) {
				swap(slot1, slot2);
			} else {
				bestfitness = fitness;
			}
		}

	}
	
}

double Clusters::getfitness() {

	double result = 0.;
	
	for ( int cluster = 0; cluster < nClusters; cluster++ ) {
		int length = matrix[order[0]][cluster] * multiplicity[order[0]];
		int maxlength = length;
		for ( int slot = 1; slot < nSlots; slot++ ) {
			if ( matrix[order[slot]][cluster] ) {
				length += multiplicity[order[slot]];
				if ( slot == nSlots - 1 && length > maxlength ) {
					maxlength = length;					
				}
			} else {
				if ( length > maxlength ) {
					maxlength = length;
				}
				length = 0;
			}
		}
		result += maxlength;
	}

	return result;
	
}

void Clusters::swap(int slot1, int slot2) {
	int temp = order[slot1];
	order[slot1] = order[slot2];
	order[slot2] = temp;
}

void Clusters::shift(int slot1, int slot2, int dslot) {
	
	if ( dslot == 0 ) { return; }
	
	vector<int> neworder;
	if ( dslot < 0 ) {
		for ( int slot = slot1; slot <= slot2; slot++ ) {
			neworder.push_back(order[slot]);
		}
		for ( int slot = slot1 + dslot; slot <= slot1 - 1; slot++ ) {
			neworder.push_back(order[slot]);
		}
	} else {	
		for ( int slot = slot2 + 1; slot <= slot2 + dslot; slot++ ) {
			neworder.push_back(order[slot]);
		}
		for ( int slot = slot1; slot <= slot2; slot++ ) {
			neworder.push_back(order[slot]);
		}		
	}
	
	int slotp = 0;
	for ( int slot = slot1 + min(dslot,0); slot <= slot2 + max(dslot,0); slot++ ) {
		order[slot] = neworder[slotp++];
	}

}

int Clusters::findbestposition(int start, int end) {

	int result = 0;
	
	int length = end - start + 1;

	double bestfitness = getfitness();
	double initialfitness = bestfitness;
		
	shift(start, end, -start);
	
	int beststart = start;
	int startp = 0;
	do {
		double fitness = getfitness();
		if ( fitness > bestfitness ) {
			bestfitness = fitness;
			beststart = startp;
		}
		if ( startp == nSlots - length) {
			break;
		}
		shift(startp, startp + length - 1, 1);
		startp++;
	} while ( true );
	
	shift(startp, nSlots - 1, beststart - startp);

	if ( beststart == start ) {
		return 0;
	} else {
		if ( debug > 1 ) {
			printf("\t\treposition [%d, %d] (%d)", start, end, (int) initialfitness);
			printf(" --> [%d, %d] (%d)\n", beststart, beststart + length - 1, (int) bestfitness);
		}
	}
	
	return 1;
	
}

int Clusters::reposition(int cluster) {
	
	int result = 0;
	
	bool go;
	do {
		
		go = false;
		
		for ( int start = 0; start < nSlots; start++ ) {
			if ( matrix[order[start]][cluster] ) {
				continue;
			}
			int end;
			for ( end = start; end < nSlots; end++ ) {
				if ( !matrix[order[end]][cluster] ) {
					continue;
				} else {
					break;
				}
			}
			end--;

			int modified = 0;
			int endp = start;
			do { 
				modified = findbestposition(start, endp);
				endp++;
			} while ( !modified && endp <= end );

			if ( modified ) { 
				go = true;
				result = 1;				
			}
			
			start = end + 1;
			
		}
	
	} while ( go );
	
	return result;
	
}

int Clusters::exchange(int cluster, int what) {

	int result = 0;

	int maxslots = 6;
	int maxlength = ( what ) ? optimallength[cluster] : nSlots - optimallength[cluster];
	int maxiter = maxlength / maxslots;

	int nslots = min(maxlength, maxslots);
	vector<int> possibleslots;
	for ( int slot = 0; slot < nSlots; slot++ ) {
		if ( what ) {
			if ( matrix[order[slot]][cluster] ) {
				possibleslots.push_back(slot);
			}
		} else {
			if ( !matrix[order[slot]][cluster] ) {
				possibleslots.push_back(slot);
			}			
		}	
	}

	vector<int> permutation;
	permutation.resize(nslots);
	for ( int i = 0; i < nslots; i++ ) {
		permutation[i] = i;
	}

	double bestfitness = getfitness();
	double initialfitness = bestfitness;

	for ( int iter = 0; iter < maxiter; iter++ ) {
		
		vector<int> bestpermutation = permutation;
		vector<int> initialpermutation = permutation;

		vector<pair<int, int> > swapslots;
		set<int> collectslots;
		do {
			int slot = random(0, possibleslots.size()-1);
			collectslots.insert(slot);
			if ( collectslots.size() > swapslots.size() ) {
				swapslots.push_back(make_pair(slot,order[slot]));
			}
		} while ( swapslots.size() < nslots );

		bool go;
		do {
			for ( int slot = 0; slot < nslots; slot++ ) {
				order[swapslots[slot].first] = swapslots[permutation[slot]].second;
			}
			double fitness = getfitness();
			if ( fitness > bestfitness ) {
				bestpermutation = permutation;
				bestfitness = fitness;
			}
			go = next_permutation(permutation.begin(), permutation.end());
		} while ( go );
		
		for ( int slot = 0; slot < nslots; slot++ ) {
			order[swapslots[slot].first] = swapslots[bestpermutation[slot]].second;
		}
		
		if ( bestpermutation != initialpermutation ) {
			result = 1;
		}
		
	}

	if ( result && debug > 1 ) {
		if ( what ) {
			printf("\t\texchange zeroes: (%d) -> (%d)\n", (int) initialfitness, (int) bestfitness);
		} else {
			printf("\t\texchange ones: (%d) -> (%d)\n", (int) initialfitness, (int) bestfitness);			
		}
	}
	return result;
	
}

void Clusters::arrange() {

	if ( debug > 0 ) {
		printf("arrange()\n");
	}
	
	double optimalsolution = 0.;
	for ( int slot = 0; slot < nSlots; slot++ ) {
		for ( int cluster = 0; cluster < nClusters; cluster++ ) {
			optimalsolution += matrix[slot][cluster] * multiplicity[slot];
		}
	}
	double actualsolution;
	
	bool go;
	do {
		
		go = false;
		
		for ( int cluster = 0; cluster < nClusters; cluster++ ) {
	
			if ( elapsedtime() > maxtime ) {
				go =  false;
				if ( debug > 0 ) {
					printf("reached time limit.\n");
				}
				break;
			}

			if ( debug > 1 ) {
				printf("\tcluster %d\n", cluster);
			}
			
			if ( reposition(cluster) ) {
				go = true;
			}
			if ( exchange(cluster, 1) ) {
				go = true;
			}
			if ( exchange(cluster, 0) ) {
				go = true;
			}

		}
		
		actualsolution = getfitness();
		if ( actualsolution == optimalsolution ) {
			if ( debug > 0 ) {
				printf("optimal solution found.\n");
			}
			break;
		}
		
	} while ( go );
	
	if ( debug > 0 ) {
		printf("reached %4.2f of optimum.\n", actualsolution/optimalsolution);
	}
	
}

void Clusters::output() {
	
	for ( int slot = 0; slot < nSlots; slot++ ) {
		printf("%3d %3d %3d: ", slot, order[slot], multiplicity[order[slot]]);
		for ( int cluster = 0; cluster < nClusters; cluster++ ) {
			printf("%d", matrix[order[slot]][cluster]);
		}
		printf("\n");
	}
	printf("fitness: %d\n\n", (int) getfitness());
	
}