#include <vector>
#include <set>
#include <map>
#include <algorithm>
#include <numeric>
#include <string>

#include "orderclusters.h"

#ifdef STANDALONE
	#define print printf
#else
	#include <Rdefines.h>
	#define print Rprintf
#endif

using namespace std;

int Clusters::random(int x, int y) {
	return rand() % (y-x+1) + x;
}

double Clusters::elapsedtime() {
	return (clock() - starttime) / ( double ) CLOCKS_PER_SEC;
}

Clusters::Clusters(vector<vector<int> > _data, vector<int> _order) {
	
	starttime = clock();
	
	data = _data;
	nSlots = data.size();
	nClusters = data[0].size();

	order.resize(nSlots);
	for ( int slot = 0; slot < nSlots; slot++ ) {
		order[slot] = _order[slot];
	}
	
	multiplicity.resize(nSlots, 1);
	optimallength.resize(nClusters);
	maximalcluster.resize(3);
	for ( int i = 0; i < 3; i++ ) {
		maximalcluster[i].resize(nClusters);
	}
	

};

Clusters::Clusters(int _nClusters, int _nSlots) {

	starttime = clock();
	status = 0;

	nClusters = _nClusters;
	nSlots = _nSlots;
	
	data.resize(nSlots);
	for ( int slot = 0; slot < nSlots; slot++ ) {
		data[slot].resize(nClusters, 0);
	}

	order.resize(nSlots);
	for ( int slot = 0; slot < nSlots; slot++ ) {
		order[slot] = slot;
	}
	
	multiplicity.resize(nSlots, 1);
	optimallength.resize(nClusters);
	maximalcluster.resize(3);
	for ( int i = 0; i < 3; i++ ) {
		maximalcluster[i].resize(nClusters);
	}

}

void Clusters::orderedsample() {

	if ( debug > 0 ) {
		print("orderedsample()\n");
	}

	for ( int cluster = 0; cluster < nClusters; cluster++ ) {
		int left = random(0, nSlots-1);
		int right = random(left, nSlots-1);	
		for ( int slot = left; slot <= right; slot++ ) {
			data[slot][cluster] = 1;
		}
		optimallength[cluster] = right - left + 1;
	}

	getfullfitness();

}

void Clusters::randomsample() {

	if ( debug > 0 ) {
		print("randomsample()\n");
	}

	for ( int cluster = 0; cluster < nClusters; cluster++ ) {
		for ( int slot = 0; slot < nSlots; slot++ ) {
			if ( random(1,10) > 6 ) {
				data[slot][cluster] = 1;
			}
		}
	}

	getfullfitness();

};

void Clusters::simplify() {

	if ( debug > 0 ) {
		print("simplify()\n");
	}
	
	initialdata = data;
	vector<vector<int> > newdata;
	vector<int> neworder;
	map<int, int> temporder;
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
			if ( data[slot1] == data[slot2] ) {
				mult++;
				redundant.insert(slot2);
				discard.insert(slot2);
			}
		}
		if ( accumulate(data[slot1].begin(), data[slot1].end(), 0) == 0 ) {
			zeroes = redundant;
			zeroes.insert(slot1);
			discard.insert(slot1);
		} else {
			newdata.push_back(data[slot1]);
			multiplicity.push_back(mult);
			temporder.insert ( pair<int, int>(slot1,newdata.size()-1) );

			redundant.insert(slot1);
			redundantslots.push_back(redundant);
		}
	}
	
	if ( zeroes.size() > 0 ) {
		redundantslots.push_back(zeroes);
	}
	
	data.resize(newdata.size());
	data = newdata;

	for ( int slot = 0; slot < nSlots; slot++ ) {
		if ( discard.find(order[slot]) == discard.end() ) {
			neworder.push_back(temporder[order[slot]]);
		}
	}
	order.resize(neworder.size());
	order = neworder;
	
	nSlots = data.size();
	
	for ( int cluster = 0; cluster < nClusters; cluster++ ) {
		int length = 0;
		for ( int slot = 0; slot < nSlots; slot++ ) {
			length += data[slot][cluster];
		}
		optimallength[cluster] = length;
	}
	
	getfullfitness();
	
	t = 0.;
	count = 0;

}

void Clusters::complexify() {
	
	if ( debug > 0 ) {
		print("complexify()\n");
	}
	
	vector<int> neworder;
	for ( int slot = 0; slot < nSlots; slot++ ) {
		for ( set<int>::iterator slotp = redundantslots[order[slot]].begin(); slotp != redundantslots[order[slot]].end(); slotp++ ) {
			neworder.push_back(*slotp);
		}
	}
	if ( redundantslots.size() > nSlots ) {
		for ( set<int>::iterator slotp = redundantslots[nSlots].begin(); slotp != redundantslots[nSlots].end(); slotp++ ) {
			neworder.push_back(*slotp);
		}
	}
	
	nSlots = neworder.size();
	order.clear();
	order = neworder;
	
	data.clear();
	data = initialdata;
	
	multiplicity.clear();
	multiplicity.resize(nSlots, 1);

	getfullfitness();

}


void Clusters::permute() {

	if ( debug > 0 ) {
		print("permute()\n");
	}
	
	int slot1 = order.size();
	while ( slot1 > 1 ) {
		slot1--;
		int slot2 = random(0, slot1);
		int temp = order[slot1];
		order[slot1] = order[slot2];
		order[slot2] = temp;
	}

	getfullfitness();

}

void Clusters::prearrange() {

	// decide whether to use fitness or similarity
	// average time spent in fitness function 0.00002s
	bool usefitness = true;
	if ( maxtime > 0 && maxtime < nSlots / 2 * (nSlots+1) * 0.00002 ) {
		usefitness = false;
	}

	if ( debug > 0 ) {
		print("prearrange() using ");
		if ( usefitness ) {
			print("fitness\n");
		} else {
			print("similarity\n");
		}
	}
	
	vector<int> oldorder(order);
	double initialfitness = getfitness();
		
	for ( int slot1 = 1; slot1 < nSlots; slot1++ ) {

		double bestfitness = getfitness();

		for ( int slot2 = 0; slot2 < slot1; slot2++ ) {
			swap(slot1, slot2);
			double fitness = ( usefitness ) ? getfitness() : getsimilarity(slot1, slot2);
			//double fitness = getfitness();
			//double fitness = getsimilarity(slot1, slot2);
			if ( fitness <= bestfitness ) {
				swap(slot1, slot2);
			} else {
				bestfitness = fitness;
			}
		}
		
		if ( elapsedtime() > maxtime && maxtime != 0 ) {
			break;
		}

	}
		
	if ( getfitness() < initialfitness ) {
		order = oldorder;
		if ( debug > 1 ) {
			print("\tinitial order better than prearrangement.\n");
		}
	}
	
}

double Clusters::getsimilarity(int slot1, int slot2) {
	double result = inner_product(data[order[slot1]].begin(), data[order[slot1]].end(), data[order[slot2]].begin(), 0.);
	return result;
}

void Clusters::getclusters() {
	
	/*
	print("\t\t\t\tmodified slots: ");
	for( set<int>::const_iterator slot = modifiedslots.begin(); slot != modifiedslots.end(); slot++ ) {
		print("%d ", *slot);
	}
	print("\n");
	*/
	
	if ( modifiedslots.size() > 1 ) {
		for ( int cluster = 0; cluster < nClusters; cluster++ ) {
			int l1 = maximalcluster[1][cluster];
			int l2 = maximalcluster[2][cluster];
			for( set<int>::const_iterator slot = modifiedslots.begin(); slot != modifiedslots.end(); slot++ ) {
				if ( *slot >= l1-1 && *slot <= l2+1 ) {
					recalculateclusters.insert(cluster);
				}
			}
		}
	}
	else if ( modifiedslots.size() == 1 && *modifiedslots.begin() == -1 ) {
		for ( int cluster = 0; cluster < nClusters; cluster++ ) {
			recalculateclusters.insert(cluster);
		}
	}
	
	/*
	print("\t\t\t\trecalculate clusters: ");
	for( set<int>::const_iterator cluster = recalculateclusters.begin(); cluster != recalculateclusters.end(); cluster++ ) {
		print("%d ", *cluster);
	}
	print("\n");
	*/
	
	modifiedslots.clear();
}

double Clusters::getfullfitness() {
	modifiedslots.clear();
	modifiedslots.insert(-1);
	return getfitness();
}

double Clusters::getfitness() {

#ifdef TIMING
	double t1 = elapsedtime();
	count++;
#endif

	double result = 0.;
	getclusters();
	
	for( set<int>::const_iterator cluster = recalculateclusters.begin(); cluster != recalculateclusters.end(); cluster++ ) {

		int l1 = 0;
		int length = 0;
		int maxlength = 0;
		int maxl1 = 0;
		int maxl2 = 0;
		bool on = false;

		for ( int slot = 0; slot < nSlots; slot++ ) {
			if ( data[order[slot]][*cluster] ) {
				length += multiplicity[order[slot]];
				if ( !on ) {
					l1 = slot;
					on = true;
				}
				if ( slot == nSlots - 1 && length > maxlength ) {
					maxlength = length;
					maxl1 = l1;
					maxl2 = nSlots - 1;
				}
			} else {
				if ( on ) {
					if ( length > maxlength ) {
						maxlength = length;
						maxl1 = l1;
						maxl2 = slot - 1;
					}
					length = 0;
					on = false;
				}
			}
		}

		maximalcluster[0][*cluster] = maxlength;
		maximalcluster[1][*cluster] = maxl1;
		maximalcluster[2][*cluster] = maxl2;

	}

	recalculateclusters.clear();
	result = accumulate(maximalcluster[0].begin(), maximalcluster[0].end(), 0.);


/*
	double result = 0.;

	for ( int cluster = 0; cluster < nClusters; cluster++ ) {
		int length = data[order[0]][cluster] * multiplicity[order[0]];
		int maxlength = length;
		for ( int slot = 1; slot < nSlots; slot++ ) {
			if ( data[order[slot]][cluster] ) {
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
*/

#ifdef TIMING
	t += elapsedtime() - t1;
#endif

/*
	print("\t\t\t\tfitness = %d\n", (int) result);
*/
	return result;
	
}

double Clusters::getoptimalfitness() {
	double optimum = 0.;
	for ( int slot = 0; slot < nSlots; slot++ ) {
		for ( int cluster = 0; cluster < nClusters; cluster++ ) {
			optimum += data[slot][cluster] * multiplicity[slot];
		}
	}
	return optimum;
}

void Clusters::swap(int slot1, int slot2) {
	if ( debug > 2 ) {
		print("\t\t\tswap: %d <-> %d\n", slot1, slot2);
	}
	int temp = order[slot1];
	order[slot1] = order[slot2];
	order[slot2] = temp;
	modifiedslots.insert(slot1);
	modifiedslots.insert(slot2);
}

void Clusters::shift(int slot1, int slot2, int dslot) {
	
	if ( dslot == 0 ) { return; }

	if ( debug > 2 ) {
		print("\t\t\tshift: [%d, %d] -> %d\n", slot1, slot2, dslot);
	}
	
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
		modifiedslots.insert(slot);
	}

}

int Clusters::findbestposition(int start, int end) {

	int result = 0;
	
	int length = end - start + 1;

	double bestfitness = getfullfitness();
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
	getfullfitness();

	if ( beststart == start ) {
		return 0;
	} else {
		if ( debug > 1 ) {
			print("\t\treposition [%d, %d] (%d)", start, end, (int) initialfitness);
			print(" --> [%d, %d] (%d)\n", beststart, beststart + length - 1, (int) bestfitness);
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
			if ( data[order[start]][cluster] ) {
				continue;
			}
			int end;
			for ( end = start; end < nSlots; end++ ) {
				if ( !data[order[end]][cluster] ) {
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

/* 
			// start backwards
			int modified = 0;
			int endp = end;
			do { 
				modified = findbestposition(start, endp);
				endp--;
			} while ( !modified && endp >= start );
*/
			
			if ( modified ) { 
				go = true;
				result = 1;
			}
			
			start = end + 1;
			
			if ( elapsedtime() > maxtime && maxtime != 0 ) {
				go = false; 
				break;
			}
			
		}
	
	} while ( go );
	
	return result;
	
}

int Clusters::exchange(int cluster, int what) {

	int result = 0;

	int maxslots = 6;
	int maxlength = ( what ) ? optimallength[cluster] : nSlots - optimallength[cluster];
	int maxiter = min(20, maxlength / maxslots);

	int nslots = min(maxlength, maxslots);
	vector<int> possibleslots;
	for ( int slot = 0; slot < nSlots; slot++ ) {
		if ( what ) {
			if ( data[order[slot]][cluster] ) {
				possibleslots.push_back(slot);
			}
		} else {
			if ( !data[order[slot]][cluster] ) {
				possibleslots.push_back(slot);
			}			
		}	
	}

	vector<int> permutation;
	permutation.resize(nslots);
	for ( int i = 0; i < nslots; i++ ) {
		permutation[i] = i;
	}

	double bestfitness = getfullfitness();
	double initialfitness = bestfitness;

	for ( int iter = 0; iter < maxiter; iter++ ) {
		
		vector<int> bestpermutation = permutation;
		vector<int> initialpermutation = permutation;

		set<int> modifiedslotstemp;

		vector<pair<int, int> > swapslots;
		set<int> collectslots;
		do {
			int slot = random(0, possibleslots.size()-1);
			collectslots.insert(slot);
			if ( collectslots.size() > swapslots.size() ) {
				swapslots.push_back(make_pair(slot,order[slot]));
				modifiedslotstemp.insert(slot);
			}
		} while ( swapslots.size() < nslots );

		if ( debug > 2 ) {
			print("\t\t\texchange: ");
			for ( int i = 0; i < nslots; i++ ) {
				print("%d ", swapslots[i].first);
			}
			print("\n");
		}

		bool go;
		do {
			for ( int slot = 0; slot < nslots; slot++ ) {
				order[swapslots[slot].first] = swapslots[permutation[slot]].second;
			}
			modifiedslots = modifiedslotstemp;
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
		
		if ( elapsedtime() > maxtime && maxtime != 0 ) {
			break;
		}
		
	}

	if ( result && debug > 1 ) {
		if ( what ) {
			print("\t\texchange zeroes: (%d) -> (%d)\n", (int) initialfitness, (int) bestfitness);
		} else {
			print("\t\texchange ones: (%d) -> (%d)\n", (int) initialfitness, (int) bestfitness);
		}
	}
	return result;
	
}

void Clusters::arrange() {

	if ( debug > 0 ) {
		print("arrange()\n");
	}
	
	optimalfitness = getoptimalfitness();
	double actualfitness;
	
	bool go;
	
	do {
		
		go = false;
		
		for ( int cluster = 0; cluster < nClusters; cluster++ ) {
	
			if ( elapsedtime() > maxtime && maxtime != 0 ) {
				go =  false;
				break;
			}

			if ( debug > 1 ) {
				print("\tcluster %d\n", cluster);
			}
			
			if ( reposition(cluster) ) {
				go = true;
			}
			if ( elapsedtime() > maxtime && maxtime != 0 ) {
				go =  false;
				break;
			}
			if ( exchange(cluster, 1) ) {
				go = true;
			}
			if ( elapsedtime() > maxtime && maxtime != 0 ) {
				go =  false;
				break;
			}
			if ( exchange(cluster, 0) ) {
				go = true;
			}

		}
		
		actualfitness = getfullfitness();
		if ( actualfitness == optimalfitness ) {
			if ( debug > 0 ) {
				print("optimal solution found.\n");
			}
			break;
		}
		
	} while ( go );
	
	if ( elapsedtime() < maxtime || maxtime == 0 ) {
		status = 1;
	} else {
		if ( debug > 0 ) {
			print("time limit reached.\n");
		}
	}
	
	if ( debug > 0 ) {
		print("reached %4.2f of optimum.\n", actualfitness/optimalfitness);
	}
	
}

double Clusters::quality() {
	return getfullfitness() / getoptimalfitness();
}

void Clusters::output() {
	
	for ( int slot = 0; slot < nSlots; slot++ ) {
		print("%3d %3d %3d: ", slot, order[slot], multiplicity[order[slot]]);
		for ( int cluster = 0; cluster < nClusters; cluster++ ) {
			print("%d", data[order[slot]][cluster]);
		}
		print("\n");
	}
	print("fitness: %d\n\n", (int) getfullfitness());
	
	/*
	for ( int cluster = 0; cluster < nClusters; cluster++ ) {
		print("cluster %d: [%d, %d], %d\n", cluster, maximalcluster[1][cluster], maximalcluster[2][cluster], maximalcluster[0][cluster]);
	}
	print("\n\n");
	*/

#ifdef TIMING
	print("time spent in fitness function: %10.8f, called it %d times\n", t, count);
	print("average time spent in fitness function: %10.8f\n", t / (double) count);
#endif
	
}
