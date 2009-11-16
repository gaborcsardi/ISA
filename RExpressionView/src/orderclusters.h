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

#ifndef __order_clusters_h__
#define __order_clusters_h__

#include <vector>
#include <set>
#include <ctime>

using namespace std;

class Clusters {

	private:
		// slots here stands for genes or samples/conditions
		// matrix to order (dim = nSlots * nClusters)
		vector<vector<int> > data;
		// unsimplified matrix (dim = nSlots' * nClusters)
		vector<vector<int> > initialdata;
		// multiplicity of the slots (dim = nSlots)
		vector<int> multiplicity;
		// duplicate rows in the initialdata
		// every element of the vector contains a set of duplicate rows
		vector<set<int> > redundantslots;
		// the number of slots in each cluster (dim = nClusters)
		vector<int> optimallength;
		// the sum over all elements in optimallength;
		double optimalfitness;

		// contains length (element 0), the beginning (1) and the end (2) of the longest contiguous clusters (dim = 3 * nClusters)
		vector<vector<int> > maximalcluster;
		// keeps track of the slots that were modified in shift and swap operations
		set<int> modifiedslots;
		// the list of clusters for which we recompute the length of the longest subcluster (depends on modifiedslots)
		set<int> recalculateclusters;

		// creates a random number which lies between the first and the second argument (values included)
		int random(int, int);
		// swaps two slots
		void swap(int, int);
		// shifts the slots between the first and the second argument by the third argument
		void shift(int, int, int);
		// finds the best position of slots between the first and the second argument
		int findbestposition(int, int);

		// figures out which clusters to update (depending on modifiedslots)
		void getclusters();

		// calculates the new fitness (sum of longest contiguous subclusters), updating only the one in recalculateclusters
		double getfitness();
		// calculates the new fitness
		double getfullfitness();
		// scalar product between two rows of data
		double getsimilarity(int, int);
		// calculates the sum of the slots contained in each cluster
		double getoptimalfitness();

		// repositions a cluster (executes findbestposition on all appropriate slots in the cluster)
		int reposition(int);
		// exchange appropriate slots in a given cluster (first argument), permuting either slots contained in the cluster 
		// (second argument = 1) or outside (0)
		int exchange(int, int);
		
		clock_t starttime;
		// calculates elapsed time since beginning of execution
		double elapsedtime();
		// measures the time spend in getfitness (only for debugging)
		double t;
		// counts the number of times getfitness is called (only for debugging)
		int count;

	public:
		// debuglevel ranging from 0 to 3
		int debug;
		// maximal execution time
		int maxtime;
		// status of calculation
		int status;
		
		// number of clusters
		int nClusters;
		// number of slots
		int nSlots;

		// vector describing the map between the initial and the optimal arrangement of slots
		vector<int> order;
		
		// constructors
		Clusters(int, int);
		Clusters(vector<vector<int> >, vector<int>);

		// initialize an ordered sample 
		void orderedsample();
		// permute ordered sample
		void permute();
		// initialize random sample
		void randomsample();

		// starting from slot 0 of the original ordering, add one slot at the time and place it
		// at the best possible position, epending on the size of the matrix, either use the fitness 
		// (for smaller matrices) or the similarity measure (for larger matrices)
		void prearrange();
		// finds optimal order (executes the reposition and exchange routines until maxtime is reached or until 
		// the order cannot be improved further)
		void arrange();

		// simplifies the initialdata by removing duplicate rows
		void simplify();
		// reinstates the original dimensions by adding the duplicate rows at the appropriate slots
		void complexify();
		// prints the ordering, multiplicity and the data
		void output();
		
		// calculates the ratio fitness/optimalfitness
		double quality();
		
};

#endif
