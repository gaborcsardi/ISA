#ifndef __order_clusters_h__
#define __order_clusters_h__

#include <vector>
#include <set>
#include <ctime>

using namespace std;

class Clusters {

	private:
		vector<vector<int> > data;
		vector<vector<int> > initialdata;
		vector<int> multiplicity;
		vector<set<int> > redundantslots;
		vector<int> optimallength;
		double optimalfitness;
		
		int random(int, int);
		void swap(int, int);
		void shift(int, int, int);
		int findbestposition(int, int);

		double getfitness();
		double getsimilarity(int, int);
		double getoptimalfitness();

		int reposition(int);
		int exchange(int, int);
		
		clock_t starttime;
		double elapsedtime();
		double t;
		int count;

	public:
		int debug;
		int maxtime;
		int status;
		
		int nClusters;
		int nSlots;

		vector<int> order;
		
		Clusters(int, int);
		Clusters(vector<vector<int> >, vector<int>);

		void orderedsample();
		void permute();
		void randomsample();

		void prearrange();
		void arrange();

		void simplify();
		void complexify();
		void output();
		
		double quality();
		
};


#endif
