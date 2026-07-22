#include <iostream>
#include <vector>

using namespace std;

const long long INF = 4e18;


class Heap {
private:
    vector<pair<long long,int>> heap;
    int n;


    void move_up(int i) {
        int k = i;
        while (true) {
            int j = k;

            if (j > 0 && heap[(j-1)/ 2] > heap[j]) {
                k = (j-1) / 2;
                swap(heap[j],heap[k]);
            }
            if (j == k) {
                return;
            }
        }
    }

    void move_down(int i) {
        int k = i;
        while (true) {
            int j = k;
            if (2*j+1<= n-1 && heap[k] > heap[2*j+1]) {
                k = 2 * j + 1;
            }
            if (2*j+1 < n-1 && heap[k] > heap[2*j+2]) {
                k = 2*j + 2;
            }
            
            if (j == k) {
                return;
            }
            
            swap(heap[j],heap[k]);
        }
    }

public:
    Heap(const vector<pair<long long,int>>& xs) {
        heap = xs;
        n = heap.size();
        build_heap();
    }

    void build_heap() {
        for (int i = n/2-1; i >= 0; i--) {
            move_down(i);
        }
    }

    void push(const pair<long long,int>& val) {
        heap.push_back(val);
        n ++;
        move_up(n-1);
    }

    pair<long long,int> pop() {
        auto min_el = heap[0];
        swap(heap[0],heap[n-1]);
        heap.pop_back();
        n--;
        move_down(0);
        return min_el;
    }


    bool empty() { return n==0; }
};

//pointer do edges zamiast kopiowania?
vector<long long> dijkstra(int n,const vector<vector<pair<int,int>>>& edges) {
    
    vector<long long> ds(n,INF);

    ds[0] = 0;

    // tutaj mogę psuć wynik te inne krawedzie do tego samego wierzchołka?
    

    Heap heap({{0,0}});

    while (not heap.empty()) {
        auto [d,v] = heap.pop();
        if (d > ds[v]) continue;

        for (auto [d_edge,u] : edges[v]) {
            if (ds[u] > d_edge + d) { //usuniecie warunku na -1 
                ds[u] = d_edge + d;
                
                heap.push({ds[u],u});
            }
        }
    }
    
    return ds;

}


int main() {
    ios::sync_with_stdio(false);
    cin.tie(nullptr);


    int n,m,k;

    cin >> n >> m >> k;

    vector<vector<pair<int,int>>> roads(n);
    // trzymam liste sasiedztwa z parami (waga,wierzcholek)
    for (int i = 0; i < m; i++) {
        int f,t,d;
        cin >> f >> t >> d;
        roads[f-1].push_back({d, t-1});
        roads[t-1].push_back({d, f-1});
    }

    vector<int> target_cities(k);

    for (int i = 0; i < k; i++) {
        int city;
        cin >> city;
        target_cities[i] = city-1;
    }

    

    vector<long long> distances = dijkstra(n,roads);

    // zmiana na long long 
    // sciezka 1-2-...-10^5 o wagach 10^4 ma final_sum = 10^14/2 

    // instead of inf i will hold -1 and make sure that i detect it so it doesnt mess things up
    long long final_sum = 0;
    for (int target_city : target_cities) {
        if (distances[target_city] == INF) {
            cout <<  "NIE" << endl;
            return 0;
        }
        final_sum += distances[target_city];

        // cout << "curr_sum " << final_sum << endl;
    }

    final_sum *= 2;

    cout <<  final_sum << endl;
    return 0;
}