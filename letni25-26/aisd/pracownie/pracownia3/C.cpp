#include <iostream>
#include <vector>

using namespace std;

class Heap {
private:
    vector<pair<int,int>> heap;
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
            return;
        }
    }

public:
    Heap(const vector<pair<int,int>>& xs) {
        heap = xs;
        n = heap.size();
        build_heap();
    }

    void build_heap() {
        for (int i = n/2-1; i >= 0; i--) {
            move_down(i);
        }
    }

    void push(const pair<int,int>& val) {
        heap.push_back(val);
        n ++;
        move_up(n-1);
    }

    pair<int,int> pop() {
        auto min_el = heap[0];
        swap(heap[0],heap[n-1]);
        heap.pop_back();
        n--;
        move_down(0);
        return min_el;
    }


    bool empty() { return n==0; }
};


vector<int> dijkstra(int n,vector<vector<pair<int,int>>> edges) {
    vector<int> visited(n,0);
    vector<int> ds(n,-1);

    ds[0] = 0;

    // tutaj mogę psuć wynik te inne krawedzie do tego samego wierzchołka
    

    Heap heap({{0,0}});

    while (not heap.empty()) {
        auto [d,v] = heap.pop();
        visited[v] = 1;
        cout << "z kopca" << v << ds[v] << endl;

        for (auto [d_edge,u] : edges[v]) {
            cout << "po krawedziach " << u << ds[u] << endl;
            if (visited[u]) {
                continue;
            }
            
            if (ds[u] == -1 || ds[u] >= d_edge + ds[v]) {
                ds[u] = d_edge + ds[v];
                
                heap.push({ds[u],u});
            }
        }
    }
    
    return ds;

}


int main() {
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

    cout << "miasta docelowe" << endl;
    for (int i = 0; i<k; i ++) {
        cout << i << ", " << target_cities[i] << "\n";
    }
    cout << endl;


    cout << "dupa z czyms" << endl;
    for (int i = 0; i < n; i++) {
        for (auto [d,v] : roads[i]) {
            cout << i << " " << v << " " << d << "\n";
        }
    }

    vector<int> distances = dijkstra(n,roads);

    cout << "teraz dziala" << endl;
    for (int i = 0; i<n; i ++) {
        cout << i << ", " << distances[i] << "\n";
    }
    cout << endl;

    // instead of inf i will hold -1 and make sure that i detect it so it doesnt mess things up
    int final_sum = 0;
    for (int target_city : target_cities) {
        if (distances[target_city] == -1) {
            cout <<  "NIE" << endl;
            return 0;
        }
        final_sum += distances[target_city];

        cout << "curr_sum " << final_sum << endl;
    }

    final_sum *= 2;

    cout <<  final_sum << endl;
    return 0;
}