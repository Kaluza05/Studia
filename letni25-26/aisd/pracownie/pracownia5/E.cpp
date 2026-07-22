#include <iostream>
#include <vector>
#include <unordered_map>
#include <algorithm>
#include <stack>
#include <cstdio>

using namespace std;

vector<pair<int,int>> DIRS = {{0,1},{0,-1},{1,0},{-1,0}};


int find_time(int x, vector<int>& xs) {
    int l = 0;
    int r = xs.size();

    while (l < r)
    {
        int m = (l + r) / 2;

        if (xs[m] < x) {
            l = m + 1;
        }
        else {
            r = m;
        }
    }

    return l - 1;
    
}

class UnionFind {
    private:
        vector<int> parent;
        vector<int> size;
        
    public:
        UnionFind(int n) {
            parent.resize(n);
            size.assign(n, 1);
            for (int i = 0; i < n; i++) {
                parent[i] = i;
            }
        }

        int find(int p) {
            if (p == parent[p]) {
                return p;
            }

            parent[p] = find(parent[p]);
            return parent[p];

        }

        int union_disj(int a, int b) {
            int x = find(a);
            int y = find(b);

            if (x == y) {
                return 0;
            }

            if (size[x] > size[y]) {
                swap(x,y);
            }

            parent[y] = x;
            size[x] += size[y];

            return 1;
        }
};

    
vector<int> solve(int n,int m,vector<vector<int>>& board,vector<int>& times) {
    unordered_map<int, vector<pair<int,int>>> pos_for_time;
    
    for(int i = 0; i < m; i ++) {
        for(int j = 0; j < n; j++) {
            int t = find_time(board[i][j], times);
            if (t == -1) {
                continue;
            }
            pos_for_time[times[t]].push_back({i,j});
        }
    }

    reverse(times.begin(), times.end());

    int prev = -1;
    int islands_prev = 0;
    vector<int> island_counts(times.size());


    UnionFind uf = UnionFind(n*m);

    for(auto t : times) {
        if (t == prev) {
            island_counts.push_back(island_counts.back());
            continue;
        }

        vector<pair<int,int>> elems = pos_for_time[t];

        int count_unions = 0;

        for(auto [y,x] : elems) {
            for (auto [dx, dy] : DIRS) {
                int xp = x + dx;
                int yp = y + dy;

                if (0 <= xp && xp < n && 0 <= yp && yp < m) {
                    if (board[yp][xp] > t) {
                        int r = uf.union_disj(n*y+x,n*yp+xp); // r is 0 or 1
                        count_unions += r;
                    }
                }
            }
        }

        int islands_now = islands_prev + elems.size() - count_unions;
        island_counts.push_back(islands_now);
        islands_prev = islands_now;
        prev = t;
    }

    reverse(island_counts.begin(), island_counts.end());

    return island_counts;


}





int main() {
    ios::sync_with_stdio(false);
    cin.tie(nullptr);

    int x;
    int y;
    

    cin >> y >> x;

    vector<vector<int>> board(y,vector<int>(x));

    for(int i = 0; i < y; i ++) {
        for(int j = 0; j < x; j ++) {
            int h;
            cin >> h;
            board[i][j] = h;
        }
    }

    int T;
    cin >> T;

    vector<int> times(T);

    for(int i = 0; i < T; i ++) {
        int time;
        cin >> time;
        times[i] = time;
    }

    vector<int> island_counts = solve(x,y,board,times);


    for(int i = 0; i < T; i++) {
        cout << island_counts[i] << ' ';
    }
    cout << endl;
}