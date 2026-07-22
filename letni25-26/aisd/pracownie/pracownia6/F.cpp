#include <iostream>
#include <vector>
#include <unordered_map>
#include <algorithm>
#include <stack>
#include <cstdio>
#include <tuple>
#include <random>
#include <cmath>
#include <set>

using namespace std;


tuple<int,vector<int>,int,vector<int>> hash_nums(int n, vector<int>& nums) {
    random_device rd;              
    mt19937 gen(rd());             
    uniform_int_distribution<> distA(1, 100);
    uniform_int_distribution<> dista(10, 10000);

    int Q = 9999991;

    int m = floor(n / log2(n));
    int A = distA(gen);
    vector<int> as_list(m, 1);

    unordered_map<int, int> inv;

    bool flag = true;

    vector<vector<int>> buckets;

    // divide numbers into n/logn buckets of rize roughly n/m = logn
    while (flag) {
        for (auto l : nums) {
            int g = ((A * l) % Q % m);
            inv[l] = g;
        }

        buckets = vector<vector<int>>(m);

        for (auto l : nums) {
            int g = inv[l];
            buckets[g].push_back(l);
        }

        flag = false;
        for (int g = 0; g < m; g++) {
            if ((int)buckets[g].size() > 2 * log2(n)) {
                flag = true;
                A = distA(gen);
            }
        }
    }


    vector<int> ys(m);
    for (int i = 0; i < m; i++) {
        ys[i] = 3 * buckets[i].size();
    }

    vector<vector<int>> final_ys(m);

    // cout << "buckets\n";
    // for (auto& b : buckets) {
    //     for (auto v : b) cout << v << " ";
    //     cout << "\n";
    // }

    for (int g = 0; g < m; g++) {
        auto& b = buckets[g];
        int fails = 0;
        // cout << "bucket: " << g << " ";
        // for (auto v : b) cout << v << " ";
        // cout << ys[g] << "\n";
        bool flag_inner = true;
        while (flag_inner) {
            set<int> curr_bucket;
            flag_inner = false;
            for (auto v : b) {
                int p = (as_list[g] * v) % Q % ys[g];
                if (curr_bucket.count(p)) {
                    as_list[g] = dista(gen);
                    flag_inner = true;
                    fails += 1;
                    break;
                }
                curr_bucket.insert(p);
            }
        }

        // vector<int> final_y(ys[g], 0);
        // for (auto v : b) {
        //     final_y[(as_list[g] * v) % Q % ys[g]] = v;
        // }

        // final_ys[g] = final_y;
    }

    return {m, ys, A, as_list};
}

int main() {
    ios::sync_with_stdio(false);
    cin.tie(nullptr);

    int N;
    

    cin >> N;

    vector<int> nums(N);

    for(int i = 0; i < N; i ++) {
        int a;
        cin >> a;
        nums[i] = a;
    }

    auto [m,ys,A,a_list] = hash_nums(N,nums);
    
    cout << m << '\n';

    for(int i = 0; i < m; i++) {
        cout << ys[i] << ' ';
    }

    cout << '\n' << A << '\n';

    for(int i = 0; i < m; i++) {
        cout << a_list[i] << ' ';
    }

    cout << endl;
}