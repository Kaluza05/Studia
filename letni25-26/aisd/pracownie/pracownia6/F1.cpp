#include <iostream>
#include <vector>
#include <unordered_map>
#include <cstdio>
#include <tuple>
#include <random>
#include <cmath>
#include <set>

using namespace std;


tuple<long long,vector<long long>,long long,vector<long long>> hash_nums(long long n, vector<long long>& nums) {
    random_device rd;              
    mt19937 gen(rd());             
    uniform_int_distribution<> distA(1, 200000);
    uniform_int_distribution<> dista(10, 100000);

    long long Q = 9999991;

    long long m = floor(n / log2(n));
    long long A = distA(gen);
    vector<long long> as_list(m, 1);

    unordered_map<long long, long long> inv;

    bool flag = true;

    vector<vector<long long>> buckets;

    // divide numbers into n/logn buckets of rize roughly n/m = logn
    while (flag) {
        for (auto l : nums) {
            long long g = ((A * l) % Q % m);
            inv[l] = g;
        }

        buckets = vector<vector<long long>>(m);

        for (auto l : nums) {
            long long g = inv[l];
            buckets[g].push_back(l);
        }

        flag = false;
        for (long long g = 0; g < m; g++) {
            if (((long long)buckets[g].size() > 2 * log2(n)) || ((long long)buckets[g].size() == 0)) {
                flag = true;
                A = distA(gen);
            }
        }
    }


    vector<long long> ys(m);
    for (long long i = 0; i < m; i++) {
        ys[i] = 3 * buckets[i].size();
    }

    vector<vector<long long>> final_ys(m);


    for (long long g = 0; g < m; g++) {
        auto& b = buckets[g];

        bool flag_inner = true;
        while (flag_inner) {
            set<long long> curr_bucket;
            flag_inner = false;
            for (auto v : b) {
                long long p = (as_list[g] * v) % Q % ys[g];
                if (curr_bucket.count(p)) {
                    as_list[g] = dista(gen);
                    flag_inner = true;
                    break;
                }
                curr_bucket.insert(p);
            }
        }

    }

    return {m, ys, A, as_list};
}

int main() {
    ios::sync_with_stdio(false);
    cin.tie(nullptr);

    long long N;
    

    cin >> N;

    vector<long long> nums(N);

    for(long long i = 0; i < N; i ++) {
        long long a;
        cin >> a;
        nums[i] = a;
    }

    auto [m,ys,A,a_list] = hash_nums(N,nums);
    
    cout << m << '\n';

    for(long long i = 0; i < m; i++) {
        cout << ys[i] << ' ';
    }

    cout << '\n' << A << '\n';

    for(long long i = 0; i < m; i++) {
        cout << a_list[i] << ' ';
    }

    cout << endl;
}