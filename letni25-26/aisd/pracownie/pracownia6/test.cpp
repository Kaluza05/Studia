#include <bits/stdc++.h>
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

long long binpow(long long a, long long b) {
    long long res = 1;
    while (b > 0) {
        if (b & 1) res *= a;
        a *= a;
        b >>= 1;
    }
    return res;
}

tuple<long long,vector<long long>,long long,vector<long long>> hash_nums(long long n, vector<long long>& nums) {
    random_device rd;              
    mt19937 gen(rd());             
    uniform_int_distribution<> distA(1, 50000000);
    uniform_int_distribution<> dista(10, 10000);

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
            if ((long long)buckets[g].size() > 2 * log2(n)) {
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

    // cout << "buckets\n";
    // for (auto& b : buckets) {
    //     for (auto v : b) cout << v << " ";
    //     cout << "\n";
    // }

    for (long long g = 0; g < m; g++) {
        auto& b = buckets[g];
        long long fails = 0;
        // cout << "bucket: " << g << " ";
        // for (auto v : b) cout << v << " ";
        // cout << ys[g] << "\n";
        bool flag_inner = true;
        while (flag_inner) {
            set<long long> curr_bucket;
            flag_inner = false;
            for (auto v : b) {
                long long p = (as_list[g] * v) % Q % ys[g];
                if (curr_bucket.count(p)) {
                    as_list[g] = dista(gen);
                    flag_inner = true;
                    fails += 1;
                    break;
                }
                curr_bucket.insert(p);
            }
        }

        // vector<long long> final_y(ys[g], 0);
        // for (auto v : b) {
        //     final_y[(as_list[g] * v) % Q % ys[g]] = v;
        // }

        // final_ys[g] = final_y;
    }

    return {m, ys, A, as_list};
}


// zakładane: long long Q;
// oraz: tuple<long long, vector<long long>, long long, vector<long long>> find_params(long long, vector<long long>&);

void generate_task() {
    random_device rd;
    mt19937 gen(rd());

    uniform_int_distribution<long long> distN(50000, 100000);
    uniform_int_distribution<long long> distV(1, 5000000);

    long long N = distN(gen);

    vector<long long> ls;
    ls.reserve(N);

    for (long long i = 0; i < N; i++) {
        ls.push_back(distV(gen));
    }

    // UWAGA: to zmienia N (jak w Twoim Pythonie)
    // zostawiamy identyczne zachowanie
    sort(ls.begin(), ls.end());
    ls.erase(unique(ls.begin(), ls.end()), ls.end());

    ofstream out("input_rand.txt");

    out << ls.size() << "\n";
    for (long long i = 0; i < (long long)ls.size(); i++) {
        out << ls[i];
        if (i + 1 < (long long)ls.size()) out << " ";
    }
    out << "\n";
}

void solve() {
    ifstream inp("input_rand.txt");

    long long N;
    inp >> N;

    vector<long long> nums(N);
    for (long long i = 0; i < N; i++) {
        inp >> nums[i];
    }

    cout << N;
    cout << endl;

    auto [m, ys, A, as_list] = hash_nums(N, nums);

    cout << "sum check " << accumulate(ys.begin(), ys.end(), 0LL)
         << " " << 3LL * N << " "
         << (accumulate(ys.begin(), ys.end(), 0LL) <= 3LL * N)
         << "\n";

    set<pair<long long, long long>> existing;
    long long Q = 9999991;

    for (auto l : nums) {
        long long q = ((A * l) % Q) % m;
        long long p = (as_list[q] * l) % Q % ys[q];

        if (existing.count({q, p})) {
            cout << "blad\n";
            cout << "collision: " << l << " (" << q << "," << p << ")\n";
            return;
        } else {
            existing.insert({q, p});
        }
    }

    cout << "OK\n";
    cout << endl;
}

int main() {
    ios::sync_with_stdio(false);
    cin.tie(nullptr);

    for (int i = 0; i < 100; i++) {
        cout << "proba " << i << "\n";
        generate_task();
        solve();
    }

    return 0;
}