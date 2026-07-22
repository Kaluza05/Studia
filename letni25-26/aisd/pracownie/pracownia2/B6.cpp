#include <iostream>
#include <vector>
#include <stack>
#include <cstdio>
// #include <chrono>

using namespace std;


pair<string,int> find_towers(int n, vector<int>& hs) {
    // auto start = chrono::high_resolution_clock::now();
    vector<int> prefix_h_sum(n);
    prefix_h_sum[0] = hs[0];

    for(int i = 1; i < n; i++) {
        prefix_h_sum[i] = hs[i] + prefix_h_sum[i-1];
    }
    // for (int i = 0; i < n; i++) {
    //     cout << prefix_h_sum[i] << " ";
    // }
    // cout << '\n';

    int h_sum = prefix_h_sum[n-1];
    // cout << h_sum;

    vector<vector<int>> dp(2,vector<int>(h_sum+1, -1));

    dp[0][0] = 0;
    dp[0][hs[0]] = 0;

    // for (int i = 0; i < 2; i++) {
    // for (int j = 0; j <= prefix_h_sum[i]; j++) {
    //     cout << dp[i][j] << " ";
    // }
    // cout << "\n";
    // }
    int parity_now = 1;
    int parity_prev = 0;

    for(int i = 1; i < n; i++) {
        dp[parity_now][hs[i]] = 0;

        for(int j = 0; j <= prefix_h_sum[i]; j++) {
            int t1 = dp[parity_prev][j];
            
            int t2;
            if (j + hs[i] > prefix_h_sum[i]) {
                t2 = -1;
            }
            else {
                t2 = dp[parity_prev][j + hs[i]];
                if (t2 != -1) {
                    t2 += hs[i];
                }
            }

            int t3 = dp[parity_prev][abs(j-hs[i])];

            
            if (t3 != -1 && j - hs[i] < 0) {
                t3 += hs[i] - j;
            }

            dp[parity_now][j] = max(max(t1,t2),t3);

        }


        parity_now ^= 1;
        parity_prev ^= 1;
    }

    vector<int> last_row = dp[parity_prev];

    // auto end1 = chrono::high_resolution_clock::now();
    // cout << " " << chrono::duration<double>(end1 - start).count() << " s\n" << endl;


    for(int i = 0; i < h_sum + 1; i ++) {
        int t = last_row[i];
        

        if (t > 0) {
            if (i == 0) {
                // auto end = chrono::high_resolution_clock::now();
                // cout << " " << chrono::duration<double>(end - start).count() << " s\n" << endl;
                return {"TAK",t};
            }
            else {
                return {"NIE",i};
            }
        }
    }

    
    
}


int main() {
    int n;
    cin >> n;

    vector<int>hs(n);
    for (int i = 0; i < n; i++) {
        cin >> hs[i];
    }


    pair<string,int> towers = find_towers(n,hs);
    string ans = towers.first;
    int height = towers.second;
    cout << ans << '\n';
    cout << height << endl;
}