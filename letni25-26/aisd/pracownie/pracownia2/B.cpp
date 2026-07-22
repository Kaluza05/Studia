#include <iostream>
#include <vector>
#include <stack>
#include <cstdio>
#include <numeric>

using namespace std;

struct PossConfig {
    bool flag = false;
    int h_left = 0;
    int h_right = 0;

};

PossConfig choose_highest(const PossConfig& t1, const PossConfig& t2, const PossConfig& t3) {
    vector<PossConfig> valid;

    if (t1.flag) valid.push_back(t1);
    if (t2.flag) valid.push_back(t2);
    if (t3.flag) valid.push_back(t3);

    if (valid.empty()) {
        return {false, 0, 0};
    }

    PossConfig best = valid[0];
    for (const auto& t : valid) {
        if (t.h_right > best.h_right) {
            best = t;
        }
    }

    return best;
}


pair<string,int> find_towers(int n, vector<int>& hs) {
    
    int h_sum = accumulate(hs.begin(),hs.end(),0);

    vector<vector<PossConfig>> dp(n,vector<PossConfig>(h_sum+1));

    for(int i = 0; i < h_sum + 1; i++) {
        if (i == 0 || i == hs[0]) {
            dp[0][i] = {true,i,0};
        }
        else {
        dp[0][i] = {false,0,0};
        }
    }

    for(int i = 1; i < n; i++) {
        dp[i][hs[i]] = {true,hs[i],0};

        for(int j = 0; j < h_sum + 1; j++) {
            PossConfig t1 = dp[i-1][j];
            
            PossConfig t2;
            if (j + hs[i] > h_sum) {
                t2 = {false,0,0};
            }
            else {
                t2 = dp[i-1][j + hs[i]];
            }

            PossConfig t3 = dp[i-1][abs(j-hs[i])];

            bool b2 = t2.flag;
            int h21 = t2.h_left;
            int h22 = t2.h_right + hs[i];

            t2 = {b2,h21,h22};

            bool b3 = t3.flag;
            int h31 = t3.h_left;
            int h32 = t3.h_right;

            if (j - hs[i] >= 0) {
                h31 += hs[i];
            }
            else {
                h32 += hs[i];
            }

            if (h31 < h32) {
                int temp = h31;
                h31 = h32;
                h32 = temp;
            }

            t3 = {b3,h31,h32};

            dp[i][j] = choose_highest(t1,t2,t3);

        }
    }

    vector<PossConfig> last_row = dp[dp.size() - 1];

    for(int i = 0; i < h_sum + 1; i ++) {
        PossConfig t = last_row[i];
        bool b = t.flag;
        int h1 = t.h_left;
        int h2 = t.h_right;

        if (b && h1 != 0 && h2 != 0) {
            if (i == 0) {
                return {"TAK",h1};
            }
            else {
                return {"NIE",h1-h2};
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