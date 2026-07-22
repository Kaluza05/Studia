#include <iostream>
#include <vector>
#include <cstdio>
#include <numeric>
#include <unordered_map>
#include <chrono>

using namespace std;


pair<string,int> find_towers(int n, vector<int>& hs) {
    auto start = chrono::high_resolution_clock::now();
    // w dp pod i trzymamy wysokosc mniejszej z wiez t ze roznica wiez wynosi i
    unordered_map<int, int> dp;
    // for (int i = 0; i < n; i++) {
    //     cout << prefix_h_sum[i] << " ";
    // }
    // cout << '\n';

    // cout << h_sum;
    int h_sum = accumulate(hs.begin(),hs.end(),0);

    dp[0] = {0};
    dp[hs[0]] = {0};

    // for (int i = 0; i < 2; i++) {
    // for (int j = 0; j <= prefix_h_sum[i]; j++) {
    //     cout << dp[i][j] << " ";
    // }
    // cout << "\n";
    // }
    

    for(int i = 1; i < n; i++) {
        int h = hs[i];

        unordered_map<int, int> new_dp;
        new_dp.reserve(dp.size() * 2);

        for(const auto& [h_diff,h_right] : dp) {

            new_dp[h_diff] = max(new_dp[h_diff],h_right);

            // add h to higher tower- increase difference more
            int new_diff = h_diff + h;
            if (new_diff < h_sum) {
                new_dp[new_diff] = max(new_dp[new_diff],h_right); // h_right stays the same we add h to the higher
            }


            new_diff = abs(h_diff - h);
            int new_right = h_right + min(h_diff,h);
            new_dp[new_diff] = max(new_dp[new_diff],new_right);



        }
        
    
        // move lepszy od swyklego przypisania 2.8s do 1.46s ale to i tak duzo
        dp = move(new_dp);

        
    }

    auto end = chrono::high_resolution_clock::now();
    cout << " " << chrono::duration<double>(end - start).count() << " s\n" << endl;
    
    // ta petla szybka
    for(int i = 0; i <= hs[n-1]; i ++) {
        if (dp.count(i) && dp[i] != 0) {
            if (i == 0) {
                
                return {"TAK",dp[i]};
            }
            return {"NIE",i};
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
