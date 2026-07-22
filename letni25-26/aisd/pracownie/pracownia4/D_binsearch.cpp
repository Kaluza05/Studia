#include <iostream>
#include <vector>
#include <stack>
#include <cstdio>


using namespace std;


int bin_search(vector<int>& xs,int v) {
    // chcemy najwieksze i  t.ze v > xs[i]
    int l = 0;
    int r = size(xs) - 1;
    int i = -1;

    while (l <= r) {
        int m = (l + r) / 2;

        if (xs[m] < v) {  // szuakmy na prawo
            i = m;
            l = m + 1;
        }
        else {
            r = m - 1;
        }
    }

    return i;
};

int btq_2(vector<int>& ls ) {
    
    // trzymamy tablice - dla kazdej dlugosci najmniejsza mozliwa koncówka dla subsequence dł i + 1
    // binary searchujemy po tej tablicy (jest ona posortowana) i rozwazamy ten wynik jako max
    
    
    vector<int> ds(1);
    ds[0] = ls[0];

    int n = size(ls);

    int max_quality = 1;

    int l = 0;
    int r = 0;
    while (l < n && r < n) {
        // zwiekszamy subsequence
        while (r+1 < n && ls[r] < ls[r + 1]) {
                r ++ ;
        }

        
        int curr_subarray_len = 1;
        while (l < r) {
            int pos = bin_search(ds,ls[l]);
            if (pos !=  -1) {
                max_quality = max(max_quality, pos + 1 + r - l + 1);
            }

            l ++;
            curr_subarray_len ++;

            if (curr_subarray_len == size(ds) + 1) {
                ds.push_back(ls[l]);
            }
            else {
                ds[curr_subarray_len-1] = min(ds[curr_subarray_len-1], ls[l]);
            }
        }

        l = l + 1;
        r = r + 1;

    }



    return max_quality;
}


int main() {
    ios::sync_with_stdio(false);
    cin.tie(nullptr);

    int T;

    cin >> T;

    for (int i = 0; i < T; i++) {
        int n;
        cin >> n;

        vector<int>xs(n);


        for (int j = 0; j < n; j++) {
            cin >> xs[j];
        }

        // mozna od razu wykonać dla tych danych i wypisać
        int ans = btq_2(xs);
        cout << ans << '\n';
    }


}