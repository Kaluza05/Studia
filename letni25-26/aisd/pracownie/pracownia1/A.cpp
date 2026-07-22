#include <iostream>
#include <vector>
#include <stack>
#include <cstdio>

using namespace std;

int n_cities_iter(int n, int m, vector<vector<char>>& board) {
    
    auto is_in = [&](int i, int j) {
        return i >= 0 && j >= 0 && i < n && j < m;
    };

    auto remove_city = [&](int i, int j) {

        stack<pair<int,int>> st;
        st.push({i,j});

        while(!st.empty()) {
            pair<int,int> p = st.top();
            st.pop();
            int a = p.first;
            int b = p.second;

            char sq = board[a][b];

            if(sq == 'A') {
                continue;
            }
            else if(sq == 'B') {
                board[a][b] = 'A';
                if(is_in(a,b-1) && (board[a][b-1]=='D'||board[a][b-1]=='E'||board[a][b-1]=='F'))
                    st.push({a,b-1});
                if(is_in(a+1,b) && (board[a+1][b]=='C'||board[a+1][b]=='D'||board[a+1][b]=='F'))
                    st.push({a+1,b});
            }
            else if(sq == 'C') {
                board[a][b] = 'A';
                if(is_in(a,b-1) && (board[a][b-1]=='D'||board[a][b-1]=='E'||board[a][b-1]=='F'))
                    st.push({a,b-1});
                if(is_in(a-1,b) && (board[a-1][b]=='B'||board[a-1][b]=='E'||board[a-1][b]=='F'))
                    st.push({a-1,b});
            }
            else if(sq == 'D') {
                board[a][b] = 'A';
                if(is_in(a,b+1) && (board[a][b+1]=='B'||board[a][b+1]=='C'||board[a][b+1]=='F'))
                    st.push({a,b+1});
                if(is_in(a-1,b) && (board[a-1][b]=='B'||board[a-1][b]=='E'||board[a-1][b]=='F'))
                    st.push({a-1,b});
            }
            else if(sq == 'E') {
                board[a][b] = 'A';
                if(is_in(a,b+1) && (board[a][b+1]=='B'||board[a][b+1]=='C'||board[a][b+1]=='F'))
                    st.push({a,b+1});
                if(is_in(a+1,b) && (board[a+1][b]=='C'||board[a+1][b]=='D'||board[a+1][b]=='F'))
                    st.push({a+1,b});
            }
            else if(sq == 'F') {
                board[a][b] = 'A';
                if(is_in(a+1,b) && (board[a+1][b]=='C'||board[a+1][b]=='D'||board[a+1][b]=='F'))
                    st.push({a+1,b});
                if(is_in(a-1,b) && (board[a-1][b]=='B'||board[a-1][b]=='E'||board[a-1][b]=='F'))
                    st.push({a-1,b});
                if(is_in(a,b+1) && (board[a][b+1]=='B'||board[a][b+1]=='C'||board[a][b+1]=='F'))
                    st.push({a,b+1});
                if(is_in(a,b-1) && (board[a][b-1]=='D'||board[a][b-1]=='E'||board[a][b-1]=='F'))
                    st.push({a,b-1});
            }
        }
    };

    int cities = 0;

    for(int i=0;i<n;i++) {
        for(int j=0;j<m;j++) {
            if(board[i][j] != 'A') {
                cities++;
                remove_city(i,j);
            }
        }
    }

    return cities;
}


int main() {
    int n, m;

    scanf("%d %d\n", &n, &m);

    char buffer[m+2];

    vector<vector<char>> board(n, vector<char>(m));

    for(int i = 0; i < n; i++) {
        fgets(buffer, sizeof(buffer), stdin);
        for(int j = 0; j < m; j++) {
            board[i][j] = buffer[j];
        }
    }

    cout << n_cities_iter(n,m,board) << endl;
}