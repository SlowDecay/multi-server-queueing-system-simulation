#include <bits/stdc++.h>
using namespace std;

mt19937 rng(chrono::steady_clock::now().time_since_epoch().count());

double get_floor(int max_floor)
{
    int f = uniform_int_distribution<int>(2, max_floor)(rng);
    return f;
}

int main()
{
    cout << get_floor(12) << endl;
    cout << get_floor(12) << endl;

    return 0;
}