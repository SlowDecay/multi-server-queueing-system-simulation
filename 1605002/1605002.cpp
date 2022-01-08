#include <bits/stdc++.h>
using namespace std;
#define M 10000
typedef vector<int> vi;

mt19937 rng(chrono::steady_clock::now().time_since_epoch().count());

double get_exponential_sample(double mean)
{
    double u = uniform_real_distribution<double>(0, 1)(rng);
    double e = -mean * log(u);

    return e;
}

double get_floor(int max_floor)
{
    int f = uniform_int_distribution<int>(2, max_floor)(rng);
    return f;
}

vi simulate_elevator()
{
    ifstream fin("input.txt");
    double endtime;
    fin >> endtime;

    int n_floors, n_elevators, capacity, batch_size;
    fin >> n_floors >> n_elevators >> capacity >> batch_size;

    double door_time, travel_time, open_time, close_time;
    fin >> door_time >> travel_time >> open_time >> close_time;

    double embark_time, disembark_time;
    fin >> embark_time >> disembark_time;

    double mean_arrival_time;
    fin >> mean_arrival_time;
    mean_arrival_time *= 60;
    
    double between[M], arrive[M];
    int floor[M];
    double elevator[M], wait[M], delivery[M];
    int selvec[n_elevators+1][n_floors+1];
    int flrvec[n_elevators+1][n_floors+1];
    int occup[n_elevators+1];
    double retrn[n_elevators+1];
    int first[n_elevators+1];
    int quecust, que;
    double startque;
    int stop[n_elevators+1];
    double eldel[n_elevators+1], operate[n_elevators+1];
    int limit;
    int mx, remain, quetotal;
    double TIME, DELTIME, ELEVTIME, MAXDEL, MAXELEV;
    int QUELEN;
    double QUETIME, MAXQUE;
    int i, j, k;
    int N;
    int m;
    int R;
    bool flag;

    step_1:

    DELTIME = ELEVTIME = MAXDEL = MAXELEV = QUELEN = QUETIME = MAXQUE = 0;
    quetotal = remain = 0;
    flag = false;

    step_2:

    i = 1;
    between[i] = get_exponential_sample(mean_arrival_time);
    floor[i] = get_floor(n_floors);
    delivery[i] = door_time;

    step_3:

    TIME = between[i];
    for(k = 1; k <= n_elevators; k++)
    {
        retrn[k] = TIME;
        stop[k] = operate[k] = 0;
    }
    for(k = 1; k < M; k++) wait[k] = 0;

    step_4:

    while(TIME <= endtime)
    {
        step_5:

        j = -1;
        for(k = 1; k <= n_elevators; k++)
        {
            if(retrn[k] <= TIME)
            {
                j = k;
                break;
            }
        }

        if(j == -1) goto step_19;

        step_6:

        first[j] = i, occup[j] = 0;
        for(k = 1; k <= n_floors; k++) selvec[j][k] = flrvec[j][k] = 0;

        step_7:

        selvec[j][floor[i]] = 1;
        flrvec[j][floor[i]] += 1;
        occup[j] += 1;

        step_8:

        i++;

        between[i] = get_exponential_sample(mean_arrival_time);
        floor[i] = get_floor(n_floors);
        TIME += between[i];
        delivery[i] = door_time;

        step_9:

        for(k = 1; k <= n_elevators; k++)
        {
            if(TIME >= retrn[k]) retrn[k] = TIME;
        }

        step_10:

        if(between[i] <= door_time && occup[j] < capacity)
        {
            for(k = first[j]; k < i; k++)
            {
                delivery[k] += between[i];
            }

            goto step_7;
        }
        else
        {
            limit = i-1;
            goto step_11;
        }

        step_11:

        for(k = first[j]; k <= limit; k++)
        {
            step_12:

            N = floor[k]-1;
            elevator[k] = travel_time*N;

            for(m = 1; m <= N; m++)
            {
                elevator[k] += flrvec[j][m]*disembark_time;
            }

            elevator[k] += disembark_time;

            for(m = 1; m <= N; m++)
            {
                elevator[k] += selvec[j][m]*(open_time+close_time);
            }

            elevator[k] += open_time;

            step_13:

            delivery[k] += elevator[k];

            step_14:

            DELTIME += delivery[k];

            step_15:

            if(delivery[k] > MAXDEL) MAXDEL = delivery[k];

            step_16:

            if(elevator[k] > MAXELEV) MAXELEV = elevator[k];
        }

        step_17:

        for(m = 1; m <= n_floors; m++) stop[j] += selvec[j][m];

        mx = -1;
        for(m = 1; m <= n_floors; m++)
        {
            if(selvec[j][m]) mx = m;
        }

        eldel[j] = (mx-1)*travel_time*2;

        for(m = 1; m <= n_floors; m++)
        {
            eldel[j] += flrvec[j][m]*disembark_time;
        }

        for(m = 1; m <= n_floors; m++)
        {
            eldel[j] += selvec[j][m]*(open_time+close_time);
        }

        retrn[j] = TIME+eldel[j];
        operate[j] = TIME+eldel[j];

        if(flag) goto step_31;

        step_18:

        goto step_4;

        step_19:

        printf("in queue\n");

        quecust = i;
        startque = TIME;
        que = 1;
        arrive[i] = TIME;

        step_20:

        i++;
        between[i] = get_exponential_sample(mean_arrival_time);
        floor[i] = get_floor(n_floors);
        TIME += between[i];
        arrive[i] = TIME;
        que++;

        step_21:

        j = -1;
        for(k = 1; k <= n_elevators; k++)
        {
            if(retrn[k] <= TIME)
            {
                j = k;
                break;
            }
        }

        if(j == -1) goto step_20;
        else goto step_22;

        step_22:

        for(k = 1; k <= n_floors; k++) selvec[j][k] = flrvec[j][k] = 0;
        remain = que-capacity;

        step_23:

        if(remain <= 0)
        {
            R = i;
            occup[j] = que;
        }
        else
        {
            R = quecust+capacity-1;
            occup[j] = capacity;
        }

        step_24:

        for(k = quecust; k <= R; k++)
        {
            selvec[j][floor[k]] = 1;
            flrvec[j][floor[k]]++;
        }

        step_25:

        if(que >= QUELEN)
        {
            //printf("--> %d\n", que);
            QUELEN = que;
        }

        step_26:

        quetotal += occup[j];
        for(m = quecust; m <= R; m++) QUETIME += TIME-arrive[m];

        step_27:

        if(TIME-startque >= MAXQUE) MAXQUE = TIME-startque;

        step_28:

        first[j] = quecust;

        step_29:

        for(k = first[j]; k <= R; k++)
        {
            delivery[k] = door_time+(TIME-arrive[k]);
            wait[k] = TIME-arrive[k];
        }

        step_30:

        if(remain <= 0)
        {
            que = 0;
            goto step_8;
        }
        else
        {
            limit = R;
            flag = true;
            goto step_11;
        }

        step_31:

        flag = false;
        que = remain;
        quecust = R+1;
        startque = arrive[R+1];

        step_32:

        goto step_20;
    }

    step_33:

    N = i-que;
    DELTIME = DELTIME/N;

    step_34:

    for(m = 1; m <= limit; m++) ELEVTIME += elevator[m]/limit;

    step_35:

    QUETIME = QUETIME/quetotal;

    step_36:

    vi res;

    res.push_back(N);
    res.push_back(round(DELTIME));
    res.push_back(round(MAXDEL));
    res.push_back(round(ELEVTIME));
    res.push_back(round(MAXELEV));
    res.push_back(QUELEN);
    res.push_back(round(QUETIME));
    res.push_back(round(MAXQUE));
    
    for(k = 1; k <= n_elevators; k++) res.push_back(round(stop[k]));
    for(k = 1; k <= n_elevators; k++) res.push_back(round(operate[k]/endtime));

    //for(k = 1; k <= i; k++) printf("%d: %f\n", k, between[k]);

    return res;
}

int main()
{
    ofstream fout("output.txt");

    for(int iter = 1; iter <= 10; iter++)
    {
        vi res = simulate_elevator();
        reverse(res.begin(), res.end());
        res.push_back(iter);
        reverse(res.begin(), res.end());

        for(int r: res) fout << r << "\t";
        fout << "\n";
    }

    return 0;
}