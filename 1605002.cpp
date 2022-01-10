#include <bits/stdc++.h>
using namespace std;
#define M 10000
typedef vector<int> vi;
typedef pair<vi, vi> pvi;

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

int get_batch_size(int max_size)
{
    int ans = 0;
    for(int i = 0; i < max_size; i++)
    {
        int p = uniform_int_distribution<int>(0, 1)(rng);
        ans += p;
    }

    return ans+1;
}

pvi simulate_elevator()
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
    int maxload[n_elevators+1];
    int departures[n_elevators+1];
    int loads[n_elevators+1];
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
    double WQUELEN;
    double QUETIME, MAXQUE;
    double MAXDELAY, AVGDELAY;
    int i, j, k;
    int N;
    int m;
    int R;
    bool flag;

    step_1:

    DELTIME = ELEVTIME = MAXDEL = MAXELEV = QUELEN = WQUELEN = QUETIME = MAXQUE = 0;
    quetotal = remain = 0;
    flag = false;

    k = 1;
    while(k+5 < M)
    {
        int batch_size = get_batch_size(6);

        between[k] = get_exponential_sample(mean_arrival_time);
        floor[k] = get_floor(n_floors);

        for(i = k+1; i < k+batch_size; i++)
        {
            between[i] = 0;
            floor[i] = get_floor(n_floors);
        }

        k += batch_size;
    }

    step_2:

    i = 1;
    //between[i] = get_exponential_sample(mean_arrival_time);
    //floor[i] = get_floor(n_floors);
    delivery[i] = door_time;

    step_3:

    TIME = between[i];
    for(k = 1; k <= n_elevators; k++)
    {
        retrn[k] = TIME;
        stop[k] = operate[k] = maxload[k] = 0;
        loads[k] = departures[k] = 0;
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

        //between[i] = get_exponential_sample(mean_arrival_time);
        //floor[i] = get_floor(n_floors);
        WQUELEN += between[i]*que;
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

        if(occup[j] == capacity) maxload[j]++;

        departures[j]++;
        loads[j] += occup[j];

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
        operate[j] = operate[j]+eldel[j];

        if(flag) goto step_31;

        step_18:

        goto step_4;

        step_19:

        quecust = i;
        startque = TIME;
        que = 1;
        arrive[i] = TIME;

        step_20:

        i++;
        //between[i] = get_exponential_sample(mean_arrival_time);
        //floor[i] = get_floor(n_floors);
        TIME += between[i];
        WQUELEN += between[i]*que;
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

    ELEVTIME = 0;
    for(m = 1; m <= limit; m++) ELEVTIME += elevator[m]/limit;

    step_35:

    QUETIME = (quetotal > 0? QUETIME/quetotal: 0);
    WQUELEN /= endtime;

    MAXDELAY = AVGDELAY = 0;
    for(m = 1; m <= N; m++) MAXDELAY = max(MAXDELAY, wait[m]), AVGDELAY += wait[m];
    AVGDELAY /= N;

    step_36:

    vi cus;

    cus.push_back(N);
    cus.push_back(round(WQUELEN));
    cus.push_back(QUELEN);
    cus.push_back(round(AVGDELAY));
    cus.push_back(round(MAXDELAY));
    cus.push_back(round(ELEVTIME));
    cus.push_back(round(MAXELEV));
    cus.push_back(round(DELTIME));
    cus.push_back(round(MAXDEL));

    vi elev;
    
    for(k = 1; k <= n_elevators; k++) elev.push_back(round(departures[k] == 0? 0: 1.0*loads[k]/departures[k]));
    for(k = 1; k <= n_elevators; k++) elev.push_back(round(operate[k]/endtime*100));
    for(k = 1; k <= n_elevators; k++) elev.push_back(round((endtime-operate[k])/endtime*100));
    for(k = 1; k <= n_elevators; k++) elev.push_back(maxload[k]);
    for(k = 1; k <= n_elevators; k++) elev.push_back(stop[k]);

    return {cus, elev};
}

int main()
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

    ofstream foutc("output_customers.csv");
    foutc << "Simulation number";
    foutc << ",Total customers";
    foutc << ",Average Queue length";
    foutc << ",Maximum Queue length";
    foutc << ",Average Delay time";
    foutc << ",Maximum Delay time";
    foutc << ",Average Elevator time";
    foutc << ",Maximum Elevator time";
    foutc << ",Average Delivery time";
    foutc << ",Maximum Delivery time\n";

    ofstream foute("output_elevators.csv");
    
    foute << "Simulation number";
    for(int j = 1; j <= n_elevators; j++) foute << ",Load size " << j;
    for(int j = 1; j <= n_elevators; j++) foute << ",Operation time " << j;
    for(int j = 1; j <= n_elevators; j++) foute << ",Available time " << j;
    for(int j = 1; j <= n_elevators; j++) foute << ",Max loads " << j;
    for(int j = 1; j <= n_elevators; j++) foute << ",Stops " << j;
    foute << "\n";

    vi cus_total, elev_total;
    int n_iterations = 10;

    for(int iter = 1; iter <= n_iterations; iter++)
    {
        pvi p = simulate_elevator();
        vi cus = p.first, elev = p.second;
        
        reverse(cus.begin(), cus.end());
        cus.push_back(iter);
        reverse(cus.begin(), cus.end());
        reverse(elev.begin(), elev.end());
        elev.push_back(iter);
        reverse(elev.begin(), elev.end());

        for(int r: cus) foutc << r << ",";
        foutc << "\n";
        for(int r: elev) foute << r << ",";
        foute << "\n";

        if(iter == 1) cus_total = cus, elev_total = elev;
        else
        {
            for(int i = 0; i < cus.size(); i++) cus_total[i] += cus[i];
            for(int i = 0; i < elev.size(); i++) elev_total[i] += elev[i];
        }
    }

    foutc << "Average,";
    for(int i = 1; i < cus_total.size(); i++)
        foutc << round(1.0*cus_total[i]/n_iterations) << ",";
    foutc << "\n";

    foute << "Average,";
    for(int i = 1; i < elev_total.size(); i++)
        foute << round(1.0*elev_total[i]/n_iterations) << ",";
    foute << "\n";

    return 0;
}