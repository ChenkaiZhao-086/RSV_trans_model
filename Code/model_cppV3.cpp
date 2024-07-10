#include <Rcpp.h>
#include <cmath>

using namespace Rcpp;
using namespace std;

NumericVector CalNj(NumericVector state, int num_age)
{
    NumericVector Nj(num_age);
    for (int i = 0; i < num_age; i++)
    {
        // set up Nj as 0,
        Nj[i] = 0;
        Nj[i] = state[1 + i * 9] + state[2 + i * 9] + state[3 + i * 9] + // First infection. No newborns
                state[4 + i * 9] + state[5 + i * 9] + state[6 + i * 9] + // Second infection
                state[7 + i * 9] + state[8 + i * 9] + state[9 + i * 9];  // Third infection
    }
    return (Nj);
}

NumericVector CalLambda(NumericVector state, int num_age, NumericMatrix contact_str, double beta, double ReducedRec_1, double ReducedRec_2)
{
    NumericVector lambda(num_age);
    NumericVector Nj = CalNj(state, num_age);
    double SumLambda = 0;
    for (size_t i = 0; i < num_age; i++)
    {
        for (size_t j = 0; j < num_age; j++)
        {
            double I0 = state[2 + j * 9];
            double I1 = state[5 + j * 9];
            double I2 = state[8 + j * 9];
            SumLambda += contact_str(i, j) * (I0 + I1 * ReducedRec_1 + I2 * ReducedRec_1 * ReducedRec_2) / Nj[j];
        }
        lambda[i] = beta * SumLambda;
    }
    return (lambda);
}

// [[Rcpp::plugins("cpp11")]]
// [[Rcpp::export]]
List ModelSimCpp(double times, NumericVector state, List parms)
{
    // Exteact parameters
    int num_age = parms["num_age"];        // 8
    double LiveBirth = parms["LiveBirth"]; // weekly live birth
    double Deaths = parms["Deaths"];       // weekly Deaths
    int StartDyingOff = parms["StartDyingOff"];
    NumericVector ageing_rates = parms["age_rates"]; // 使用了一个设置好的针对不同年龄组的ageing rate向量
    double omega_m = parms["omega_m"];               // Duration of immunity from mom
    double beta_base = parms["beta_base"];
    double beta_seasonal = parms["beta_seasonal"];
    double phi = parms["phi"];
    NumericMatrix contact_str = parms["contact_str"];
    double gamma = parms["gamma"];
    double omega_inf = parms["omega_inf"];       // Duration of immunity from infection
    double ReducedSus_1 = parms["ReducedSus_1"]; // Reduced sus, 2 vs 1
    double ReducedSus_2 = parms["ReducedSus_2"]; // Reduced sus, n vs 2
    double ReducedRec_1 = parms["ReducedRec_1"]; // Reduced recovery rate, 2 vs 1
    double ReducedRec_2 = parms["ReducedRec_2"]; // Reduced recovery rate, n vs 2

    // seasonal function
    double beta = beta_base + beta_seasonal * cos((2 * M_PI * times / 365) - phi);

    // force of infection lambda
    NumericVector lambda = CalLambda(state, num_age, contact_str, beta, ReducedRec_1, ReducedRec_2);

    NumericVector yode(num_age * 9 + 1);

    // calculate total number of people at dying age
    float dying_pop = 0;
    for (size_t age = StartDyingOff; age < num_age; age++)
    {
        for (size_t j = 0; j < num_age; j++)
        {
            dying_pop += state[age + j];
        }
    }

    for (size_t i = 0; i < num_age; i++) // 注意，这里的i是指针对年龄组的循环，而不是针对时间的循环，使用ode进行求解的时候只需要写明具体变化的流程，不需要写一个针对时间的模拟
    {
        if (i == 0)
        { // first age group
            // M
            yode[0] = (omega_m * state[0] -
                       ageing_rates[i] * state[0] +
                       LiveBirth);

            // S0
            yode[1] = (omega_m * state[0]     // M
                       - lambda[i] * state[1] // S0
                       - ageing_rates[i] * state[1]);

            // I0
            yode[2] = (lambda[i] * state[1] -
                       gamma * state[2] -
                       ageing_rates[i] * state[2]);

            // R0
            yode[3] = (gamma * state[2] -
                       omega_inf * state[3] -
                       ageing_rates[i] * state[3]);

            // S1
            yode[4] = (omega_inf * state[3] -
                       ReducedSus_1 * lambda[i] * state[4] -
                       ageing_rates[i] * state[4]);

            // I1
            yode[5] = (ReducedSus_1 * lambda[i] * state[4] -
                       gamma * ReducedSus_1 * state[5] -
                       ageing_rates[i] * state[5]);

            // R1
            yode[6] = (gamma * ReducedSus_1 * state[5] -
                       omega_inf * state[6] -
                       ageing_rates[i] * state[6]);

            // S2
            yode[7] = (omega_inf * state[6] -
                       ReducedSus_1 * ReducedSus_2 * lambda[i] * state[7] +
                       omega_inf * state[9] // more than 3 infection
                       - ageing_rates[i] * state[7]);

            // I2
            yode[8] = (ReducedSus_1 * ReducedSus_2 * lambda[i] * state[7] -
                       gamma * ReducedSus_1 * ReducedSus_2 * state[8] -
                       ageing_rates[i] * state[8]);

            // R2
            yode[9] = (gamma * ReducedSus_1 * ReducedSus_2 * state[8] -
                       omega_inf * state[9] -
                       ageing_rates[i] * state[9]);
        }
        else if (i == (num_age - 1))
        {
            // for the last age group - only dying out
            // S0
            yode[1 + i * 9] = (omega_m * state[0] -           // M
                               lambda[i] * state[1 + i * 9] + // S0
                               ageing_rates[i - 1] * state[1 + (i - 1) * 9] -
                               Deaths * state[1] / dying_pop); //

            // I0
            yode[2 + i * 9] = (lambda[i] * state[1 + i * 9] -
                               gamma * state[2 + i * 9] +
                               ageing_rates[i - 1] * state[2 + (i - 1) * 9] -
                               Deaths * state[2] / dying_pop);

            // R0
            yode[3 + i * 9] = (gamma * state[2 + i * 9] -
                               omega_inf * state[3 + i * 9] +
                               ageing_rates[i - 1] * state[3 + (i - 1) * 9] -
                               Deaths * state[3] / dying_pop);

            // S1
            yode[4 + i * 9] = (omega_inf * state[3 + i * 9] -
                               ReducedSus_1 * lambda[i] * state[4 + i * 9] +
                               ageing_rates[i - 1] * state[4 + (i - 1) * 9] -
                               Deaths * state[4] / dying_pop);

            // I1
            yode[5 + i * 9] = (ReducedSus_1 * lambda[i] * state[4 + i * 9] -
                               gamma * ReducedSus_1 * state[5 + i * 9] +
                               ageing_rates[i - 1] * state[5 + (i - 1) * 9] -
                               Deaths * state[5] / dying_pop);

            // R1
            yode[6 + i * 9] = (gamma * ReducedSus_1 * state[5 + i * 9] -
                               omega_inf * state[6 + i * 9] +
                               ageing_rates[i - 1] * state[6 + (i - 1) * 9] -
                               Deaths * state[6] / dying_pop);

            // S2
            yode[7 + i * 9] = (omega_inf * state[6 + i * 9] -
                               ReducedSus_1 * ReducedSus_2 * lambda[i] * state[7 + i * 9] +
                               omega_inf * state[9 + i * 9] + // more than 3 infection
                               ageing_rates[i - 1] * state[7 + (i - 1) * 9] -
                               Deaths * state[7] / dying_pop);

            // I2
            yode[8 + i * 9] = (ReducedSus_1 * ReducedSus_2 * lambda[i] * state[7 + i * 9] -
                               gamma * ReducedSus_1 * ReducedSus_2 * state[8 + i * 9] +
                               ageing_rates[i - 1] * state[8 + (i - 1) * 9] -
                               Deaths * state[8] / dying_pop);

            // R2
            yode[9 + i * 9] = (gamma * ReducedSus_1 * ReducedSus_2 * state[8 + i * 9] -
                               omega_inf * state[9 + i * 9] +
                               ageing_rates[i - 1] * state[9 + (i - 1) * 9] -
                               Deaths * state[9] / dying_pop);
        }
        else
        { // Other age group
            // S0
            yode[1 + i * 9] = (omega_m * state[0]                               // M
                               - lambda[i] * state[1 + i * 9]                   // S0
                               - ageing_rates[i] * state[1 + i * 9]             // - aging
                               + ageing_rates[i - 1] * state[1 + (i - 1) * 9]); //+ aging 注意这里是i-2，上一个年龄组

            // I0
            yode[2 + i * 9] = (lambda[i] * state[1 + i * 9] -
                               gamma * state[2 + i * 9] -
                               ageing_rates[i] * state[2 + i * 9] +
                               ageing_rates[i - 1] * state[2 + (i - 1) * 9]);

            // R0
            yode[3 + i * 9] = (gamma * state[2 + i * 9] -
                               omega_inf * state[3 + i * 9] -
                               ageing_rates[i] * state[3 + i * 9] +
                               ageing_rates[i - 1] * state[3 + (i - 1) * 9]);

            // S1
            yode[4 + i * 9] = (omega_inf * state[3 + i * 9] -
                               ReducedSus_1 * lambda[i] * state[4 + i * 9] -
                               ageing_rates[i] * state[4 + i * 9] +
                               ageing_rates[i - 1] * state[4 + (i - 1) * 9]);

            // I1
            yode[5 + i * 9] = (ReducedSus_1 * lambda[i] * state[4 + i * 9] -
                               gamma * ReducedSus_1 * state[5 + i * 9] -
                               ageing_rates[i] * state[5 + i * 9] +
                               ageing_rates[i - 1] * state[5 + (i - 1) * 9]);

            // R1
            yode[6 + i * 9] = (gamma * ReducedSus_1 * state[5 + i * 9] -
                               omega_inf * state[6 + i * 9] -
                               ageing_rates[i] * state[6 + i * 9] +
                               ageing_rates[i - 1] * state[6 + (i - 1) * 9]);

            // S2
            yode[7 + i * 9] = (omega_inf * state[6 + i * 9] -
                               ReducedSus_1 * ReducedSus_2 * lambda[i] * state[7 + i * 9] +
                               omega_inf * state[9 + i * 9] // more than 3 infection
                               - ageing_rates[i] * state[7 + i * 9] +
                               ageing_rates[i - 1] * state[7 + (i - 1) * 9]);

            // I2
            yode[8 + i * 9] = (ReducedSus_1 * ReducedSus_2 * lambda[i] * state[7 + i * 9] -
                               gamma * ReducedSus_1 * ReducedSus_2 * state[8 + i * 9] -
                               ageing_rates[i] * state[8 + i * 9] +
                               ageing_rates[i - 1] * state[8 + (i - 1) * 9]);

            // R2
            yode[9 + i * 9] = (gamma * ReducedSus_1 * ReducedSus_2 * state[8 + i * 9] -
                               omega_inf * state[9 + i * 9] -
                               ageing_rates[i] * state[9 + i * 9] +
                               ageing_rates[i - 1] * state[9 + (i - 1) * 9]);
        }
    }

    List yout = List::create(yode);
    return (yout);
}
