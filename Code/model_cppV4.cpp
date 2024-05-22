#include <Rcpp.h>
#include <cmath>

using namespace Rcpp;
using namespace std;

// NumericVector CalNj(NumericVector state, int num_age)
// {
//     NumericVector Nj(num_age);
//     for (int i = 0; i < num_age; i++)
//     {
//         // set up Nj as 0,
//         Nj[i] = 0;
//         Nj[i] = state[1 + i * 9] + state[2 + i * 9] + state[3 + i * 9] + // First infection. No newborns
//                 state[4 + i * 9] + state[5 + i * 9] + state[6 + i * 9] + // Second infection
//                 state[7 + i * 9] + state[8 + i * 9] + state[9 + i * 9];  // Third infection
//     }
//     return (Nj);
// }

NumericVector CalLambda(NumericVector state, int num_age, NumericMatrix contact_str, double beta)
{
    NumericVector lambda(num_age);
    double SumLambda = 0;
    for (size_t i = 0; i < num_age; i++)
    {
        SumLambda = 0;
        for (size_t j = 0; j < num_age; j++)
        {
            double I0 = state[2 + j * 9];
            double I1 = state[5 + j * 9];
            double I2 = state[8 + j * 9];
            SumLambda += contact_str(i, j) * (I0 + I1 + I2);
        }
        lambda[i] = beta * SumLambda;
    }
    return (lambda);
}

double NormalDensity(double x, double mean, double sd)
{
    return exp(-((x / 365 - mean)) * ((x / 365 - mean)) / (2 * sd * sd));
};

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
    double seasonal_wavelength = parms["seasonal_wavelength"];
    double phi = parms["phi"];
    NumericMatrix contact_str = parms["contact_str"];
    double gamma = parms["gamma"];
    double omega_inf = parms["omega_inf"];       // Duration of immunity from infection
    double ReducedSus_1 = parms["ReducedSus_1"]; // Reduced sus, 2 vs 1
    double ReducedSus_2 = parms["ReducedSus_2"]; // Reduced sus, n vs 2
    double ReducedRec_1 = parms["ReducedRec_1"]; // Reduced recovery rate, 2 vs 1
    double ReducedRec_2 = parms["ReducedRec_2"]; // Reduced recovery rate, n vs 2

    NumericVector Age_Sus = parms["Age_Sus"];

    // seasonal function
    // double beta = beta_base * (1 + beta_seasonal * cos((2 * M_PI * times / 365) - phi));
    // double beta = beta_base + beta_seasonal * cos((2 * M_PI * times / 365) - phi);

    double NormSeasonal = NormalDensity(fmod(times, 365), phi - 0.5, seasonal_wavelength) +
                          NormalDensity(fmod(times, 365), phi + 0.5, seasonal_wavelength); // For symmetry
    // double beta = beta_base * (1 + beta_seasonal * NormSeasonal);
    double beta = beta_base + beta_seasonal * NormSeasonal;

    // force of infection lambda
    NumericVector lambda = CalLambda(state, num_age, contact_str, beta);

    NumericVector yode(num_age * 10 + 1);

    // calculate total number of people at dying age
    float dying_pop = 0;
    for (size_t age = StartDyingOff; age <= num_age; age++)
    {
        for (size_t j = 1; j < 10; j++)
        {
            dying_pop += state[j + 9 * (age - 1)];
        }
    }

    for (size_t i = 0; i < num_age; i++) // 注意，这里的i是针对年龄组的循环，而不是针对时间的循环，使用ode进行求解的时候只需要写明具体变化的流程，不需要写一个针对时间的模拟
    {
        if (i == 0)
        { // first age group
            // M
            yode[0] = (LiveBirth //
                       - omega_m * state[0]);

            // S0
            yode[1] = (omega_m * state[0]                  // M
                       - lambda[i] * state[1] * Age_Sus[0] // S0
                       - ageing_rates[i] * state[1]);

            // I0
            yode[2] = (lambda[i] * state[1] * Age_Sus[0] -
                       gamma * state[2] -
                       ageing_rates[i] * state[2]);

            // R0
            yode[3] = (gamma * state[2] -
                       omega_inf * state[3] -
                       ageing_rates[i] * state[3]);

            // S1
            yode[4] = (omega_inf * state[3] -
                       ReducedSus_1 * lambda[i] * state[4] * Age_Sus[0] -
                       ageing_rates[i] * state[4]);

            // I1
            yode[5] = (ReducedSus_1 * lambda[i] * state[4] * Age_Sus[0] -
                       gamma * ReducedRec_1 * state[5] -
                       ageing_rates[i] * state[5]);

            // R1
            yode[6] = (gamma * ReducedRec_1 * state[5] -
                       omega_inf * state[6] -
                       ageing_rates[i] * state[6]);

            // S2
            yode[7] = (omega_inf * state[6] -
                       ReducedSus_1 * ReducedSus_2 * lambda[i] * state[7] * Age_Sus[0] +
                       omega_inf * state[9] // more than 3 infection
                       - ageing_rates[i] * state[7]);

            // I2
            yode[8] = (ReducedSus_1 * ReducedSus_2 * lambda[i] * state[7] * Age_Sus[0] -
                       gamma * ReducedRec_1 * ReducedRec_2 * state[8] -
                       ageing_rates[i] * state[8]);

            // R2
            yode[9] = (+gamma * ReducedRec_1 * ReducedRec_2 * state[8] -
                       omega_inf * state[9] -
                       ageing_rates[i] * state[9]);

            yode[10] = (lambda[i] * state[1] * Age_Sus[0] +
                        ReducedSus_1 * lambda[i] * state[4] * Age_Sus[0] +
                        ReducedSus_1 * ReducedSus_2 * lambda[i] * state[7] * Age_Sus[0]) -
                       state[10];
        }
        else if (i == (num_age - 1))
        {
            // for the last age group - only dying out
            // S0
            yode[1 + i * 10] = (-lambda[i] * state[1 + i * 10] * Age_Sus[i] +
                                ageing_rates[i - 1] * state[1 + (i - 1) * 10] -
                                Deaths * state[1 + i * 10] / dying_pop); //

            // I0
            yode[2 + i * 10] = (lambda[i] * state[1 + i * 10] * Age_Sus[i] -
                                gamma * state[2 + i * 10] +
                                ageing_rates[i - 1] * state[2 + (i - 1) * 10] -
                                Deaths * state[2 + i * 10] / dying_pop);

            // R0
            yode[3 + i * 10] = (gamma * state[2 + i * 10] -
                                omega_inf * state[3 + i * 10] +
                                ageing_rates[i - 1] * state[3 + (i - 1) * 10] -
                                Deaths * state[3 + i * 10] / dying_pop);

            // S1
            yode[4 + i * 10] = (omega_inf * state[3 + i * 10] -
                                ReducedSus_1 * lambda[i] * state[4 + i * 10] * Age_Sus[i] +
                                ageing_rates[i - 1] * state[4 + (i - 1) * 10] -
                                Deaths * state[4 + i * 10] / dying_pop);

            // I1
            yode[5 + i * 10] = (ReducedSus_1 * lambda[i] * state[4 + i * 10] * Age_Sus[i] -
                                gamma * ReducedRec_1 * state[5 + i * 10] +
                                ageing_rates[i - 1] * state[5 + (i - 1) * 10] -
                                Deaths * state[5 + i * 10] / dying_pop);

            // R1
            yode[6 + i * 10] = (gamma * ReducedRec_1 * state[5 + i * 10] -
                                omega_inf * state[6 + i * 10] +
                                ageing_rates[i - 1] * state[6 + (i - 1) * 10] -
                                Deaths * state[6 + i * 10] / dying_pop);

            // S2
            yode[7 + i * 10] = (omega_inf * state[6 + i * 10] -
                                ReducedSus_1 * ReducedSus_2 * lambda[i] * state[7 + i * 10] * Age_Sus[i] +
                                omega_inf * state[9 + i * 10] + // more than 3 infection
                                ageing_rates[i - 1] * state[7 + (i - 1) * 10] -
                                Deaths * state[7 + i * 10] / dying_pop);

            // I2
            yode[8 + i * 10] = (ReducedSus_1 * ReducedSus_2 * lambda[i] * state[7 + i * 10] * Age_Sus[i] -
                                gamma * ReducedRec_1 * ReducedRec_2 * state[8 + i * 10] +
                                ageing_rates[i - 1] * state[8 + (i - 1) * 10] -
                                Deaths * state[8 + i * 10] / dying_pop);

            // R2
            yode[9 + i * 10] = (gamma * ReducedRec_1 * ReducedRec_2 * state[8 + i * 10] -
                                omega_inf * state[9 + i * 10] +
                                ageing_rates[i - 1] * state[9 + (i - 1) * 10] -
                                Deaths * state[9 + i * 10] / dying_pop);

            yode[10 + i * 10] = (lambda[i] * state[1 + i * 10] * Age_Sus[i] +
                                 ReducedSus_1 * lambda[i] * state[4 + i * 10] * Age_Sus[i] +
                                 ReducedSus_1 * ReducedSus_2 * lambda[i] * state[7 + i * 10] * Age_Sus[i]) -
                                state[10 + i * 10];
        }
        else if (i >= StartDyingOff - 1)
        {
            // S0
            yode[1 + i * 10] = (-lambda[i] * state[1 + i * 10] * Age_Sus[i]       // S0
                                - ageing_rates[i] * state[1 + i * 10]             // - aging
                                + ageing_rates[i - 1] * state[1 + (i - 1) * 10] - //+ aging 注意这里是i-2，上一个年龄组
                                Deaths * state[1 + i * 10] / dying_pop);

            // I0
            yode[2 + i * 10] = (lambda[i] * state[1 + i * 10] * Age_Sus[i] -
                                gamma * state[2 + i * 10] -
                                ageing_rates[i] * state[2 + i * 10] +
                                ageing_rates[i - 1] * state[2 + (i - 1) * 10] -
                                Deaths * state[2 + i * 10] / dying_pop);

            // R0
            yode[3 + i * 10] = (gamma * state[2 + i * 10] -
                                omega_inf * state[3 + i * 10] -
                                ageing_rates[i] * state[3 + i * 10] +
                                ageing_rates[i - 1] * state[3 + (i - 1) * 10] -
                                Deaths * state[3 + i * 10] / dying_pop);

            // S1
            yode[4 + i * 10] = (omega_inf * state[3 + i * 10] -
                                ReducedSus_1 * lambda[i] * state[4 + i * 10] * Age_Sus[i] -
                                ageing_rates[i] * state[4 + i * 10] +
                                ageing_rates[i - 1] * state[4 + (i - 1) * 10] -
                                Deaths * state[4 + i * 10] / dying_pop);

            // I1
            yode[5 + i * 10] = (ReducedSus_1 * lambda[i] * state[4 + i * 10] * Age_Sus[i] -
                                gamma * ReducedRec_1 * state[5 + i * 10] -
                                ageing_rates[i] * state[5 + i * 10] +
                                ageing_rates[i - 1] * state[5 + (i - 1) * 10] -
                                Deaths * state[5 + i * 10] / dying_pop);

            // R1
            yode[6 + i * 10] = (gamma * ReducedRec_1 * state[5 + i * 10] -
                                omega_inf * state[6 + i * 10] -
                                ageing_rates[i] * state[6 + i * 10] +
                                ageing_rates[i - 1] * state[6 + (i - 1) * 10] -
                                Deaths * state[6 + i * 10] / dying_pop);

            // S2
            yode[7 + i * 10] = (omega_inf * state[6 + i * 10] -
                                ReducedSus_1 * ReducedSus_2 * lambda[i] * state[7 + i * 10] * Age_Sus[i] +
                                omega_inf * state[9 + i * 10] // more than 3 infection
                                - ageing_rates[i] * state[7 + i * 10] +
                                ageing_rates[i - 1] * state[7 + (i - 1) * 10] -
                                Deaths * state[7 + i * 10] / dying_pop);

            // I2
            yode[8 + i * 10] = (ReducedSus_1 * ReducedSus_2 * lambda[i] * state[7 + i * 10] * Age_Sus[i] -
                                gamma * ReducedRec_1 * ReducedRec_2 * state[8 + i * 10] -
                                ageing_rates[i] * state[8 + i * 10] +
                                ageing_rates[i - 1] * state[8 + (i - 1) * 10] -
                                Deaths * state[8 + i * 10] / dying_pop);

            // R2
            yode[9 + i * 10] = (gamma * ReducedRec_1 * ReducedRec_2 * state[8 + i * 10] -
                                omega_inf * state[9 + i * 10] -
                                ageing_rates[i] * state[9 + i * 10] +
                                ageing_rates[i - 1] * state[9 + (i - 1) * 10] -
                                Deaths * state[9 + i * 10] / dying_pop);

            yode[10 + i * 10] = (lambda[i] * state[1 + i * 10] * Age_Sus[i] +
                                 ReducedSus_1 * lambda[i] * state[4 + i * 10] * Age_Sus[i] +
                                 ReducedSus_1 * ReducedSus_2 * lambda[i] * state[7 + i * 10] * Age_Sus[i]) -
                                state[10 + i * 10];
        }
        else
        { // Other age group
            // S0
            yode[1 + i * 10] = (-lambda[i] * state[1 + i * 10] * Age_Sus[i]       // S0
                                - ageing_rates[i] * state[1 + i * 10]             // - aging
                                + ageing_rates[i - 1] * state[1 + (i - 1) * 10]); //+ aging 注意这里是i-2，上一个年龄组

            // I0
            yode[2 + i * 10] = (lambda[i] * state[1 + i * 10] * Age_Sus[i] -
                                gamma * state[2 + i * 10] -
                                ageing_rates[i] * state[2 + i * 10] +
                                ageing_rates[i - 1] * state[2 + (i - 1) * 10]);

            // R0
            yode[3 + i * 10] = (gamma * state[2 + i * 10] -
                                omega_inf * state[3 + i * 10] -
                                ageing_rates[i] * state[3 + i * 10] +
                                ageing_rates[i - 1] * state[3 + (i - 1) * 10]);

            // S1
            yode[4 + i * 10] = (omega_inf * state[3 + i * 10] -
                                ReducedSus_1 * lambda[i] * state[4 + i * 10] * Age_Sus[i] -
                                ageing_rates[i] * state[4 + i * 10] +
                                ageing_rates[i - 1] * state[4 + (i - 1) * 10]);

            // I1
            yode[5 + i * 10] = (ReducedSus_1 * lambda[i] * state[4 + i * 10] * Age_Sus[i] -
                                gamma * ReducedRec_1 * state[5 + i * 10] -
                                ageing_rates[i] * state[5 + i * 10] +
                                ageing_rates[i - 1] * state[5 + (i - 1) * 10]);

            // R1
            yode[6 + i * 10] = (gamma * ReducedRec_1 * state[5 + i * 10] -
                                omega_inf * state[6 + i * 10] -
                                ageing_rates[i] * state[6 + i * 10] +
                                ageing_rates[i - 1] * state[6 + (i - 1) * 10]);

            // S2
            yode[7 + i * 10] = (omega_inf * state[6 + i * 10] -
                                ReducedSus_1 * ReducedSus_2 * lambda[i] * state[7 + i * 10] * Age_Sus[i] +
                                omega_inf * state[9 + i * 10] // more than 3 infection
                                - ageing_rates[i] * state[7 + i * 10] +
                                ageing_rates[i - 1] * state[7 + (i - 1) * 10]);

            // I2
            yode[8 + i * 10] = (ReducedSus_1 * ReducedSus_2 * lambda[i] * state[7 + i * 10] * Age_Sus[i] -
                                gamma * ReducedRec_1 * ReducedRec_2 * state[8 + i * 10] -
                                ageing_rates[i] * state[8 + i * 10] +
                                ageing_rates[i - 1] * state[8 + (i - 1) * 10]);

            // R2
            yode[9 + i * 10] = (gamma * ReducedRec_1 * ReducedRec_2 * state[8 + i * 10] -
                                omega_inf * state[9 + i * 10] -
                                ageing_rates[i] * state[9 + i * 10] +
                                ageing_rates[i - 1] * state[9 + (i - 1) * 10]);

            yode[10 + i * 10] = (lambda[i] * state[1 + i * 10] * Age_Sus[i] +
                                 ReducedSus_1 * lambda[i] * state[4 + i * 10] * Age_Sus[i] +
                                 ReducedSus_1 * ReducedSus_2 * lambda[i] * state[7 + i * 10] * Age_Sus[i]) -
                                state[10 + i * 10];
        }
    }

    List yout = List::create(yode);
    return (yout);
}
