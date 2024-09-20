#include <Rcpp.h>
#include <cmath>
#include <algorithm>

using namespace Rcpp;
using namespace std;

NumericVector CalLambda(NumericVector state, int num_age, NumericMatrix contact_str, double beta)
{
    NumericVector lambda(num_age);
    double SumLambda = 0;
    for (size_t i = 0; i < num_age; i++)
    {
        SumLambda = 0;
        for (size_t j = 0; j < num_age; j++)
        {
            double I0 = state[3 + j * 13];
            double I1 = state[7 + j * 13];
            double I2 = state[11 + j * 13];
            SumLambda += contact_str(i, j) * (I0 + I1 + I2);
        }
        lambda[i] = beta * SumLambda;
    }
    return (lambda);
}

double NormalDensity(double x, double mean, double sd)
{
    const double pi = 3.14159265358979323846;
    return (1.0 / (sd * sqrt(2 * pi))) * exp(-0.5 * pow((x - mean) / sd, 2));
}

double SymmSeasonal(double x, double phi, double seasonal_wavelength)
{
    double diff = fmod((x - phi + 182.5), 365) - 182.5;
    return NormalDensity(diff, 0, seasonal_wavelength);
}

bool is_in_vector(double value, Rcpp::NumericVector vec)
{
    return std::find(vec.begin(), vec.end(), value) != vec.end();
}

// [[Rcpp::plugins("cpp11")]]
// [[Rcpp::export]]
List ModelSimCpp_Immu(double times, NumericVector state, List parms)
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

    // Vaccine
    NumericVector Vac_start = parms["Vac_start"];
    // double Effacy = parms["Effacy"];
    // NumericVector VacProp = parms["VacProp"];
    double NeedVacPopulation = parms["NeedVacPopulation"];
    NumericVector VacAgeGroup = parms["VacAgeGroup"];
    double omega_vac = parms["omega_vac"];

    // seasonal function
    double NormSeasonal = SymmSeasonal(times, phi, seasonal_wavelength);
    double beta = beta_base + beta_seasonal * NormSeasonal;

    // force of infection lambda
    NumericVector lambda = CalLambda(state, num_age, contact_str, beta);

    // Calculate total number of people at dying age group
    float dying_pop = 0;
    for (size_t age = StartDyingOff; age <= num_age; age++)
    {
        for (size_t j = 1; j < 13; j++)
        {
            dying_pop += state[j + 13 * (age - 1)];
        }
    }

    // Calculate vaccine parameters and how many people need to be vaccinated if it is time to vaccinate
    NumericVector VacPopulation(num_age * 3); // 默认填充0
    if (is_in_vector(static_cast<int>(times), Vac_start))
    {
        // Define vaccine parameters
        // Effacy = parms["Effacy"];

        // Calculate how many people need to be vaccinated
        for (size_t i = 0; i < num_age; i++)
        {
            VacPopulation[0 + i * 3] = state[1 + i * 13] * NeedVacPopulation * VacAgeGroup[i]; // Effacy * VacProp[i];
            VacPopulation[1 + i * 3] = state[5 + i * 13] * NeedVacPopulation * VacAgeGroup[i]; // Effacy * VacProp[i];
            VacPopulation[2 + i * 3] = state[9 + i * 13] * NeedVacPopulation * VacAgeGroup[i]; // Effacy * VacProp[i];
        }
    }
    // else
    // {
    //     Effacy = 0;
    // }

    // initialize yode vector to store the result
    NumericVector yode(num_age * 13 + 1);

    for (size_t i = 0; i < num_age; i++) // 注意，这里的i是针对年龄组的循环，而不是针对时间的循环，使用ode进行求解的时候只需要写明具体变化的流程，不需要写一个针对时间的模拟
    {
        if (i == 0)
        { // first age group
            // M
            yode[0] = (LiveBirth //
                       - omega_m * state[0]);

            // S0
            yode[1] = (omega_m * state[0] -                // M
                       lambda[i] * state[1] * Age_Sus[0] - // S0
                       ageing_rates[i] * state[1] -        //
                       VacPopulation[0 + i * 3] +          // state[1] * Effacy * VacProp[i] +
                       state[2] * omega_vac);

            // V0
            yode[2] = (VacPopulation[0 + i * 3] - // state[1] * Effacy * VacProp[i]
                       state[2] * omega_vac -
                       ageing_rates[i] * state[2]);

            // I0
            yode[3] = (lambda[i] * state[1] * Age_Sus[0] -
                       gamma * state[3] -
                       ageing_rates[i] * state[3]);

            // R0
            yode[4] = (gamma * state[3] -
                       omega_inf * state[4] -
                       ageing_rates[i] * state[4]);

            // S1
            yode[5] = (omega_inf * state[4] -
                       ReducedSus_1 * lambda[i] * state[5] * Age_Sus[0] -
                       ageing_rates[i] * state[5] -
                       VacPopulation[1 + i * 3] + // state[5] * Effacy * VacProp[i] +
                       state[6] * omega_vac);

            // V1
            yode[6] = (VacPopulation[1 + i * 3] - // state[5] * Effacy * VacProp[i] -
                       state[6] * omega_vac -
                       ageing_rates[i] * state[6]);

            // I1
            yode[7] = (ReducedSus_1 * lambda[i] * state[5] * Age_Sus[0] -
                       gamma * ReducedRec_1 * state[7] -
                       ageing_rates[i] * state[7]);

            // R1
            yode[8] = (gamma * ReducedRec_1 * state[7] -
                       omega_inf * state[8] -
                       ageing_rates[i] * state[8]);

            // S2
            yode[9] = (omega_inf * state[8] -
                       ReducedSus_1 * ReducedSus_2 * lambda[i] * state[9] * Age_Sus[0] +
                       omega_inf * state[12] - // more than 3 infection
                       ageing_rates[i] * state[9] -
                       VacPopulation[2 + i * 3] + // state[9] * Effacy * VacProp[i] +
                       state[10] * omega_vac);

            // V2
            yode[10] = (VacPopulation[2 + i * 3] - // state[9] * Effacy * VacProp[i] -
                        state[10] * omega_vac -
                        ageing_rates[i] * state[10]);

            // I2
            yode[11] = (ReducedSus_1 * ReducedSus_2 * lambda[i] * state[9] * Age_Sus[0] -
                        gamma * ReducedRec_1 * ReducedRec_2 * state[11] -
                        ageing_rates[i] * state[11]);

            // R2
            yode[12] = (gamma * ReducedRec_1 * ReducedRec_2 * state[11] -
                        omega_inf * state[12] -
                        ageing_rates[i] * state[12]);

            yode[13] = (lambda[i] * state[1] * Age_Sus[0] +
                        ReducedSus_1 * lambda[i] * state[5] * Age_Sus[0] +
                        ReducedSus_1 * ReducedSus_2 * lambda[i] * state[9] * Age_Sus[0]) -
                       state[13];
        }
        else if (i == (num_age - 1))
        {
            // for the last age group - only dying out
            // S0
            yode[1 + i * 13] = (-lambda[i] * state[1 + i * 13] * Age_Sus[i] +
                                ageing_rates[i - 1] * state[1 + (i - 1) * 13] -
                                VacPopulation[0 + i * 3] + // state[1 + i * 13] * Effacy * VacProp[i] +
                                state[2 + i * 13] * omega_vac -
                                Deaths * state[1 + i * 13] / dying_pop); //

            // V0
            yode[2 + i * 13] = (VacPopulation[0 + i * 3] - // state[1 + i * 13] * Effacy * VacProp[i] -
                                state[2 + i * 13] * omega_vac +
                                ageing_rates[i - 1] * state[2 + (i - 1) * 13] -
                                Deaths * state[2 + i * 13] / dying_pop);

            // I0
            yode[3 + i * 13] = (lambda[i] * state[1 + i * 13] * Age_Sus[i] -
                                gamma * state[3 + i * 13] +
                                ageing_rates[i - 1] * state[3 + (i - 1) * 13] -
                                Deaths * state[3 + i * 13] / dying_pop);

            // R0
            yode[4 + i * 13] = (gamma * state[3 + i * 13] -
                                omega_inf * state[4 + i * 13] +
                                ageing_rates[i - 1] * state[4 + (i - 1) * 13] -
                                Deaths * state[4 + i * 13] / dying_pop);

            // S1
            yode[5 + i * 13] = (omega_inf * state[4 + i * 13] -
                                ReducedSus_1 * lambda[i] * state[5 + i * 13] * Age_Sus[i] +
                                ageing_rates[i - 1] * state[5 + (i - 1) * 13] -
                                VacPopulation[1 + i * 3] + //  state[5 + i * 13] * Effacy * VacProp[i] +
                                state[6 + i * 13] * omega_vac -
                                Deaths * state[5 + i * 13] / dying_pop);

            // V1
            yode[6 + i * 13] = (VacPopulation[1 + i * 3] - // state[5 + i * 13] * Effacy * VacProp[i] -
                                state[6 + i * 13] * omega_vac +
                                ageing_rates[i - 1] * state[6 + (i - 1) * 13] -
                                Deaths * state[6 + i * 13] / dying_pop);

            // I1
            yode[7 + i * 13] = (ReducedSus_1 * lambda[i] * state[5 + i * 13] * Age_Sus[i] -
                                gamma * ReducedRec_1 * state[7 + i * 13] +
                                ageing_rates[i - 1] * state[7 + (i - 1) * 13] -
                                Deaths * state[7 + i * 13] / dying_pop);

            // R1
            yode[8 + i * 13] = (gamma * ReducedRec_1 * state[7 + i * 13] -
                                omega_inf * state[8 + i * 13] +
                                ageing_rates[i - 1] * state[8 + (i - 1) * 13] -
                                Deaths * state[8 + i * 13] / dying_pop);

            // S2
            yode[9 + i * 13] = (omega_inf * state[8 + i * 13] -
                                ReducedSus_1 * ReducedSus_2 * lambda[i] * state[9 + i * 13] * Age_Sus[i] +
                                omega_inf * state[12 + i * 13] + // more than 3 infection
                                ageing_rates[i - 1] * state[9 + (i - 1) * 13] -
                                VacPopulation[2 + i * 3] + //  state[9 + i * 13] * Effacy * VacProp[i] +
                                state[10 + i * 13] * omega_vac -
                                Deaths * state[9 + i * 13] / dying_pop);

            // V2
            yode[10 + i * 13] = (VacPopulation[2 + i * 3] - // state[9 + i * 13] * Effacy * VacProp[i] -
                                 state[10 + i * 13] * omega_vac +
                                 ageing_rates[i - 1] * state[10 + (i - 1) * 13] -
                                 Deaths * state[10 + i * 13] / dying_pop);

            // I2
            yode[11 + i * 13] = (ReducedSus_1 * ReducedSus_2 * lambda[i] * state[9 + i * 13] * Age_Sus[i] -
                                 gamma * ReducedRec_1 * ReducedRec_2 * state[11 + i * 13] +
                                 ageing_rates[i - 1] * state[11 + (i - 1) * 13] -
                                 Deaths * state[11 + i * 13] / dying_pop);

            // R2
            yode[12 + i * 13] = (gamma * ReducedRec_1 * ReducedRec_2 * state[11 + i * 13] -
                                 omega_inf * state[12 + i * 13] +
                                 ageing_rates[i - 1] * state[12 + (i - 1) * 13] -
                                 Deaths * state[12 + i * 13] / dying_pop);

            yode[13 + i * 13] = (lambda[i] * state[1 + i * 13] * Age_Sus[i] +
                                 ReducedSus_1 * lambda[i] * state[5 + i * 13] * Age_Sus[i] +
                                 ReducedSus_1 * ReducedSus_2 * lambda[i] * state[9 + i * 13] * Age_Sus[i]) -
                                state[13 + i * 13];
        }
        else if (i >= StartDyingOff - 1)
        {
            // S0
            yode[1 + i * 13] = (-lambda[i] * state[1 + i * 13] * Age_Sus[i]       // S0
                                - ageing_rates[i] * state[1 + i * 13]             // - aging
                                + ageing_rates[i - 1] * state[1 + (i - 1) * 13] - //+ aging 注意这里是i-2，上一个年龄组
                                VacPopulation[0 + i * 3] +                        // - state[1 + i * 13] * Effacy * VacProp[i] +
                                state[2 + i * 13] * omega_vac -
                                Deaths * state[1 + i * 13] / dying_pop);

            // V0
            yode[2 + i * 13] = (VacPopulation[0 + i * 3] - // state[1 + i * 13] * Effacy * VacProp[i] -
                                state[2 + i * 13] * omega_vac -
                                ageing_rates[i] * state[2 + i * 13] +
                                ageing_rates[i - 1] * state[2 + (i - 1) * 13] -
                                Deaths * state[2 + i * 13] / dying_pop);

            // I0
            yode[3 + i * 13] = (lambda[i] * state[1 + i * 13] * Age_Sus[i] -
                                gamma * state[3 + i * 13] -
                                ageing_rates[i] * state[3 + i * 13] +
                                ageing_rates[i - 1] * state[3 + (i - 1) * 13] -
                                Deaths * state[3 + i * 13] / dying_pop);

            // R0
            yode[4 + i * 13] = (gamma * state[3 + i * 13] -
                                omega_inf * state[4 + i * 13] -
                                ageing_rates[i] * state[4 + i * 13] +
                                ageing_rates[i - 1] * state[4 + (i - 1) * 13] -
                                Deaths * state[4 + i * 13] / dying_pop);

            // S1
            yode[5 + i * 13] = (omega_inf * state[4 + i * 13] -
                                ReducedSus_1 * lambda[i] * state[5 + i * 13] * Age_Sus[i] -
                                ageing_rates[i] * state[5 + i * 13] +
                                ageing_rates[i - 1] * state[5 + (i - 1) * 13] -
                                VacPopulation[1 + i * 3] + // state[5 + i * 13] * Effacy * VacProp[i] +
                                state[6 + i * 13] * omega_vac -
                                Deaths * state[5 + i * 13] / dying_pop);

            // V1
            yode[6 + i * 13] = (VacPopulation[1 + i * 3] - // state[5 + i * 13] * Effacy * VacProp[i] -
                                state[6 + i * 13] * omega_vac -
                                ageing_rates[i] * state[6 + i * 13] +
                                ageing_rates[i - 1] * state[6 + (i - 1) * 13] -
                                Deaths * state[6 + i * 13] / dying_pop);

            // I1
            yode[7 + i * 13] = (ReducedSus_1 * lambda[i] * state[5 + i * 13] * Age_Sus[i] -
                                gamma * ReducedRec_1 * state[7 + i * 13] -
                                ageing_rates[i] * state[7 + i * 13] +
                                ageing_rates[i - 1] * state[7 + (i - 1) * 13] -
                                Deaths * state[7 + i * 13] / dying_pop);

            // R1
            yode[8 + i * 13] = (gamma * ReducedRec_1 * state[7 + i * 13] -
                                omega_inf * state[8 + i * 13] -
                                ageing_rates[i] * state[8 + i * 13] +
                                ageing_rates[i - 1] * state[8 + (i - 1) * 13] -
                                Deaths * state[8 + i * 13] / dying_pop);

            // S2
            yode[9 + i * 13] = (omega_inf * state[8 + i * 13] -
                                ReducedSus_1 * ReducedSus_2 * lambda[i] * state[9 + i * 13] * Age_Sus[i] +
                                omega_inf * state[12 + i * 13] // more than 3 infection
                                - ageing_rates[i] * state[9 + i * 13] +
                                ageing_rates[i - 1] * state[9 + (i - 1) * 13] -
                                VacPopulation[2 + i * 3] + // state[9 + i * 13] * Effacy * VacProp[i] +
                                state[10 + i * 13] * omega_vac -
                                Deaths * state[9 + i * 13] / dying_pop);

            // V2
            yode[10 + i * 13] = (VacPopulation[2 + i * 3] - // state[9 + i * 13] * Effacy * VacProp[i] -
                                 state[10 + i * 13] * omega_vac -
                                 ageing_rates[i] * state[10 + i * 13] +
                                 ageing_rates[i - 1] * state[10 + (i - 1) * 13] -
                                 Deaths * state[10 + i * 13] / dying_pop);

            // I2
            yode[11 + i * 13] = (ReducedSus_1 * ReducedSus_2 * lambda[i] * state[9 + i * 13] * Age_Sus[i] -
                                 gamma * ReducedRec_1 * ReducedRec_2 * state[11 + i * 13] -
                                 ageing_rates[i] * state[11 + i * 13] +
                                 ageing_rates[i - 1] * state[11 + (i - 1) * 13] -
                                 Deaths * state[11 + i * 13] / dying_pop);

            // R2
            yode[12 + i * 13] = (gamma * ReducedRec_1 * ReducedRec_2 * state[11 + i * 13] -
                                 omega_inf * state[12 + i * 13] -
                                 ageing_rates[i] * state[12 + i * 13] +
                                 ageing_rates[i - 1] * state[12 + (i - 1) * 13] -
                                 Deaths * state[12 + i * 13] / dying_pop);

            yode[13 + i * 13] = (lambda[i] * state[1 + i * 13] * Age_Sus[i] +
                                 ReducedSus_1 * lambda[i] * state[5 + i * 13] * Age_Sus[i] +
                                 ReducedSus_1 * ReducedSus_2 * lambda[i] * state[9 + i * 13] * Age_Sus[i]) -
                                state[13 + i * 13];
        }
        else
        { // Other age group
            // S0
            yode[1 + i * 13] = (-lambda[i] * state[1 + i * 13] * Age_Sus[i] -   // S0
                                ageing_rates[i] * state[1 + i * 13] +           // - aging
                                ageing_rates[i - 1] * state[1 + (i - 1) * 13] - //
                                VacPopulation[0 + i * 3] +                      // state[1 + i * 13] * Effacy * VacProp[i] +
                                state[2 + i * 13] * omega_vac);

            // V0
            yode[2 + i * 13] = (VacPopulation[0 + i * 3] - // state[1 + i * 13] * Effacy * VacProp[i] -
                                state[2 + i * 13] * omega_vac -
                                ageing_rates[i] * state[2 + i * 13] +
                                ageing_rates[i - 1] * state[2 + (i - 1) * 13]);

            // I0
            yode[3 + i * 13] = (lambda[i] * state[1 + i * 13] * Age_Sus[i] -
                                gamma * state[3 + i * 13] -
                                ageing_rates[i] * state[3 + i * 13] +
                                ageing_rates[i - 1] * state[3 + (i - 1) * 13]);

            // R0
            yode[4 + i * 13] = (gamma * state[3 + i * 13] -
                                omega_inf * state[4 + i * 13] -
                                ageing_rates[i] * state[4 + i * 13] +
                                ageing_rates[i - 1] * state[4 + (i - 1) * 13]);

            // S1
            yode[5 + i * 13] = (omega_inf * state[4 + i * 13] -
                                ReducedSus_1 * lambda[i] * state[5 + i * 13] * Age_Sus[i] -
                                ageing_rates[i] * state[5 + i * 13] +
                                ageing_rates[i - 1] * state[5 + (i - 1) * 13] -
                                VacPopulation[1 + i * 3] + // state[5 + i * 13] * Effacy * VacProp[i] +
                                state[6 + i * 13] * omega_vac);

            // V1
            yode[6 + i * 13] = (VacPopulation[1 + i * 3] - // state[5 + i * 13] * Effacy * VacProp[i] -
                                state[6 + i * 13] * omega_vac -
                                ageing_rates[i] * state[6 + i * 13] +
                                ageing_rates[i - 1] * state[6 + (i - 1) * 13]);

            // I1
            yode[7 + i * 13] = (ReducedSus_1 * lambda[i] * state[5 + i * 13] * Age_Sus[i] -
                                gamma * ReducedRec_1 * state[7 + i * 13] -
                                ageing_rates[i] * state[7 + i * 13] +
                                ageing_rates[i - 1] * state[7 + (i - 1) * 13]);

            // R1
            yode[8 + i * 13] = (gamma * ReducedRec_1 * state[7 + i * 13] -
                                omega_inf * state[8 + i * 13] -
                                ageing_rates[i] * state[8 + i * 13] +
                                ageing_rates[i - 1] * state[8 + (i - 1) * 13]);

            // S2
            yode[9 + i * 13] = (omega_inf * state[8 + i * 13] -
                                ReducedSus_1 * ReducedSus_2 * lambda[i] * state[9 + i * 13] * Age_Sus[i] +
                                omega_inf * state[12 + i * 13] // more than 3 infection
                                - ageing_rates[i] * state[9 + i * 13] +
                                ageing_rates[i - 1] * state[9 + (i - 1) * 13] -
                                VacPopulation[2 + i * 3] + // state[9 + i * 13] * Effacy * VacProp[i] +
                                state[10 + i * 13] * omega_vac);

            // V2
            yode[10 + i * 13] = (VacPopulation[2 + i * 3] - // state[9 + i * 13] * Effacy * VacProp[i] -
                                 state[10 + i * 13] * omega_vac -
                                 ageing_rates[i] * state[10 + i * 13] +
                                 ageing_rates[i - 1] * state[10 + (i - 1) * 13]);

            // I2
            yode[11 + i * 13] = (ReducedSus_1 * ReducedSus_2 * lambda[i] * state[9 + i * 13] * Age_Sus[i] -
                                 gamma * ReducedRec_1 * ReducedRec_2 * state[11 + i * 13] -
                                 ageing_rates[i] * state[11 + i * 13] +
                                 ageing_rates[i - 1] * state[11 + (i - 1) * 13]);

            // R2
            yode[12 + i * 13] = (gamma * ReducedRec_1 * ReducedRec_2 * state[11 + i * 13] -
                                 omega_inf * state[12 + i * 13] -
                                 ageing_rates[i] * state[12 + i * 13] +
                                 ageing_rates[i - 1] * state[12 + (i - 1) * 13]);

            yode[13 + i * 13] = (lambda[i] * state[1 + i * 13] * Age_Sus[i] +
                                 ReducedSus_1 * lambda[i] * state[5 + i * 13] * Age_Sus[i] +
                                 ReducedSus_1 * ReducedSus_2 * lambda[i] * state[9 + i * 13] * Age_Sus[i]) -
                                state[13 + i * 13];
        }
    }

    List yout = List::create(yode);
    return (yout);
}
