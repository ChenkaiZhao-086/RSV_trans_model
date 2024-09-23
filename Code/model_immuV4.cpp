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
            double I0 = state[4 + j * 20] + state[5 + j * 20];   // I0 + VI0
            double I1 = state[10 + j * 20] + state[11 + j * 20]; // I1 + VI1
            double I2 = state[16 + j * 20] + state[17 + j * 20]; // I2 + VI2
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
    int num_age = parms["num_age"];                            // 11
    double LiveBirth = parms["LiveBirth"];                     // weekly live birth
    double Deaths = parms["Deaths"];                           // weekly Deaths
    int StartDyingOff = parms["StartDyingOff"];                //
    NumericVector ageing_rates = parms["age_rates"];           // 使用了一个设置好的针对不同年龄组的ageing rate向量
    double omega_m = parms["omega_m"];                         // Duration of immunity from mom
    double beta_base = parms["beta_base"];                     //
    double beta_seasonal = parms["beta_seasonal"];             //
    double seasonal_wavelength = parms["seasonal_wavelength"]; //
    double phi = parms["phi"];                                 //
    NumericMatrix contact_str = parms["contact_str"];          // Contact Matrix
    double gamma = parms["gamma"];                             //
    double omega_inf = parms["omega_inf"];                     // Duration of immunity from infection
    double ReducedSus_1 = parms["ReducedSus_1"];               // Reduced sus, 2 vs 1
    double ReducedSus_2 = parms["ReducedSus_2"];               // Reduced sus, n vs 2
    double ReducedRec_1 = parms["ReducedRec_1"];               // Reduced recovery rate, 2 vs 1
    double ReducedRec_2 = parms["ReducedRec_2"];               // Reduced recovery rate, n vs 2

    NumericVector Age_Sus = parms["Age_Sus"]; // Age specific susceptibility, 2 types

    // Vaccine
    NumericVector Vac_start = parms["Vac_start"];                // Which timestep to start vaccination
    double NeedVacPopulation = parms["NeedVacPopulation"];       // Proportion of S to be vaccinated
    double NeedVacPopulation_SV = parms["NeedVacPopulation_SV"]; // Propotion of S to be vaccinated to prevent hospitalization
    NumericVector VacAgeGroup = parms["VacAgeGroup"];            // Determine which age group to vaccinate
    double omega_vac = parms["omega_vac"];                       // Duration of immunity from vaccination

    // seasonal function
    double NormSeasonal = SymmSeasonal(times, phi, seasonal_wavelength);
    double beta = beta_base + beta_seasonal * NormSeasonal;

    // force of infection lambda
    NumericVector lambda = CalLambda(state, num_age, contact_str, beta);

    // Calculate total number of people at dying age group
    float dying_pop = 0;
    for (size_t age = StartDyingOff; age <= num_age; age++)
    {
        for (size_t j = 1; j < 19; j++)
        // 注意这里要 j < 19 因为现在有了两个最后的舱室用来储存新增的病例数，所以要变成19，否则会把新增病例数当成一个舱室要计算
        {
            dying_pop += state[j + 20 * (age - 1)];
        }
    }

    // Calculate vaccine parameters and how many people need to be vaccinated if it is time to vaccinate
    NumericVector VacPopulation(num_age * 3); // 默认填充0
    if (is_in_vector(static_cast<int>(times), Vac_start))
    {
        // Calculate how many people need to be vaccinated
        for (size_t i = 0; i < num_age; i++)
        {
            VacPopulation[0 + i * 3] = state[1 + i * 20] * NeedVacPopulation * VacAgeGroup[i];
            VacPopulation[1 + i * 3] = state[7 + i * 20] * NeedVacPopulation * VacAgeGroup[i];
            VacPopulation[2 + i * 3] = state[13 + i * 20] * NeedVacPopulation * VacAgeGroup[i];
        }
    }

    // Calculate vaccine parameters and how many people need to be vaccinated (!!!Could be infected!!!)
    NumericVector VacPopulation_SV(num_age * 3);
    if (is_in_vector(static_cast<int>(times), Vac_start))
    {
        // Calculate how many people need to be vaccinated
        for (size_t i = 0; i < num_age; i++)
        {
            VacPopulation_SV[0 + i * 3] = state[1 + i * 20] * NeedVacPopulation_SV * VacAgeGroup[i];
            VacPopulation_SV[1 + i * 3] = state[7 + i * 20] * NeedVacPopulation_SV * VacAgeGroup[i];
            VacPopulation_SV[2 + i * 3] = state[13 + i * 20] * NeedVacPopulation_SV * VacAgeGroup[i];
        }
    }

    // initialize yode vector to store the result
    NumericVector yode(num_age * 20 + 1);

    for (size_t i = 0; i < num_age; i++)
    {
        if (i == 0)
        { // first age group
            // M
            yode[0] = (+LiveBirth             //
                       - omega_m * state[0]); // Wane immunity (infant)

            // S0
            yode[1] = (+omega_m * state[0]                 // M
                       - lambda[i] * state[1] * Age_Sus[i] // S0 to I0
                       - VacPopulation[0 + i * 3]          // Normal vaccine, move to V0
                       - VacPopulation_SV[0 + i * 3]       // Prevent hospitalization, move to SV0
                       + state[2] * omega_vac              // Wange immunity from V0
                       + state[3] * omega_vac              // Wange immunity from SV0
                       - ageing_rates[i] * state[1]);      // Ageing

            // V0
            yode[2] = (+VacPopulation[0 + i * 3]      // Normal vaccine from S0
                       - state[2] * omega_vac         // Wange immunity
                       - ageing_rates[i] * state[2]); // Ageing

            // SV0
            yode[3] = (+VacPopulation_SV[0 + i * 3]        // Prevent hospitalization
                       - lambda[i] * state[3] * Age_Sus[i] // SV0 to IV0
                       - state[3] * omega_vac              // Wane immunity
                       - ageing_rates[i] * state[3]);      // Ageing

            // I0
            yode[4] = (+lambda[i] * state[1] * Age_Sus[i] // From S0
                       - gamma * state[4]                 // Recovered
                       - ageing_rates[i] * state[4]);     // Ageing

            // IV0
            yode[5] = (+lambda[i] * state[3] * Age_Sus[i] // From SV0
                       - gamma * state[5]                 // Recovered
                       - ageing_rates[i] * state[5]);     // Ageing

            // R0
            yode[6] = (+gamma * state[4]              // Recovered from I0
                       + gamma * state[5]             // Recovered from IV0
                       - omega_inf * state[6]         // Wane immunity of Recovered
                       - ageing_rates[i] * state[6]); // Ageing

            // S1
            yode[7] = (+omega_inf * state[6]                              // Wane immunity from R0
                       - ReducedSus_1 * lambda[i] * state[7] * Age_Sus[i] // S1 to I1
                       - VacPopulation[1 + i * 3]                         // Normal vaccine, move to V1
                       - VacPopulation_SV[1 + i * 3]                      // Prevent hospitalization, move to SV1
                       + state[8] * omega_vac                             // Wane immunity from V1
                       + state[9] * omega_vac                             // Wane immunity from SV1
                       - ageing_rates[i] * state[7]);                     // Ageing

            // V1
            yode[8] = (+VacPopulation[1 + i * 3]      // Normal vaccine from S1
                       - state[8] * omega_vac         // Wane immunity
                       - ageing_rates[i] * state[8]); // Ageing

            // SV1
            yode[9] = (+VacPopulation_SV[1 + i * 3]                       // Prevent hospitalization
                       - ReducedSus_1 * lambda[i] * state[9] * Age_Sus[i] // SV1 to IV1
                       - state[9] * omega_vac                             // Wane immunity
                       - ageing_rates[i] * state[9]);                     // Ageing

            //  I1
            yode[10] = (+ReducedSus_1 * lambda[i] * state[7] * Age_Sus[i] // From S1
                        - gamma * ReducedRec_1 * state[10]                // Recovered
                        - ageing_rates[i] * state[10]);                   // Ageing

            // IV1
            yode[11] = (+ReducedSus_1 * lambda[i] * state[9] * Age_Sus[i] // From SV1
                        - gamma * ReducedRec_1 * state[11]                // Recovered
                        - ageing_rates[i] * state[11]);                   // Ageing

            // R1
            yode[12] = (+gamma * ReducedRec_1 * state[10]  // Recovered from I1
                        + gamma * ReducedRec_1 * state[11] // Recovered from IV1
                        - omega_inf * state[12]            // Wane immunity of Recovered
                        - ageing_rates[i] * state[12]);    // Ageing

            // S2
            yode[13] = (+omega_inf * state[12]                                             // Wane immunity from R1
                        + omega_inf * state[18]                                            // More than 3 infection
                        - ReducedSus_1 * ReducedSus_2 * lambda[i] * state[13] * Age_Sus[i] // S2 to I2
                        - VacPopulation[2 + i * 3]                                         // Normal vaccine, move to V2
                        - VacPopulation_SV[2 + i * 3]                                      // Prevent hospitalization, move to SV2
                        + state[14] * omega_vac                                            // Wane immunity from V2
                        + state[15] * omega_vac                                            // Wane immunity from SV2
                        - ageing_rates[i] * state[13]);                                    // Ageing

            // V2
            yode[14] = (+VacPopulation[2 + i * 3]       // Normal vaccine from S2
                        - state[14] * omega_vac         // Wane immunity
                        - ageing_rates[i] * state[14]); // Ageing

            // SV2
            yode[15] = (+VacPopulation_SV[2 + i * 3]                                       // Prevent hospitalization
                        - ReducedSus_1 * ReducedSus_2 * lambda[i] * state[15] * Age_Sus[i] // SV2 to IV2
                        - state[15] * omega_vac                                            // Wane immunity
                        - ageing_rates[i] * state[15]);                                    // Ageing

            // I2
            yode[16] = (+ReducedSus_1 * ReducedSus_2 * lambda[i] * state[13] * Age_Sus[i] // From S2
                        - gamma * ReducedRec_1 * ReducedRec_2 * state[16]                 // Recovered
                        - ageing_rates[i] * state[16]);                                   // Ageing

            // IV2
            yode[17] = (+ReducedSus_1 * ReducedSus_2 * lambda[i] * state[15] * Age_Sus[i] // From SV2
                        - gamma * ReducedRec_1 * ReducedRec_2 * state[17]                 // Recovered
                        - ageing_rates[i] * state[17]);                                   // Ageing

            // R2
            yode[18] = (+gamma * ReducedRec_1 * ReducedRec_2 * state[16]  // Recovered from I2
                        + gamma * ReducedRec_1 * ReducedRec_2 * state[17] // Recovered from IV2
                        - omega_inf * state[18]                           // Wane immunity of Recovered
                        - ageing_rates[i] * state[18]);                   // Ageing

            // Report the number of new cases from I0, I1, I2
            yode[19] = (lambda[i] * state[1] * Age_Sus[i] +                                 // I0
                        ReducedSus_1 * lambda[i] * state[7] * Age_Sus[i] +                  // I1
                        ReducedSus_1 * ReducedSus_2 * lambda[i] * state[13] * Age_Sus[i]) - // I2
                       state[19];                                                           // Last timestep

            // Report the number of new cases from IV0, IV1, IV2
            yode[20] = (lambda[i] * state[3] * Age_Sus[i] +                                 // IV0
                        ReducedSus_1 * lambda[i] * state[9] * Age_Sus[i] +                  // IV1
                        ReducedSus_1 * ReducedSus_2 * lambda[i] * state[15] * Age_Sus[i]) - // IV2
                       state[20];                                                           // Last timestep
        }
        else if (i == (num_age - 1))
        {
            // for the last age group - only dying out
            // S0
            yode[1 + i * 20] = (-lambda[i] * state[1 + i * 20] * Age_Sus[i]     // S0 to I0
                                - VacPopulation[0 + i * 3]                      // Normal vaccine, move to V0
                                - VacPopulation_SV[0 + i * 3]                   // Prevent hospitalization, move to SV0
                                + state[2 + i * 20] * omega_vac                 // Wange immunity from V0
                                + state[3 + i * 20] * omega_vac                 // Wange immunity from SV0
                                + ageing_rates[i - 1] * state[1 + (i - 1) * 20] // Ageing in
                                - Deaths * state[1 + i * 20] / dying_pop);      // Death

            // V0
            yode[2 + i * 20] = (+VacPopulation[0 + i * 3]                       // Normal vaccine from S0
                                - state[2 + i * 20] * omega_vac                 // Wange immunity
                                + ageing_rates[i - 1] * state[2 + (i - 1) * 20] // Ageing in
                                - Deaths * state[2 + i * 20] / dying_pop);      // Death

            // SV0
            yode[3 + i * 20] = (+VacPopulation_SV[0 + i * 3]                    // Prevent hospitalization
                                - lambda[i] * state[3 + i * 20] * Age_Sus[i]    // SV0 to IV0
                                - state[3 + i * 20] * omega_vac                 // Wane immunity
                                + ageing_rates[i - 1] * state[3 + (i - 1) * 20] // Ageing in
                                - Deaths * state[3 + i * 20] / dying_pop);      // Death

            // I0
            yode[4 + i * 20] = (+lambda[i] * state[1 + i * 20] * Age_Sus[i]     // From S0
                                - gamma * state[4 + i * 20]                     // Recovered
                                + ageing_rates[i - 1] * state[4 + (i - 1) * 20] // Ageing in
                                - Deaths * state[4 + i * 20] / dying_pop);      // Death

            // IV0
            yode[5 + i * 20] = (+lambda[i] * state[3 + i * 20] * Age_Sus[i]     // From SV0
                                - gamma * state[5 + i * 20]                     // Recovered
                                + ageing_rates[i - 1] * state[5 + (i - 1) * 20] // Ageing in
                                - Deaths * state[5 + i * 20] / dying_pop);      // Death

            // R0
            yode[6 + i * 20] = (+gamma * state[4 + i * 20]                      // Recovered from I0
                                + gamma * state[5 + i * 20]                     // Recovered from IV0
                                - omega_inf * state[6 + i * 20]                 // Wane immunity of Recovered
                                + ageing_rates[i - 1] * state[6 + (i - 1) * 20] // Ageing in
                                - Deaths * state[6 + i * 20] / dying_pop);      // Death

            // S1
            yode[7 + i * 20] = (+omega_inf * state[6 + i * 20]                              // Wane immunity from R0
                                - ReducedSus_1 * lambda[i] * state[7 + i * 20] * Age_Sus[i] // S1 to I1
                                - VacPopulation[1 + i * 3]                                  // Normal vaccine, move to V1
                                - VacPopulation_SV[1 + i * 3]                               // Prevent hospitalization, move to SV1
                                + state[8 + i * 20] * omega_vac                             // Wane immunity from V1
                                + state[9 + i * 20] * omega_vac                             // Wane immunity from SV1
                                + ageing_rates[i - 1] * state[7 + (i - 1) * 20]             // Ageing in
                                - Deaths * state[7 + i * 20] / dying_pop);                  // Death

            // V1
            yode[8 + i * 20] = (+VacPopulation[1 + i * 3]                       // Normal vaccine from S1
                                - state[8 + i * 20] * omega_vac                 // Wane immunity
                                + ageing_rates[i - 1] * state[8 + (i - 1) * 20] // Ageing in
                                - Deaths * state[8 + i * 20] / dying_pop);      // Death

            // SV1
            yode[9 + i * 20] = (+VacPopulation_SV[1 + i * 3]                                // Prevent hospitalization
                                - ReducedSus_1 * lambda[i] * state[9 + i * 20] * Age_Sus[i] // SV1 to IV1
                                - state[9 + i * 20] * omega_vac                             // Wane immunity
                                + ageing_rates[i - 1] * state[9 + (i - 1) * 20]             // Ageing in
                                - Deaths * state[9 + i * 20] / dying_pop);                  // Death

            //  I1
            yode[10 + i * 20] = (+ReducedSus_1 * lambda[i] * state[7 + i * 20] * Age_Sus[i] // From S1
                                 - gamma * ReducedRec_1 * state[10 + i * 20]                // Recovered
                                 + ageing_rates[i - 1] * state[10 + (i - 1) * 20]           // Ageing in
                                 - Deaths * state[10 + i * 20] / dying_pop);                // Death

            // IV1
            yode[11 + i * 20] = (+ReducedSus_1 * lambda[i] * state[9 + i * 20] * Age_Sus[i] // From SV1
                                 - gamma * ReducedRec_1 * state[11 + i * 20]                // Recovered
                                 + ageing_rates[i - 1] * state[11 + (i - 1) * 20]           // Ageing in
                                 - Deaths * state[11 + i * 20] / dying_pop);                // Death

            // R1
            yode[12 + i * 20] = (+gamma * ReducedRec_1 * state[10 + i * 20]       // Recovered from I1
                                 + gamma * ReducedRec_1 * state[11 + i * 20]      // Recovered from IV1
                                 - omega_inf * state[12 + i * 20]                 // Wane immunity of Recovered
                                 + ageing_rates[i - 1] * state[12 + (i - 1) * 20] // Ageing in
                                 - Deaths * state[12 + i * 20] / dying_pop);      // Death

            // S2
            yode[13 + i * 20] = (+omega_inf * state[12 + i * 20]                                             // Wane immunity from R1
                                 + omega_inf * state[18 + i * 20]                                            // More than 3 infection
                                 - ReducedSus_1 * ReducedSus_2 * lambda[i] * state[13 + i * 20] * Age_Sus[i] // S2 to I2
                                 - VacPopulation[2 + i * 3]                                                  // Normal vaccine, move to V2
                                 - VacPopulation_SV[2 + i * 3]                                               // Prevent hospitalization, move to SV2
                                 + state[14 + i * 20] * omega_vac                                            // Wane immunity from V2
                                 + state[15 + i * 20] * omega_vac                                            // Wane immunity from SV2
                                 + ageing_rates[i - 1] * state[13 + (i - 1) * 20]                            // Ageing in
                                 - Deaths * state[13 + i * 20] / dying_pop);                                 // Death

            // V2
            yode[14 + i * 20] = (+VacPopulation[2 + i * 3]                        // Normal vaccine from S2
                                 - state[14 + i * 20] * omega_vac                 // Wane immunity
                                 + ageing_rates[i - 1] * state[14 + (i - 1) * 20] // Ageing in
                                 - Deaths * state[14 + i * 20] / dying_pop);      // Death

            // SV2
            yode[15 + i * 20] = (+VacPopulation_SV[2 + i * 3]                                                // Prevent hospitalization
                                 - ReducedSus_1 * ReducedSus_2 * lambda[i] * state[15 + i * 20] * Age_Sus[i] // SV2 to IV2
                                 - state[15 + i * 20] * omega_vac                                            // Wane immunity
                                 + ageing_rates[i - 1] * state[15 + (i - 1) * 20]                            // Ageing in
                                 - Deaths * state[15 + i * 20] / dying_pop);                                 // Death

            // I2
            yode[16 + i * 20] = (+ReducedSus_1 * ReducedSus_2 * lambda[i] * state[13 + i * 20] * Age_Sus[i] // From S2
                                 - gamma * ReducedRec_1 * ReducedRec_2 * state[16 + i * 20]                 // Recovered
                                 + ageing_rates[i - 1] * state[16 + (i - 1) * 20]                           // Ageing in
                                 - Deaths * state[16 + i * 20] / dying_pop);                                // Death

            // IV2
            yode[17 + i * 20] = (+ReducedSus_1 * ReducedSus_2 * lambda[i] * state[15 + i * 20] * Age_Sus[i] // From SV2
                                 - gamma * ReducedRec_1 * ReducedRec_2 * state[17 + i * 20]                 // Recovered
                                 + ageing_rates[i - 1] * state[17 + (i - 1) * 20]                           // Ageing in
                                 - Deaths * state[17 + i * 20] / dying_pop);                                // Death

            // R2
            yode[18 + i * 20] = (+gamma * ReducedRec_1 * ReducedRec_2 * state[16 + i * 20] // Recovered from I2
                                                                                           //  gamma * ReducedRec_1 * ReducedRec_2 * state[17 + i * 20] - // Recovered from IV2
                                 - omega_inf * state[18 + i * 20]                          // Wane immunity of Recovered
                                 + ageing_rates[i - 1] * state[18 + (i - 1) * 20]          // Ageing in
                                 - Deaths * state[18 + i * 20] / dying_pop);               // Death

            // Report the number of new cases from I0, I1, I2
            yode[19 + i * 20] = (lambda[i] * state[1 + i * 20] * Age_Sus[i] +                                 // I0
                                 ReducedSus_1 * lambda[i] * state[7 + i * 20] * Age_Sus[i] +                  // I1
                                 ReducedSus_1 * ReducedSus_2 * lambda[i] * state[13 + i * 20] * Age_Sus[i]) - // I2
                                state[19 + i * 20];                                                           // Last timestep

            // Report the number of new cases from IV0, IV1, IV2
            yode[20 + i * 20] = (lambda[i] * state[3 + i * 20] * Age_Sus[i] +                                 // IV0
                                 ReducedSus_1 * lambda[i] * state[9 + i * 20] * Age_Sus[i] +                  // IV1
                                 ReducedSus_1 * ReducedSus_2 * lambda[i] * state[15 + i * 20] * Age_Sus[i]) - // IV2
                                state[20 + i * 20];
        }
        else if (i >= StartDyingOff - 1)
        {
            // S0
            yode[1 + i * 20] = (-lambda[i] * state[1 + i * 20] * Age_Sus[i]     // S0 to I0
                                - VacPopulation[0 + i * 3]                      // Normal vaccine, move to V0
                                - VacPopulation_SV[0 + i * 3]                   // Prevent hospitalization, move to SV0
                                + state[2 + i * 20] * omega_vac                 // Wange immunity from V0
                                + state[3 + i * 20] * omega_vac                 // Wange immunity from SV0
                                - ageing_rates[i] * state[1 + i * 20]           // Aging out
                                + ageing_rates[i - 1] * state[1 + (i - 1) * 20] // Aging in
                                - Deaths * state[1 + i * 20] / dying_pop);      // Deaths

            // V0
            yode[2 + i * 20] = (+VacPopulation[0 + i * 3]                       // Normal vaccine from S0
                                - state[2 + i * 20] * omega_vac                 // Wange immunity
                                - ageing_rates[i] * state[2 + i * 20]           // Aging out
                                + ageing_rates[i - 1] * state[2 + (i - 1) * 20] // Aging in
                                - Deaths * state[2 + i * 20] / dying_pop);      // Deaths

            // SV0
            yode[3 + i * 20] = (+VacPopulation_SV[0 + i * 3]                    // Prevent hospitalization
                                - lambda[i] * state[3 + i * 20] * Age_Sus[i]    // SV0 to IV0
                                - state[3 + i * 20] * omega_vac                 // Wane immunity
                                - ageing_rates[i] * state[3 + i * 20]           // Aging out
                                + ageing_rates[i - 1] * state[3 + (i - 1) * 20] // Aging in
                                - Deaths * state[3 + i * 20] / dying_pop);      // Deaths

            // I0
            yode[4 + i * 20] = (+lambda[i] * state[1 + i * 20] * Age_Sus[i]     // From S0
                                - gamma * state[4 + i * 20]                     // Recovered
                                - ageing_rates[i] * state[4 + i * 20]           // Aging out
                                + ageing_rates[i - 1] * state[4 + (i - 1) * 20] // Aging in
                                - Deaths * state[4 + i * 20] / dying_pop);      // Deaths

            // IV0
            yode[5 + i * 20] = (+lambda[i] * state[3 + i * 20] * Age_Sus[i]     // From SV0
                                - gamma * state[5 + i * 20]                     // Recovered
                                - ageing_rates[i] * state[5 + i * 20]           // Aging out
                                + ageing_rates[i - 1] * state[5 + (i - 1) * 20] // Aging in
                                - Deaths * state[5 + i * 20] / dying_pop);      // Deaths

            // R0
            yode[6 + i * 20] = (+gamma * state[4 + i * 20]                      // Recovered from I0
                                + gamma * state[5 + i * 20]                     // Recovered from IV0
                                - omega_inf * state[6 + i * 20]                 // Wane immunity of Recovered
                                - ageing_rates[i] * state[6 + i * 20]           // Aging out
                                + ageing_rates[i - 1] * state[6 + (i - 1) * 20] // Aging in
                                - Deaths * state[6 + i * 20] / dying_pop);      // Deaths

            // S1
            yode[7 + i * 20] = (+omega_inf * state[6 + i * 20]                              // Wane immunity from R0
                                - ReducedSus_1 * lambda[i] * state[7 + i * 20] * Age_Sus[i] // S1 to I1
                                - VacPopulation[1 + i * 3]                                  // Normal vaccine, move to V1
                                - VacPopulation_SV[1 + i * 3]                               // Prevent hospitalization, move to SV1
                                + state[8 + i * 20] * omega_vac                             // Wane immunity from V1
                                + state[9 + i * 20] * omega_vac                             // Wane immunity from SV1
                                - ageing_rates[i] * state[7 + i * 20]                       // Aging out
                                + ageing_rates[i - 1] * state[7 + (i - 1) * 20]             // Aging in
                                - Deaths * state[7 + i * 20] / dying_pop);                  // Deaths

            // V1
            yode[8 + i * 20] = (+VacPopulation[1 + i * 3]                       // Normal vaccine from S1
                                - state[8 + i * 20] * omega_vac                 // Wane immunity
                                - ageing_rates[i] * state[8 + i * 20]           // Aging out
                                + ageing_rates[i - 1] * state[8 + (i - 1) * 20] // Aging in
                                - Deaths * state[8 + i * 20] / dying_pop);      // Deaths

            // SV1
            yode[9 + i * 20] = (+VacPopulation_SV[1 + i * 3]                                // Prevent hospitalization
                                - ReducedSus_1 * lambda[i] * state[9 + i * 20] * Age_Sus[i] // SV1 to IV1
                                - state[9 + i * 20] * omega_vac                             // Wane immunity
                                - ageing_rates[i] * state[9 + i * 20]                       // Aging out
                                + ageing_rates[i - 1] * state[9 + (i - 1) * 20]             // Aging in
                                - Deaths * state[9 + i * 20] / dying_pop);                  // Deaths

            //  I1
            yode[10 + i * 20] = (+ReducedSus_1 * lambda[i] * state[7 + i * 20] * Age_Sus[i] // From S1
                                 - gamma * ReducedRec_1 * state[10 + i * 20]                // Recovered
                                 - ageing_rates[i] * state[10 + i * 20]                     // Aging out
                                 + ageing_rates[i - 1] * state[10 + (i - 1) * 20]           // Aging in
                                 - Deaths * state[10 + i * 20] / dying_pop);                // Deaths

            // IV1
            yode[11 + i * 20] = (+ReducedSus_1 * lambda[i] * state[9 + i * 20] * Age_Sus[i] // From SV1
                                 - gamma * ReducedRec_1 * state[11 + i * 20]                // Recovered
                                 - ageing_rates[i] * state[11 + i * 20]                     // Aging out
                                 + ageing_rates[i - 1] * state[11 + (i - 1) * 20]           // Aging in
                                 - Deaths * state[11 + i * 20] / dying_pop);                // Deaths

            // R1
            yode[12 + i * 20] = (+gamma * ReducedRec_1 * state[10 + i * 20]       // Recovered from I1
                                 + gamma * ReducedRec_1 * state[11 + i * 20]      // Recovered from IV1
                                 - omega_inf * state[12 + i * 20]                 // Wane immunity of Recovered
                                 - ageing_rates[i] * state[12 + i * 20]           // Aging out
                                 + ageing_rates[i - 1] * state[12 + (i - 1) * 20] // Aging in
                                 - Deaths * state[12 + i * 20] / dying_pop);      // Deaths

            // S2
            yode[13 + i * 20] = (+omega_inf * state[12 + i * 20]                                             // Wane immunity from R1
                                 + omega_inf * state[18 + i * 20]                                            // More than 3 infection
                                 - ReducedSus_1 * ReducedSus_2 * lambda[i] * state[13 + i * 20] * Age_Sus[i] // S2 to I2
                                 - VacPopulation[2 + i * 3]                                                  // Normal vaccine, move to V2
                                 - VacPopulation_SV[2 + i * 3]                                               // Prevent hospitalization, move to SV2
                                 + state[14 + i * 20] * omega_vac                                            // Wane immunity from V2
                                 + state[15 + i * 20] * omega_vac                                            // Wane immunity from SV2
                                 - ageing_rates[i] * state[13 + i * 20]                                      // Aging out
                                 + ageing_rates[i - 1] * state[13 + (i - 1) * 20]                            // Aging in
                                 - Deaths * state[13 + i * 20] / dying_pop);                                 // Deaths

            // V2
            yode[14 + i * 20] = (+VacPopulation[2 + i * 3]                        // Normal vaccine from S2
                                 - state[14 + i * 20] * omega_vac                 // Wane immunity
                                 - ageing_rates[i] * state[14 + i * 20]           // Aging out
                                 + ageing_rates[i - 1] * state[14 + (i - 1) * 20] // Aging in
                                 - Deaths * state[14 + i * 20] / dying_pop);      // Deaths

            // SV2
            yode[15 + i * 20] = (+VacPopulation_SV[2 + i * 3]                     // Prevent hospitalization
                                                                                  //-  ReducedSus_1 * ReducedSus_2 * lambda[i] * state[15 + i * 20] * Age_Sus[i]  // SV2 to IV2
                                 - state[15 + i * 20] * omega_vac                 // Wane immunity
                                 - ageing_rates[i] * state[15 + i * 20]           // Aging out
                                 + ageing_rates[i - 1] * state[15 + (i - 1) * 20] // Aging in
                                 - Deaths * state[15 + i * 20] / dying_pop);      // Deaths

            // I2
            yode[16 + i * 20] = (+ReducedSus_1 * ReducedSus_2 * lambda[i] * state[13 + i * 20] * Age_Sus[i] // From S2
                                 - gamma * ReducedRec_1 * ReducedRec_2 * state[16 + i * 20]                 // Recovered
                                 - ageing_rates[i] * state[16 + i * 20]                                     // Aging out
                                 + ageing_rates[i - 1] * state[16 + (i - 1) * 20]                           // Aging in
                                 - Deaths * state[16 + i * 20] / dying_pop);                                // Deaths

            // IV2
            yode[17 + i * 20] = (+ReducedSus_1 * ReducedSus_2 * lambda[i] * state[15 + i * 20] * Age_Sus[i] // From SV2
                                 - gamma * ReducedRec_1 * ReducedRec_2 * state[17 + i * 20]                 // Recovered
                                 - ageing_rates[i] * state[17 + i * 20]                                     // Aging out
                                 + ageing_rates[i - 1] * state[17 + (i - 1) * 20]                           // Aging in
                                 - Deaths * state[17 + i * 20] / dying_pop);                                // Deaths

            // R2
            yode[18 + i * 20] = (+gamma * ReducedRec_1 * ReducedRec_2 * state[16 + i * 20]  // Recovered from I2
                                 + gamma * ReducedRec_1 * ReducedRec_2 * state[17 + i * 20] // Recovered from IV2
                                 - omega_inf * state[18 + i * 20]                           // Wane immunity of Recovered
                                 - ageing_rates[i] * state[18 + i * 20]                     // Aging out
                                 + ageing_rates[i - 1] * state[18 + (i - 1) * 20]           // Aging in
                                 - Deaths * state[18 + i * 20] / dying_pop);                // Deaths

            // Report the number of new cases from I0, I1, I2
            yode[19 + i * 20] = (lambda[i] * state[1 + i * 20] * Age_Sus[i] +                                 // I0
                                 ReducedSus_1 * lambda[i] * state[7 + i * 20] * Age_Sus[i] +                  // I1
                                 ReducedSus_1 * ReducedSus_2 * lambda[i] * state[13 + i * 20] * Age_Sus[i]) - // I2
                                state[19 + i * 20];                                                           // Last timestep

            // Report the number of new cases from IV0, IV1, IV2
            yode[20 + i * 20] = (lambda[i] * state[3 + i * 20] * Age_Sus[i] +                                 // IV0
                                 ReducedSus_1 * lambda[i] * state[9 + i * 20] * Age_Sus[i] +                  // IV1
                                 ReducedSus_1 * ReducedSus_2 * lambda[i] * state[15 + i * 20] * Age_Sus[i]) - // IV2
                                state[20 + i * 20];                                                           // Last timestep
        }
        else
        { // Other age group
            // S0
            yode[1 + i * 20] = (-lambda[i] * state[1 + i * 20] * Age_Sus[i]       // S0 to I0
                                - VacPopulation[0 + i * 3]                        // Normal vaccine, move to V0
                                - VacPopulation_SV[0 + i * 3]                     // Prevent hospitalization, move to SV0
                                + state[2 + i * 20] * omega_vac                   // Wange immunity from V0
                                + state[3 + i * 20] * omega_vac                   // Wange immunity from SV0
                                - ageing_rates[i] * state[1 + i * 20]             // Aging out
                                + ageing_rates[i - 1] * state[1 + (i - 1) * 20]); // Aging in

            // V0
            yode[2 + i * 20] = (+VacPopulation[0 + i * 3]                         // Normal vaccine from S0
                                - state[2 + i * 20] * omega_vac                   // Wange immunity
                                - ageing_rates[i] * state[2 + i * 20]             // Aging out
                                + ageing_rates[i - 1] * state[2 + (i - 1) * 20]); // Aging in

            // SV0
            yode[3 + i * 20] = (+VacPopulation_SV[0 + i * 3]                      // Prevent hospitalization
                                - lambda[i] * state[3 + i * 20] * Age_Sus[i]      // SV0 to IV0
                                - state[3 + i * 20] * omega_vac                   // Wane immunity
                                - ageing_rates[i] * state[3 + i * 20]             // Aging out
                                + ageing_rates[i - 1] * state[3 + (i - 1) * 20]); // Aging in

            // I0
            yode[4 + i * 20] = (+lambda[i] * state[1 + i * 20] * Age_Sus[i]       // From S0
                                - gamma * state[4 + i * 20]                       // Recovered
                                - ageing_rates[i] * state[4 + i * 20]             // Aging out
                                + ageing_rates[i - 1] * state[4 + (i - 1) * 20]); // Aging in

            // IV0
            yode[5 + i * 20] = (+lambda[i] * state[3 + i * 20] * Age_Sus[i]       // From SV0
                                - gamma * state[5 + i * 20]                       // Recovered
                                - ageing_rates[i] * state[5 + i * 20]             // Aging out
                                + ageing_rates[i - 1] * state[5 + (i - 1) * 20]); // Aging in

            // R0
            yode[6 + i * 20] = (+gamma * state[4 + i * 20]                        // Recovered from I0
                                + gamma * state[5 + i * 20]                       // Recovered from IV0
                                - omega_inf * state[6 + i * 20]                   // Wane immunity of Recovered
                                - ageing_rates[i] * state[6 + i * 20]             // Aging out
                                + ageing_rates[i - 1] * state[6 + (i - 1) * 20]); // Aging in

            // S1
            yode[7 + i * 20] = (+omega_inf * state[6 + i * 20]                              // Wane immunity from R0
                                - ReducedSus_1 * lambda[i] * state[7 + i * 20] * Age_Sus[i] // S1 to I1
                                - VacPopulation[1 + i * 3]                                  // Normal vaccine, move to V1
                                - VacPopulation_SV[1 + i * 3]                               // Prevent hospitalization, move to SV1
                                + state[8 + i * 20] * omega_vac                             // Wane immunity from V1
                                + state[9 + i * 20] * omega_vac                             // Wane immunity from SV1
                                - ageing_rates[i] * state[7 + i * 20]                       // Aging out
                                + ageing_rates[i - 1] * state[7 + (i - 1) * 20]);           // Aging in

            // V1
            yode[8 + i * 20] = (+VacPopulation[1 + i * 3]                         // Normal vaccine from S1
                                - state[8 + i * 20] * omega_vac                   // Wane immunity
                                - ageing_rates[i] * state[8 + i * 20]             // Aging out
                                + ageing_rates[i - 1] * state[8 + (i - 1) * 20]); // Aging in

            // SV1
            yode[9 + i * 20] = (+VacPopulation_SV[1 + i * 3]                                // Prevent hospitalization
                                - ReducedSus_1 * lambda[i] * state[9 + i * 20] * Age_Sus[i] // SV1 to IV1
                                - state[9 + i * 20] * omega_vac                             // Wane immunity
                                - ageing_rates[i] * state[9 + i * 20]                       // Aging out
                                + ageing_rates[i - 1] * state[9 + (i - 1) * 20]);           // Aging in

            //  I1
            yode[10 + i * 20] = (+ReducedSus_1 * lambda[i] * state[7 + i * 20] * Age_Sus[i] // From S1
                                 - gamma * ReducedRec_1 * state[10 + i * 20]                // Recovered
                                 - ageing_rates[i] * state[10 + i * 20]                     // Aging out
                                 + ageing_rates[i - 1] * state[10 + (i - 1) * 20]);         // Aging in

            // IV1
            yode[11 + i * 20] = (+ReducedSus_1 * lambda[i] * state[9 + i * 20] * Age_Sus[i] // From SV1
                                 - gamma * ReducedRec_1 * state[11 + i * 20]                // Recovered
                                 - ageing_rates[i] * state[11 + i * 20]                     // Aging out
                                 + ageing_rates[i - 1] * state[11 + (i - 1) * 20]);         // Aging in

            // R1
            yode[12 + i * 20] = (+gamma * ReducedRec_1 * state[10 + i * 20]         // Recovered from I1
                                 + gamma * ReducedRec_1 * state[11 + i * 20]        // Recovered from IV1
                                 - omega_inf * state[12 + i * 20]                   // Wane immunity of Recovered
                                 - ageing_rates[i] * state[12 + i * 20]             // Aging out
                                 + ageing_rates[i - 1] * state[12 + (i - 1) * 20]); // Aging in

            // S2
            yode[13 + i * 20] = (+omega_inf * state[12 + i * 20]                                             // Wane immunity from R1
                                 + omega_inf * state[18 + i * 20]                                            // More than 3 infection
                                 - ReducedSus_1 * ReducedSus_2 * lambda[i] * state[13 + i * 20] * Age_Sus[i] // S2 to I2
                                 - VacPopulation[2 + i * 3]                                                  // Normal vaccine, move to V2
                                 - VacPopulation_SV[2 + i * 3]                                               // Prevent hospitalization, move to SV2
                                 + state[14 + i * 20] * omega_vac                                            // Wane immunity from V2
                                 + state[15 + i * 20] * omega_vac                                            // Wane immunity from SV2
                                 - ageing_rates[i] * state[13 + i * 20]                                      // Aging out
                                 + ageing_rates[i - 1] * state[13 + (i - 1) * 20]);                          // Aging in

            // V2
            yode[14 + i * 20] = (+VacPopulation[2 + i * 3]                          // Normal vaccine from S2
                                 - state[14 + i * 20] * omega_vac                   // Wane immunity
                                 - ageing_rates[i] * state[14 + i * 20]             // Aging out
                                 + ageing_rates[i - 1] * state[14 + (i - 1) * 20]); // Aging in

            // SV2
            yode[15 + i * 20] = (+VacPopulation_SV[2 + i * 3]                                                // Prevent hospitalization
                                 - ReducedSus_1 * ReducedSus_2 * lambda[i] * state[15 + i * 20] * Age_Sus[i] // SV2 to IV2
                                 - state[15 + i * 20] * omega_vac                                            // Wane immunity
                                 - ageing_rates[i] * state[15 + i * 20]                                      // Aging out
                                 + ageing_rates[i - 1] * state[15 + (i - 1) * 20]);                          // Aging in

            // I2
            yode[16 + i * 20] = (+ReducedSus_1 * ReducedSus_2 * lambda[i] * state[13 + i * 20] * Age_Sus[i] // From S2
                                 - gamma * ReducedRec_1 * ReducedRec_2 * state[16 + i * 20]                 // Recovered
                                 - ageing_rates[i] * state[16 + i * 20]                                     // Aging out
                                 + ageing_rates[i - 1] * state[16 + (i - 1) * 20]);                         // Aging in

            // IV2
            yode[17 + i * 20] = (+ReducedSus_1 * ReducedSus_2 * lambda[i] * state[15 + i * 20] * Age_Sus[i] // From SV2
                                 - gamma * ReducedRec_1 * ReducedRec_2 * state[17 + i * 20]                 // Recovered
                                 - ageing_rates[i] * state[17 + i * 20]                                     // Aging out
                                 + ageing_rates[i - 1] * state[17 + (i - 1) * 20]);                         // Aging in

            // R2
            yode[18 + i * 20] = (+gamma * ReducedRec_1 * ReducedRec_2 * state[16 + i * 20]  // Recovered from I2
                                 + gamma * ReducedRec_1 * ReducedRec_2 * state[17 + i * 20] // Recovered from IV2
                                 - omega_inf * state[18 + i * 20]                           // Wane immunity of Recovered
                                 - ageing_rates[i] * state[18 + i * 20]                     // Aging out
                                 + ageing_rates[i - 1] * state[18 + (i - 1) * 20]);         // Aging in

            // Report the number of new cases from I0, I1, I2
            yode[19 + i * 20] = (lambda[i] * state[1 + i * 20] * Age_Sus[i] +                                 // I0
                                 ReducedSus_1 * lambda[i] * state[7 + i * 20] * Age_Sus[i] +                  // I1
                                 ReducedSus_1 * ReducedSus_2 * lambda[i] * state[13 + i * 20] * Age_Sus[i]) - // I2
                                state[19 + i * 20];                                                           // Last timestep

            // Report the number of new cases from IV0, IV1, IV2
            yode[20 + i * 20] = (lambda[i] * state[3 + i * 20] * Age_Sus[i] +                                 // IV0
                                 ReducedSus_1 * lambda[i] * state[9 + i * 20] * Age_Sus[i] +                  // IV1
                                 ReducedSus_1 * ReducedSus_2 * lambda[i] * state[15 + i * 20] * Age_Sus[i]) - // IV2
                                state[20 + i * 20];                                                           // Last timestep
        }
    }

    List yout = List::create(yode);
    return (yout);
}
