
data {
    int<lower=2> T; // number of observed time points
    int<lower=2> H; // forecast horizon - number of weeks to do prediction
    int G; // number of age groups
    array[G] int<lower=0> N; // population size
    // int K; // order of the AR process on beta
    
    array[G] real<lower=0, upper=1> alpha; // recovery rate
    
    // array[T+H] int wk; // week index of each row of data
    int df;
    matrix[df, T+H] B;
    
    array[G, T] int y; // observed counts
    
    int<lower=0> sd_phi_df; // -> \infty gives a half N(0, scale^2) prior
    real<lower=0> sd_phi_scale;
}

transformed data {
    // vector[G] zeros = rep_vector(0, G);
    int Tmax = T + H;
    // int num_weeks = max(wk); // dimension of seasonal pattern
}

parameters {
    array[G] real<lower=0, upper=0.1> i_init;
    real<lower=0, upper=0.3> mu; // waning immunity rate

    real<lower=0, upper=0.5> kappa; // between age mixing rate
    array[G] real<lower=0, upper=0.05> rho; // probability an infectious case goes to hospital
    
    // vector[num_weeks] z_phi; // seasonal transmission pattern (uncentered parameterization)
    // array[G] real a;
    array[G] row_vector[df] b; // age x week transmission pattern
    
    // shared (for now) params for each age's ts:
    // real<lower=0> sd_phi;
    real<lower=0> sd_psi;
    
    // real<lower=-1, upper=1> w0;
    real<lower=-1, upper=1> w1;
    real<lower=-1, upper=1> w2;
    
    real<lower=10> inv_disp;
}

transformed parameters {
    // vector[num_weeks] phi; // seasonal transmission
    array[G] vector[Tmax] psi; // age x week transmission pattern
    array[G] vector[Tmax] beta;
    
    matrix[G, Tmax] C; // new infections
    matrix[G, Tmax] S;
    matrix[G, Tmax] I;
    
    // transform seasonal effect to orig. scale
    // phi[1] = z_phi[1];
    // phi[2] = z_phi[2];
    // for (i in 3:num_weeks) {
    //     phi[i] = 2*phi[i-1] - phi[i-2] + sd_phi*z_phi[i];
    // }
    // phi[3:num_weeks] = 2*phi[2:(num_weeks-1)] - phi[1:(num_weeks-2)] + sd_phi*z_phi[3:num_weeks];
    
    // set up transmission rate and initial conditions
    for (g in 1:G) {
        psi[g] = to_vector(b[g]*B);
        // beta[g] = exp(phi[wk] + psi[g]) / N[g];
        beta[g] = exp(psi[g]);
        
        C[g, 1] = y[g, 1];
        S[g, 1] = N[g] * (1-i_init[g]);
        I[g, 1] = N[g]*i_init[g];
    }
    
    // simulate model
    for (t in 2:(Tmax)) {
        for (g in 1:G) {
            real prob_inf = 0;
            for (gg in 1:G) {
                if (g == gg)
                    prob_inf += beta[gg, t]/N[g] * (1-kappa)^(G-1) * I[gg, t-1];
                else
                    prob_inf += beta[gg, t]/N[g] * kappa * I[gg, t-1];
                // print(prob_inf);
            }
            C[g, t] = S[g, t-1] * prob_inf;
            S[g, t] = S[g, t-1] - C[g, t] + mu*(N[g]-S[g, t-1]-I[g, t-1]);
            I[g, t] = I[g, t-1] + C[g, t] - alpha[g]*I[g, t-1];
        }
    }
    // print(phi);
    // print(psi);
    // print(I);
}

model {
    // hyperpriors
    // rho ~ beta(1.2, 5);
    kappa ~ beta(1.2, 5);
    inv_disp ~ cauchy(10, 3);
    // alpha[1] ~ uniform(0.7, 0.95);
    // alpha[2] ~ uniform(0.7, 0.95);

    // sd_phi ~ student_t(3, 0, 1);
    sd_psi ~ student_t(sd_phi_df, 0, sd_phi_scale);
    // w0 ~ normal(0, 0.8);
    w1 ~ normal(0, 0.8);
    w2 ~ normal(0, 0.8);
    
    // z_phi[3:num_weeks] ~ normal(0, 1);
    
    for (g in 1:G) {
        b[g, 1] ~ normal(0, sd_psi);
        // b[g, 2:df] ~ normal(w1*b[g, 1:(df-1)], sd_psi);
        b[g, 2] ~ normal(0, sd_psi);
        b[g, 3:df] ~ normal(w1*b[g, 2:(df-1)]+w2*b[g, 1:(df-2)], sd_psi);
        
        y[g, 3:T] ~ neg_binomial_2(rho[g] * C[g, 3:T], inv_disp); // TODO: C indexing ineff.
    }
}

generated quantities {
    array[G, Tmax] int yhat;
    array[G, Tmax] real eff_rnot_g;
    array[Tmax] real eff_rnot;
    
    for (g in 1:G) {
        yhat[g, 1] = y[g, 1];
        yhat[g, 2:Tmax] = neg_binomial_2_rng(rho[g] * C[g, 2:Tmax], inv_disp);
        
        // assumes G=2:
        int gg;
        if (g == 1)
            gg = 2;
        if (g == 2)
            gg = 1;
        for (t in 1:Tmax) { // TODO very inefficient lol
            eff_rnot_g[g, t] = beta[g, t] * ((1-kappa)*S[g, t] + kappa*S[gg, t]) / (alpha[g]*N[g]);
        }
    }
    for (t in 1:Tmax) { // TODO very inefficient lol
        eff_rnot[t] = (I[1, t]*eff_rnot_g[1, t] + I[2, t]*eff_rnot_g[2, t]) / (I[1, t]+I[2, t]);
    }
}
