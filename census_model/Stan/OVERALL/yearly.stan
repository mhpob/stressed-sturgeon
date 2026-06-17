functions {
  // THE FORWARD ALGORITHM (Updated to 6 States)
  real forward_alg(
    array[] int y, int K, int n_states, vector start_dist, 
    array[] matrix gamma_mat, array[] matrix rho
  ) {
    vector[n_states] acc;
    vector[n_states] gamma_next;
    array[K] vector[n_states] alpha;

    real log_trans;
    real log_emit;
    real log_init;

    // --- DAY 1 ---
    for (s in 1:n_states) {
      log_init = log(fmax(start_dist[s], 1e-20));
      log_emit = log(fmax(rho[1][s, y[1]], 1e-20));
      alpha[1][s] = log_init + log_emit;
    }

    // --- DAY 2 to K ---
    for (t in 2:K) {
      for (s_next in 1:n_states) {
        for (s_prev in 1:n_states) {
          log_trans = log(fmax(gamma_mat[t-1][s_prev, s_next], 1e-20));
          acc[s_prev] = alpha[t-1][s_prev] + log_trans;
        }
        gamma_next[s_next] = log_sum_exp(acc);
      }
      for (s in 1:n_states) {
        log_emit = log(fmax(rho[t][s, y[t]], 1e-20));
        alpha[t][s] = gamma_next[s] + log_emit;
      }
    }
    return log_sum_exp(alpha[K]);
  }
}

data {
  int<lower=1> K;                       
  int<lower=1> n_years;                 
  
  // Robust Design Temporal Indexing
  array[n_years] int<lower=1> yr_start; 
  array[n_years] int<lower=1> yr_end;   
  array[K] int<lower=0, upper=1> is_winter_jump; 
  
  // --- NEW: Piecewise Covariate Data ---
  array[K] int<lower=1, upper=366> doy;  // Day of the year for each timestep t
  int<lower=1, upper=366> doy_log_start; // Date when logistic regression begins
  int<lower=1, upper=366> doy_log_end;   // Date when logistic regression ends
  vector[K] temp;

  // Telemetry Data (Augmented)
  int<lower=1> n_telem;       
  int<lower=n_telem> G;           
  array[G, K] int<lower=1, upper=5> y;  
  array[G, K] int<lower=0, upper=1> TelemIndicator;
  
  // SSS Data
  int<lower=1> Ksss;              
  int<lower=1> V;                 
  array[Ksss, V] int<lower=0> sssMat;   
  array[Ksss] int<lower=1, upper=K> sssSurveyOcc; 
  array[Ksss] int<lower=2, upper=5> sssReach;            
}

transformed data {
  // Pre-calculate a vector of days for fast vector math later
  vector[K] d_vec;
  for (d in 1:K) {
    d_vec[d] = d;
  }
}

parameters {
  // Recruitment 
  real<upper=0> beta1;                   
  real<lower=0, upper=1> psi;            
  vector<lower=0, upper=1>[n_years] psi_year; 
  
  // --- NEW: Piecewise Retention Parameters ---
  real<lower=0.5, upper=1> phi_high; // Constant high probability early in season
  real<lower=0, upper=0.5> phi_low;  // Constant low probability late in season
  real alpha0;               // Baseline logit per reach (for middle window)
  real alpha1;                     // Covariate effect (for middle window)
  
  // Movement Probabilities
  simplex[4] init_probs;      // Where they start (reaches 1-4)
  simplex[4] entry_probs;     // Where they enter (reaches 1-4)
  
  // 5th element is the probability of moving to State 6 (Exit)
  simplex[4] move_probs_LNR;  
  simplex[4] move_probs_LMC;  
  simplex[4] move_probs_UMC;  
  simplex[4] move_probs_UNR;  
  
  // Detection
  vector<lower=0, upper=1>[4] pbar_raw; 
  real<lower=0, upper=1> p_sss; 
  
  // Superpopulation
  real<lower=n_telem> LambdaSuper;
}

transformed parameters {
  vector<lower=0, upper=1>[K] phi;
  vector[K] eta_t;               // Daily conditional entry prob
  array[K-1] matrix[6, 6] Gamma; // 6-State Transition Matrices      
  // array[G, K] matrix[6, 5] Rho;  // Observation Matrices 
  matrix[6, 5] Rho_active;
  matrix[6, 5] Rho_inactive;     
  matrix[K, 6] state_probs;      // Population distribution  

  // 1. Calculate Per-Year Conditional Entry (eta_t)
  for (yr in 1:n_years) {
    int start_idx = yr_start[yr];
    int end_idx = yr_end[yr];
    int D_yr = end_idx - start_idx + 1;
    
    // Create the timing curve for this specific year
    vector[D_yr] logits_b = beta1 * d_vec[1:D_yr];
    vector[D_yr] b_yr = softmax(logits_b);
    
    // Convert unconditional probability to hazard rate (eta_t)
    real cumulative_entered = 0.0;
    for(d in 1:D_yr) {
      int t = start_idx + d - 1;
      real unconditional_entry = psi_year[yr] * b_yr[d];
      real prob_state1 = 1.0 - cumulative_entered;
      
      eta_t[t] = unconditional_entry / fmax(prob_state1, 1e-9);
      eta_t[t] = fmax(fmin(eta_t[t], 1.0 - 1e-9), 1e-9); 
      
      cumulative_entered += unconditional_entry;
    }
  }
  
  // Fill winter jump days with 0 entry
  for(t in 1:K) {
    if (is_winter_jump[t] == 1) eta_t[t] = 0.0;
  }

  for(t in 1:K) {
    if (doy[t] < doy_log_start) {
      phi[t] = phi_high;
    } else if (doy[t] > doy_log_end) {
      phi[t] = phi_low;
    } else {
      // Calculates once per day, applies to all reaches
      phi[t] = inv_logit(alpha0 + alpha1 * temp[t]);
    }
  }

  // 2. Build Transition Matrices (Gamma)
  for(t in 1:(K-1)){
    Gamma[t] = rep_matrix(0.0, 6, 6);
    
    if (is_winter_jump[t] == 0) {
      // --- ACTIVE SEASON ---
      // FROM Not Entered (1)
      Gamma[t, 1, 1] = 1 - eta_t[t]; 
      Gamma[t, 1, 2] = eta_t[t] * entry_probs[1]; 
      Gamma[t, 1, 3] = eta_t[t] * entry_probs[2]; 
      Gamma[t, 1, 4] = eta_t[t] * entry_probs[3]; 
      Gamma[t, 1, 5] = eta_t[t] * entry_probs[4]; 
      
      // FROM Reaches (2-5). The 5th element maps to State 6 (Exit)
      Gamma[t, 2, 2] = phi[t] * fmax(move_probs_LNR[1], 1e-9); 
      Gamma[t, 2, 3] = phi[t] * fmax(move_probs_LNR[2], 1e-9); 
      Gamma[t, 2, 4] = phi[t] * fmax(move_probs_LNR[3], 1e-9); 
      Gamma[t, 2, 5] = phi[t] * fmax(move_probs_LNR[4], 1e-9); 
      Gamma[t, 2, 6] = 1 - phi[t]; 
  
      Gamma[t, 3, 2] = phi[t] * fmax(move_probs_LMC[1], 1e-9);
      Gamma[t, 3, 3] = phi[t] * fmax(move_probs_LMC[2], 1e-9); 
      Gamma[t, 3, 4] = phi[t] * fmax(move_probs_LMC[3], 1e-9);
      Gamma[t, 3, 5] = phi[t] * fmax(move_probs_LMC[4], 1e-9);
      Gamma[t, 3, 6] = 1 - phi[t];
  
      Gamma[t, 4, 2] = phi[t] * fmax(move_probs_UMC[1], 1e-9);
      Gamma[t, 4, 3] = phi[t] * fmax(move_probs_UMC[2], 1e-9); 
      Gamma[t, 4, 4] = phi[t] * fmax(move_probs_UMC[3], 1e-9);
      Gamma[t, 4, 5] = phi[t] * fmax(move_probs_UMC[4], 1e-9);
      Gamma[t, 4, 6] = 1 - phi[t];
  
      Gamma[t, 5, 2] = phi[t] * fmax(move_probs_UNR[1], 1e-9);
      Gamma[t, 5, 3] = phi[t] * fmax(move_probs_UNR[2], 1e-9); 
      Gamma[t, 5, 4] = phi[t] * fmax(move_probs_UNR[3], 1e-9);
      Gamma[t, 5, 5] = phi[t] * fmax(move_probs_UNR[4], 1e-9);
      Gamma[t, 5, 6] = 1 - phi[t];
      
      // FROM Exited (6) -> Stays in Exited
      Gamma[t, 6, 6] = 1.0;

    } else {
      // --- WINTER JUMP (Seasonal Reset) ---
      // 100% of fish flush back to the Unentered Pool (State 1) 
      // This includes fish that were in the river (2-5) and fish that already exited (6)
      Gamma[t, 1, 1] = 1.0;
      Gamma[t, 2, 1] = 1.0;
      Gamma[t, 3, 1] = 1.0;
      Gamma[t, 4, 1] = 1.0;
      Gamma[t, 5, 1] = 1.0;
      Gamma[t, 6, 1] = 1.0;
    }
  }
  
  // 3. Build Rho (Observation) - 6 States
  Rho_active = rep_matrix(0.0, 6, 5);
  Rho_inactive = rep_matrix(0.0, 6, 5);

  // States 1 (Pool) and 6 (Exited) are unobservable (always output 1: "Not Seen")
  Rho_active[1, 1] = 1.0;   Rho_inactive[1, 1] = 1.0;
  Rho_active[6, 1] = 1.0;   Rho_inactive[6, 1] = 1.0;

  // Active River States (2-5)
  for (s in 1:4) {
    int state = s + 1; // Map 1-4 to matrix rows 2-5

    // If TelemIndicator == 1 (Active)
    Rho_active[state, 1] = 1.0 - pbar_raw[s]; // Probability of not being seen
    Rho_active[state, s + 1] = pbar_raw[s];   // Probability of being seen in correct reach

    // If TelemIndicator == 0 (Inactive), detection probability is 0
    Rho_inactive[state, 1] = 1.0; // 100% chance of "Not Seen"
    // Rho_inactive[state, s + 1] remains 0.0 from the rep_matrix initialization
  }  
  
  // for(i in 1:G){
  //   for(k in 1:K){
  //     Rho[i, k] = rep_matrix(0.0, 6, 5);
  //
  //     // Unobservable states (Pool and Exited)
  //     Rho[i, k, 1, 1] = 1.0;
  //     Rho[i, k, 6, 1] = 1.0;
  //
  //     // Active States (2-5)
  //     real p_curr; 
  //      
  //     p_curr = pbar_raw[1] * TelemIndicator[i, k];
  //     Rho[i, k, 2, 1] = 1 - p_curr; Rho[i, k, 2, 2] = p_curr;     
  //
  //     p_curr = pbar_raw[2] * TelemIndicator[i, k];
  //     Rho[i, k, 3, 1] = 1 - p_curr; Rho[i, k, 3, 3] = p_curr;      
  //
  //     p_curr = pbar_raw[3] * TelemIndicator[i, k];
  //     Rho[i, k, 4, 1] = 1 - p_curr; Rho[i, k, 4, 4] = p_curr;      
  //     
  //     p_curr = pbar_raw[4] * TelemIndicator[i, k];
  //     Rho[i, k, 5, 1] = 1 - p_curr; Rho[i, k, 5, 5] = p_curr;      
  //   }
  // }

  // 4. Propagate Population Probabilities
  {
    row_vector[6] init_dist = rep_row_vector(0.0, 6);
    real safe_eta1 = fmax(fmin(eta_t[1], 0.999999), 1e-9);

    init_dist[1] = 1 - safe_eta1;
    init_dist[2] = fmax(safe_eta1 * init_probs[1], 1e-9); 
    init_dist[3] = fmax(safe_eta1 * init_probs[2], 1e-9); 
    init_dist[4] = fmax(safe_eta1 * init_probs[3], 1e-9); 
    init_dist[5] = fmax(safe_eta1 * init_probs[4], 1e-9);
    
    state_probs[1] = init_dist;
    row_vector[6] current_dist = init_dist;
    
    for(t in 2:K){
      current_dist = current_dist * Gamma[t-1];
      state_probs[t] = current_dist;
    }
  }
}

model {
  // Priors
  LambdaSuper ~ normal(70, 25); 
  p_sss ~ beta(10, 3); 
  pbar_raw ~ beta(5, 1.5); 
  alpha0 ~ normal(0, 1.5);
  alpha1 ~ normal(0.2, 0.15);
  beta1 ~ normal(-0.1, 0.05);
  psi ~ beta(1, 1);
  psi_year ~ beta(2, 2);
  // Piecewise Priors
  phi_high ~ beta(10, 2);    
  phi_low ~ beta(2, 10);     

  // Priors for movement (now including the 5th "Exit" dimension)
  init_probs ~ dirichlet(rep_vector(1, 4));
  entry_probs ~ dirichlet(rep_vector(1, 4));
  move_probs_LNR ~ dirichlet(rep_vector(1, 4)); 
  move_probs_LMC ~ dirichlet(rep_vector(1, 4)); 
  move_probs_UMC ~ dirichlet( [10, 10, 10, 2]' ); 
  move_probs_UNR ~ dirichlet( [10, 10, 2, 10]' ); 

  // Likelihood 1: Marked Individuals 
  vector[6] start_xi = to_vector(state_probs[1]); 

  for (i in 1:G) {
    // Build this individual's sequence of Rho matrices on the fly
    array[K] matrix[6, 5] Rho_i;
    for (k in 1:K) {
      if (TelemIndicator[i, k] == 1) {
        Rho_i[k] = Rho_active;
      } else {
        Rho_i[k] = Rho_inactive;
      }
    }
    
    // Pass the temporary Rho_i into the forward algorithm
    if (sum(y[i]) > K) { 
      target += log(psi) + forward_alg(y[i], K, 6, start_xi, Gamma, Rho_i);
    } else {
       real log_prob_zero = forward_alg(y[i], K, 6, start_xi, Gamma, Rho_i);
       target += log_sum_exp(log(psi) + log_prob_zero, log1m(psi));
    }
  }

  // Likelihood 2: SSS Count Data
  for (k in 1:Ksss) {
    int day = sssSurveyOcc[k];
    int reach = sssReach[k]; 
    real expected_count = fmax(LambdaSuper * state_probs[day, reach] * p_sss, 1e-9);
    // for(v in 1:V){
    //   sssMat[k, v] ~ poisson(expected_count);
    // }
    sssMat[k] ~ poisson(expected_count);
  }
}

generated quantities {
  // --- Population Sizes ---
  real<lower=0> Nsuper = LambdaSuper;
  real<lower=0> Msuper = psi * G;
  real Usuper = Nsuper - Msuper; 

  // --- Per-day Expected Abundance ---
  vector[K] N_LNR = LambdaSuper * col(state_probs, 2);
  vector[K] N_LMC = LambdaSuper * col(state_probs, 3);
  vector[K] N_UMC = LambdaSuper * col(state_probs, 4);
  vector[K] N_UNR = LambdaSuper * col(state_probs, 5);
  vector[K] N = N_LNR + N_LMC + N_UMC + N_UNR;
  
  vector[K] M_LNR = Msuper * col(state_probs, 2);
  vector[K] M_LMC = Msuper * col(state_probs, 3);
  vector[K] M_UMC = Msuper * col(state_probs, 4);
  vector[K] M_UNR = Msuper * col(state_probs, 5);
  vector[K] M = M_LNR + M_LMC + M_UMC + M_UNR;

  // --- Expected Continuous Recruits (nR, mR, uR) ---
  vector[K] enter_prob = rep_vector(0.0, K);
  enter_prob[1] = eta_t[1];

  // Probability of entering on day t is being in state 1 on t-1, 
  // multiplied by the conditional entry probability eta_t
  enter_prob[2:K] = col(state_probs, 1)[1:(K-1)] .* eta_t[2:K];

  vector[K] nR = Nsuper * enter_prob;
  vector[K] mR = Msuper * enter_prob;
  vector[K] uR = Usuper * enter_prob;

  // --- Realized Population Processes ---
  int<lower=0> Nsuper_realized = poisson_rng(LambdaSuper);
  int<lower=0> Msuper_realized = binomial_rng(G, psi);
  int<lower=0> Usuper_realized;
  
  // Logic Check for Impossible outcomes
  if (Msuper_realized > Nsuper_realized) {
      Nsuper_realized = Msuper_realized; 
      Usuper_realized = 0;
  } else {
      Usuper_realized = Nsuper_realized - Msuper_realized;
  }

  // Arrays to hold the realized daily recruits
  array[K] int<lower=0> nR_realized = rep_array(0, K);
  array[K] int<lower=0> mR_realized = rep_array(0, K);
  array[K] int<lower=0> uR_realized = rep_array(0, K);

  // Distribute the realized fish per year 
  for (yr in 1:n_years) {
    int start_idx = yr_start[yr];
    int end_idx = yr_end[yr];
    int D_yr = end_idx - start_idx + 1;
    
    // Recreate the entry simplex (b_yr) for the specific year
    vector[D_yr] logits_b;
    for(d in 1:D_yr) logits_b[d] = beta1 * d;
    vector[D_yr] b_yr = softmax(logits_b); 
    
    // 1. Determine how many of the superpopulation enter THIS year
    int N_yr = binomial_rng(Nsuper_realized, psi_year[yr]);
    int M_yr = binomial_rng(Msuper_realized, psi_year[yr]);
    int U_yr;
    if (Usuper_realized > 0) {
        U_yr = binomial_rng(Usuper_realized, psi_year[yr]);
    } else {
        U_yr = 0;
    }
    
    // 2. Distribute those specific fish across the days of the year
    array[D_yr] int n_yr_arr = multinomial_rng(b_yr, N_yr);
    array[D_yr] int m_yr_arr = multinomial_rng(b_yr, M_yr);
    array[D_yr] int u_yr_arr;
    if (U_yr > 0) {
        u_yr_arr = multinomial_rng(b_yr, U_yr);
    } else {
        u_yr_arr = rep_array(0, D_yr);
    }
    
    // 3. Map the daily results back to the global K-length arrays
    for (d in 1:D_yr) {
      int t = start_idx + d - 1;
      nR_realized[t] = n_yr_arr[d];
      mR_realized[t] = m_yr_arr[d];
      uR_realized[t] = u_yr_arr[d];
    }
  }
}