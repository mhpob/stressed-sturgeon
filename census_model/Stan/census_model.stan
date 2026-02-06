functions {
  // THE FORWARD ALGORITHM
  // Calculates the likelihood of a capture history by summing over all possible state paths
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
      // Use fmax to prevent "Artificial Cliffs" where valid small probs became -Inf.
      log_init = log(fmax(start_dist[s], 1e-20));
      log_emit = log(fmax(rho[1][s, y[1]], 1e-20));

      alpha[1][s] = log_init + log_emit;
    }

    // --- DAY 2 to K ---
    for (t in 2:K) {
      // Transitions (Gamma; Omega in Coleman et al. 2024)
      for (s_next in 1:n_states) {
        for (s_prev in 1:n_states) {
          log_trans = log(fmax(gamma_mat[t-1][s_prev, s_next], 1e-20));
          acc[s_prev] = alpha[t-1][s_prev] + log_trans;
        }
        gamma_next[s_next] = log_sum_exp(acc);
      }

      // Update with Observation for Day t
      for (s in 1:n_states) {
        log_emit = log(fmax(rho[t][s, y[t]], 1e-20));
        alpha[t][s] = gamma_next[s] + log_emit;
      }
    }

    return log_sum_exp(alpha[K]);
  }
}

data {
  int<lower=1> K;             // Number of survey occasions (days)

  // Telemetry Data
  int<lower=1> n_telem;       // Number of telemetered fish (marked individuals)
  int<lower=n_telem> G;       // Augmented data, must be >> number of marked individuals    
  array[G, K] int<lower=1, upper=5> y;  // Capture histories (1=NotSeen, 2=LNR, etc.)
  array[G, K] int<lower=0, upper=1> TelemIndicator;
  
  // Covariates
  vector[K] temp;
  
  // SSS Data
  int<lower=1> Ksss;              // Number of SSS surveys
  int<lower=1> V;                 // Passes per survey
  array[Ksss, V] int<lower=0> sssMat;   // Counts
  array[Ksss] int<lower=1, upper=K> sssSurveyOcc; // Which day (1:K)
  array[Ksss] int<lower=2, upper=5> sssReach;  // Which reach (2:5 corresponding to LNR:UNR)           
}

parameters {
  // Survival & Recuitment
  real<upper=0> beta1;
  real alpha0;
  real alpha1;
  real<lower=0, upper=1> psi;  // Data augmentation inclusion prob    
  
  // Movement Probabilities
  simplex[4] init_probs;   // Beginning dist (LNR, LMC, UMC, UNR)       
  simplex[4] entry_probs;  // Dist of new entrants (LNR, LMC, UMC, UNR)
  // move_probs_X are the pi matrix in Coleman et al. 2024
  simplex[4] move_probs_LNR;  // From LNR to...
  simplex[4] move_probs_LMC;  // From LMC to...
  simplex[4] move_probs_UMC;  // From UMC to...
  simplex[4] move_probs_UNR;  // From UNR to...
  
  // Detection
  vector<lower=0, upper=1>[4] pbar_raw; // Probability of detection in a reach (2:5)
  real<lower=0, upper=1> p_sss; // Detection probability for SSS
  
  // Population Size (expected superpopulation size, not the actual realized N)
  real<lower=n_telem> LambdaSuper;
}

transformed parameters {
  vector<lower=0, upper=1>[K] phi;
  vector[K] b;                    // Entry probability per day
  array[K-1] matrix[6, 6] Gamma; // State Transition Matrices, Omega in Coleman et al. 2024       
  array[G, K] matrix[6, 5] Rho;  // Observation Matrices (Marked)       
  matrix[K, 6] state_probs;     // Prob of ANY fish being in state s at time k  
  // vector[K] temp_std;           

  // 1. Survival & Entry
  // temp_std = (temp - mean(temp)) / sd(temp);
  phi = inv_logit(alpha0 + alpha1 * temp);
  
  {
    vector[K] logits_b;
    for(k in 1:K) logits_b[k] = beta1 * k;
    b = softmax(logits_b); 
  }

  // 2. Build Transition Matrices (Gamma; Omega in Coleman et al. 2024)
  // Rows: From, Cols: To. States: 1:NE, 2:LNR, 3:LMC, 4:UMC, 5:UNR, 6:Exit
  for(t in 1:(K-1)){
    // Calculate Conditional Entry (Hazard Rate)
    // "Given I haven't entered yet, what is the chance I enter now?"
    // Note: Stan separates "entering now" from "staying not entered" differently.
    // We calculate "eta" equivalent implicitly via the state_probs propagation below.
    // Note the line below flips what is in the JAGS code. 
    //    In JAGS, it was 1-sum of past, in Stan it is sum of future
    real eta_t = b[t+1] / sum(b[(t+1):K]); // Conditional entry prob
    if(t == K-1) {
        eta_t = 1.0; // Final time step: everyone who hasn't entered must enter
    } else {
        eta_t = fmax(eta_t, 1e-9);  // Nudge to prevent log(0) issues in the forward algorithm  
        eta_t = fmin(eta_t, 1.0 - 1e-9); // Nudge to prevent log(1 - p) issues in the forward algorithm
    }
    
    // FROM Not Entered (1)
    Gamma[t, 1, 1] = 1 - eta_t; // Stay Not Entered
    Gamma[t, 1, 2] = eta_t * entry_probs[1]; // to LNR
    Gamma[t, 1, 3] = eta_t * entry_probs[2]; // to LMC
    Gamma[t, 1, 4] = eta_t * entry_probs[3]; // to UMC
    Gamma[t, 1, 5] = eta_t * entry_probs[4]; // to UNR
    Gamma[t, 1, 6] = 0;

    // FROM LNR (2)
    Gamma[t, 2, 1] = 0;
    Gamma[t, 2, 2] = phi[t] * fmax(move_probs_LNR[1], 1e-9); // stay LNR
    Gamma[t, 2, 3] = phi[t] * fmax(move_probs_LNR[2], 1e-9); // to LMC
    Gamma[t, 2, 4] = phi[t] * fmax(move_probs_LNR[3], 1e-9); // to UMC
    Gamma[t, 2, 5] = phi[t] * fmax(move_probs_LNR[4], 1e-9); // to UNR
    Gamma[t, 2, 6] = 1 - phi[t];                             // Exit/die

    // FROM LMC (3)
    Gamma[t, 3, 1] = 0;
    Gamma[t, 3, 2] = phi[t] * fmax(move_probs_LMC[1], 1e-9);
    Gamma[t, 3, 3] = phi[t] * fmax(move_probs_LMC[2], 1e-9); 
    Gamma[t, 3, 4] = phi[t] * fmax(move_probs_LMC[3], 1e-9);
    Gamma[t, 3, 5] = phi[t] * fmax(move_probs_LMC[4], 1e-9);
    Gamma[t, 3, 6] = 1 - phi[t];

    // FROM UMC (4)
    Gamma[t, 4, 1] = 0;
    Gamma[t, 4, 2] = phi[t] * fmax(move_probs_UMC[1], 1e-9);
    Gamma[t, 4, 3] = phi[t] * fmax(move_probs_UMC[2], 1e-9); 
    Gamma[t, 4, 4] = phi[t] * fmax(move_probs_UMC[3], 1e-9);
    Gamma[t, 4, 5] = phi[t] * fmax(move_probs_UMC[4], 1e-9);
    Gamma[t, 4, 6] = 1 - phi[t];

    // FROM UNR (5)
    Gamma[t, 5, 1] = 0;
    Gamma[t, 5, 2] = phi[t] * fmax(move_probs_UNR[1], 1e-9);
    Gamma[t, 5, 3] = phi[t] * fmax(move_probs_UNR[2], 1e-9); 
    Gamma[t, 5, 4] = phi[t] * fmax(move_probs_UNR[3], 1e-9);
    Gamma[t, 5, 5] = phi[t] * fmax(move_probs_UNR[4], 1e-9);
    Gamma[t, 5, 6] = 1 - phi[t];

    // FROM Exited (6)
    Gamma[t, 6, 1:5] = rep_row_vector(0, 5); // Can't re-enter
    Gamma[t, 6, 6] = 1; // Stays exited
  }
  
  // 3. Build Rho (Observation)
  // Dimensions: [Time, State, Obs]
  // Obs: 1=NotSeen, 2=LNR, 3=LMC, 4=UMC, 5=UNR
  for(i in 1:G){
    for(k in 1:K){
      Rho[i, k] = rep_matrix(0, 6, 5);

      // States 1 (NE) and 6 (Exit) can never be detected
      Rho[i, k, 1, 1] = 1.0;
      Rho[i, k, 6, 1] = 1.0;

      // Active States (2-5)
      // We use pbar_raw vector. index 1->LNR(state 2), 2->LMC(state 3), etc.
      real p_curr; 
      
      // LNR
      p_curr = pbar_raw[1] * TelemIndicator[i, k];
      Rho[i, k, 2, 1] = 1 - p_curr;
      Rho[i, k, 2, 2] = p_curr;     

      // LMC
      p_curr = pbar_raw[2] * TelemIndicator[i, k];
      Rho[i, k, 3, 1] = 1 - p_curr;
      Rho[i, k, 3, 3] = p_curr;      

      // UMC
      p_curr = pbar_raw[3] * TelemIndicator[i, k];
      Rho[i, k, 4, 1] = 1 - p_curr;
      Rho[i, k, 4, 4] = p_curr;      
      
      // UNR
      p_curr = pbar_raw[4] * TelemIndicator[i, k];
      Rho[i, k, 5, 1] = 1 - p_curr;
      Rho[i, k, 5, 5] = p_curr;      
    }
  }

  // 4. Propagate Population Probabilities (for SSS count data)
  {
    row_vector[6] init_dist;
    real safe_b1 = fmax(fmin(b[1], 0.999999), 1e-9);

    // Initial distribution (Day 1)
    init_dist[1] = 1 - safe_b1;
    init_dist[2] = fmax(safe_b1 * init_probs[1], 1e-9); 
    init_dist[3] = fmax(safe_b1 * init_probs[2], 1e-9); 
    init_dist[4] = fmax(safe_b1 * init_probs[3], 1e-9); 
    init_dist[5] = fmax(safe_b1 * init_probs[4], 1e-9);
    init_dist[6] = 0;
    
    state_probs[1] = init_dist;
    row_vector[6] current_dist = init_dist;
    
    for(t in 2:K){
      // Propagate: distribution * transition_matrix
      current_dist = current_dist * Gamma[t-1];
      state_probs[t] = current_dist;
    }
  }
}

model {
  // --- Priors ---
  // Centering on values and CIs offered by Coleman et al. 2024
  LambdaSuper ~ normal(70, 25); // "95% sure the average population is between 20 and 120."
  p_sss ~ beta(10, 3); // Mean = 0.83
  pbar_raw ~ beta(5, 1.5); // Mean ~0.77, allowing for some lower values due to p_4 in 2021
  beta1 ~ normal(-0.1, 0.05);
  alpha1 ~ normal(0.2, 0.15);
  psi ~ beta(1, 1);
  alpha0 ~ normal(0, 1.5);

  // theta vector (entry distribution) gets a Dirichlet prior:
  init_probs ~ dirichlet(rep_vector(1, 4));
  entry_probs ~ dirichlet(rep_vector(1, 4));

  // pi matrix rows (movement probabilities within river):
  move_probs_LNR ~ dirichlet(rep_vector(1, 4)); // pi row 2 (from LNR)
  move_probs_LMC ~ dirichlet(rep_vector(1, 4)); // pi row 3 (from LMC)
  move_probs_UMC ~ dirichlet( [10, 10, 10, 2]' ); // pi row 4 (from UMC, less likely to go to UNR)
  move_probs_UNR ~ dirichlet( [10, 10, 2, 10]' ); // pi row 5 (from UNR, less likely to go to UMC)

  // --- Likelihood 1: Marked Individuals ---
  vector[6] start_xi = to_vector(state_probs[1]); 

  for (i in 1:G) {
    if (sum(y[i]) > K) { // Check if individual was ever seen (sum of y > K if only 1s)
      // Conditional on being in superpopulation:
      // We mix the "psi" explicitly or just run it on observed.
      // Standard JS data augmentation in Stan usually conditions on observed:

      target += log(psi) + forward_alg(y[i], K, 6, start_xi, Gamma, Rho[i]);
    } else {
      // "Zero trick" or marginalizing 'psi' for never-seen individuals 
      // is handled in 'generated quantities' or via a specific N-mixture likelihood.
      // Assuming G contains only potential captures + augmentation
      // Log-probability of history y[i] (which is all 1s/zeros):

       real log_prob_zero = forward_alg(y[i], K, 6, start_xi, Gamma, Rho[i]);
       target += log_sum_exp(log(psi) + log_prob_zero, log1m(psi));
    }
  }

  // --- Likelihood 2: SSS Count Data ---
  for (k in 1:Ksss) {
    int day = sssSurveyOcc[k];
    int reach = sssReach[k]; 
    
    // Safety clamp 
    real expected_count = fmax(LambdaSuper * state_probs[day, reach] * p_sss, 1e-9);
    
    for(v in 1:V){
      // JAGS: sssMat ~ dbin(p_sss, Nreach[day, reach])
      // Stan: sssMat ~ Poisson(Expected_Count)
      sssMat[k, v] ~ poisson(expected_count);
    }
  }
}

generated quantities {
  real<lower=0> Nsuper = LambdaSuper;
  real<lower=0> Msuper = psi * G;
  // Allow negative here for diagnostics
  real Usuper = Nsuper - Msuper; 
  
  vector[K] nR = Nsuper * b;
  vector[K] mR = Msuper * b;
  vector[K] uR = Usuper * b;

  // --- Realized ---
  int<lower=0> Nsuper_realized = poisson_rng(LambdaSuper);
  int<lower=0> Msuper_realized = binomial_rng(G, psi);
  
  // Logic Check for Impossible outcomes: M cannot be greater than N.
  // If random chance makes M > N, we cap M at N to match reality
  // Need to do this as Usuper_realized is passed to multinomial_rng below,
  //  and will crash if negative.
  int<lower=0> Usuper_realized;
  if (Msuper_realized > Nsuper_realized) {
      Nsuper_realized = Msuper_realized; // Expand population to cover marked fish
      Usuper_realized = 0;
  } else {
      Usuper_realized = Nsuper_realized - Msuper_realized;
  }

  // Recruitment
  array[K] int<lower=0> nR_realized = multinomial_rng(b, Nsuper_realized);
  array[K] int<lower=0> mR_realized = multinomial_rng(b, Msuper_realized);
  array[K] int<lower=0> uR_realized;
    // Check to prevent multinomial_rng from crashing if Usuper_realized is 0 or negative
    // Can happen due to random chance in the Poisson and Binomial draws 
  if (Usuper_realized > 0) {
      uR_realized = multinomial_rng(b, Usuper_realized);
  } else {
      uR_realized = rep_array(0, K);
  }

  // --- Per-day Estimates ---
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
}
