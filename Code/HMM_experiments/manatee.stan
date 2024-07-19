// -------------------------------------------------
// States (S):
// 1 age0
// 2 age1
// 3 age2
// 4 age3
// 5 age4+
// 6 dead
// Observations (O):
// 1 seen as calf
// 2 seen as subadult
// 3 seen as adult
// 4 not seen
// -------------------------------------------------

functions {
  /**
   * Return an integer value denoting occasion of first capture.
   * This function is derived from Stan Modeling Language
   * User's Guide and Reference Manual.
   *
   * @param y         Observed values
   * @return Occasion of first capture
   */
  int first_capture(array[] int y_i) {
    for (k in 1 : size(y_i)) {
      if (y_i[k] != 4) {
        return k;
      }
    }
    return 0;
  }
}
data {
  int<lower=0> nind;
  int<lower=0> n_occasions;
  array[nind, n_occasions] int<lower=1, upper=4> y;
}
transformed data {
  int n_occ_minus_1 = n_occasions - 1;
  array[nind] int<lower=0, upper=n_occasions> first;
  
  for (i in 1 : nind) {
    first[i] = first_capture(y[i]);
  }
}
parameters {
  real<lower=0, upper=1> mean_phi0; // Mean age0 survival
  real<lower=0, upper=1> mean_phi1; // Mean 1y survival
  real<lower=0, upper=1> mean_phi2; // Mean 2y survival
  real<lower=0, upper=1> mean_phi3; // Mean 3y survival
  real<lower=0, upper=1> mean_phiad; // Mean ad survival
  real<lower=0, upper=1> mean_pC; // Mean recapture calf
  real<lower=0, upper=1> mean_pSA; // Mean recapture subadult
  real<lower=0, upper=1> mean_pA; // Mean recapture adult
  real<lower=0, upper=1> mean_d0c; // Mean record of YOY as calf
  real<lower=0, upper=1> mean_d0s; // Mean record of YOY as subadult
  real<lower=0, upper=1> mean_d1c; // Mean record of age1 as calf
  real<lower=0, upper=1> mean_d1s; // Mean record of age1 as subadult
  real<lower=0, upper=1> mean_d2c; // Mean record of age2 as calf
  real<lower=0, upper=1> mean_d2s; // Mean record of age2 as subadult
  real<lower=0, upper=1> mean_d3c; // Mean record of age3 as calf
  real<lower=0, upper=1> mean_d3s; // Mean record of age3 as subadult
  real<lower=0, upper=1> mean_d4c; // Mean record of age4+ as calf
  real<lower=0, upper=1> mean_d4s; // Mean record of age4+ as subadult
}
transformed parameters {
  vector<lower=0, upper=1>[n_occ_minus_1] phi_0; // YOY survival prob.
  vector<lower=0, upper=1>[n_occ_minus_1] phi_1; // First year survival prob.
  vector<lower=0, upper=1>[n_occ_minus_1] phi_2; // Second year survival prob.
  vector<lower=0, upper=1>[n_occ_minus_1] phi_3; // third year survival prob.
  vector<lower=0, upper=1>[n_occ_minus_1] phi_ad; // Adult survival prob.
  vector<lower=0, upper=1>[n_occ_minus_1] p_C; // Recapture prob. of calves
  vector<lower=0, upper=1>[n_occ_minus_1] p_SA; // Recapture prob. of subadults
  vector<lower=0, upper=1>[n_occ_minus_1] p_A; // Recapture prob. of adults
  vector<lower=0, upper=1>[n_occ_minus_1] d0c; // Prob. record of YOY as calf
  vector<lower=0, upper=1>[n_occ_minus_1] d0s; // Prob. record of YOY as subadult
  vector<lower=0, upper=1>[n_occ_minus_1] d1c; // Prob. record of age1 as calf
  vector<lower=0, upper=1>[n_occ_minus_1] d1s; // Prob. record of age1 as subadult
  vector<lower=0, upper=1>[n_occ_minus_1] d2c; // Prob. record of age2 as calf
  vector<lower=0, upper=1>[n_occ_minus_1] d2s; // Prob. record of age2 as subadult
  vector<lower=0, upper=1>[n_occ_minus_1] d3c; // Prob. record of age3 as calf
  vector<lower=0, upper=1>[n_occ_minus_1] d3s; // Prob. record of age3 as subadult
  vector<lower=0, upper=1>[n_occ_minus_1] d4c; // Prob. record of age4+ as calf
  vector<lower=0, upper=1>[n_occ_minus_1] d4s; // Prob. record of age4+ as subadult
  array[6, nind, n_occ_minus_1] simplex[6] ps; // prob. state
  array[6, nind, n_occ_minus_1] simplex[4] po; // prob. observation
  
  // Constraints
  for (t in 1 : n_occ_minus_1) {
    phi_0[t] = mean_phi0;
    phi_1[t] = mean_phi1;
    phi_2[t] = mean_phi2;
    phi_3[t] = mean_phi3;
    phi_ad[t] = mean_phiad;
    p_C[t] = mean_pC;
    p_SA[t] = mean_pSA;
    p_A[t] = mean_pA;
    d0c[t] = mean_d0c;
    d0s[t] = mean_d0s;
    d1c[t] = mean_d1c;
    d1s[t] = mean_d1s;
    d2c[t] = mean_d2c;
    d2s[t] = mean_d2s;
    d3c[t] = mean_d3c;
    d3s[t] = mean_d3s;
    d4c[t] = mean_d4c;
    d4s[t] = mean_d4s;
  }
  
  // Define state-transition and observation matrices
  for (i in 1 : nind) {
    // Define probabilities of state S(t+1) given S(t)
    // survival matrix
    for (t in 1 : n_occ_minus_1) {
      ps[1, i, t, 1] = 0.0; // prob state 1 -> 1 for ind. i at transition t
      ps[1, i, t, 2] = phi_0[t]; // prob state 1 -> 2 for ind. i at transition t
      ps[1, i, t, 3] = 0.0; // prob state 1 -> 3 for ind. i at transition t
      ps[1, i, t, 4] = 0.0; // etc., etc.
      ps[1, i, t, 5] = 0.0;
      ps[1, i, t, 6] = 1.0 - phi_0[t];
      ps[2, i, t, 1] = 0.0;
      ps[2, i, t, 2] = 0.0;
      ps[2, i, t, 3] = phi_1[t];
      ps[2, i, t, 4] = 0.0;
      ps[2, i, t, 5] = 0.0;
      ps[2, i, t, 6] = 1.0 - phi_1[t];
      ps[3, i, t, 1] = 0.0;
      ps[3, i, t, 2] = 0.0;
      ps[3, i, t, 3] = 0.0;
      ps[3, i, t, 4] = phi_2[t];
      ps[3, i, t, 5] = 0.0;
      ps[3, i, t, 6] = 1.0 - phi_2[t];
      ps[4, i, t, 1] = 0.0;
      ps[4, i, t, 2] = 0.0;
      ps[4, i, t, 3] = 0.0;
      ps[4, i, t, 4] = 0.0;
      ps[4, i, t, 5] = phi_3[t];
      ps[4, i, t, 6] = 1.0 - phi_3[t];
      ps[5, i, t, 1] = 0.0;
      ps[5, i, t, 2] = 0.0;
      ps[5, i, t, 3] = 0.0;
      ps[5, i, t, 4] = 0.0;
      ps[5, i, t, 5] = phi_ad[t];
      ps[5, i, t, 6] = 1.0 - phi_ad[t];
      ps[6, i, t, 1] = 0.0;
      ps[6, i, t, 2] = 0.0;
      ps[6, i, t, 3] = 0.0;
      ps[6, i, t, 4] = 0.0;
      ps[6, i, t, 5] = 0.0;
      ps[6, i, t, 6] = 1.0;
            
      // Define probabilities of O(t) given S(t)
      po[1, i, t, 1] = p_C[t] * d0c[t]; // prob of age0 observed as calf
      po[1, i, t, 2] = p_C[t] * d0s[t]; // prob of age0 observed as SA
      po[1, i, t, 3] = p_C[t] * (1.0-(d0c[t]+d0s[t])); // prob of age0 observed as A
      po[1, i, t, 4] = 1.0 - p_C[t]; // prob of age0 not seen
      po[2, i, t, 1] = p_C[t] * d1c[t]; // prob of age1 observed as calf
      po[2, i, t, 2] = p_C[t] * d1s[t]; // prob of age1 observed as SA
      po[2, i, t, 3] = p_C[t] * (1.0-(d1c[t]+d1s[t])); // prob of age1 observed as A
      po[2, i, t, 4] = 1.0 - p_C[t]; // prob of age1 not seen
      po[3, i, t, 1] = p_SA[t] * d2c[t]; 
      po[3, i, t, 2] = p_SA[t] * d2s[t];
      po[3, i, t, 3] = p_SA[t] * (1.0-(d2c[t]+d2s[t]));
      po[3, i, t, 4] = 1.0 - p_SA[t];
      po[4, i, t, 1] = p_SA[t] * d3c[t]; 
      po[4, i, t, 2] = p_SA[t] * d3s[t];
      po[4, i, t, 3] = p_SA[t] * (1.0-(d3c[t]+d3s[t]));
      po[4, i, t, 4] = 1.0 - p_SA[t];
      po[5, i, t, 1] = p_A[t] * d4c[t];
      po[5, i, t, 2] = p_A[t] * d4s[t];
      po[5, i, t, 3] = p_A[t] * (1.0-(d4c[t]+d4s[t]));
      po[5, i, t, 4] = 1.0 - p_A[t];
      po[6, i, t, 1] = 0.0; // prob if dead
      po[6, i, t, 2] = 0.0;
      po[6, i, t, 3] = 0.0;
      po[6, i, t, 4] = 1.0;
    }
  }
}
model {
  array[6] real acc; // put in N states
  array[n_occasions] vector[6] gamma;
  
  // Priors
  // Uniform priors are implicitly defined.
  
  // Likelihood
  // Forward algorithm derived from Stan Modeling Language
  // User's Guide and Reference Manual
  for (i in 1 : nind) {
    if (first[i] > 0) {
      for (k in 1 : 6) {
        gamma[first[i], k] = k == y[i, first[i]];
      }
      
      for (t in (first[i] + 1) : n_occasions) {
        for (k in 1 : 6) {
          for (j in 1 : 6) {
            acc[j] = gamma[t - 1, j] * ps[j, i, t - 1, k]
                     * po[k, i, t - 1, y[i, t]];
          }
          gamma[t, k] = sum(acc);
        }
      }
      target += log(sum(gamma[n_occasions]));
    }
  }
}
