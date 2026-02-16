

data {
  int<lower=1> J;                     // number of interviewees
  int<lower=1> K;                     // number of questions
  int<lower=1> A;                     // number of years
  int<lower=1> N;                     // number of answers to questions (observations)
  int<lower=1> R;                           // number of roles (teacher or student)
  array[N] int<lower=1,upper=J> jj;   // interviewee ID for observation n
  array[N] int<lower=1,upper=K> kk;   // question for observation n
  array[N] int<lower=1,upper=A> aa;   // interview year for observation n (2017=1, 2019=2)
  array[N] int<lower=1,upper=R> rr;         // role for observation n (teacher=1, student=2)
  array[N] int<lower=0,upper=1> y;    // prob of positive response for obs n
}

transformed data {
  vector[2] zeros = [0,0]';       // column vector of 0s, note transpose at the end to turn into row vectors
  vector[2] ones = [1,1]';        // column vector of 1s
  vector[2] SixHundreths = [0.1,0.1]'; //[0.06,0.06]'
}


parameters {

  //real muInt;                 //intercept mean
  array[J] vector[A] zInt;                  //J-array of column T-vectors of intercept z-scores for each individual
  vector<lower=0>[A] sigmaInt;     //intercept stdevs: 2-array of T-vectors, one vector for each ethnicity
  cholesky_factor_corr[A] L_R;   //2-array of cholesky factor TxT matrices, for correlation matrix for intercepts

  //array[A] vector[J] b1;

  vector[K] beta;                // different beta and gamma for each question in each year
  //real muGamma;
  vector<lower=0>[K] gamma;

  //array[K] vector<lower=-1>[R] off_Gamma;  // discrimination constrained positive in model block

  //real<lower=0> sigma_beta;      // scale of question
  //real<lower=0> sigma_gamma;     // scale of discrimination
}


transformed parameters {

  matrix[J,A] off_Int;                //matrix of random offsets for each individual from mean intercept, rows = indivs, cols = intercepts
                                      //Stan manual pg 150-151, Rethinking pg 409

  for (j in 1:J) {
    off_Int[j] = (diag_pre_multiply(sigmaInt, L_R) * zInt[j])';  //create cholesky factor for cov matrix and multiply by matrix of intercept z-scores, then transpose to turn into row vector.
  }
}



model {
  vector[N] params;
  real alpha;
  //real gamma;
  
  //muInt ~ normal(0,1);
  zInt ~ multi_normal(zeros, diag_matrix(ones));        //array of intercept z-scores for each indiv, sample two at a time from a normal
  sigmaInt ~ normal(1,1); //exponential(0.1); //1 10
  L_R ~ lkj_corr_cholesky(4);

  beta ~ normal(0,1);               //sigma_beta);
  //muGamma ~ normal(0,sigma_gamma);
  gamma ~ normal(1,1);

  //off_Gamma ~ multi_normal(zeros, diag_matrix(ones));
  
  //sigma_beta ~ exponential(100);    //10  // exponential(beta), where here beta = lambda = 1/mean
  //sigma_gamma ~ exponential(100);   // or use uniform(0,5), both prevent ceiling effect


  for (n in 1:N) {

    alpha = 0 + off_Int[jj[n],aa[n]];                      //random effect for person per year

    //gamma = 1 + off_Gamma[kk[n],rr[n]];                       // constrain gamma positive

    //params[n] = gamma*(alpha - beta[kk[n]]);
    params[n] = gamma[kk[n]]*(alpha - beta[kk[n]]);    

  } //for

  y ~ bernoulli_logit(params);
}


//generated quantities {       //for computing waic
  //vector[N] log_lik;
  //real alpha;

  //matrix[J,A] aInt;           //reconstructed intercept for each individual
  //matrix[A,A] R;         //2-array of TxT correlation matrices
  //matrix[A,A] Cov_R;       //2-array of TxT variance-covariance matrices

  //for (n in 1:N){
  //  aInt[jj[n],aa[n]] = muInt + off_Int[jj[n],aa[n]];

    //alpha = muInt + off_Int[jj[n],aa[n]];

    //log_lik[n] = bernoulli_logit_lpmf( y[n] | gamma[kk[n]]*(alpha - beta[kk[n]]) );

  //} //for

  //R = L_R * L_R';
  //Cov_R = diag_pre_multiply(sigmaInt, L_R) * diag_pre_multiply(sigmaInt, L_R)';
//}

