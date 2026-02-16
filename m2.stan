

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

  array[J] vector[A] zInt;       //J-array of column T-vectors of intercept z-scores for each individual
  vector<lower=0>[A] sigmaInt;   //intercept stdevs: 2-array of T-vectors, one vector for each ethnicity
  cholesky_factor_corr[A] L_R;   //2-array of cholesky factor TxT matrices, for correlation matrix for intercepts

  vector[K] beta;                // different beta and gamma for each question in each year
  vector<lower=0>[K] gamma;
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
  
  zInt ~ multi_normal(zeros, diag_matrix(ones));        //array of intercept z-scores for each indiv, sample two at a time from a normal
  sigmaInt ~ normal(1,1); 
  L_R ~ lkj_corr_cholesky(4);

  beta ~ normal(0,1); 
  gamma ~ normal(1,1);


  for (n in 1:N) {

    alpha = 0 + off_Int[jj[n],aa[n]];                      //random effect for person per year

    params[n] = gamma[kk[n]]*(alpha - beta[kk[n]]);    

  } //for

  y ~ bernoulli_logit(params);
}



