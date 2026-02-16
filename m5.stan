

data {
  int<lower=1> J;                           // number of interviewees
  int<lower=1> K;                           // number of questions
  int<lower=1> A;                           // number of years
  int<lower=1> S;                           // number of schools
  int<lower=1> R;                           // number of roles (teacher or student)
  int<lower=1> N;                           // number of answers to questions (observations)
  array[N] int<lower=1,upper=J> jj;         // interviewee ID for observation n
  array[N] int<lower=1,upper=K> kk;         // question for observation n
  array[N] int<lower=1,upper=A> aa;         // interview year for observation n (2017=1, 2019=2)
  array[N] int<lower=1,upper=S> ss;         // school for observation n, 1=Yomi, 2=Boca, 3=Salv, 4=Shin
  array[N] int<lower=1,upper=R> rr;         // role for observation n (teacher=1, student=2)
  array[N] int<lower=0,upper=1> y;          // prob of positive response for obs n

  int<lower=1> G;                           // number of grades
  array[N] int<lower=0,upper=G> Grade;      // grade for obs n
}

transformed data {
  vector[2] zeros = [0,0]';       // column vector of 0s, note transpose at the end to turn into row vectors
  vector[2] ones = [1,1]';
  vector[2] halves = [0.5,0.5]';
  vector[2] SixHundreths = [0.06,0.06]'; //[0.05,0.05]';   //[0.1,0.1]';
  vector[G] twos = [2,2,2,2,2]';
}


parameters {

  //real muInt;                             // overall mean ability (intercept)

  array[J] vector[A] zInt;                // J-array of column A-vectors of intercept offset z-scores for each individual for each year
  vector<lower=0>[A] sigmaInt;            // intercept offset stdevs: one for each year
  cholesky_factor_corr[A] L_R;            // cholesky factor AxA matrices, for correlation matrix for intercept offsets


  array[R,S] vector[A] zSch;              // RxS-array of column A-vectors of intercept offset z-scores for each school for each year for each role
  array[R] vector<lower=0>[A] sigmaSch;   // R-array of school offset stdevs: one for each year
  array[R] cholesky_factor_corr[A] L_S;   // R-array of cholesky factor AxA matrices, for correlation matrix for school offsets


  array[S,A] simplex[G] X;       // matrix of grade simpleces, rows=schools, columns=years
  array[S] vector[A] bGrade;     // coef for grade, by school per year

  //real muBeta;
  vector[K] beta;
  //real muGamma;
  //vector<lower=0>[K] gamma;

  //array[K] vector[R] off_Beta;
  array[K] vector<lower=-1>[R] off_Gamma;     // discrimination constrained positive in model block

  //real<lower=0> sigma_beta;      // scale of question
  //real<lower=0> sigma_gamma;     // scale of discrimination
}


transformed parameters {

  array[S,A] vector[G] CumuGrade;     // array matrix of cumulative sum vectors, rows=schools, columns=years
  matrix[J,A] off_Int;                // location of people (differing from the mean), i.e., random effect for person, for average school (per year)
  array[R,S] row_vector[A] off_Sch;   // location of average person from each school (differing from mean), i.e., random effect for school, for average person, for each role (per year)

  for (j in 1:J) {
    off_Int[j] = (diag_pre_multiply(sigmaInt, L_R) * zInt[j])';  //create cholesky factor for cov matrix and multiply by matrix of intercept z-scores, then transpose to turn into row vector. year-specific random effect for individual
  }

  for (r in 1:R) {
    for (s in 1:S) {
      off_Sch[r,s] = (diag_pre_multiply(sigmaSch[r], L_S[r]) * zSch[r,s])';  // role- and year-specific random effect for school
    } 
  }

  for (s in 1:S) {
    for (a in 1:A) {
      CumuGrade[s,a] = cumulative_sum(X[s,a]);
    } 
  }

}


model {
  vector[N] params;
  real alpha;
  //real beta;
  real gamma;
  
  //muInt ~ normal(0,0.1);

  zInt ~ multi_normal(zeros, diag_matrix(ones));
  sigmaInt ~ normal(1,1);                         // exponential(10);
  L_R ~ lkj_corr_cholesky(4);

  for (r in 1:R) {
    sigmaSch[r] ~ normal(1,1);                         //exponential(10);
    L_S[r] ~ lkj_corr_cholesky(4);

    for (s in 1:S) {
      zSch[r,s] ~ multi_normal(zeros, diag_matrix(ones)); // halves
    } // s
  } // r

  for (s in 1:S) {
    for (a in 1:A) {
        X[s,a] ~ dirichlet(twos);
    } 
  }
  bGrade ~ multi_normal(zeros, diag_matrix(ones)); 


  //muBeta ~ normal(0,sigma_beta);
  beta ~ normal(0,1);               //normal(0,sigma_beta);
  //muGamma ~ normal(0,sigma_gamma);
  //gamma ~ normal(1,1);               //normal(0,sigma_gamma);
  
  //off_Beta ~ multi_normal(zeros, diag_matrix(ones));
  off_Gamma ~ multi_normal(zeros, diag_matrix(ones)); //SixHundreths

  //sigma_beta ~ exponential(10);    // exponential(beta), where here beta = lambda = 1/mean
  ///sigma_gamma ~ exponential(100);   // or use uniform(0,5), both prevent ceiling effect



  for (n in 1:N) {

    alpha = 0 +
            off_Int[jj[n],aa[n]] +
            off_Sch[rr[n],ss[n],aa[n]] +
            ( rr[n] - 1 ) * bGrade[ss[n],aa[n]] * CumuGrade[ss[n],aa[n],Grade[n]];     // turn off grade effect for teachers rr=1

    //beta = muBeta + off_Beta[kk[n],rr[n]];             // separate beta for each question for each role
    gamma = 1 + off_Gamma[kk[n],rr[n]];   // constrain gamma positive

    params[n] = gamma*( alpha - beta[kk[n]] );
    //params[n] = gamma[kk[n]]*(alpha - beta[kk[n]]); 

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

