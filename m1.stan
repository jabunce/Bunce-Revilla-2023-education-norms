

data {
  int<lower=1> J; // number of interviewees
  int<lower=1> K; // number of questions
  int<lower=1> N; // number of answers to questions (observations)
  array[N] int<lower=1,upper=J> jj; // interviewee ID for observation n
  array[N] int<lower=1,upper=K> kk; // question for observation n
  array[N] int<lower=0,upper=1> y; // prob of positive response for obs n
}

parameters {

  vector[K] beta;
  vector<lower=0>[K] gamma;

  real b0;     // mean interviewee location in latent dimension (mean ability intercept)
  vector[J] b1;	// location of people (differing from the mean), i.e., random effect for person

  real<lower=0> sigma_beta; // scale of question
  real<lower=0> sigma_gamma; // scale of discrimination
}


model {
  vector[N] params;
  real alpha;
  
  b0 ~ normal(0,1);
  b1 ~ normal(0,1); //identifying prior for location and scale

  beta ~ normal(0,sigma_beta);
  gamma ~ normal(0,sigma_gamma);
  
  sigma_beta ~ exponential(1); //exponential(beta), where here beta = lambda = 1/mean
  sigma_gamma ~ exponential(1); //or use uniform(0,5), both prevent ceiling effect


  for (n in 1:N) {

    alpha = ( b0 + b1[jj[n]] );	//random effect for person

    params[n] = gamma[kk[n]]*(alpha - beta[kk[n]]);

  } //for

  y ~ bernoulli_logit(params);
}

generated quantities {       //for computing waic
  vector[N] log_lik;
  vector[J] alpha;

  for (n in 1:N){

    alpha[jj[n]] = ( b0 + b1[jj[n]] );

    log_lik[n] = bernoulli_logit_lpmf( y[n] |    //needed for waic function, see Stan manual pg479
				            gamma[kk[n]]*(alpha[jj[n]] - beta[kk[n]]) );
  } //for
}


