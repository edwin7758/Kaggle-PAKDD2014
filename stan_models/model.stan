data {
  int<lower=1> nRows;
  int<lower=1> nComponents;
  int<lower=1> nModuleComponents;
  int<lower=1> nModuleComponentTimes;
  
  int<lower=1> moduleComponentMapper[nModuleComponents,2];
  int<lower=0> prevModuleComponentTimeMapper[nModuleComponentTimes];
  
  int<lower=1,upper=nModuleComponents> moduleComponent[nRows];
  int<lower=1,upper=nModuleComponentTimes> moduleComponentTime[nRows];
  
  int<lower=0> repairCount[nRows];
  int<lower=1> saleCount[nRows];
  int<lower=0> age[nRows];
  int<lower=1,upper=12> repairMonth[nRows];
}
parameters {
  vector[27] mu_c[nComponents];
  vector[27] alpha_mc[nModuleComponents];
  real<lower=0> rho_mc;
  
  vector[33] mu_decay_c[nComponents];
  vector[33] alpha_decay_mc[nModuleComponents];
  real<lower=0> rho_decay_mc;
  
  real beta_mct_start_mu;
  real<lower=0> beta_mct_start_sigma;
  real<lower=0> beta_mct_sigma;
  real beta_mct_scale[nModuleComponentTimes];
  
  real beta_repairMonth11[11];
}
transformed parameters {
  vector[80] beta_mc_age[nModuleComponents]; 
  real beta_all_month[12];
  
  for(i in 1:11)
  	beta_all_month[i] <- beta_repairMonth11[i];
  beta_all_month[12] <- 0;  
  
  {  
    matrix[27,27] Sigma_mc;  
    matrix[27,27] L_mc;
    matrix[33,33] Sigma_decay_mc;  
    matrix[33,33] L_decay_mc;
    
    for(i in 1:27)
    {
      for(j in i:27)
      {
        real tmp;
        tmp <- 1.0*exp(-rho_mc*pow(abs(i-j),2)) + if_else(i==j, 1, 0.0);
        Sigma_mc[i,j] <- tmp;
        Sigma_mc[j,i] <- tmp;
      }
    }
    
    for(i in 1:33)
    {
      for(j in i:33)
      {
        real tmp;
        tmp <- 1.0*exp(-rho_decay_mc*pow(abs(i-j),2)) + if_else(i==j, 0.05, 0.0);
        Sigma_decay_mc[i,j] <- tmp;
        Sigma_decay_mc[j,i] <- tmp;
      }
    }
    
    L_mc <- cholesky_decompose(Sigma_mc);
    L_decay_mc <- cholesky_decompose(Sigma_decay_mc);
    
    for(mc in 1:nModuleComponents)
    {
      vector[27] b;
      vector[33] decay;
      
      int c;
      c <- moduleComponentMapper[mc,2];
    
      b <- mu_c[c] + L_mc * alpha_mc[mc];
      for(t in 1:27)
      {
        beta_mc_age[mc,t] <- b[t];
      }
    
      decay <- mu_decay_c[c] +  L_decay_mc * alpha_decay_mc[mc];
      for(t in 28:60)
      {
        beta_mc_age[mc,t] <- beta_mc_age[mc, t-1] - exp(decay[t-27]);
      }
    
      for(t in 61:80)
      {
        beta_mc_age[mc,t] <- beta_mc_age[mc, t-1] - exp(decay[33]);
      }
    }
  }
}

model {  
  //likelihood
  real theta[nRows];
  for(i in 1:nRows)
  {
    int a;
    int month;
    int mct;
    int mc;
    mct <- moduleComponentTime[i];
    mc <- moduleComponent[i];
    month <- repairMonth[i];
    a <- age[i] + 1;
    
    theta[i] <- beta_all_month[month] + beta_mct_scale[mct] + beta_mc_age[mc, a];   
  }
  repairCount ~ binomial_logit(saleCount, theta);
  
  //priors 
  beta_repairMonth11 ~ normal(0,1);
  
  rho_mc ~ cauchy(0,0.5);
  rho_decay_mc ~ cauchy(0,0.5);
  for(mc in 1:nModuleComponents)
  {
    alpha_mc[mc] ~ normal(0,1);
    alpha_decay_mc[mc] ~ normal(0,1);
  }
  
  for(c in 1:nComponents)
  {
	mu_c[c] ~ normal(0, 2);
    mu_decay_c[c] ~ normal(-2.5, 0.5);
  }
  
  beta_mct_start_mu ~ normal(-10, 3);
  beta_mct_start_sigma ~ cauchy(0, 1);
  beta_mct_sigma ~ cauchy(0, 0.5);
  for(mct in 1:nModuleComponentTimes)
  {
    int prev;
    prev <- prevModuleComponentTimeMapper[mct];
    if(prev==0)
      beta_mct_scale[mct] ~ normal(beta_mct_start_mu, beta_mct_start_sigma);
    else
      beta_mct_scale[mct] ~ normal(beta_mct_scale[prev], beta_mct_sigma);
  }
}
