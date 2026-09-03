
// Predicting movement paths of Hadza foragers based on latent habitat features using Bayesian path selection analysis
// D.Deffner, 2023 (deffner@mpib-berlin.mpg.de)

//Data block: Define and name the size of each observed variable
data{
   int N;              //Number of observations
   int id[N];          //Unique individual identification
   int N_id;           //Number of individuals
   int N_feat;         //Number of features
   int N_cand;         //Number of candidate paths
   int path_lengths[N];//Length of individual paths
   int sex[N];         //Sex of participant: female (=1) vs. male (=2)
   real features[N,max(path_lengths), N_cand+1, N_feat]; //Feature design arrays for each of N choices, [,,1,] represents actual paths, [,,2:N_cand,] represent candidate paths
                                                         // [,,,1] TRI, [,,,2] Elephant distance, [,,,3] Road distance, [,,,4] River Distance
}

//Parameter block: Define and name the size of each unobserved variable.
parameters{
  
  //Latent habitat features for each track; feature means will be used to predict path choice
  real<lower=0>features_mu[N, N_cand+1, N_feat];
  real<lower=0>features_sigma[N, N_cand+1, N_feat];

  //Weights of habitat features for woman and men
   matrix[N_feat,2] weights;

   //Varying effects clustered on individual
    matrix[N_feat,N_id] z_ID;
    vector<lower=0>[N_feat] sigma_ID;
    cholesky_factor_corr[N_feat] Rho_ID;
}

//Transformed Parameters block: Here we multiply z-scores with variances and Cholesky factors to get varying effects back to right scale
transformed parameters{
      matrix[N_id,N_feat] v_ID;
      v_ID = ( diag_pre_multiply( sigma_ID , Rho_ID ) * z_ID )';
}

//Model block: Here we compute the log posterior
model{

  //Vector for choice probabilities
  vector[N_cand+1] p;

  //Priors
  //Latent habitat features
    for (i in 1:N){
      for (j in 1: (N_cand+1)){
        for (k in 1: N_feat){
        //Means  
        features_mu[i,j, k] ~ lognormal(log(2), 1);
        //Standard deviations
        features_sigma[i,j, k] ~ exponential(3);
       }
     }
    }
  
  //Feature weights
  for (i in 1:N_feat){
    for (j in 1:2){
         weights[i,j] ~ normal(0,1);
    }
  }

  //Varying effects priors
  to_vector(z_ID) ~ normal(0,1);
  sigma_ID ~ exponential(1);
  Rho_ID ~ lkj_corr_cholesky(4);
  
  //Loop over all paths
  for (i in 1:N){
    
    //Habitat features of each track are distributed normally centered on latent mean of track
    for (j in 1: (N_cand+1)){
      for (k in 1: N_feat){
        features[i,1:path_lengths[i],j, k] ~ normal(features_mu[i, j, k], features_sigma[i,j,k]);
       }
     }
     
  //Compute choice probabilities using softmax function
  p = softmax(to_matrix(features_mu[i]) * (weights[ , sex[i]] + v_ID[id[i],]' ) );

  //Add log probability of observed choice to target
  target += categorical_lpmf(1 | p);
  
  }
}// end model


//Compute log pointwise predictive densities for model comparison
generated quantities {

  vector[N] log_lik;
  vector[N_cand+1] p;

  for (i in 1:N) {
    
  //Compute choice probabilities using softmax function
  p = softmax(to_matrix(features_mu[i]) * (weights[ , sex[i]] + v_ID[id[i],]' ) );

  //Add log probability of observed choice to target
  log_lik[i] = categorical_lpmf(1 | p);
  }

}//end generated quantities

