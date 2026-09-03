
// Predicting movement paths of Hadza foragers based on latent habitat features using Bayesian path selection analysis
// D.Deffner, 2023 (deffner@mpib-berlin.mpg.de)

//Data block: Define and name the size of each observed variable
data{
   int N;              //Number of observations
   int id[N];          //Unique individual identification
   int N_id;           //Number of individuals
   int N_cand;         //Number of candidate paths
   int path_lengths[N];//Length of individual paths
   int sex[N];         //Sex of participant: female (=1) vs. male (=2)
   real features[N,max(path_lengths), N_cand+1]; 
}

//Parameter block: Define and name the size of each unobserved variable.
parameters{  
  
  //Latent habitat features for each track; feature means will be used to predict path choice
  real<lower=0>features_mu[N, N_cand+1];
  real<lower=0>features_sigma[N, N_cand+1];

  //Weights of habitat features for woman and men
   real weights[2];

   //Varying effects clustered on individual
    vector[N_id] z_ID;
    real<lower=0> sigma_ID;
}

//Model block: Here we compute the log posterior
model{

  //Vector for choice probabilities
  vector[N_cand+1] p;

  //Priors
  //Latent habitat features
    for (i in 1:N){
      for (j in 1: (N_cand+1)){
        //Means  
        features_mu[i,j] ~ lognormal(log(2), 1);
        //Standard deviations
        features_sigma[i,j] ~ exponential(3);
       
     }
    }
  
  //Feature weights
   weights ~ normal(0,1);

  

  //Varying effects priors
  z_ID ~ normal(0,1);
  sigma_ID ~ exponential(1);

  //Loop over all paths
  for (i in 1:N){
    
    //Habitat features of each track are distributed normally centered on latent mean of track
    for (j in 1: (N_cand+1)){
        features[i,1:path_lengths[i],j] ~ normal(features_mu[i, j], features_sigma[i,j]);
       
     }
     
  //Compute choice probabilities using softmax function
  p = softmax( to_vector(features_mu[i,]) * (weights[sex[i]] + z_ID[id[i]]*sigma_ID) );

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
  p = softmax( to_vector(features_mu[i,]) * (weights[sex[i]] + z_ID[id[i]]*sigma_ID) );
  
  //Add log probability of observed choice to target
  log_lik[i] = categorical_lpmf(1 | p);
  }

}//end generated quantities

