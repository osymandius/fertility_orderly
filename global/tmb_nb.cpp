#include <TMB.hpp>                                // Links in the TMB libraries

template<class Type>
Type objective_function<Type>::operator() ()
  
{

  using namespace density;

  Type nll = 0;

  PARAMETER(beta_0);

  DATA_SPARSE_MATRIX(M_obs);

  DATA_MATRIX(X_tips_dummy);
  // DATA_VECTOR(beta_tips_dummy);
  PARAMETER_VECTOR(beta_tips_dummy);

  DATA_SPARSE_MATRIX(Z_tips_dhs);
  DATA_SPARSE_MATRIX(R_tips);
  DATA_SPARSE_MATRIX(X_extract_dhs);
  DATA_SPARSE_MATRIX(X_extract_ais);
  // DATA_SCALAR(log_prec_rw_tips);
  PARAMETER(log_prec_rw_tips);
  PARAMETER_VECTOR(u_tips);

  DATA_SPARSE_MATRIX(R_country);
  DATA_SPARSE_MATRIX(Z_country);
  // PARAMETER_VECTOR(u_country);
  // PARAMETER(log_prec_country);

 

  DATA_SPARSE_MATRIX(Z_spatial);
  DATA_SPARSE_MATRIX(R_spatial);
  DATA_SCALAR(rankdef_R_spatial); // rank deficiency of the R_spatial structure matrix
  
  PARAMETER_VECTOR(u_spatial_str);
  PARAMETER(log_prec_spatial);

 
  // observations

  DATA_VECTOR(log_offset_dhs);
  DATA_VECTOR(births_obs_dhs);

  DATA_VECTOR(pop);
  DATA_INTEGER(mics_toggle);
  DATA_INTEGER(out_toggle);
  DATA_INTEGER(eth_toggle);

 
  nll -= dnorm(beta_0, Type(0), Type(sqrt(1/0.001)), true);

  ///////////////////

  nll -= dnorm(beta_tips_dummy, Type(0.16004639), Type(0.05275746), true).sum();
  // nll -= dnorm(beta_tips_dummy, Type(0.05), Type(0.1), true).sum();
  // nll -= dlgamma(beta_tips_dummy, Type(1.73), Type(1/17.326), true).sum();
  
  
  nll -= dnorm(log_prec_rw_tips, Type(5.93891343), Type(0.39490186), true);
  // nll -= dlgamma(log_prec_rw_tips, Type(31), Type(1/3.922), true);
  Type prec_rw_tips = exp(log_prec_rw_tips); 

  nll -= Type(-0.5) * (u_tips * (R_tips * u_tips)).sum();
  nll -= dnorm(u_tips.sum(), Type(0), Type(0.01) * u_tips.size(), true);

  /////////////////


  ///////////////////

  nll -= dnorm(log_prec_spatial, Type(9.88115250), Type(0.98245296), true);
  Type prec_spatial = exp(log_prec_spatial);

  nll -= Type(-0.5) * (u_spatial_str * (R_spatial * u_spatial_str)).sum();
  
  nll -= dnorm(u_spatial_str.sum(), Type(0), Type(0.01) * u_spatial_str.size(), 1);

  ///////////////////

  // nll -= dlgamma(log_prec_country, Type(1), Type(20000), true);
  // Type prec_country = exp(log_prec_country); 

  // nll -= Type(-0.5) * (u_country * (R_country * u_country)).sum();

  // PARAMETER_VECTOR(u_spatial_iid); 
  // PARAMETER(logit_spatial_rho);

  // nll -= Type(-0.5) * (u_spatial_str * (R_spatial * u_spatial_str)).sum();
  // nll -= dnorm(u_spatial_str.sum(), Type(0), Type(0.01) * u_spatial_str.size(), 1);

  // nll -= dnorm(u_spatial_iid, Type(0), Type(1), true).sum();

  // Type spatial_rho(exp(logit_spatial_rho)/(1+exp(logit_spatial_rho)));
  // nll -= log(spatial_rho) +  log(1 - spatial_rho); // Jacobian adjustment for inverse logit'ing the parameter... 
  // nll -= dbeta(spatial_rho, Type(0.5), Type(0.5), true);
  
  // nll -= dlgamma(log_prec_spatial, Type(1), Type(20000), true);
  // Type prec_spatial = exp(log_prec_spatial);
  
  // vector<Type> spatial = sqrt(1/prec_spatial) * (sqrt(1 - spatial_rho) * u_spatial_iid + sqrt(spatial_rho) * u_spatial_str);

  ///////////////////

  DATA_SPARSE_MATRIX(Z_age);
  DATA_SPARSE_MATRIX(R_age);
  PARAMETER(log_prec_rw_age);
  PARAMETER_VECTOR(u_age); 

  nll -= dnorm(log_prec_rw_age, Type(1.22623881), Type(0.50366674), true);
  Type prec_rw_age = exp(log_prec_rw_age);

  nll += GMRF(R_age)(u_age);
  nll -= dnorm(u_age.sum(), Type(0), Type(0.01) * u_age.size(), true);

  ///


  ///////////////////

  DATA_SPARSE_MATRIX(Z_period);
  DATA_SPARSE_MATRIX(R_period);
  PARAMETER(log_prec_rw_period);
  PARAMETER_VECTOR(u_period); 
 
 
  // nll -= dnorm(log_prec_rw_period, Type(5.93204716), Type(0.80520811), true);
  nll -= dlgamma(log_prec_rw_period, Type(1), Type(20000), true);
  Type prec_rw_period = exp(log_prec_rw_period);

  // nll -= Type(-0.5) * (u_period * (R_period * u_period)).sum();
  // nll -= dnorm(u_period.sum(), Type(0), Type(0.01) * u_period.size(), true);

  PARAMETER(lag_logit_phi_period);
  
  nll -= dnorm(lag_logit_phi_period, Type(0), Type(sqrt(1/0.15)), true);
  Type phi_period = 2*exp(lag_logit_phi_period)/(1+exp(lag_logit_phi_period))-1;
  
  nll += AR1(Type(phi_period))(u_period);

  //   PARAMETER_VECTOR(lag_logit_ar2_phi_period);

  // nll -= dnorm(lag_logit_ar2_phi_period, Type(0), Type(sqrt(1/0.15)), true).sum();
  // vector<Type> ar2_phi_period = 2*exp(lag_logit_ar2_phi_period)/(1+exp(lag_logit_ar2_phi_period)) -1;

  // ARk_t<Type> ar2_period(ar2_phi_period);

  // nll += ar2_period(u_period);

 

  ///////////////////////

  vector<Type> log_lambda(
                     beta_0
                     + Z_age * u_age * sqrt(1/prec_rw_age)
                     + Z_period * u_period * sqrt(1/prec_rw_period)
                     // + Z_spatial * spatial                     
                     + Z_spatial * u_spatial_str * sqrt(1/prec_spatial)
                     // + Z_country * u_country * sqrt(1/prec_country)

                     );

  
  vector<Type> u_tips_constr = u_tips - u_tips[3];

  

    vector<Type> mu_obs_pred_dhs(X_extract_dhs * (M_obs * log_lambda)
                          + Z_tips_dhs * u_tips_constr * sqrt(1/prec_rw_tips)  // TIPS RW
                          + X_tips_dummy * beta_tips_dummy          // TIPS fixed effect
                          // + X_urban_dummy * beta_urban_dummy          // Urban fixed effect
                          + log_offset_dhs    
                          );

    
  PARAMETER(log_overdispersion);
  nll -= dnorm(log_overdispersion, Type(0), Type(2.5), true);
  Type overdispersion = exp(log_overdispersion); 
  vector<Type> var_nbinom = exp(mu_obs_pred_dhs) + ((exp(mu_obs_pred_dhs)) * (exp(mu_obs_pred_dhs)) * overdispersion);

  nll -= dnbinom2(births_obs_dhs, exp(mu_obs_pred_dhs), var_nbinom, true).sum();  



  vector<Type> lambda(exp(log_lambda));


  vector<Type> births(lambda * pop);


  

  if(out_toggle) {

    DATA_SPARSE_MATRIX(A_asfr_out);
    DATA_SPARSE_MATRIX(A_tfr_out);

    vector<Type> births_out(A_asfr_out * births);
    vector<Type> population_out(A_asfr_out * pop);
    vector<Type> lambda_out(births_out / population_out);

    vector<Type> tfr_out(A_tfr_out * lambda_out);

    REPORT(lambda_out);
    REPORT(tfr_out);
    REPORT(births_out);
  }
  
  // REPORT(lambda);
  // REPORT(births);

  REPORT(log_prec_spatial);
  // REPORT(logit_spatial_rho);

 

  REPORT(log_prec_rw_age);
  REPORT(log_prec_rw_period);
  REPORT(log_prec_rw_tips);

  // REPORT(beta_tips_dummy);
  // // REPORT(beta_urban_dummy);

  // REPORT(eta1);
  // REPORT(eta2);
  // REPORT(eta3);

  // REPORT(u_tips);
  // REPORT(u_tips_constr);
  // REPORT(u_period);
  // REPORT(u_age);

  // REPORT(beta_0);


  return nll;
  
}
