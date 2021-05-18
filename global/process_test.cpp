#include <TMB.hpp>                                // Links in the TMB libraries

template<class Type>
Type objective_function<Type>::operator() ()
  
{

  using namespace density;

  Type nll = 0;

  PARAMETER(beta_0);

  DATA_SPARSE_MATRIX(M_naomi_obs);
  DATA_SPARSE_MATRIX(M_full_obs);
  // DATA_SPARSE_MATRIX(M_aggregated_obs);

  DATA_MATRIX(X_tips_dummy);
  // PARAMETER_VECTOR(beta_tips_dummy);

  DATA_SPARSE_MATRIX(Z_tips_dhs);
  DATA_SPARSE_MATRIX(Z_tips_ais);
  DATA_SPARSE_MATRIX(R_tips);
  // PARAMETER(log_prec_rw_tips);
  // PARAMETER_VECTOR(u_tips);
  // PARAMETER(lag_logit_phi_tips);

  DATA_SPARSE_MATRIX(X_extract_dhs);
  DATA_SPARSE_MATRIX(X_extract_ais);
  DATA_SPARSE_MATRIX(X_extract_mics);

  
  // observations

  DATA_VECTOR(log_offset_naomi);
  DATA_VECTOR(births_obs_naomi);
  
  DATA_VECTOR(log_offset_dhs);
  DATA_VECTOR(births_obs_dhs);

  DATA_VECTOR(log_offset_ais);
  DATA_VECTOR(births_obs_ais);

  DATA_VECTOR(pop);
  DATA_INTEGER(mics_toggle);
  // DATA_INTEGER(out_toggle);

  DATA_SPARSE_MATRIX(A_full_obs)

  // DATA_MATRIX(X_urban_dummy);
  // PARAMETER_VECTOR(beta_urban_dummy);

 
  nll -= dnorm(beta_0, Type(0), Type(sqrt(1/0.001)), true);

  ///////////////////

  // nll -= dnorm(beta_tips_dummy, Type(0), Type(sqrt(1/0.001)), true).sum();
  // nll -= dnorm(beta_tips_dummy, Type(0.05), Type(0.1), true).sum();
  // nll -= dlgamma(beta_tips_dummy, Type(1.73), Type(1/17.326), true).sum();
  
  
  // nll -= dlgamma(log_prec_rw_tips, Type(1), Type(20000), true);
  // Type prec_rw_tips = exp(log_prec_rw_tips); 

  // nll -= Type(-0.5) * (u_tips * (R_tips * u_tips)).sum();
  // nll -= dnorm(u_tips.sum(), Type(0), Type(0.01) * u_tips.size(), true);

  // nll -= dnorm(lag_logit_phi_tips, Type(0), Type(sqrt(1/0.15)), true);
  // Type phi_tips = 2*exp(lag_logit_phi_tips)/(1+exp(lag_logit_phi_tips))-1;
  // nll += AR1(Type(phi_tips))(u_tips);

   
  ///////////////////

  DATA_SPARSE_MATRIX(Z_period);
  DATA_SPARSE_MATRIX(R_period);
  PARAMETER(log_prec_rw_period);
  PARAMETER_VECTOR(u_period); 
 
 
  // nll -= dnorm(log_prec_rw_period, Type(5.93204716), Type(0.80520811), true);
  nll -= dlgamma(log_prec_rw_period, Type(1), Type(20000), true);
  Type prec_rw_period = exp(log_prec_rw_period);


  // // RW
  // nll -= Type(-0.5) * (u_period * (R_period * u_period)).sum();
  // nll -= dnorm(u_period.sum(), Type(0), Type(0.01) * u_period.size(), true);

  // // AR1
  PARAMETER(lag_logit_phi_period);
  
  nll -= dnorm(lag_logit_phi_period, Type(0), Type(sqrt(1/0.15)), true);
  Type phi_period = 2*exp(lag_logit_phi_period)/(1+exp(lag_logit_phi_period))-1;
  
  nll += AR1(Type(phi_period))(u_period);

 
  //Smooth iid

  PARAMETER(log_prec_smooth_iid);
  DATA_SPARSE_MATRIX(R_smooth_iid);
  DATA_SPARSE_MATRIX(Z_smooth_iid_dhs);
  // DATA_SPARSE_MATRIX(Z_smooth_iid_ais);
  PARAMETER_VECTOR(u_smooth_iid);

  nll -= dlgamma(log_prec_smooth_iid, Type(1), Type(20000), true);
  Type prec_smooth_iid = exp(log_prec_smooth_iid);

  // nll += GMRF(R_smooth_iid)(u_smooth_iid);
  nll -= Type(-0.5) * (u_smooth_iid * (R_smooth_iid * u_smooth_iid)).sum();

  ///////////////////////

  vector<Type> log_lambda(
                     beta_0
                     // + Z_age * u_age * sqrt(1/prec_rw_age)
                     + Z_period * u_period * sqrt(1/prec_rw_period)
                     // + Z_smooth_iid_dhs * u_smooth_iid * sqrt(1/prec_smooth_iid)
                     // + X_period * beta_period
                     // + Z_spatial * spatial                     
                     // + Z_spatial * u_spatial_str * sqrt(1/prec_spatial)
                     // + Z_country * u_country * sqrt(1/prec_country)
                     // + Z_omega1 * omega1_v * sqrt(1/prec_omega1)
                     // + Z_omega2 * omega2_v * sqrt(1/prec_omega2)
                     // + Z_interaction1 * eta1_v * sqrt(1/prec_eta1)
                     // + Z_interaction2 * eta2_v * sqrt(1/prec_eta2)
                     // + Z_interaction3 * eta3_v * sqrt(1/prec_eta3)
                     );

  
  // vector<Type> u_tips_constr = u_tips - u_tips[3];


  // vector<Type> mu_obs_pred_dhs(log_lambda + log_offset_dhs);

  vector<Type> lambda(exp(log_lambda));
  vector<Type> births(lambda * pop);

  vector<Type> births_full(A_full_obs * births);
  vector<Type> pop_full(A_full_obs * pop);
  vector<Type> lambda_out(births_full/pop_full);

  vector<Type> mu_obs_pred_dhs(X_extract_dhs * (M_full_obs * log(lambda_out))
                                // + Z_tips_dhs * u_tips * sqrt(1/prec_rw_tips)  // TIPS RW
                                // + X_tips_dummy * beta_tips_dummy          // TIPS fixed effect
                                // + Z_interaction4 * eta4_v * sqrt(1/prec_eta4)
                                // + X_spike_2000_dhs * beta_spike_2000          // spike 2000
                                // + X_spike_1999_dhs * beta_spike_1999          // spike 1999
                                // + X_spike_2001_dhs * beta_spike_2001          // spike 2001
                                + Z_smooth_iid_dhs * u_smooth_iid * sqrt(1/prec_smooth_iid)
                                + log_offset_dhs

                );

  nll -= dpois(births_obs_dhs, exp(mu_obs_pred_dhs), true).sum();   

  // DATA_SPARSE_MATRIX(A_tfr_out);
  // vector<Type> tfr_out(A_tfr_out * lambda_out);

  
  // REPORT(tfr_out);
  // REPORT(lambda_out);
  REPORT(lambda);

  // REPORT(log_prec_smooth_iid);
  // REPORT(u_smooth_iid);

  // REPORT(log_prec_spatial);

  // REPORT(log_prec_eta1);
  // REPORT(lag_logit_eta1_phi_age);
  // REPORT(lag_logit_eta1_phi_period);

  // REPORT(log_prec_eta2);
  // REPORT(lag_logit_eta2_phi_period);

  // REPORT(log_prec_eta3);
  // REPORT(lag_logit_eta3_phi_age);

  // REPORT(log_prec_country);

  // REPORT(log_prec_omega1);
  // REPORT(lag_logit_omega1_phi_age);

  // REPORT(log_prec_omega2);
  // REPORT(lag_logit_omega2_phi_period);

  // REPORT(log_prec_rw_age);
  REPORT(log_prec_rw_period);
  // REPORT(log_prec_rw_tips);

  REPORT(lag_logit_phi_period);

  REPORT(log_prec_smooth_iid);
  REPORT(u_smooth_iid);

  // REPORT(beta_period);
  // REPORT(lag_logit_phi_arima_period);

  // REPORT(beta_tips_dummy);
  // REPORT(u_tips);
  // REPORT(log_prec_eta4);
  // REPORT(lag_logit_eta4_phi_tips);
  // REPORT(beta_urban_dummy);

  // REPORT(eta1);
  // REPORT(eta2);
  // REPORT(eta3);

  // REPORT(beta_0);


  return nll;
  
}
