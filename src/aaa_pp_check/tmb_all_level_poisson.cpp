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

  DATA_SPARSE_MATRIX(X_extract_dhs);
  DATA_SPARSE_MATRIX(X_extract_ais);
  DATA_SPARSE_MATRIX(X_extract_mics);

  DATA_SPARSE_MATRIX(R_country);
  // DATA_SPARSE_MATRIX(Z_country);
  // PARAMETER_VECTOR(u_country);
  // PARAMETER(log_prec_country);

  // DATA_SPARSE_MATRIX(Z_omega1);
  // PARAMETER_ARRAY(omega1);
  // PARAMETER(log_prec_omega1);
  // PARAMETER(lag_logit_omega1_phi_age);

  // DATA_SPARSE_MATRIX(Z_omega2);
  // PARAMETER_ARRAY(omega2);
  // PARAMETER(log_prec_omega2);
  // PARAMETER(lag_logit_omega2_phi_period);

  DATA_SPARSE_MATRIX(Z_spatial);
  DATA_SPARSE_MATRIX(R_spatial);
  DATA_SCALAR(rankdef_R_spatial); // rank deficiency of the R_spatial structure matrix
  
  PARAMETER_VECTOR(u_spatial_str);
  PARAMETER(log_prec_spatial);
  
  DATA_SPARSE_MATRIX(Z_interaction1);
  // PARAMETER_ARRAY(eta1);
  // PARAMETER(log_prec_eta1);
  // PARAMETER(lag_logit_eta1_phi_age);
  // PARAMETER(lag_logit_eta1_phi_period);

  DATA_SPARSE_MATRIX(Z_interaction2);
  // PARAMETER_ARRAY(eta2);
  // PARAMETER(log_prec_eta2);
  // PARAMETER(lag_logit_eta2_phi_period);

  DATA_SPARSE_MATRIX(Z_interaction3);
  // PARAMETER_ARRAY(eta3);
  // PARAMETER(log_prec_eta3);
  // PARAMETER(lag_logit_eta3_phi_age);
 
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
  // // nll -= dnorm(beta_tips_dummy, Type(0.05), Type(0.1), true).sum();
  // // nll -= dlgamma(beta_tips_dummy, Type(1.73), Type(1/17.326), true).sum();
  
  
  // nll -= dlgamma(log_prec_rw_tips, Type(1), Type(20000), true);
  // nll -= dlgamma(log_prec_rw_tips, Type(31), Type(1/3.922), true);
  // Type prec_rw_tips = exp(log_prec_rw_tips); 

  // nll -= Type(-0.5) * (u_tips * (R_tips * u_tips)).sum();
  // nll -= dnorm(u_tips.sum(), Type(0), Type(0.01) * u_tips.size(), true);

  /////////////////

  // nll -= dnorm(beta_urban_dummy, Type(0), Type(sqrt(1/0.001)), true).sum();

  ///////////////////

  nll -= dlgamma(log_prec_spatial, Type(1), Type(20000), true);
  Type prec_spatial = exp(log_prec_spatial);

  nll -= Type(-0.5) * (u_spatial_str * (R_spatial * u_spatial_str)).sum();
  
  nll -= dnorm(u_spatial_str.sum(), Type(0), Type(0.01) * u_spatial_str.size(), 1);

  ///////////////////

  // nll -= dlgamma(log_prec_country, Type(1), Type(20000), true);
  // Type prec_country = exp(log_prec_country); 
  // 
  // nll -= Type(-0.5) * (u_country * (R_country * u_country)).sum();


  ///////////////////

  DATA_SPARSE_MATRIX(Z_age);
  DATA_SPARSE_MATRIX(R_age);
  PARAMETER(log_prec_rw_age);
  PARAMETER_VECTOR(u_age); 

  nll -= dlgamma(log_prec_rw_age, Type(1), Type(20000), true);
  Type prec_rw_age = exp(log_prec_rw_age);

  nll += GMRF(R_age)(u_age);
  nll -= dnorm(u_age.sum(), Type(0), Type(0.01) * u_age.size(), true);

  ///

  // nll -= dlgamma(log_prec_omega1, Type(1), Type(20000), true);
  // Type prec_omega1 = exp(log_prec_omega1);

  // nll -= dnorm(lag_logit_omega1_phi_age, Type(0), Type(sqrt(1/0.15)), true);
  // Type omega1_phi_age = 2*exp(lag_logit_omega1_phi_age)/(1+exp(lag_logit_omega1_phi_age))-1;
  
  // nll += SEPARABLE(AR1(Type(omega1_phi_age)), GMRF(R_country))(omega1);
  // vector<Type> omega1_v(omega1);

  ///////////////////

  DATA_SPARSE_MATRIX(Z_period);
  DATA_SPARSE_MATRIX(R_period);
  PARAMETER(log_prec_rw_period);
  PARAMETER_VECTOR(u_period); 
 
 
  // nll -= dnorm(log_prec_rw_period, Type(5.93204716), Type(0.80520811), true);
  nll -= dlgamma(log_prec_rw_period, Type(1), Type(20000), true);
  Type prec_rw_period = exp(log_prec_rw_period);


  // // RW
  nll -= Type(-0.5) * (u_period * (R_period * u_period)).sum();
  nll -= dnorm(u_period.sum(), Type(0), Type(0.01) * u_period.size(), true);

  // // AR1
  // PARAMETER(lag_logit_phi_period);
  
  // nll -= dnorm(lag_logit_phi_period, Type(0), Type(sqrt(1/0.15)), true);
  // Type phi_period = 2*exp(lag_logit_phi_period)/(1+exp(lag_logit_phi_period))-1;
  
  // nll += AR1(Type(phi_period))(u_period);

  // // ARIMA(1,1,0) with trend
  // DATA_SPARSE_MATRIX(X_period);
  // PARAMETER(lag_logit_phi_arima_period);

  // PARAMETER_VECTOR(beta_period);
  // nll -= dnorm(beta_period, Type(0), Type(sqrt(1/0.001)), true).sum();

  // nll -= dnorm(lag_logit_phi_arima_period, Type(0), Type(sqrt(1/0.15)), true);
  // Type lag_logit_phi_arima_period = 5;
  // Type phi_arima_period = 2*exp(lag_logit_phi_arima_period)/(1+exp(lag_logit_phi_arima_period))-1;

  // int n = u_period.size();

  // vector<Type> u_period_diff(n - 1);

  // for (int i = 1; i < n; i++) {
  //   u_period_diff[i - 1] = u_period[i] - u_period[i - 1];
  // }

  // nll += AR1(Type(phi_arima_period))(u_period_diff);

  ///

  // nll -= dlgamma(log_prec_omega2, Type(1), Type(20000), true);
  // Type prec_omega2 = exp(log_prec_omega2);

  // nll -= dnorm(lag_logit_omega2_phi_period, Type(0), Type(sqrt(1/0.15)), true);
  // Type omega2_phi_period = 2*exp(lag_logit_omega2_phi_period)/(1+exp(lag_logit_omega2_phi_period))-1;
  
  // nll += SEPARABLE(AR1(Type(omega2_phi_period)), GMRF(R_country))(omega2);
  // vector<Type> omega2_v(omega2);


  ////////////////////
  // ETA-1 - Age x time interaction

  // nll -= dnorm(log_prec_eta1, Type(2.65279151), Type(0.29716224), true);
  // nll -= dlgamma(log_prec_eta1, Type(1), Type(20000), true);
  // Type prec_eta1 = exp(log_prec_eta1);

  // nll -= dnorm(lag_logit_eta1_phi_age, Type(2.55395758), Type(0.26019927), true);
  // nll -= dnorm(lag_logit_eta1_phi_age, Type(0), Type(sqrt(1/0.15)), true);
  // Type eta1_phi_age = 2*exp(lag_logit_eta1_phi_age)/(1+exp(lag_logit_eta1_phi_age))-1;

  // nll -= dnorm(lag_logit_eta1_phi_period, Type(4.71453548), Type(0.31483987), true);
  // nll -= dnorm(lag_logit_eta1_phi_period, Type(0), Type(sqrt(1/0.15)), true);
  // Type eta1_phi_period = 2*exp(lag_logit_eta1_phi_period)/(1+exp(lag_logit_eta1_phi_period))-1;
  
  // nll += SEPARABLE(AR1(Type(eta1_phi_age)), SEPARABLE(AR1(Type(eta1_phi_period)), GMRF(R_country)))(eta1);
  // vector<Type> eta1_v(eta1);

  ///////////////////
   // ETA-2 - Space x time interaction

  // DATA_SPARSE_MATRIX(R_period_iid);

  // nll -= dnorm(log_prec_eta2, Type(6.92577668), Type(0.23404592), true);
  // nll -= dlgamma(log_prec_eta2, Type(1), Type(20000), true);
  // Type prec_eta2 = exp(log_prec_eta2);

  // nll -= dnorm(lag_logit_eta2_phi_period, Type(-1.85559582), Type(0.37270676), true);
  // nll -= dnorm(lag_logit_eta2_phi_period, Type(0), Type(sqrt(1/0.15)), true);
  // Type eta2_phi_period = 2*exp(lag_logit_eta2_phi_period)/(1+exp(lag_logit_eta2_phi_period))-1;
  
  // nll += SEPARABLE(AR1(Type(eta2_phi_period)), GMRF(R_spatial))(eta2);
  // nll += SEPARABLE(GMRF(R_period), GMRF(R_spatial))(eta2);

  // Adjust normalising constant for rank deficience of R_spatial. SEPARABLE calculates the
  // normalizing constant assuming full rank precision matrix. Add the component of the
  // constant back.
  
  // Type log_det_Qar1_eta2((eta2.cols() - 1) * log(1 - eta2_phi_period * eta2_phi_period));
  // nll -= rankdef_R_spatial * 0.5 * (log_det_Qar1_eta2 - log(2 * PI));

  // sum-to-zero on space x time interaction. Ensure each space effects (row) in each year (col) sum to zeo.
  // for (int i = 0; i < eta2.cols(); i++) {
  //   nll -= dnorm(eta2.col(i).sum(), Type(0), Type(0.01) * eta2.col(i).size(), true);}

  // for (int i = 0; i < eta2.transpose().cols(); i++) {
  //   nll -= dnorm(eta2.transpose().col(i).sum(), Type(0), Type(0.01) * eta2.transpose().col(i).size(), true);}


  // vector<Type> eta2_v(eta2);

  ////////////////////

  // nll -= dnorm(log_prec_eta3, Type(2.47668668), Type(0.06081623), true);
  // nll -= dlgamma(log_prec_eta3, Type(1), Type(20000), true);
  // Type prec_eta3 = exp(log_prec_eta3);

  // nll -= dnorm(lag_logit_eta3_phi_age, Type(3.66116349), Type(0.09653723), true);
  // nll -= dnorm(lag_logit_eta3_phi_age, Type(0), Type(sqrt(1/0.15)), true);
  // Type eta3_phi_age = 2*exp(lag_logit_eta3_phi_age)/(1+exp(lag_logit_eta3_phi_age))-1;

  
  // nll += SEPARABLE(AR1(Type(eta3_phi_age)), GMRF(R_spatial))(eta3);

  // Adjust normalising constant for rank deficience of R_spatial. SEPARABLE calculates the
  // normalizing constant assuming full rank precision matrix. Add the component of the
  // constant back.
  
  // Type log_det_Qar1_eta3((eta3.cols() - 1) * log(1 - eta3_phi_age * eta3_phi_age));
  // nll -= rankdef_R_spatial * 0.5 * (log_det_Qar1_eta3 - log(2 * PI));

  // // sum-to-zero on space x age interaction. Ensure each space effects (row) in each year (col) sum to zeo.
  // for (int i = 0; i < eta3.cols(); i++) {
    // nll -= dnorm(eta3.col(i).sum(), Type(0), Type(0.01) * eta3.col(i).size(), true);}


  // vector<Type> eta3_v(eta3);

  //Smooth iid

  // PARAMETER(log_prec_smooth_iid);
  // DATA_SPARSE_MATRIX(R_smooth_iid);
  // DATA_SPARSE_MATRIX(Z_smooth_iid_dhs);
  // DATA_SPARSE_MATRIX(Z_smooth_iid_ais);
  // // DATA_SPARSE_MATRIX(Z_smooth_iid_ais);
  // PARAMETER_VECTOR(u_smooth_iid);

  // nll -= dlgamma(log_prec_smooth_iid, Type(1), Type(20000), true);
  // Type prec_smooth_iid = exp(log_prec_smooth_iid);

  // // nll += GMRF(R_smooth_iid)(u_smooth_iid);
  // nll -= Type(-0.5) * (u_smooth_iid * (R_smooth_iid * u_smooth_iid)).sum();

  ///////////////////////

  vector<Type> log_lambda(
                     beta_0
                     + Z_age * u_age * sqrt(1/prec_rw_age)
                     + Z_period * u_period * sqrt(1/prec_rw_period)
                     // + X_period * beta_period
                     // + Z_spatial * spatial                     
                     + Z_spatial * u_spatial_str * sqrt(1/prec_spatial)
                     // + Z_country * u_country * sqrt(1/prec_country)
                     // + Z_omega1 * omega1_v * sqrt(1/prec_omega1)
                     // + Z_omega2 * omega2_v * sqrt(1/prec_omega2)
                     // + Z_interaction1 * eta1_v * sqrt(1/prec_eta1)
                     // + Z_interaction2 * eta2_v * sqrt(1/prec_eta2)
                     // + Z_interaction3 * eta3_v * sqrt(1/prec_eta3)
                     );

  
  // vector<Type> u_tips_constr = u_tips - u_tips[3];

  PARAMETER_VECTOR(beta_spike_2000);
  PARAMETER_VECTOR(beta_spike_1999);
  PARAMETER_VECTOR(beta_spike_2001);

  DATA_MATRIX(X_spike_2000_dhs);
  DATA_MATRIX(X_spike_1999_dhs);
  DATA_MATRIX(X_spike_2001_dhs); 

  DATA_MATRIX(X_spike_2000_ais);
  DATA_MATRIX(X_spike_1999_ais);
  DATA_MATRIX(X_spike_2001_ais); 

  nll -= dnorm(beta_spike_2000, Type(0), Type(2.5), true).sum();
  nll -= dnorm(beta_spike_1999, Type(0), Type(2.5), true).sum();
  nll -= dnorm(beta_spike_2001, Type(0), Type(2.5), true).sum();

  vector<Type> mu_obs_pred_naomi(M_naomi_obs * log_lambda
                          + log_offset_naomi    
                          );

  vector<Type> lambda(exp(log_lambda));
  vector<Type> births(lambda * pop);

  vector<Type> births_full(A_full_obs * births);
  vector<Type> pop_full(A_full_obs * pop);
  vector<Type> lambda_out(births_full/pop_full);

  vector<Type> mu_obs_pred_dhs(X_extract_dhs * (M_full_obs * log(lambda_out))
                                // + Z_tips_dhs * u_tips_constr * sqrt(1/prec_rw_tips)  // TIPS RW
                                // + X_tips_dummy * beta_tips_dummy          // TIPS fixed effect
                                + X_spike_2000_dhs * beta_spike_2000          // spike 2000
                                + X_spike_1999_dhs * beta_spike_1999          // spike 1999
                                + X_spike_2001_dhs * beta_spike_2001          // spike 2001
                                // + Z_smooth_iid_dhs * u_smooth_iid * sqrt(1/prec_smooth_iid)
                                + log_offset_dhs

                );

  vector<Type> mu_obs_pred_ais(X_extract_ais * (M_full_obs * log(lambda_out))
                                + X_spike_2000_ais * beta_spike_2000          // spike 2000
                                + X_spike_1999_ais * beta_spike_1999          // spike 1999
                                + X_spike_2001_ais * beta_spike_2001          // spike 2001
                                // + Z_tips_ais * u_tips_constr * sqrt(1/prec_rw_tips)  // TIPS RW
                                // + Z_smooth_iid_ais * u_smooth_iid * sqrt(1/prec_smooth_iid)
                                + log_offset_ais

                );

  // PARAMETER(log_overdispersion);
  // nll -= dnorm(log_overdispersion, Type(0), Type(2.5), true);
  // Type overdispersion = exp(log_overdispersion); 
  
  // vector<Type> var_nbinom_dhs = exp(mu_obs_pred_dhs) + ((exp(mu_obs_pred_dhs)) * (exp(mu_obs_pred_dhs)) * overdispersion);
  // vector<Type> var_nbinom_ais = exp(mu_obs_pred_ais) + ((exp(mu_obs_pred_ais)) * (exp(mu_obs_pred_ais)) * overdispersion);

  // nll -= dnbinom2(births_obs_dhs, exp(mu_obs_pred_dhs), var_nbinom_dhs, true).sum();  
  // nll -= dnbinom2(births_obs_ais, exp(mu_obs_pred_ais), var_nbinom_ais, true).sum();

  nll -= dpois(births_obs_dhs, exp(mu_obs_pred_dhs), true).sum();  
  nll -= dpois(births_obs_ais, exp(mu_obs_pred_ais), true).sum();  


  if(mics_toggle) {

    DATA_SPARSE_MATRIX(Z_tips_mics);
    DATA_SPARSE_MATRIX(R_tips_mics);
    // DATA_SPARSE_MATRIX(Z_smooth_iid_mics);

    DATA_VECTOR(log_offset_mics);
    DATA_VECTOR(births_obs_mics);
    
    // PARAMETER_VECTOR(u_tips_mics);

    DATA_MATRIX(X_spike_2000_mics);
    DATA_MATRIX(X_spike_1999_mics);
    DATA_MATRIX(X_spike_2001_mics); 

    // nll -= Type(-0.5) * (u_tips_mics * (R_tips_mics * u_tips_mics)).sum();
    // nll -= dnorm(u_tips_mics.sum(), Type(0), Type(0.01) * u_tips_mics.size(), true);

    // vector<Type> u_tips_mics_constr = u_tips_mics - u_tips_mics[1];


    vector<Type> mu_obs_pred_mics(X_extract_mics * (M_full_obs * log(lambda_out))
                                  // + Z_tips_mics * u_tips_mics_constr * sqrt(1/prec_rw_tips)     // TIPS RW
                                  + X_spike_2000_mics * beta_spike_2000          // spike 2000
                                  + X_spike_1999_mics * beta_spike_1999          // spike 1999
                                  + X_spike_2001_mics * beta_spike_2001          // spike 2001
                                  // + Z_smooth_iid_mics * u_smooth_iid * sqrt(1/prec_smooth_iid) 
                                  + log_offset_mics

                );

    // vector<Type> var_nbinom_mics = exp(mu_obs_pred_mics) + ((exp(mu_obs_pred_mics)) * (exp(mu_obs_pred_mics)) * overdispersion);
    // nll -= dnbinom2(births_obs_mics, exp(mu_obs_pred_mics), var_nbinom_mics, true).sum();  

    nll -= dpois(births_obs_mics, exp(mu_obs_pred_mics), true).sum(); 

    REPORT(mu_obs_pred_mics);

  }


  DATA_SPARSE_MATRIX(A_tfr_out);
  vector<Type> tfr_out(A_tfr_out * lambda_out);

  
  REPORT(tfr_out);
  REPORT(lambda_out);
  // REPORT(lambda);

  // REPORT(log_prec_smooth_iid);
  // REPORT(u_smooth_iid);

  // REPORT(log_prec_spatial);
  // REPORT(logit_spatial_rho);

  // REPORT(log_prec_eta1);
  // REPORT(eta1_phi_age);
  // REPORT(eta1_phi_period);

  // REPORT(log_prec_eta2);
  // REPORT(eta2_phi_period);

  // REPORT(log_prec_eta3);
  // REPORT(eta3_phi_age);

  // REPORT(log_prec_country);

  // REPORT(log_prec_omega1);
  // REPORT(omega1_phi_age);

  // REPORT(log_prec_omega2);
  // REPORT(omega2_phi_period);

  // REPORT(log_prec_rw_age);
  // REPORT(log_prec_rw_period);
  // REPORT(log_prec_rw_tips);

  // REPORT(beta_period);
  // REPORT(phi_arima_period);

  // REPORT(beta_tips_dummy);
  // // REPORT(beta_urban_dummy);

  // REPORT(u_period);
  // REPORT(u_age);
  // REPORT(u_spatial_str);
  // REPORT(u_tips);
  // REPORT(eta1);
  // REPORT(eta2);
  // REPORT(eta3);

  // REPORT(beta_0);

  // Posterior predictive checks
  REPORT(mu_obs_pred_ais);
  REPORT(mu_obs_pred_dhs);


  return nll;
  
}
