#include <TMB.hpp>                                // Links in the TMB libraries

template<class Type>
Type objective_function<Type>::operator() ()

{

  using namespace density;
  using namespace Eigen;

  Type nll = 0;

  PARAMETER(beta_0);

  DATA_SPARSE_MATRIX(M_naomi_obs);
  DATA_SPARSE_MATRIX(M_full_obs);

  // DATA_MATRIX(X_tips_dummy);
  // DATA_MATRIX(X_tips_dummy_9_11);
  // DATA_MATRIX(X_tips_dummy_5);
  DATA_SPARSE_MATRIX(X_tips_fe);

  DATA_SPARSE_MATRIX(Z_tips);
  DATA_SPARSE_MATRIX(R_tips);
  DATA_SPARSE_MATRIX(Z_zeta2);
  DATA_SPARSE_MATRIX(R_zeta2);
  DATA_SPARSE_MATRIX(R_survey);

  DATA_SPARSE_MATRIX(X_extract_dhs);
  DATA_SPARSE_MATRIX(X_extract_ais);
  DATA_SPARSE_MATRIX(X_extract_mics);
  DATA_SPARSE_MATRIX(X_extract_phia);

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

  DATA_SPARSE_MATRIX(Z_interaction1);
  DATA_SPARSE_MATRIX(Z_interaction2);
  DATA_SPARSE_MATRIX(Z_interaction3);


  // observations

  DATA_VECTOR(log_offset_naomi);
  DATA_VECTOR(births_obs_naomi);

  DATA_VECTOR(log_offset_dhs);
  DATA_VECTOR(births_obs_dhs);

  DATA_VECTOR(log_offset_ais);
  DATA_VECTOR(births_obs_ais);

  DATA_VECTOR(log_offset_phia);
  DATA_VECTOR(births_obs_phia);

  DATA_VECTOR(pop);
  DATA_INTEGER(mics_toggle);
  DATA_INTEGER(mics_only_toggle);
  DATA_INTEGER(eth_toggle);
  DATA_INTEGER(zwe_toggle);
  DATA_INTEGER(mwi_rwa_toggle);
  DATA_INTEGER(mwi_toggle);
  DATA_INTEGER(subnational_toggle);
  DATA_INTEGER(multiple_survey_toggle);

  DATA_SPARSE_MATRIX(A_full_obs);
  DATA_SPARSE_MATRIX(A_tfr_out);

  DATA_MATRIX(X_urban_dummy);
  DATA_SPARSE_MATRIX(X_spike_2010);


  nll -= dnorm(beta_0, Type(0), Type(sqrt(1/0.001)), true);

  ///////////////////

  // PARAMETER(log_prec_rw_tips);
  // nll -= dnorm(log_prec_rw_tips, Type(5.3), Type(1.3), true);
  // Type log_prec_rw_tips = 5;
  // Type prec_rw_tips = exp(log_prec_rw_tips);
  // nll -= dgamma(prec_rw_tips, Type(1), Type(2000), true);
  
  // PARAMETER_VECTOR(u_tips);

  // PARAMETER_VECTOR(beta_tips_dummy_5);
  // nll -= dnorm(beta_tips_dummy_5, Type(-0.05), Type(0.1), true).sum();

  PARAMETER_VECTOR(beta_tips_fe);

  if(mics_only_toggle) {
    nll -= dnorm(beta_tips_fe, Type(0.05), Type(0.1), true).sum();
  } else {
    nll -= dnorm(beta_tips_fe[0], Type(0.05), Type(0.1), true);
    nll -= dnorm(beta_tips_fe[1], Type(-0.05), Type(0.1), true);
    nll -= dnorm(beta_tips_fe[2], Type(0.05), Type(0.1), true);
    nll -= dnorm(beta_tips_fe[3], Type(0.05), Type(0.1), true);
  }
  

  /////////

  // PARAMETER(lag_logit_phi_tips);
  // 
  // nll -= dnorm(lag_logit_phi_tips, Type(0), Type(sqrt(1/0.15)), true);
  // Type phi_tips = 2*exp(lag_logit_phi_tips)/(1+exp(lag_logit_phi_tips))-1;
  // nll += AR1(Type(phi_tips))(u_tips);
  // 
  // vector<Type> u_tips_constr = u_tips - u_tips[3];


  //////////////////////////////////////

  

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

  // nll -= dlgamma(log_prec_rw_age, Type(1), Type(20000), true);
  Type prec_rw_age = exp(log_prec_rw_age);
  nll -= dgamma(prec_rw_age, Type(1), Type(2000), true);

  // nll += GMRF(R_age)(u_age);
  // nll -= dnorm(u_age.sum(), Type(0), Type(0.01) * u_age.size(), true);

  PARAMETER(lag_logit_phi_age);
  nll -= dnorm(lag_logit_phi_age, Type(0), Type(sqrt(1/0.15)), true);
  Type phi_age = 2*exp(lag_logit_phi_age)/(1+exp(lag_logit_phi_age))-1;
  nll += AR1(Type(phi_age))(u_age);

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


  nll -= dnorm(log_prec_rw_period, Type(6.3), Type(2.5), true);
  Type prec_rw_period = exp(log_prec_rw_period);
  // nll -= dgamma(prec_rw_period, Type(1), Type(2000), true);
  // nll -= dnorm(prec_rw_period, Type(6.3), Type(2.5), true);


  // // RW
  // nll -= Type(-0.5) * (u_period * (R_period * u_period)).sum();
  // nll -= dnorm(u_period.sum(), Type(0), Type(0.01) * u_period.size(), true);

  // AR1
  PARAMETER(lag_logit_phi_period);

  //use this
  nll -= dnorm(lag_logit_phi_period, Type(0), Type(sqrt(1/0.15)), true);
  Type phi_period = 2*exp(lag_logit_phi_period)/(1+exp(lag_logit_phi_period))-1;
  
  // not this
  // Type phi_period(exp(logit_phi_period)/(1+exp(logit_phi_period)));
  // nll -= log(phi_period) +  log(1 - phi_period); // Jacobian adjustment for inverse logit'ing the parameter...
  // nll -= dbeta(phi_period, Type(0.5), Type(0.5), true);

  // Type phi_period = 0.99;

  nll += AR1(Type(phi_period))(u_period);

  // ARIMA(1,1,0) with trend
  DATA_SPARSE_MATRIX(X_period);
  // PARAMETER(lag_logit_phi_arima_period);

  PARAMETER_VECTOR(beta_period);
  nll -= dnorm(beta_period, Type(-0.01309), Type(0.01441), true).sum();

  // nll -= dnorm(lag_logit_phi_arima_period, Type(0), Type(sqrt(1/0.15)), true);
  // Type phi_arima_period = 2*exp(lag_logit_phi_arima_period)/(1+exp(lag_logit_phi_arima_period))-1;

  // int n = u_period.size();

  // vector<Type> u_period_diff(n - 1);

  // for (int i = 1; i < n; i++) {
  //   u_period_diff[i - 1] = u_period[i] - u_period[i - 1];
  // }

  // nll += AR1(Type(phi_arima_period))(u_period_diff);
  
  // nll -= dnorm(u_period.sum(), Type(0), Type(0.01) * u_period.size(), true);

  ///

  // nll -= dlgamma(log_prec_omega2, Type(1), Type(20000), true);
  // Type prec_omega2 = exp(log_prec_omega2);

  // nll -= dnorm(lag_logit_omega2_phi_period, Type(0), Type(sqrt(1/0.15)), true);
  // Type omega2_phi_period = 2*exp(lag_logit_omega2_phi_period)/(1+exp(lag_logit_omega2_phi_period))-1;

  // nll += SEPARABLE(AR1(Type(omega2_phi_period)), GMRF(R_country))(omega2);
  // vector<Type> omega2_v(omega2);


  ////////////////////
  // ETA-1 - Age x time interaction

  PARAMETER_ARRAY(eta1);
  PARAMETER(log_prec_eta1);
  PARAMETER(logit_eta1_phi_age);
  PARAMETER(logit_eta1_phi_period);

  ///////////
  // nll -= dnorm(log_prec_eta1, Type(2.65279151), Type(0.29716224), true);
  // nll -= dnorm(lag_logit_eta1_phi_age, Type(2.55395758), Type(0.26019927), true);
  // nll -= dnorm(lag_logit_eta1_phi_period, Type(4.71453548), Type(0.31483987), true);

  // nll -= dlgamma(log_prec_eta1, Type(1), Type(20000), true);


  // nll -= dnorm(lag_logit_eta1_phi_age, Type(0), Type(sqrt(1/0.15)), true);
  // Type eta1_phi_age = 2*exp(lag_logit_eta1_phi_age)/(1+exp(lag_logit_eta1_phi_age))-1;

  // nll -= dnorm(lag_logit_eta1_phi_period, Type(0), Type(sqrt(1/0.15)), true);
  // Type eta1_phi_period = 2*exp(lag_logit_eta1_phi_period)/(1+exp(lag_logit_eta1_phi_period))-1;
  ///////////

  Type prec_eta1 = exp(log_prec_eta1);
  nll -= dgamma(prec_eta1, Type(1), Type(2000), true);

  Type eta1_phi_age(exp(logit_eta1_phi_age)/(1+exp(logit_eta1_phi_age)));
  nll -= log(eta1_phi_age) +  log(1 - eta1_phi_age); // Jacobian adjustment for inverse logit'ing the parameter...
  nll -= dbeta(eta1_phi_age, Type(0.5), Type(0.5), true);

  Type eta1_phi_period(exp(logit_eta1_phi_period)/(1+exp(logit_eta1_phi_period)));
  nll -= log(eta1_phi_period) +  log(1 - eta1_phi_period); // Jacobian adjustment for inverse logit'ing the parameter...
  nll -= dbeta(eta1_phi_period, Type(0.5), Type(0.5), true);

  nll += SEPARABLE(AR1(Type(eta1_phi_age)), SEPARABLE(AR1(Type(eta1_phi_period)), GMRF(R_country)))(eta1);
  vector<Type> eta1_v(eta1);

  //Smooth iid

  PARAMETER(log_prec_smooth_iid);
  DATA_SPARSE_MATRIX(R_smooth_iid);

  DATA_SPARSE_MATRIX(Z_smooth_iid);
  PARAMETER_VECTOR(u_smooth_iid);

  Type prec_smooth_iid = exp(log_prec_smooth_iid);
  nll -= dgamma(prec_smooth_iid, Type(1), Type(2000), true);

  nll -= Type(-0.5) * (u_smooth_iid * (R_smooth_iid * u_smooth_iid)).sum();

  vector<Type> log_lambda(
                     beta_0
                     + Z_age * u_age * sqrt(1/prec_rw_age)
                     + Z_period * u_period * sqrt(1/prec_rw_period)
                     + X_period * beta_period
                     );

  if(zwe_toggle) {

    PARAMETER_VECTOR(beta_spike_2010);
    nll -= dnorm(beta_spike_2010, Type(0.05), Type(0.1), true).sum();

    log_lambda = log_lambda + (X_spike_2010 * beta_spike_2010);

  }

  if(mwi_toggle) {

    PARAMETER_VECTOR(beta_spike_famine);
    DATA_SPARSE_MATRIX(X_spike_famine);
    nll -= dnorm(beta_spike_famine, Type(0), Type(2.5), true).sum();

    log_lambda = log_lambda + (X_spike_famine * beta_spike_famine);
  }

  if(subnational_toggle) {

    ///////// SPATIAL MODELS

    PARAMETER_VECTOR(u_spatial_str);
    PARAMETER(log_prec_spatial);

    // nll -= dlgamma(log_prec_spatial, Type(1), Type(20000), true);
    nll -= dnorm(log_prec_spatial, Type(3), Type(0.3), true);

    // Type log_prec_spatial = 3.16;
    Type prec_spatial = exp(log_prec_spatial);
    // nll -= dgamma(prec_spatial, Type(1), Type(2000), true);

    nll -= Type(-0.5) * (u_spatial_str * (R_spatial * u_spatial_str)).sum();

    nll -= dnorm(u_spatial_str.sum(), Type(0), Type(0.01) * u_spatial_str.size(), 1);

      ///////////////////
     // ETA-2 - Space x time interaction
  //
    PARAMETER_ARRAY(eta2);
    PARAMETER(log_prec_eta2);
    PARAMETER(logit_eta2_phi_period);



    // DATA_SPARSE_MATRIX(R_period_iid);
    // nll -= dnorm(log_prec_eta2, Type(6.92577668), Type(0.23404592), true);
    // nll -= dnorm(lag_logit_eta2_phi_period, Type(-1.85559582), Type(0.37270676), true);

    // nll -= dlgamma(log_prec_eta2, Type(1), Type(20000), true);


    // nll -= dnorm(lag_logit_eta2_phi_period, Type(0), Type(sqrt(1/0.15)), true);
    // Type eta2_phi_period = 2*exp(lag_logit_eta2_phi_period)/(1+exp(lag_logit_eta2_phi_period))-1;

    // Type log_prec_eta2 = 8;
    Type prec_eta2 = exp(log_prec_eta2);
    nll -= dgamma(prec_eta2, Type(1), Type(2000), true);

    Type eta2_phi_period(exp(logit_eta2_phi_period)/(1+exp(logit_eta2_phi_period)));
    nll -= log(eta2_phi_period) +  log(1 - eta2_phi_period); // Jacobian adjustment for inverse logit'ing the parameter...
    nll -= dbeta(eta2_phi_period, Type(0.5), Type(0.5), true);
    // Type eta2_phi_period = 0.99;

    nll += SEPARABLE(AR1(Type(eta2_phi_period)), GMRF(R_spatial))(eta2);

    Type log_det_Qar1_eta2((eta2.cols() - 1) * log(1 - eta2_phi_period * eta2_phi_period));
    nll -= rankdef_R_spatial * 0.5 * (log_det_Qar1_eta2 - log(2 * PI));

    for (int i = 0; i < eta2.cols(); i++) {
      nll -= dnorm(eta2.col(i).sum(), Type(0), Type(0.01) * eta2.col(i).size(), true);}

    vector<Type> eta2_v(eta2);


    ////////////////////

    PARAMETER_ARRAY(eta3);
    PARAMETER(log_prec_eta3);
    PARAMETER(logit_eta3_phi_age);

    // nll -= dnorm(log_prec_eta3, Type(2.47668668), Type(0.06081623), true);
    // nll -= dnorm(lag_logit_eta3_phi_age, Type(3.66116349), Type(0.09653723), true);
    // nll -= dlgamma(log_prec_eta3, Type(1), Type(20000), true);

    // nll -= dnorm(lag_logit_eta3_phi_age, Type(0), Type(sqrt(1/0.15)), true);
    // Type eta3_phi_age = 2*exp(lag_logit_eta3_phi_age)/(1+exp(lag_logit_eta3_phi_age))-1;

    Type prec_eta3 = exp(log_prec_eta3);
    nll -= dgamma(prec_eta3, Type(1), Type(2000), true);

    Type eta3_phi_age(exp(logit_eta3_phi_age)/(1+exp(logit_eta3_phi_age)));
    nll -= log(eta3_phi_age) +  log(1 - eta3_phi_age); // Jacobian adjustment for inverse logit'ing the parameter...
    nll -= dbeta(eta3_phi_age, Type(0.5), Type(0.5), true);

    nll += SEPARABLE(AR1(Type(eta3_phi_age)), GMRF(R_spatial))(eta3);

    Type log_det_Qar1_eta3((eta3.cols() - 1) * log(1 - eta3_phi_age * eta3_phi_age));
    nll -= rankdef_R_spatial * 0.5 * (log_det_Qar1_eta3 - log(2 * PI));

    for (int i = 0; i < eta3.cols(); i++) {
      nll -= dnorm(eta3.col(i).sum(), Type(0), Type(0.01) * eta3.col(i).size(), true);}

    vector<Type> eta3_v(eta3);

    log_lambda = log_lambda
                  + Z_spatial * u_spatial_str * sqrt(1/prec_spatial)
                  + Z_interaction1 * eta1_v * sqrt(1/prec_eta1)
                  + Z_interaction2 * eta2_v * sqrt(1/prec_eta2)
                  + Z_interaction3 * eta3_v * sqrt(1/prec_eta3);

  }

  ///////////////////////

  if(eth_toggle) {

    PARAMETER_VECTOR(beta_urban_dummy);
    nll -= dnorm(beta_urban_dummy, Type(0), Type(sqrt(1/0.001)), true).sum();

    log_lambda = log_lambda + (X_urban_dummy * beta_urban_dummy);

  }

  vector<Type> mu_obs_pred_naomi(M_naomi_obs * log_lambda
                          + log_offset_naomi
                          );

  vector<Type> lambda(exp(log_lambda));
  vector<Type> births(lambda * pop);

  vector<Type> births_full(A_full_obs * births);
  vector<Type> pop_full(A_full_obs * pop);
  vector<Type> lambda_out(births_full/pop_full);

  vector<Type> tfr_out(A_tfr_out * lambda_out);


  vector<Type> u_smooth_lh(Z_smooth_iid * u_smooth_iid * sqrt(1/prec_smooth_iid));
  vector<Type> tips_fe_lh(X_tips_fe * beta_tips_fe);

  vector<Type> zeta2_lh;

  vector<Type> spike_2010_lh;

  vector<Type> spike_1999_lh;
  vector<Type> spike_2000_lh;
  vector<Type> spike_2001_lh;

  vector<Type> mu_obs_pred_dhs(X_extract_dhs * (M_full_obs * log(lambda_out))
                                + X_extract_dhs * tips_fe_lh
                                + X_extract_dhs * u_smooth_lh
                                + log_offset_dhs

                );

  vector<Type> mu_obs_pred_ais(X_extract_ais * (M_full_obs * log(lambda_out))
                                + X_extract_ais * u_smooth_lh
                                + X_extract_ais * tips_fe_lh
                                + log_offset_ais

                );

  vector<Type> mu_obs_pred_phia(X_extract_phia * (M_full_obs * log(lambda_out))
                                 + log_offset_phia

  );

  if(!mwi_rwa_toggle) {

    PARAMETER_VECTOR(beta_spike_2000);
    PARAMETER_VECTOR(beta_spike_1999);
    PARAMETER_VECTOR(beta_spike_2001);

    DATA_MATRIX(X_spike_2000);
    DATA_MATRIX(X_spike_1999);
    DATA_MATRIX(X_spike_2001);

    nll -= dnorm(beta_spike_2000, Type(0.05), Type(0.2), true).sum();
    nll -= dnorm(beta_spike_1999, Type(-0.05), Type(0.2), true).sum();
    nll -= dnorm(beta_spike_2001, Type(-0.05), Type(0.2), true).sum();

    spike_1999_lh = X_spike_1999 * beta_spike_1999;
    spike_2000_lh = X_spike_2000 * beta_spike_2000;
    spike_2001_lh = X_spike_2001 * beta_spike_2001;

    mu_obs_pred_dhs = mu_obs_pred_dhs
      + X_extract_dhs * spike_1999_lh
      + X_extract_dhs * spike_2000_lh
      + X_extract_dhs * spike_2001_lh;

    mu_obs_pred_ais = mu_obs_pred_ais
      + X_extract_ais * spike_1999_lh
      + X_extract_ais * spike_2000_lh
      + X_extract_ais * spike_2001_lh;

  }

  if(multiple_survey_toggle) {

    PARAMETER_ARRAY(zeta2);
    PARAMETER(log_prec_zeta2);

    Type prec_zeta2 = exp(log_prec_zeta2);
    nll -= dnorm(log_prec_zeta2, Type(5), Type(1), true);

    nll += SEPARABLE(GMRF(R_zeta2), GMRF(R_survey))(zeta2);
    vector<Type> zeta2_v(zeta2);
    zeta2_lh = Z_zeta2  * zeta2_v * sqrt(1/prec_zeta2);

    mu_obs_pred_dhs = mu_obs_pred_dhs
      + X_extract_dhs * zeta2_lh;
    
    mu_obs_pred_ais = mu_obs_pred_ais
      + X_extract_ais * zeta2_lh;
    
  }

  nll -= dpois(births_obs_dhs, exp(mu_obs_pred_dhs), true).sum();
  nll -= dpois(births_obs_ais, exp(mu_obs_pred_ais), true).sum();
  nll -= dpois(births_obs_phia, exp(mu_obs_pred_phia), true).sum();


  if(mics_toggle) {

    DATA_SPARSE_MATRIX(Z_tips_mics);
    DATA_SPARSE_MATRIX(R_tips_mics);

    DATA_VECTOR(log_offset_mics);
    DATA_VECTOR(births_obs_mics);

    vector<Type> mu_obs_pred_mics(X_extract_mics * (M_full_obs * log(lambda_out))
                                  + X_extract_mics * u_smooth_lh
                                  + X_extract_mics * tips_fe_lh
                                  + log_offset_mics

                );

    if(!mwi_rwa_toggle) {
      mu_obs_pred_mics = mu_obs_pred_mics
        + X_extract_mics * spike_1999_lh
        + X_extract_mics * spike_2000_lh
        + X_extract_mics * spike_2001_lh;
    }

    if(multiple_survey_toggle) {
      mu_obs_pred_mics = mu_obs_pred_mics
        + X_extract_mics * zeta2_lh;
    }

    nll -= dpois(births_obs_mics, exp(mu_obs_pred_mics), true).sum();

    // REPORT(mu_obs_pred_mics);

  }


  REPORT(tfr_out);
  REPORT(lambda_out);
  // REPORT(u_period_lh);
  // REPORT(lambda);

  // REPORT(log_prec_eta1);
  // REPORT(eta1_phi_age);
  // REPORT(eta1_phi_period);
  // REPORT(eta1);

  // REPORT(log_prec_country);

  // REPORT(log_prec_omega1);
  // REPORT(omega1_phi_age);

  // REPORT(log_prec_omega2);
  // REPORT(omega2_phi_period);

  // REPORT(log_prec_rw_age);
  // REPORT(log_prec_rw_period);
  // REPORT(log_prec_rw_tips);

  // REPORT(log_prec_smooth_iid);

  // REPORT(beta_period);
  // REPORT(phi_period);
  // REPORT(phi_arima_period);

  // REPORT(beta_tips_dummy_5);
  // REPORT(beta_tips_fe);
  // REPORT(zeta2);

  // REPORT(u_period);
  // REPORT(u_age);
  // REPORT(u_tips);
  // REPORT(u_spatial_str);
  // REPORT(eta2);
  // REPORT(eta3);

  // REPORT(beta_0);
  
  // REPORT(beta_spike_1999);
  // REPORT(beta_spike_2000);
  // REPORT(beta_spike_2001);

  // Posterior predictive checks
  // REPORT(mu_obs_pred_ais);
  // REPORT(mu_obs_pred_dhs);


  return nll;

}
