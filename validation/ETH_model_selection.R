library(orderly)
id <- orderly_run('aaa_model_selection', data.frame(iso3 = 'ETH'))
orderly_commit(id)
