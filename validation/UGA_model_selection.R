library(orderly)
id <- orderly_run('aaa_model_selection', data.frame(iso3 = 'UGA'))
orderly_commit(id)
