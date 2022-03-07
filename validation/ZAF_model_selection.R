library(orderly)
id <- orderly_run('aaa_model_selection', data.frame(iso3 = 'ZAF'))
orderly_commit(id)
