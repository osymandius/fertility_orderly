library(orderly)
id <- orderly_run('aaa_model_selection', data.frame(iso3 = 'SWZ'))
orderly_commit(id)
