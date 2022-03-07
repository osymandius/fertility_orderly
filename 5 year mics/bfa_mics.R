library(orderly)

id <- orderly_run("aaa_fit", data.frame(iso3 = "BFA"))

orderly_commit(id)

orderly_push_archive("aaa_fit", id = id, remote = "real")