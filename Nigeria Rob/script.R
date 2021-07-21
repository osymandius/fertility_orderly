devtools::install_github("osymandius/dfertility")
devtools::install_github("mrc-ide/naomi")
install.packages("TMB")

library(TMB)
library(naomi)
library(dfertility)

nga_for_rob <- readRDS("nga_for_rob.rds")

list2env(nga_for_rob, envir = .GlobalEnv)

obj <-  TMB::MakeADFun(data = tmb_int$data,
                       parameters = tmb_int$par,
                       DLL = "dfertility",
                       random = tmb_int$random,
                       hessian = FALSE)

f <- stats::nlminb(obj$par, obj$fn, obj$gr)
f$par.fixed <- f$par
f$par.full <- obj$env$last.par

fit <- c(f, obj = list(obj))

class(fit) <- "naomi_fit"  # this is hacky...
fit <- naomi::sample_tmb(fit, random_only=FALSE)

tmb_results <- dfertility::tmb_outputs(fit, mf, areas)

write_csv(tmb_results, "fr.csv")
