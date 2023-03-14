library(countrycode)
setwd("/Volumes/HOMES/os210")

ssa_names <- c("Angola", "Botswana", "Eswatini", "Ethiopia", "Kenya", "Lesotho",  "Malawi", "Mozambique", "Namibia", "Rwanda", "South Africa", "South Sudan", "Uganda", "United Republic of Tanzania", "Zambia", "Zimbabwe", "Benin", "Burkina Faso", "Burundi", "Cameroon", "Central African Republic", "Chad", "Congo", "Côte d'Ivoire", "Democratic Republic of the Congo", "Equatorial Guinea", "Gabon", "Gambia", "Ghana", "Guinea", "Guinea-Bissau", "Liberia", "Mali", "Niger", "Nigeria", "Senegal", "Sierra Leone", "Togo")
ssa_iso3 <- countrycode(ssa_names, "country.name", "iso3c")

# directory to save contexts (change as appropriate)
root <- "/Volumes/HOMES/os210"
# ensure it exists, and if not, create it
# threemc::create_dirs_r(root)

# orderly repos with tasks to be bundled
orderly_root <- "~/Documents/GitHub/fertility_orderly/"

# path to save bundles
path_bundles <- file.path(root, "bundles")
# threemc::create_dirs_r(path_bundles)

# path to output bundles
output_path <- "output"
# threemc::create_dirs_r(output_path)

# working countries  
iso3 <- ssa_iso3[!ssa_iso3 %in% c("SSD", "ETH", "GNQ", "GMB", "BWA", "ZAF", "COD", "NGA")]

#### bundle orderly tasks #### 

# pack up 01_modelling for each country
if (length(iso3) == 1) {
  bundles <- orderly::orderly_bundle_pack(path_bundles,
                                          "aaa_fit",
                                          parameters = list(iso3 = iso3),
                                          root = orderly_root)
} else {
  bundles <- lapply(iso3, function(x) orderly::orderly_bundle_pack(path_bundles,
                                                                   "aaa_fit",
                                                                   parameters = list(
                                                                     iso3 = x
                                                                   ),
                                                                   root = orderly_root))
}


#### contexts ####

# change directory to cluster
# setwd(dirname(root))
#37984
# cluster config
options(didehpc.cluster = "fi--didemrchnb", 
        didehpc.username = "os210", 
        didehpc.repos = "https://cran.ma.imperial.ac.uk/",
        didehpc.home = getwd())

config <- didehpc::didehpc_config(credentials = list(username = "os210", password = Sys.getenv("DIDE_PASSWORD")),
                                  workdir = root, cores = 8)

# setup context for orderly task (packages required, etc)
ctx <- context::context_save(
  root,
  packages = c(
    "TMB", "orderly", "dfertility", "naomi"
  ),
  package_sources = conan::conan_sources(c(
    "github::mrc-ide/first90release",
    "github::mrc-ide/naomi",
    "github::osymandius/dfertility"
  ))
)
# queue above context on cluster
obj <- didehpc::queue_didehpc(context = ctx, config = config)


#### run bundles ####


if (length(iso3) == 1) {
  # (relative) paths to bundles
  paths <- paste(last(strsplit(dirname(bundles$path), "/")[[1]]),
                 basename(bundles$path), sep = "/")
  
  # send orderly task to cluster!
  t <- obj$enqueue(orderly::orderly_bundle_run(
    path = paths, workdir = output_path
  ))
} else {
  # (relative) paths to bundles
  paths <- lapply(bundles, function(x) {
    paste(last(strsplit(dirname(x$path), "/")[[1]]), 
          basename(x$path), sep = "/")
  })
  
  # send orderly tasks to cluster! wait till they have all run before proceeding
  t <- obj$lapply(paths, orderly::orderly_bundle_run, workdir = output_path)
  
  # look at individual tasks for logs
  tasks <- t$tasks
}


#### import completed tasks ####

if (length(iso3) == 1) {
  output <- strsplit(t$wait(100)$path, "\\\\")[[1]]
  output_filename <- output[length(output)]
  orderly::orderly_bundle_import(file.path(root, output_path, output_filename),
                                 root = orderly_root)
} else {
  for (output in t$wait(100)) {
    out <- strsplit(output$path, "\\\\")[[1]]
    output_filename <- out[length(out)]
    orderly::orderly_bundle_import(file.path(root, output_path, output_filename),
                                   root = orderly_root)
  }
}

# push committed reports to sharepoint folder
cr <-  orderly::orderly_list_archive(root = orderly_root)
lapply(seq_along(cr$id), function(x) orderly::orderly_push_archive(
  name = cr$name[x], id = cr$id[x], root = orderly_root)
)