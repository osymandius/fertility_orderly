library(haven)
library(rdhs)
library(dplyr)
library(tidyr)
library(demogsurv)
library(survey)
library(readr)
library(stringr)

path <- "~/Imperial College London/HIV Inference Group - WP - Documents/Data/household surveys/PHIA/datasets"

ind_files <- list(cam = "CMR/datasets/202 CAMPHIA 2017-2018 Adult Interview Dataset (DTA).zip",
                  civ = "CIV/datasets/202 CIPHIA 2017-2018 Adult Interview Dataset (DTA).zip",
                  lso = "LSO/datasets/202_LePHIA 2016-2017 Adult Interview Dataset (DTA).zip",
                  nam = "NAM/datasets/202_NAMPHIA 2017 Adult Interview Dataset (DTA).zip",
                  rwa = "RWA/datasets/202_202 RPHIA 2018-2019 Adult Interview Dataset (DTA).zip",
                  uga = "UGA/datasets/202 UPHIA 2016-2017 Adult Interview Dataset (DTA).zip",
                  zwe = "ZWE/datasets/202_ZIMPHIA 2015-2016 Adult Interview Dataset (DTA).zip")

bio_files <- list(cam = "CMR/datasets/302 CAMPHIA 2017-2018 Adult Biomarker Dataset (DTA).zip",
                  civ = "CIV/datasets/302 CIPHIA 2017-2018 Adult Biomarker Dataset (DTA).zip",
                  lso = "LSO/datasets/302_LePHIA 2016-2017 Adult Biomarker Dataset (DTA).zip",
                  nam = "NAM/datasets/302_NAMPHIA 2017 Adult Biomarker Dataset (DTA).zip",
                  rwa = "RWA/datasets/302 RPHIA 2018-2019 Adult Biomarker Dataset (DTA).zip",
                  uga = "UGA/datasets/302 UPHIA 2016-2017 Adult Biomarker Dataset (DTA).zip",
                  zwe = "ZWE/datasets/302_ZIMPHIA 2015-2016 Adult Biomarker Dataset (DTA).zip")

combined_files <- list(swz = "SWZ/datasets/SHIMS2 2016-2017 Household Interview and Biomarker Datasets v2.0 (CSV).zip",
                       mwi = "MWI/datasets/MPHIA 2015-2016 Household Interview and Biomarker Datasets v2.0 (CSV).zip",
                       ken = "KEN/datasets/KENPHIA 2018 Household Interview and Biomarker Datasets v1.1 (CSV).zip",
                       tza = "TZA/datasets/THIS 2016-2017 Household Interview and Biomarker Datasets v2.0 (CSV).zip",
                       zmb = "ZMB/datasets/ZAMPHIA 2016 Household Interview and Biomarker Datasets v2.0 (CSV).zip")

geo_files <- list(
                  cam = "CMR/datasets/CAMPHIA 2017-2018 PR Geospatial Data 20210628.zip",
                  civ = "CIV/datasets/CIPHIA 2017-2018 PR Geospatial Data 20210804.zip",
                  swz = "SWZ/datasets/SHIMS2 2016-2017 PR Geospatial Data 20211022.zip",
                  lso = "LSO/datasets/LePHIA 2016-2017 PR Geospatial Data 20210518.zip",
                  mwi = "MWI/datasets/MPHIA 2015-2016 PR Geospatial Data 20210917.zip",
                  nam = "NAM/datasets/NAMPHIA 2017 PR Geospatial Data 20210603.zip",
                  rwa = "RWA/datasets/RPHIA 2018-2019 PR Geospatial Data 20210722.zip",
                  tza = "TZA/datasets/THIS 2016-2017 PR Geospatial Data 20211014.zip",
                  uga = "UGA/datasets/UPHIA 2016-2017 PR Geospatial Data 20210602.zip",
                  zmb = "ZMB/datasets/ZAMPHIA 2016 PR Geospatial Data 20210920.zip",
                  zwe = "ZWE/datasets/ZIMPHIA 2015-2016 PR Geospatial Data 20211001.zip")

ind_raw <- parallel::mclapply(ind_files, function(x) read_zipdata(file.path(path, x)), mc.cores = 8)
bio_raw <- parallel::mclapply(bio_files, function(x) read_zipdata(file.path(path, x)), mc.cores = 8)
geo_raw <- parallel::mclapply(geo_files, function(x) read_zipdata(file.path(path, x)), mc.cores = 8)

ind_combined_raw <- parallel::mclapply(combined_files, function(x) read_zipdata(file.path(path, x), "adultind.csv", read.csv, na.strings = ". "), mc.cores = 8)
bio_combined_raw <- parallel::mclapply(combined_files, function(x) read_zipdata(file.path(path, x), "adultbio.csv", read.csv, na.strings = ". "), mc.cores = 8)

ind_raw_t <- c(ind_raw, ind_combined_raw %>% lapply(function(x) x %>% mutate(across(where(is.character), str_trim))))
bio_raw_t <- c(bio_raw, bio_combined_raw %>% lapply(function(x) x %>% mutate(across(where(is.character), str_trim))))

# ind_raw_t <- lapply(ind_raw_t, function(x) {
#   x$pregnum[x$pregnum == "."] <- NA
#   x$pregnum[x$pregnum == "."] <- NA
#   
#   x$pregnum <- as.numeric(x$pregnum)
#   x$pregnum <- as.numeric(x$pregnum)
#   x
# })
#' Individual variables:
#' - country
#' - householdid
#' - personid
#' - indstatus
#' - gender
#' - age
#' - urban
#' - varstrat
#' - varunit
#' - surveystmonth: Interview start month
#' - surveystyear: Interview start year
#' - intwt0
#'
#' - pregnum: How many times have you been pregnant including a current pregnancy? (top coded at 11)
#' - pregnumdk
#' - liveb: Have you ever had a pregnancy that resulted in a live birth?
#' - childa2012
#' - childa2012dk
#' - delivered12months
#' - delivered3years
#' - prgtwin
#' - pregnm: How many live children were born from your last pregnancy? (top coded at 2)
#' - pregnant: Are you pregnant now?pregnant
#' - pregancystatus: recoded version of pregnant
#' - pregmonths: How many months pregnant are you?
#' - pregmonthsdk
#' - pregnant: Are you pregnant now?
#' - sex12months
#' - sexever

#' ## Create a pooled dataset of all surveys
#'


ind_vars <- c("country", "centroidid", "householdid", "personid", "indstatus", "gender", "age", "urban", "varstrat", "varunit",  "intwt0",
              "pregnum", "pregnumdk", "liveb", "childa2012", "childa2012dk", "delivered12months", "delivered3years", "prgtwin",
              "pregnant", "pregnancystatus", "sexever", "sex12months")
ind_vars_opt <- c("surveystdt", "surveystyear", "surveystmonth", "pregnm", "pregmonths", "pregmonthsdk", "county")

bio_vars <- c("personid", "bt_status", "btwt0", "hiv1statusfinalsurvey")

# ind_combined_raw$ken %>% select("country", "centroidid", "householdid", "personid", "indstatus", "gender", "age", "urban", "varstrat", "varunit",  "intwt0", "surveystyear", "surveystmonth", "county")

ind <- lapply(ind_raw_t, select, all_of(ind_vars), any_of(ind_vars_opt)) %>%
  lapply(function(x) {
    x[x == "."] <- NA
    type.convert(x, as.is = TRUE)
  }) %>%
  bind_rows()

bio <- lapply(bio_raw_t, select, all_of(bio_vars)) %>%
  lapply(function(x) {
    x[x == "."] <- NA
    type.convert(x, as.is = TRUE)
  }) %>%
  bind_rows()

dat <- left_join(ind, bio, by = "personid")

#' ## Filter to women of reproductive age, indstatus = 1
#'
## indstatus
##
## 1 - Eligible Respondent
## 2 - Eligible Non-Respondent
## 4 - Unknown Eligibility Status
## 7 - Rostered in Error
## 8 - Not Sampled
## 9 - Non-defacto Participants

stopifnot(dat$indstatus %in% 1:9)
stopifnot(dat$age %in% 15:80)
stopifnot(dat$gender %in% c(1, 2, 99))

count(dat, gender, age) %>%
  spread(gender, n) %>%
  print(n = Inf)

dat <- filter(dat, gender == 2, age %in% 15:49, indstatus == 1)

# Create survey DOI CMC
dat <- dat %>%
  mutate(
    cluster = paste0(varstrat + varunit),
    surveyst_cmc = 12 * (surveystyear - 1900) + surveystmonth
  )

#' Six observations missing survey date; assign as cluster median
filter(dat, is.na(surveyst_cmc))

dat <- dat %>%
  group_by(country, cluster) %>%
  mutate(
    surveyst_cmc = if_else(!is.na(surveyst_cmc), surveyst_cmc, floor(median(surveyst_cmc, na.rm = TRUE)))
  ) %>%
  ungroup()

#' Assign a date of birth in CMC by randomly assigning 0:11 months past age
set.seed(1907421)
dat <- dat %>%
  mutate(
    dob_cmc = surveyst_cmc - 12 * age - sample(0:11, nrow(.), replace = TRUE)
  )

count(ind, pregnum, pregnumdk) %>%
  spread(pregnumdk, n)

count(ind, pregnum, liveb) %>%
  spread(liveb, n)

count(ind, pregnum, childa2012) %>%
  spread(childa2012, n)


dat %>%
  count(country, pregnum) %>%
  spread(pregnum, n) %>%
  print(n = Inf)

#' There's a handful of women who have missing values for `pregnum`.
#' * In some cases they are coded -9 (refused) for `pregnumdk`, but not all.
#' * All further birth history questions are NA.
#' * In SWZ, LSO, NAM, UGA the `pregnant` variable is completed. In MWI, TZA, ZMB, ZWE,
#'   the `pregnant` variable is missing as well.
#' * DECISION: treat as missing data and drop
#'
#' For cases with pregnum = -7 (OUT OF RANGE), they appear to be 'yes' been pregnant; retain.
#'

dat <- filter(dat, !is.na(pregnum))

#' Randomly assign a CMC date of birth for children born in past 12 months
#'
#' * Note: delivered12months is coded = 3 (Mother, missing last delivery date) if childa2012 = 0;
#'         These are not missing data.

count(dat, country, delivered12months) %>%
  spread(delivered12months, n)

dat %>%
  filter(childa2012 == 0) %>%
  count(country, delivered12months)

set.seed(83650207)
dat <- dat %>%
  mutate(
    chdob_cmc = if_else(delivered12months == 1, surveyst_cmc - sample(1:12, nrow(.), replace = TRUE), NA_real_)
  )

count(dat, country, prgtwin) %>%
  spread(prgtwin, n)

count(dat, country, pregnm) %>%
  spread(pregnm, n)

dat <- dat %>%
  mutate(
    num_births = case_when(delivered12months == 1 & prgtwin == 1 & !is.na(pregnm) ~ pregnm,
                           delivered12months == 1 & prgtwin == 1 & is.na(pregnm) ~ 2,
                           delivered12months == 1 ~ 1,
                           TRUE ~ NA_real_)
  )

#' Fix C\xf4te d'Ivoire

dat$country <- sub("C\xf4te d'Ivoire", "Cote d'Ivoire", dat$country)
count(dat, country)


#' ## Create HIV, pregnancy status, and sexual history indicators

dhs_cc <- c("Cote d'Ivoire" = "CI",
            "Cameroon" = "CM",
            "Kenya" = "KE",
            "Lesotho" = "LS",
            "Malawi" = "MW",
            "Nigeria" = "NG",
            "Namibia" = "NM",
            "Rwanda" = "RW",
            "Eswatini" = "SZ",
            "Tanzania" = "TZ",
            "Uganda" = "UG",
            "Zambia" = "ZM",
            "Zimbabwe" = "ZW")

survey_year <- c("Cote d'Ivoire" = 2017,
                 "Cameroon" = 2017,
                 "Kenya" = 2018,
                 "Lesotho" = 2017,
                 "Malawi" = 2017,
                 "Nigeria" = 2018,
                 "Namibia" = 2017,
                 "Rwanda" = 2019,
                 "Eswatini" = 2017,
                 "Tanzania" = 2017,
                 "Uganda" = 2017,
                 "Zambia" = 2016,
                 "Zimbabwe" = 2016)

dat <- dat %>%
  mutate(
    iso3 = countrycode::countrycode(country, 'country.name', 'iso3c'),
    survyear = factor(survey_year[as.character(country)]),
    surveyid = factor(paste0(dhs_cc[as.character(country)], survyear, "PHIA")),
    survey_id = factor(paste0(iso3, survyear, "PHIA")),
    country = factor(country),
    eversex = recode(sexever, `1` = 1, `2` = 0, .default = NA_real_),
    sex12m = case_when(sexever == 2 ~ 0,
                       sex12months == 2 ~ 0,
                       sex12months == 1 ~ 1),
    prev = recode(hiv1statusfinalsurvey, `1` = 1, `2` = 0, .default = NA_real_),
    hivstatus = recode(hiv1statusfinalsurvey,
                       `1` = "positive", `2` = "negative", .default = NA_character_),
    hivstatus = factor(hivstatus),
    currpreg = recode(pregnancystatus, `1` = 1, `2` = 0, .default = NA_real_)
  )


stopifnot(!is.na(dat$survyear))
stopifnot(!is.na(dat$surveyid))

iso3_c <- countrycode::countrycode(unique(dat$country), "country.name", "iso3c")

setwd(rprojroot::find_rstudio_root_file())

areas <- lapply(paste0("archive/", tolower(iso3_c), "_data_areas"), list.files, full.names= TRUE) %>%
  lapply(tail, 1) %>%
  lapply(list.files, full.names = TRUE, pattern = "areas.geojson") %>%
  lapply(sf::read_sf) %>%
  lapply(select, -any_of("epp_level"))

geo_raw <- geo_raw %>%
  bind_rows() %>%
  st_as_sf(coords = c("longitude", "latitude"), crs = st_crs(areas %>% bind_rows()))

max_area_l <- areas %>% 
  lapply(filter, area_level == max(area_level)) %>%
  lapply(st_make_valid)
  
centroid_areas <- geo_raw %>%
  st_join(max_area_l %>% bind_rows()) %>%
  select(centroidid, area_id)

## 18 centroids not mapped to areas. These sit right on the edge of a boundary. st_snap doesn't seem to work. Should fix.
dat %>%
  left_join(centroid_areas) %>%
  filter(is.na(area_id)) %>%
  distinct(country, centroidid)

dat <- dat %>%
  left_join(centroid_areas) %>%
  filter(!is.na(area_id))

births <- dat %>%
  filter(delivered12months == 1) %>%
  {data.frame(
    survey_id = rep(.$survey_id, times = .$num_births),
    area_id = rep(.$area_id, times = .$num_births),
    personid = rep(.$personid, times = .$num_births),
    chdob_cmc = rep(.$chdob_cmc, times = .$num_births)
  )}

datlst <- dat %>%
  group_by(iso3) %>%
  group_split()

birthslst <- births %>%
  separate(area_id, into = c("iso3", NA), sep = 3, remove = FALSE) %>%
  group_by(iso3) %>%
  group_split()

names(datlst) <- datlst %>% bind_rows() %>% distinct(iso3) %>% unlist()
names(birthslst) <- birthslst %>% bind_rows() %>% distinct(iso3) %>% unlist()
names(areas) <- iso3_c

res <- Map(function(dat, births, areas) {
  areas_wide <- naomi::spread_areas(areas)
  dat <- dat %>%
    left_join(areas_wide %>% select(area_id, area_id1) %>% st_drop_geometry())
  births <- births %>%
    left_join(areas_wide %>% select(area_id, area_id1) %>% st_drop_geometry())
  out <- list()
  out$dat <- dat
  out$births <- births
  out
    
}, datlst, birthslst[names(datlst)], areas[names(datlst)])


dat_admin1_list <- res %>%
  lapply("[", "dat") %>%
  unlist(recursive = FALSE) %>%
  bind_rows() %>%
  group_by(area_id1) %>%
  group_split()

births_admin1_list <- res %>%
  lapply("[", "births") %>%
  unlist(recursive = FALSE) %>%
  bind_rows() %>%
  group_by(area_id1) %>%
  group_split()

dat_admin0_list <- dat_admin1_list %>%
  bind_rows() %>%
  group_by(iso3) %>%
  group_split()

births_admin0_list <- births_admin1_list %>%
  bind_rows() %>%
  group_by(iso3) %>%
  group_split()

asfr_admin1 <- mcmapply(calc_asfr, data = dat_admin0_list, bhdata = births_admin0_list,
                 MoreArgs = list(
                   by=~survey_id + country + survyear + area_id1, tips = c(0, 1),
                   period = 2015:2019,
                   clusters = ~cluster, strata = NULL, weight = "intwt0",
                   id = "personid", dob = "dob_cmc", intv = "surveyst_cmc", bvars = "chdob_cmc",
                   counts = TRUE,
                   varmethod = "none"
                 ), SIMPLIFY = FALSE)

asfr_admin0 <- parallel::mcmapply(calc_asfr, data = dat_admin0_list, bhdata = births_admin0_list,
                        MoreArgs = list(
                          by=~survey_id + country + survyear, tips = c(0, 1),
                          period = 2015:2019,
                          clusters = ~cluster, strata = NULL, weight = "intwt0",
                          id = "personid", dob = "dob_cmc", intv = "surveyst_cmc", bvars = "chdob_cmc",
                          counts = TRUE,
                          varmethod = "none"
                        ), SIMPLIFY = FALSE)

asfr <- mcmapply(calc_asfr, data = dat_admin1_list, bhdata = births_admin1_list,
                        MoreArgs = list(
                          by=~survey_id + country + survyear + area_id, tips = c(0, 1),
                          period = 2015:2019,
                          clusters = ~cluster, strata = NULL, weight = "intwt0",
                          id = "personid", dob = "dob_cmc", intv = "surveyst_cmc", bvars = "chdob_cmc",
                          counts = TRUE,
                          varmethod = "none"
                        ), SIMPLIFY = FALSE)


asfr <- asfr %>%
  bind_rows() %>%
  left_join(naomi::get_age_groups() %>% select(age_group, age_group_label), by=c("agegr" = "age_group_label")) %>%
  select(-agegr)

asfr_admin1 <- asfr_admin1 %>%
  bind_rows() %>%
  left_join(naomi::get_age_groups() %>% select(age_group, age_group_label), by=c("agegr" = "age_group_label")) %>%
  select(-agegr)

asfr_admin0 <- asfr_admin0 %>%
  bind_rows() %>%
  left_join(naomi::get_age_groups() %>% select(age_group, age_group_label), by=c("agegr" = "age_group_label")) %>%
  select(-agegr)

tfr_admin0 <- asfr_admin0 %>%
  group_by(survey_id, period, tips) %>%
  summarise(median = 5*sum(asfr)) %>%
  mutate(variable = "tfr")

tfr_admin1 <- asfr_admin1 %>%
  group_by(survey_id, period, tips) %>%
  summarise(median = 5*sum(asfr)) %>%
  mutate(variable = "tfr")

write_csv(asfr, "global/phia_asfr.csv")  
write_csv(asfr_admin1, "global/phia_asfr_admin1.csv")  
write_csv(tfr_admin0, "global/phia_tfr_admin0.csv")  
write_csv(tfr_admin1, "global/phia_tfr_admin1.csv")  
write_csv(asfr_admin0, "global/phia_asfr_admin0.csv")  

#' Calculate indicators

library(parallel)
options(mc.cores = parallel::detectCores())

tfr <- mcmapply(calc_tfr, data = datlst, bhdata = birthslst,
                MoreArgs = list(
                  by=~surveyid + country + survyear, tips = c(0, 1),
                  clusters = ~cluster, strata = NULL, weight = "intwt0",
                  id = "personid", dob = "dob_cmc", intv = "surveyst_cmc", bvars = "chdob_cmc"
                ), SIMPLIFY = FALSE)
                
asfr <- mcmapply(calc_asfr, data = datlst, bhdata = birthslst,
                 MoreArgs = list(
                   by=~surveyid + country + survyear, tips = c(0, 1),
                   clusters = ~cluster, strata = NULL, weight = "intwt0",
                   id = "personid", dob = "dob_cmc", intv = "surveyst_cmc", bvars = "chdob_cmc"
                 ), SIMPLIFY = FALSE)


## ASFR by HIV status

tfrhiv <- mcmapply(calc_tfr, data = datlst, bhdata = birthslst,
                   MoreArgs = list(
                     by=~surveyid + country + survyear + hivstatus, tips = c(0, 1),
                     clusters = ~cluster, strata = NULL, weight = "btwt0",
                     id = "personid", dob = "dob_cmc", intv = "surveyst_cmc", bvars = "chdob_cmc"
                   ), SIMPLIFY = FALSE)

asfrhiv <- mcmapply(calc_asfr, data = datlst, bhdata = birthslst,
                    MoreArgs = list(
                      by=~surveyid + country + survyear + hivstatus, tips = c(0, 1),
                      clusters = ~cluster, strata = NULL, weight = "btwt0",
                      id = "personid", dob = "dob_cmc", intv = "surveyst_cmc", bvars = "chdob_cmc"
                    ), SIMPLIFY = FALSE)


## Percentage currently pregnant

calc_bin <- function(dat,
                     agegr=c(15, 50),
                     byhiv=FALSE,
                     weights=if(byhiv) ~btwt0 else ~intwt0,
                     strata=NULL,
                     formula=~currpreg){

  print(as.character(dat$surveyid[1]))

  dat$agegr <- cut(dat$age, agegr, demogsurv:::.epis_labels(agegr), TRUE, FALSE)
  if(!byhiv)
    dat$hivstatus <- "all"
  des <- svydesign(ids=~cluster, data=dat[!is.na(dat[[all.vars(weights)]]), ],
                   strata=strata, weights=weights)

  val <- left_join(
    svyby(formula, ~surveyid + country + survyear + hivstatus + agegr,
          des, unwtd.count, na.rm=TRUE) %>%
    rename(n = counts) %>%
    select(-se),
    svyby(formula, ~surveyid + country + survyear + hivstatus + agegr,
          des, svyciprop, vartype=c("se", "ci"), na.rm=TRUE),
    by = c("surveyid", "country", "survyear", "hivstatus", "agegr")
  )
  names(val) <- sub("^se\\..*", "se", names(val))

  val
}

phia_preg <- c(
  mclapply(datlst, calc_bin),
  mclapply(datlst, calc_bin, agegr=3:10*5),
  mclapply(datlst, calc_bin, byhiv=TRUE),
  mclapply(datlst, calc_bin, agegr=3:10*5, byhiv=TRUE)
) %>%
  bind_rows()

## Ever had sex

phia_eversex <- c(
  mclapply(datlst, calc_bin, formula=~eversex),
  mclapply(datlst, calc_bin, agegr=3:10*5, formula=~eversex),
  mclapply(datlst, calc_bin, byhiv=TRUE, formula=~eversex),
  mclapply(datlst, calc_bin, agegr=3:10*5, byhiv=TRUE, formula=~eversex)
) %>%
  bind_rows()

phia_sex12m <- c(
  mclapply(datlst, calc_bin, formula=~sex12m),
  mclapply(datlst, calc_bin, agegr=3:10*5, formula=~sex12m),
  mclapply(datlst, calc_bin, byhiv=TRUE, formula=~sex12m),
  mclapply(datlst, calc_bin, agegr=3:10*5, byhiv=TRUE, formula=~sex12m)
) %>%
  bind_rows()


## HIV prevalence among currently pregnant

datpreg <- lapply(datlst, filter, currpreg == 1, age %in% 15:49)

phia_pregprev <- c(
  mclapply(datpreg, calc_bin, formula=~prev, weights=~btwt0),
  mclapply(datpreg, calc_bin, agegr=3:10*5, formula=~prev, weights=~btwt0)
) %>%
  bind_rows() %>%
  mutate(hivstatus = NULL)

phia_prev <- c(
  mclapply(datlst, calc_bin, formula=~prev, weights=~btwt0),
  mclapply(datlst, calc_bin, agegr=3:10*5, formula=~prev, weights=~btwt0)
) %>%
  bind_rows() %>%
  mutate(hivstatus = NULL)


## pool and save

phia_tfr <- tfrhiv %>%
  bind_rows() %>%
  bind_rows(
    tfr %>%
    bind_rows() %>%
    mutate(hivstatus = "all")
  ) %>%
  rename(se = se_tfr)

phia_asfr <- asfrhiv %>%
  bind_rows() %>%
  bind_rows(
    bind_rows(asfr) %>% mutate(hivstatus = "all")
  ) %>%
  rename(se = se_asfr)

write_csv(phia_preg, here("data", "phia", "phia_preg.csv"), na = "")
write_csv(phia_eversex, here("data", "phia", "phia_eversex.csv"), na = "")
write_csv(phia_sex12m, here("data", "phia", "phia_sex12m.csv"), na = "")
write_csv(phia_pregprev, here("data", "phia", "phia_pregprev.csv"), na = "")
write_csv(phia_prev, here("data", "phia", "phia_prev.csv"), na = "")
write_csv(phia_tfr, here("data", "phia", "phia_tfr.csv"), na = "")
write_csv(phia_asfr, here("data", "phia", "phia_asfr.csv"), na = "")
