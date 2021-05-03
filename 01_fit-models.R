ids <- c("MW2015DHS", "ZW2015DHS", "TZ2012AIS", "CI2012DHS")
inf_models <- c("constant", "iid", "icar", "bym", "fck", "fik", "ck", "ik")
fns <- list(constant_inla, iid_inla, besag_inla, bym2_inla, fck_inla, fik_inla, ck_stan, ik_stan)

pars <- rlist::list.expand(
  id = ids, 
  inf_model = inf_models,
  fn = NA,
  ctx_ver = NA
)

# Add fn entries to list of lists
k <- length(ids)
for(i in seq_along(pars)) {
  pars[[i]]$fn <- rep(fns, each = k)[[i]]
  pars[[i]]$ctx_ver <- ctx_ver
}

assign(
  paste0("pars_01_", ctx_ver),
  pars
)

assign(
  paste0("grp_01_", ctx_ver), 
  obj$enqueue_bulk(pars, fit_id, do_call = TRUE)
)

rm(ids, inf_models, fns, pars, k, i)

# Special case: islands in Tanzania (no Stan so fit locally)

# Create the data for without islands if it doesn't already exist
if(!("TZ2012AIS-no_islands" %in% unique(hiv_surveys$survey_id))) {
  hiv_surveys <- readRDS("data/hiv_surveys.rds")
  tz <- filter(hiv_surveys, survey_id == "TZ2012AIS")
  nb <- sf_to_nb(tz) # Neighbourhood list
  comp <- spdep::n.comp.nb(nb) # Connected components
  table(comp$comp.id) # The number of districts in each component
  is_mainland <- comp$comp.id == 1
  tz_no_islands <- tz[is_mainland, ] # Only those on the mainland
  tz_no_islands$survey_id <- "TZ2012AIS-no_islands" # Change the ID so that fit_id works
  hiv_surveys_appended <- rbind(hiv_surveys, tz_no_islands)
  assertthat::assert_that(nrow(hiv_surveys_appended) - nrow(hiv_surveys) == sum(is_mainland))
  saveRDS(hiv_surveys_appended, "data/hiv_surveys.rds")
}

ids <- c("TZ2012AIS-no_islands")
inf_models <- c("constant", "iid", "icar", "bym", "fck", "fik")
fns <- list(constant_inla, iid_inla, besag_inla, bym2_inla, fck_inla, fik_inla)

pars <- expand.grid("id" = ids, "inf_model" = inf_models)

k <- length(ids)
pars$fn <- rep(fns, k)
pars$ctx_ver <- ctx_ver

purrr::pmap_df(pars, fit_id)
