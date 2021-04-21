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
