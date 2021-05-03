# Quicker INLA models: don't need to be parallel over data splits here

ids <- c("MW2015DHS", "ZW2015DHS", "TZ2012AIS", "CI2012DHS")
types <- c("LOO", "SLOO")
inf_models <- c("constant", "iid", "icar", "bym", "fck", "fik")
fns <- list(constant_inla, iid_inla, besag_inla, bym2_inla, fck_inla, fik_inla)

pars <- rlist::list.expand(
  id = ids,
  type = types,
  inf_model = inf_models,
  fn = NA,
  ctx_ver = NA
)

# Add fn entries to list of lists
k <- length(ids) * length(types)
for(i in seq_along(pars)) {
  pars[[i]]$fn <- rep(fns, each = k)[[i]]
  pars[[i]]$ctx_ver <- ctx_ver
}

assign(
  paste0("pars_02_inla_", ctx_ver),
  pars
)

assign(
  paste0("grp_02_inla_", ctx_ver), 
  obj$enqueue_bulk(pars, cross_validate_id, do_call = TRUE)
)

rm(ids, types, inf_models, fns, pars, k, i)

# Slower Stan models: add more jobs here

stan_pars_setup <- function(ids, index) {
  ids <- ids
  index <- index
  types <- c("LOO", "SLOO")
  inf_models <- c("ck", "ik")
  fns <- list(ck_stan, ik_stan)
  
  pars <- rlist::list.expand(
    id = ids,
    indices = index,
    type = types,
    inf_model = inf_models,
    fn = NA,
    ctx_ver = NA
  )
  
  k <- length(ids) * length(types) * length(index)
  for(i in seq_along(pars)) {
    pars[[i]]$fn <- rep(fns, each = k)[[i]]
    pars[[i]]$ctx_ver <- ctx_ver
  }
  
  return(pars)
}

pars_mw <- stan_pars_setup(ids = "MW2015DHS", index = list(1:7, 8:14, 15:21, 22:28)) # 4 splits * 2 models * 2 types = 16 jobs
pars_zw <- stan_pars_setup(ids = "ZW2015DHS", index = list(1:10, 11:20, 21:30, 31:40, 41:50, 51:60)) # 6 * 2 * 2 = 24 jobs
pars_ci <- stan_pars_setup(ids = "CI2012DHS", index = list(1:11, 12:22, 23:33)) # 3 * 2 * 2 = 12 jobs

assign(paste0("pars_02_stan_mw_", ctx_ver), pars_mw)
assign(paste0("grp_02_stan_mw_", ctx_ver), obj$enqueue_bulk(pars_mw, cross_validate_sets, do_call = TRUE))

assign(paste0("pars_02_stan_zw_", ctx_ver), pars_zw)
assign(paste0("grp_02_stan_zw_", ctx_ver), obj$enqueue_bulk(pars_zw, cross_validate_sets, do_call = TRUE))

assign(paste0("pars_02_stan_ci_", ctx_ver), pars_ci)
assign(paste0("grp_02_stan_ci_", ctx_ver), obj$enqueue_bulk(pars_ci, cross_validate_sets, do_call = TRUE))

# Special case: islands in Tanzania (no Stan so fit locally)

ids <- c("TZ2012AIS-no_islands")
types <- c("LOO", "SLOO")
inf_models <- c("constant", "iid", "icar", "bym", "fck", "fik")
fns <- list(constant_inla, iid_inla, besag_inla, bym2_inla, fck_inla, fik_inla)

pars <- expand.grid("id" = ids, "type" = types, "inf_model" = inf_models)

k <- length(ids) * length(types)
pars$fn <- rep(fns, k)
pars$ctx_ver <- ctx_ver

purrr::pmap_df(pars, cross_validate_id)
