#' Function to fit single model.
#'
#' @param sf A simple features object with some geometry.
#' @param fn A fitting function.
#' @return A fitted model.
fit_model <- function(sf, fn, ...) {
  fn(sf, ...)  
}

#' Function to load particular sf, fit list of models then save them.
#'
#' @param id A HIV survey identifier e.g. `"MW2015DHS"`.
#' @param inf_model An inferential model name as string e.g. `"iid"`.
#' @param fn An inferential fitting function, e.g. `iid_inla`.
#' @param ctx_ver The version of the context.
fit_id <- function(id, inf_model, fn, ctx_ver){
  hiv_surveys <- readRDS("data/hiv_surveys.rds")
  sf <- subset(hiv_surveys, survey_id == id)
  res <- fit_model(sf, fn)
  output_dir <- paste0("data/", ctx_ver, "/fits/", id, "/")
  safe_saveRDS(object = res, output_dir = output_dir, file = paste0(inf_model))
}
