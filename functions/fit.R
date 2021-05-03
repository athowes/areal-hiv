#' Function to load particular sf, fit list of models then save them.
#'
#' @param id A HIV survey identifier e.g. `"MW2015DHS"`.
#' @param inf_model An inferential model name as string e.g. `"iid_inla"`.
#' @param ctx_ver The version of the context.
fit_id <- function(id, inf_model, fn, ctx_ver){
  hiv_surveys <- readRDS("data/hiv_surveys.rds")
  sf <- subset(hiv_surveys, survey_id == id)
  fn <- get(inf_model, mode = "function")
  res <- fn(sf)
  output_dir <- paste0("data/", ctx_ver, "/fits/", id, "/")
  safe_saveRDS(object = res, output_dir = output_dir, file = paste0(strsplit(inf_model, "_")[[1]][1]))
}
