#' Function to load particular sf, cross-validate manually, compute and save the scores.
#'
#' @param id A HIV survey identifier e.g. `"MW2015DHS"`.
#' @param inf_model An inferential model name as string e.g. `"iid_inla"`.
#' @param type The type of cross-validation to perform e.g. `"LOO"`.
#' @param ctx_ver The version of the context.
cross_validate_id <- function(id, inf_model, type = "LOO", ctx_ver) {
  hiv_surveys <- readRDS("data/hiv_surveys.rds")
  sf <- subset(hiv_surveys, survey_id == id)
  fn <- get(inf_model, mode = "function")
  res <- cross_validate(sf = sf, type = type, fn = fn)
  
  output_dir <- paste0("data/", ctx_ver, "/cv/", id, "/")
  safe_saveRDS(
    object = res, 
    output_dir = output_dir, 
    file = paste0(strsplit(inf_model, "_")[[1]][1], "_", tolower(type))
  )
}

#' Function to cross-validate only part of the training sets (to allow parallelisation)
#' 
cross_validate_sets <- function(id, inf_model, type = "LOO", indices = NA, ctx_ver, S = 4000, ...) {
  hiv_surveys <- readRDS("data/hiv_surveys.rds")
  sf <- subset(hiv_surveys, survey_id == id)
  fn <- get(inf_model, mode = "function")
  
  # Mostly from https://github.com/athowes/bsae/blob/master/R/cross_validate.R
  training_sets <- create_folds(sf, remove_cols = c("y", "est"), type = type)
  if(!is.na(indices)){
    training_sets <- training_sets[indices]
  }
  
  complete <- 0
  total <- length(training_sets)
  fits <- lapply(
    training_sets,
    FUN = function(training_set) {
      fit <- fn(training_set$data, ...)
      complete <<- complete + 1
      print(paste0(complete, "/", total, " models fit."))
      return(fit)
    }
  )

  for(i in 1:total) training_sets[[i]]$fit <- fits[[i]]

  scores <- data.frame(t(sapply(
    training_sets,
    FUN = function(x) held_out_metrics(fit = x$fit, sf = sf, i = x$predict_on, S = S)
  )))

  res <- list(scores = scores)

  safe_saveRDS(
    object = res, 
    output_dir = paste0("data/", ctx_ver, "/cv/", id, "/"), 
    file = paste0(strsplit(inf_model, "_")[[1]][1], "_", tolower(type), "_", min(indices), "-", max(indices))
  )
}
