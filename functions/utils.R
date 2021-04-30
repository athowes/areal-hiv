safe_saveRDS <- function(object, output_dir, file) {
  if (!dir.exists(output_dir)) {
    dir.create(output_dir, recursive = TRUE)
    message(output_dir, " was created")
  }
  saveRDS(object, file = paste0(output_dir, "/", file, ".rds"))
}

rename_df <- function(df) {
  if("geometry" %in% names(df)) {
    df$geometry <- recode_factor(df$geometry,            
      "CI2012DHS" = "Cote d'Ivoire 2012 DHS", 
      "MW2015DHS" = "Malawi 2015 DHS",
      "TZ2012AIS" = "Tanzania 2012 AIS",
      "ZW2015DHS" = "Zimbabwe 2015 DHS"
    )
  }
  
  if("inf_model" %in% names(df)) {
    df$inf_model <- recode_factor(df$inf_model,
      "constant" = "Constant",
      "iid" = "IID", 
      "icar" = "Besag", 
      "bym" = "BYM2", 
      "fck" = "FCK", 
      "ck" = "CK", 
      "fik" = "FIK",
      "ik" = "IK"
    )
  }
  
  if("type" %in% names(df)) {
    df$type <- recode_factor(df$type, 
      "loo" = "LOO", 
      "sloo" = "SLOO"
    )
  }
  
  return(df)
}
