safe_saveRDS <- function(object, output_dir, file) {
  if (!dir.exists(output_dir)) {
    dir.create(output_dir, recursive = TRUE)
    message(output_dir, " was created")
  }
  saveRDS(object, file = paste0(output_dir, "/", file, ".rds"))
}

rename_df <- function(df) {
  df$geometry <- recode_factor(df$geometry,            
    "CI2012DHS" = "2012 DHS Cote d'Ivoire", 
    "MW2015DHS" = "2015 DHS Malawi",
    "TZ2012AIS" = "2012 AIS Tanzania",
    "ZW2015DHS" = "2015 DHS Zimbabwe"
  )
  
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
  
  return(df)
}
