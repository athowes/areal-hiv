# Checking the poor performance of the Stan models according to SLOO (and LOO)

hiv_surveys <- readRDS("data/hiv_surveys.rds")
mw <- subset(hiv_surveys, survey_id == "MW2015DHS")

# output = TRUE returns the fitted model, without it only the scores are saved
res_loo <- cross_validate(mw, type = "LOO", fn = ck_stan, output = TRUE)
res_sloo <- cross_validate(mw, type = "SLOO", fn = ck_stan, output = TRUE)
safe_saveRDS(res_loo, output_dir = "data/checks/", file = "res_loo")
safe_saveRDS(res_sloo, output_dir = "data/checks/", file = "res_sloo")

# SLOO-CRPS tends to be greater than than LOO-CRPS
crps_loo <- unlist(res_loo$scores$crps)
crps_sloo <- unlist(res_sloo$scores$crps)
plot(crps_loo, crps_sloo, xlab = "LOO", ylab = "SLOO")

fits_loo <- lapply(res_loo$training_sets, function(set) set$fit)
fits_sloo <- lapply(res_sloo$training_sets, function(set) set$fit)

rhat_check(fits_loo, 1.025)
rhat_check(fits_loo, 1.05)

rhat_check(fits_sloo, 1.025)
rhat_check(fits_sloo, 1.05)
