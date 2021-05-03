# Check fit_id and cross_validate_id using the fastest fitting model(s)
fit_id("MW2015DHS", inf_model = "constant_inla", ctx_ver = "local-test")
cross_validate_id("MW2015DHS", type = "LOO", inf_model = "constant_inla", ctx_ver = "local-test")

fit_id("MW2015DHS", inf_model = "iid_inla", ctx_ver = "local-test")
cross_validate_id("MW2015DHS", type = "LOO", inf_model = "iid_inla", ctx_ver = "local-test")
