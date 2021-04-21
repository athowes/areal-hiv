# Comparison of Stan to Harrison's results using variational inference

source("../00b_local-setup.R")

hiv_surveys <- readRDS("../data/hiv_surveys.rds")

sf <- subset(hiv_surveys, survey_id == "TZ2012AIS") %>%
  mutate(id = row_number())

df <- filter(fit_df, geometry == "TZ2012AIS") %>%
  rename_df() %>%
  filter(inf_model %in% c("IK")) %>%
  inner_join(sf, by = "id") %>%
  select(inf_model, mean, lower, upper, id, name_2)

write.csv(df, file = "tz_results_ah.csv")

hz <- read.csv("../data/tz_results.csv")

hz_df <- hz %>%
  rename(mean = prevalence_mean) %>%
  mutate(inf_model = "VI", 
         id = row_number(),
         lower = mean - prevalence_lower,
         upper = mean + prevalence_upper) %>%
  select(-counties, -X, -prevalence_lower, -prevalence_upper)

rbind(df, hz_df) %>%
  ggplot(aes(x = reorder(name_2, mean), y = mean, ymin = lower, ymax = upper,
             group = inf_model, color = inf_model)) +
  geom_pointrange(position = position_dodge(width = 0.6), alpha = 0.8) +
  labs(x = "District", y = "Posterior prevalence estimate", col = "Inferential model") +
  theme_minimal() +
  scale_color_manual(values = cbpalette) +
  coord_flip()
