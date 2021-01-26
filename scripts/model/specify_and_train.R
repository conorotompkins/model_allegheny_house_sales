
#specify lm model
lm_mod <- linear_reg() %>% 
  set_engine("lm")

#create lm workflow
lm_wflow <- workflow() %>% 
  add_model(lm_mod) %>% 
  add_recipe(model_recipe)

#specify rf model
cores <- parallel::detectCores()
cores

rf_mod <- rand_forest() %>% 
  set_engine("ranger",
             num.threads = cores,
             importance = "impurity",
             keep.inbag = TRUE) %>% 
  set_mode("regression")

#create lm workflow
rf_wflow <- workflow() %>% 
  add_model(rf_mod) %>% 
  add_recipe(model_recipe)

#specify bagged tree model
bag_spec <- bag_tree(min_n = 25) %>%
  set_engine("rpart", 
             times = 25,
             control = control_bag(allow_parallel = T)) %>%
  set_mode("regression")

bag_wf <- workflow() %>%
  add_model(bag_spec) %>% 
  add_recipe(model_recipe)

#resample
folds_train <- vfold_cv(train_data, v = 10)

keep_pred <- control_resamples(save_pred = TRUE)

#fit lm against resampled training data
lm_res <- lm_wflow %>%
  fit_resamples(resamples = folds_train,
                control = keep_pred)

#fit rf against resampled training data
rf_res <- rf_wflow %>%
  fit_resamples(resamples = folds_train,
                control = keep_pred)

#fit bag against resampled training data
bag_res <- bag_wf %>%
  fit_resamples(resamples = folds_train,
                control = keep_pred)

#compare predictions against training data across models
train_predictions_scatter <- collect_predictions(lm_res) %>% 
  mutate(model = "lm") %>% 
  bind_rows(collect_predictions(rf_res) %>% 
              mutate(model = "rf")) %>% 
  bind_rows(collect_predictions(bag_res) %>% 
              mutate(model = "bag")) %>% 
  ggplot(aes(log10(sale_price_adj), .pred)) +
  geom_density_2d_filled() +
  #coord_obs_pred() +
  geom_abline(color = "white", lty = 2) +
  coord_cartesian(xlim = c(4.5, 6), ylim = c(4.5, 6)) +
  facet_wrap(~model, ncol = 1)

train_predictions_scatter

train_predictions_scatter %>% 
  ggsave(filename = "output/train_predictions_scatter.png")

collect_metrics(lm_res)
collect_metrics(rf_res)
collect_metrics(bag_res)

#fit against entire training data set
lm_fit <- lm_wflow %>%
  fit(data = train_data)

lobstr::obj_size(lm_fit)

rf_fit <- rf_wflow %>%
  fit(data = train_data)

lobstr::obj_size(rf_fit)

bag_fit <- bag_wf %>% 
  fit(data = train_data)

obj_size(bag_fit)

small_data <- model_recipe %>% 
  prep() %>% 
  bake(test_data[1,])

bag_fit %>% 
  pull_workflow_fit() %>% 
  butcher(verbose = T) %>% 
  predict(small_data)

bag_fit %>% 
  predict(test_data[1,])

#save model objects
write_rds(lm_fit, "data/lm_model_fit.rds")

write_rds(rf_fit, "data/rf_model_fit.rds")

bag_fit %>% 
  # pull_workflow_fit() %>% 
  # butcher(verbose = T) %>% 
  write_rds("data/bag_model_fit.rds")