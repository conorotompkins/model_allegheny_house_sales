library(tictoc)

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
future::plan("multisession")

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
tic()
lm_res <- lm_wflow %>%
  fit_resamples(resamples = folds_train,
                control = keep_pred)
toc()

lm_metrics <- lm_res %>% 
  select(.predictions) %>% 
  unnest(.predictions) %>% 
  mutate(.pred_dollar = 10^.pred) %>% 
  metrics(truth = sale_price_adj, estimate = .pred_dollar)

lm_metrics

#fit rf against resampled training data
#11 minutes to run
tic()
rf_res <- rf_wflow %>%
  fit_resamples(resamples = folds_train,
                control = keep_pred)
toc()

rf_metrics <- rf_res %>% 
  select(.predictions) %>% 
  unnest(.predictions) %>% 
  mutate(.pred_dollar = 10^.pred) %>% 
  metrics(truth = sale_price_adj, estimate = .pred_dollar)

rf_metrics

#fit bag against resampled training data
#28 minutes to run
tic()
bag_res <- bag_wf %>%
  fit_resamples(resamples = folds_train,
                control = keep_pred)
toc()

bag_metrics <- bag_res %>% 
  select(.predictions) %>% 
  unnest(.predictions) %>% 
  mutate(.pred_dollar = 10^.pred) %>% 
  metrics(truth = sale_price_adj, estimate = .pred_dollar)

bag_metrics

#compare predictions against training data across models

#train metrics
train_metrics <- tibble(model_name = c("lm", "random forest", "bagged tree"),
                       dataset = "train",
                       model_metrics = list(lm_metrics, rf_metrics, bag_metrics)) %>%
  unnest(model_metrics)

train_metrics %>% 
  write_csv("output/train_metrics.csv")

train_metric_graph <- train_metrics %>% 
  select(model_name, .metric, .estimate) %>% 
  pivot_wider(names_from = .metric, values_from = .estimate) %>% 
  ggplot(aes(rmse, rsq)) +
  geom_label(aes(label = model_name))

ggsave(train_metric_graph, filename = "output/train_metric_graph.png")

train_predictions_scatter <- collect_predictions(lm_res) %>% 
  mutate(model = "lm") %>% 
  bind_rows(collect_predictions(rf_res) %>% 
              mutate(model = "random forest")) %>% 
  bind_rows(collect_predictions(bag_res) %>% 
              mutate(model = "bagged tree")) %>% 
  ggplot(aes(log10(sale_price_adj), .pred)) +
  geom_density_2d_filled() +
  geom_abline(color = "white", lty = 2) +
  coord_cartesian(xlim = c(4.5, 6), ylim = c(4.5, 6)) +
  facet_wrap(~model, ncol = 1)

train_predictions_scatter %>% 
  ggsave(filename = "output/train_predictions_scatter.png",
         width = 12,
         height = 12)

bagged_tree_train_scatter <- collect_predictions(bag_res) %>% 
  mutate(model = "bagged tree") %>% 
  ggplot(aes(log10(sale_price_adj), .pred)) +
  geom_density_2d_filled() +
  geom_abline(color = "white", lty = 2) +
  coord_cartesian(xlim = c(4.5, 6), ylim = c(4.5, 6)) +
  facet_wrap(~model, ncol = 1)

bagged_tree_train_scatter %>% 
  ggsave(filename = "output/bagged_tree_train_scatter.png",
         width = 12,
         height = 12)

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

bag_fit %>% 
  predict(test_data[1,])

#save model objects
write_rds(lm_fit, "data/lm_model_fit.rds")

write_rds(rf_fit, "data/rf_model_fit.rds")

bag_fit %>% 
  write_rds("data/bag_model_fit.rds")
