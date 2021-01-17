library(tidymodels)
library(ranger)
library(baguette)

model_recipe <- recipe(mpg ~ .,
                       data = mtcars)

rf_mod <- rand_forest() %>% 
  set_engine("ranger",
             importance = "impurity",
             keep.inbag = TRUE
             ) %>% 
  set_mode("regression")

#create lm workflow
rf_wflow <- workflow() %>% 
  add_model(rf_mod) %>% 
  add_recipe(model_recipe)

rf_wflow

rf_fit <- rf_wflow %>% 
  fit(mtcars)

rf_fit %>% 
  predict(mtcars) %>% 
  bind_cols(mtcars)

rf_fit %>% 
  predict(mtcars, type = "conf_int")

rf_fit %>% 
  predict(mtcars[5,], type = "conf_int")

manual_fit <- ranger(mpg ~ ., mtcars)

manual_fit %>% 
  predict(mtcars) %>% str()

#bagged
bag_spec <- bag_tree(min_n = 10) %>%
  set_engine("rpart", 
             times = 5,
             control = control_bag(allow_parallel = T)) %>%
  set_mode("regression")

bag_wf <- workflow() %>%
  add_model(bag_spec) %>% 
  add_recipe(model_recipe)



bag_fit <- bag_wf %>% 
  fit(mtcars)

bag_fit %>% 
  predict(mtcars) %>% 
  bind_cols(mtcars)

bag_fit %>% 
  predict(mtcars, type = "conf_int")

rf_fit %>% 
  predict(mtcars[5,], type = "conf_int")

