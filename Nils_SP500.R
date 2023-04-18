
#Renditen für den Aktienindex S&P 500 zwischen 2001 und 2005

#Bsp: Lag2 Prozentuale Rendite für die letzten 2 Tage 
#Volume: Handelsvolumen (Anzahl der täglich gehandelten Aktien in Milliarden)
#Today: Prozentuale Rendite für heute
#Direction: Sagt ob heute hoch oder tief gegangen ist (nicht in Modell nehmen)

library(tidymodels)
library(tidyverse)
library(easystats)

sp500 <- read_csv("Smarket.csv")

sp500 <- sp500 %>% 
  rename(ID = ...1)

sum(is.na(sp500))

set.seed(42)
d_split <- initial_split(sp500)
d_train <- training(d_split)
d_test <- testing(d_split)


lm <- lm(Today ~ Lag1 + Year, data = d_train)
summary(lm)


recipe_knn <- recipe(Today ~ Lag1 + Lag2 + Lag3 + Lag4 + Lag5, data = d_train)
#Lag1 + Lag2 + Lag3 + Lag4 + Lag5

knn_modell <- nearest_neighbor(
  mode = "regression",
  neighbors = tune()
)

wflow_knn <- workflow() %>% 
  add_recipe(recipe_knn) %>% 
  add_model(knn_modell)

set.seed(42)
folds <- vfold_cv(d_train, v = 5, repeats = 2)

d_resample <- tune_grid(wflow_knn, resamples = folds, control = control_grid(save_workflow = T),
                        grid = grid_regular(neighbors(range = c(1,10))))


d_resample %>% collect_metrics()
autoplot(d_resample)

show_best(d_resample, metric = "rmse")
show_best(d_resample, metric = "rsq")

fitbest <- fit_best(d_resample, metric = "rsq") #mein Modell
fitbest

fit_last <- last_fit(fitbest, d_split)
fit_last %>% collect_metrics() #rmse 1.35; rsq 00.007

sp_zsm <- d_test %>% 
  mutate(pred = predict(fitbest, new_data = d_test)) #gleich Ergebnis

test_mit_vorhersage <- 
  augment(fitbest, new_data = d_test) #gleiches Ergebnis

test_mit_vorhersage <- test_mit_vorhersage %>% 
  mutate(Direction_pred = case_when(
    .pred > 0 ~ "Up",
    .pred < 0 ~ "Down"
  ))

test_mit_vorhersage %>% 
  count(Direction == Direction_pred) %>% 
  mutate(Anteil = n/sum(n))