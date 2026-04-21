pacman::p_load(tidymodels, tidyverse, janitor, readxl, RSQLite, DBI, lubridate, 
               dplyr, stringr, readr, discrim, future)

# Indlæsning af datasæt
subscription <- read_delim("subscription_v2.csv", delim = ";")


behavior <- read.csv("behavior.csv")


cancellation <- read.csv("cancellation.csv")


# Kontrol
View(subscription)
View(behavior)
View(cancellation)

cancellation <- cancellation %>%
  mutate(
    expiration_date = as.Date(expiration_date)
  )

#Fix format
subscription <- subscription %>%
  mutate(
    subscription_cancel_date = as.Date(subscription_cancel_date, format = "%d-%m-%Y"),
    order_date = as.Date(order_date),
    birthdate = as.Date(birthdate,format = "%d-%m-%Y"),
    usr_created = as.Date(usr_created, format = "%d-%m-%Y"),
    first_campaign_day = as.Date(first_campaign_day, format = "%d-%m-%Y"),
    last_campaign_day = as.Date(last_campaign_day, format = "%d-%m-%Y")
  )


#finder dubletter i datasæt sub+can

subscription %>%
  count(pseudo_id) %>%
  filter(n > 1)

cancellation %>%
  count(pseudo_id) %>%
  filter(n > 1)

#Filtrer cancellation ved at fjerne dubletter

cancellation_ <- cancellation %>%
  arrange(pseudo_id, desc(expiration_date)) %>%
  distinct(pseudo_id, .keep_all = TRUE)

# Joining af datasæt 


join_datasæt <- subscription |>
  dplyr::left_join(cancellation, by = "pseudo_id")


join_datasæt <- join_datasæt |>
  filter(!is.na(koen))


join_datasæt <- join_datasæt |>
  mutate(
    type = replace_na(type, "aktiv"),
    reason = replace_na(reason, "aktivt abonnement")
  )

#opdeling af demografi (alder)

join_datasæt <- join_datasæt %>%
  mutate(
    birthdate = as.Date(birthdate, format = "%d-%m-%Y")
  )

join_datasæt <- join_datasæt %>%
  mutate(
    age = floor(as.numeric(interval(birthdate, ymd("2025-01-01")) / years(1)))
  )


# Rensning af data

#Omdøb observationer der har en forkert canceldate til aktive abonnementer
join_datasæt <- join_datasæt |>
  mutate(subscription_cancel_date = ifelse(
    subscription_cancel_date == "01-01-3000",
    "aktive abonnementer",
    as.character(subscription_cancel_date)
  ))


#Lav first_campaign_day til en dato variabel
join_datasæt <- join_datasæt |>
  mutate(first_campaign_day = as.Date(first_campaign_day, format = "%d-%m-%Y"))


# Lav expiration_date til en date
join_datasæt <- join_datasæt |>
  mutate(
    expiration_date = as.Date(expiration_date, format = "%Y-%m-%d")
  )

#fjern eller find anden måde at filtrere på

#Lav last_campaign_day til en dato variabel
join_datasæt <- join_datasæt |>
  mutate(
    last_campaign_day = as.Date(last_campaign_day, format = "%d-%m-%Y")
  )

#Lav variabel days_after_campaign
join_datasæt <- join_datasæt |>
  mutate(
    days_after_campaign = as.numeric(expiration_date - last_campaign_day)
  )



#fjerne N/A, undtagen dem der opstår pga aktivt abonnement
cols_allow_na <- c("type", "reason", "expiration_date", 
                   "days_after_campaign")

join_datasæt <- join_datasæt %>%
  filter(
    rowSums(is.na(select(., -all_of(cols_allow_na)))) == 0
  )

#Fjerne dubletter pseudo_id
join_datasæt <- join_datasæt[!duplicated(join_datasæt$pseudo_id), ]

#Fjerner enkelt fejl observation
join_datasæt <- join_datasæt[join_datasæt$pseudo_id != "add49e9f3efbc829a809336238069353", ]

#Tjek kolonner 
colSums(is.na(join_datasæt))


join_datasæt |> 
  count(str_remove(str_extract(order_trackertag, "utm_source=[^&]+"), "utm_source="), sort = TRUE)




# ----------------------------
# Nye variabler i behavior df
# ---------------------------

# Laver ny aggregeret df, hvor pseudo id'er samles efter hvor mange gange de optræder

behavior_agg <- behavior %>%
  group_by(pseudo_id) %>%
  summarise(
    aktivitet = n(),
    .groups = "drop"
  )



#Kig på behavior og laver struktur til page_url
# Artikeltype: laver en mellemversion af behavior med kategorier i page_url
behavior_type <- behavior %>%
  mutate(
    article_type = case_when(
      str_detect(page_url_clean, "politik") ~ "politik",
      str_detect(page_url_clean, "indland") ~ "indland",
      str_detect(page_url_clean, "udland") ~ "udland",
      str_detect(page_url_clean, "erhverv") ~ "erhverv",
      str_detect(page_url_clean, "sport") ~ "sport",
      str_detect(page_url_clean, "kultur") ~ "kultur",
      str_detect(page_url_clean, "debat") ~ "debat",
      str_detect(page_url_clean, "livsstil") ~ "livsstil",
      str_detect(page_url_clean, "international") ~ "international",
      TRUE ~ "andet"
    )
  )


# Artikeltype: tæller antal sidevisninger pr. pseudo_id i hver kategori
article_type_agg <- behavior_type %>%
  group_by(pseudo_id, article_type) %>%
  summarise(
    antal_sidevisninger = n(),
    .groups = "drop"
  ) %>%
  pivot_wider(
    names_from = article_type,
    values_from = antal_sidevisninger,
    values_fill = 0
  )


# ------------------------
# Aggrere behavior datasæt
# ------------------------


# Så laver vi en ny dataframe, hvor kampagne er opdelt i tre perioder

# Tilføjer kampagnestart til behavior
behavior_periode <- behavior %>%
  left_join(
    join_datasæt %>% select(pseudo_id, first_campaign_day), # Vi bruger first_campaign_day, da vi skal vide hvornår kampagnen starter
    by = "pseudo_id"
  ) %>%
  mutate(
    dt = as.Date(dt),                                      # vi gør dato til datoformat
    day_number = as.numeric(dt - first_campaign_day) + 1,  # # Finder hvilken dag observationen ligger på
    periode = case_when(                                   # opdeler i 3 perioder
      day_number >= 1  & day_number <= 10 ~ "aktivitet_start_10",
      day_number >= 11 & day_number <= 20 ~ "aktivitet_midt_10",
      day_number >= 21 & day_number <= 30 ~ "aktivitet_slut_10",
      TRUE ~ NA_character_
    )
  ) %>%
  filter(day_number >= 1 & day_number <= 30)               # kun de første 30 dage


# Vi tælle aktivitet for hver pseudo-id i hver periode
aktivitet_perioder <- behavior_periode %>%
  group_by(pseudo_id, periode) %>%
  summarise(
    aktivitet = n(),                                       # antal sidevisninger
    .groups = "drop"
  )

# Gør perioderne til kolonner
aktivitet_wide <- aktivitet_perioder %>%
  pivot_wider(
    names_from = periode,
    values_from = aktivitet,
    values_fill = 0                                        # Vi sørger for, at de steder hvor der er NA bliver til 0
  )


# Vi laver samlet aktivitet i de første 30 dage
behavior_agg <- aktivitet_wide %>%
  mutate(
    aktivitet_total_30 = aktivitet_start_10 + aktivitet_midt_10 + aktivitet_slut_10
  )


# Scroll Depth: Herefter kan vi lave variablerne for gennemsnitlig scroll depth, 
scroll_agg <- behavior %>%
  mutate(
    scroll_depth = as.numeric(scroll_depth)
  ) %>%
  group_by(pseudo_id) %>%
  summarise(
    avg_scroll_depth = mean(scroll_depth, na.rm = TRUE),
    .groups = "drop"
  )



# Forskellige artikler: hvor mange forskellige artiker de har læst

artikel_agg <- behavior %>%
  mutate(
    article_id = str_extract(page_url_clean, "ECE\\d{8}")
  ) %>%
  group_by(pseudo_id) %>%
  summarise(
    forskellige_artikler = n_distinct(article_id, na.rm = TRUE),
    .groups = "drop"
  )



# Låste Artikler: Hvor stor en andel af låste artikler de læser
restricted_agg <- behavior %>%
  mutate(
    page_restricted = if_else(page_restricted == "yes", 1, 0)
  ) %>%
  group_by(pseudo_id) %>%
  summarise(
    restricted_share = mean(page_restricted, na.rm = TRUE),
    .groups = "drop"
  )


# Nu kan vi samle alle nye df i det samelde behavior_agg
behavior_agg <- behavior_agg %>%
  left_join(scroll_agg, by = "pseudo_id") %>%
  left_join(artikel_agg, by = "pseudo_id") %>%
  left_join(restricted_agg, by = "pseudo_id") %>%
  left_join(article_type_agg, by = "pseudo_id")



view(behavior_agg)
View(join_datasæt)


#Tjek NA
colSums(is.na(join_datasæt))
colSums(is.na(behavior_agg))

#Join behavior_agg og join_datasæt
final_df <- join_datasæt %>%
  left_join(behavior_agg, by = "pseudo_id")

#Se final dataframe
view(final_df)

#Tjek NA
colSums(is.na(final_df))

#Opret variabel abonnement_længde
final_df <- final_df %>%
  mutate(
    abonnement_længde = as.numeric(expiration_date - first_campaign_day)
  )

#Lav churn variabel
final_df <- final_df %>%
  mutate(
    churn_binary = case_when(
      is.na(abonnement_længde) ~ 1,
      abonnement_længde <= 60 ~ 0,
      abonnement_længde > 60 ~ 1
    )
  )

#Fjern obervationer der har under 60 dage i abonnements længde
final_df <- final_df %>%
  filter(abonnement_længde >= 60 | is.na(abonnement_længde))

#Fjern overskydende NA fra behavior
final_df <- final_df %>%
  filter(!is.na(aktivitet_total_30))

#Fjerner en observation med account days under 60
final_df <- final_df %>%
  filter(account_active_days != 6)

#Fjerne ikke brugbare vbariabler
final_df <- final_df %>%
  select(-expiration_date, -days_after_campaign, -abonnement_længde)

final_df %>%
  mutate(source = str_extract(order_trackertag, "(?<=utm_source=)[^&]+")) %>%
  count(source, sort = TRUE)


final_df <- final_df %>%
  mutate(
    order_trackertag = case_when(
      str_detect(order_trackertag, "mail") ~ "mail",
      str_detect(order_trackertag, "jp.dk") ~ "jp.dk",
      str_detect(order_trackertag, "adwords") ~ "adwords",
      TRUE ~ "andet"
    )
  )

# Bjanes workflow med vores data ------------------------------------------
model_df <- final_df |> 
  select(
    -pseudo_id,
    -subscription_cancel_date,
    -order_date,
    -birthdate,
    -usr_created,
    -order_trackertag,
    -first_campaign_day,
    -last_campaign_day,
    -type,
    -reason,
    -permission_given_today,
    -aktivitet_total_30
  ) |> 
  mutate(
    churn_binary = factor(churn_binary, levels = c(1, 0), labels = c("Yes", "No")),
    koen = as.factor(koen),
    permission_given_order = as.factor(permission_given_order)
  )


set.seed(7)
churn_split <- initial_split(model_df, prop = 0.8, strata = churn_binary)

churn_train <- training(churn_split)
churn_test  <- testing(churn_split)

churn_folds <- vfold_cv(churn_train, strata = churn_binary)

jp_recipe <- 
  recipe(formula = churn_binary ~ ., data = churn_train) |> 
  step_normalize(all_numeric_predictors()) |> 
  step_novel(all_nominal_predictors()) |> 
  step_dummy(all_nominal_predictors(), one_hot = TRUE) |> 
  step_zv(all_predictors())

# Modeller
#### Decision tree
decision_tree_rpart_spec <- 
  decision_tree(tree_depth = tune(), min_n = tune(), cost_complexity = tune()) |> 
  set_engine("rpart") |> 
  set_mode("classification")

# Regulariseret lineær regression
logistic_reg_glmnet_spec <- 
  logistic_reg(penalty = tune(), mixture = tune()) |> 
  set_engine("glmnet") |> 
  set_mode("classification")

# Naive Bayes
naive_bayes_naivebayes_spec <- 
  naive_Bayes(smoothness = tune(), Laplace = tune()) |> 
  set_engine("naivebayes") |> 
  set_mode("classification")

# k-nearest neighbor (knn) 
nearest_neighbor_kknn_spec <-
  nearest_neighbor(neighbors = tune(),
                   weight_func = tune(), dist_power = tune()) |> 
  set_engine("kknn") |> 
  set_mode("classification")

# Random forrest
rand_forest_ranger_spec <-
  rand_forest(mtry = tune(), min_n = tune()) |> 
  set_engine("ranger") |> 
  set_mode("classification")

# SVM lineær 
svm_linear_kernlab_spec <- 
  svm_linear(cost = tune(), margin = tune()) |> 
  set_engine("kernlab") |> 
  set_mode("classification")

# SVM RBF
svm_rbf_kernlab_spec <- 
  svm_rbf(cost = tune(), rbf_sigma = tune(), margin = tune()) |> 
  set_engine("kernlab") |> 
  set_mode("classification")

# XGBoost
boost_tree_xgboost_spec <-
  boost_tree(trees = tune(), mtry = tune(), learn_rate = tune()) |> 
  set_engine("xgboost") |> 
  set_mode("classification")


# Workflow
jp_workflow_set <- 
  workflow_set(
    preproc = list(rec = jp_recipe),
    models = list(decision_tree = decision_tree_rpart_spec,
                  logistic_reg = logistic_reg_glmnet_spec, 
                  naive_bayes = naive_bayes_naivebayes_spec,
                  knn = nearest_neighbor_kknn_spec, 
                  rand_forest = rand_forest_ranger_spec,
                  svm_linear = svm_linear_kernlab_spec, 
                  svm_rbf = svm_rbf_kernlab_spec,
                  xgboost = boost_tree_xgboost_spec
    )
  )
jp_workflow_set 


# Tuning
grid_ctrl <- control_grid(
  verbose = TRUE,
  save_pred = TRUE,
  parallel_over = "everything",
  save_workflow = TRUE
)


# Definer hvilke metrics vi vil evaluere modellerne på
jp_metrics <- metric_set(accuracy, roc_auc, f_meas, sens, yardstick::spec)

# Aktiver parallel computing (hurtigere modeltræning)
plan(multisession)

# Start timer (så vi kan måle hvor lang tid det tager)
strt.time <- Sys.time()

# Kør grid search på alle modeller i workflow_set
grid_results <- jp_workflow_set %>%
  workflow_map(
    verbose = TRUE,          # viser hvad der sker undervejs
    seed = 7,             # gør resultater reproducerbare
    resamples = churn_folds, # cross-validation folds
    grid = 7,                # antal parameter-kombinationer
    control = grid_ctrl,     # styring af tuning (gem predictions osv.)
    metrics = jp_metrics     # hvilke metrics der beregnes
  )

# Hvor lang tid tog det?
Sys.time() - strt.time

# Slå parallel computing fra igen
plan(sequential)

# Rangér modellerne og se hvilke der performer bedst
grid_results %>% 
  rank_results(select_best = TRUE) %>% 
  select(wflow_id, .metric, mean) %>% 
  pivot_wider(names_from = .metric, values_from = mean) %>% 
  arrange(-f_meas)   # sorter efter F1-score

# Plot performance for bedste modeller
autoplot(grid_results, select_best = TRUE)

# Udvælg bedste hyperparameters for en specifik model
best_results <- grid_results |> 
  extract_workflow_set_result("rec_xgboost") |> 
  select_best(metric = "f_meas")

best_results

# Tag workflowet og indsæt de bedste parametre
final_wf <- grid_results |> 
  extract_workflow("rec_xgboost") |> 
  finalize_workflow(best_results)

# Træn modellen på træningsdata og test den på testdata
churn_last_fit <- final_wf |> 
  last_fit(churn_split, metrics = jp_metrics)

churn_last_fit

# Se model performance på testdata
collect_metrics(churn_last_fit)

# Hent predictioner fra modellen
test_preds <- churn_last_fit |> 
  collect_predictions()

test_preds

# Lav confusion matrix (hvordan modellen klassificerer)
churn_last_fit |>
  collect_predictions() |>
  conf_mat(estimate = .pred_class, truth = churn_binary)


test_preds <- test_preds |> 
  mutate(
    fejltype = case_when(
      churn_binary == "Yes" & .pred_class == "Yes" ~ "True Positive",
      churn_binary == "No"  & .pred_class == "No"  ~ "True Negative",
      churn_binary == "No"  & .pred_class == "Yes" ~ "False Positive",
      churn_binary == "Yes" & .pred_class == "No"  ~ "False Negative"
    )
  )

test_preds |> 
  count(fejltype)

# Træn endelig model på hele datasættet (klar til brug)
final_model <- fit(final_wf, model_df)

final_model
