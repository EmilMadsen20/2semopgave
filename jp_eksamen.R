if (!require("pacman")) install.packages("pacman")
pacman::p_load(tidyverse, tidymodels, lubridate, dplyr, stringr, readr, readxl, 
               themis, table1, ggpubr, broom, ggfortify, GGally,
               PerformanceAnalytics, car, caret, skimr, discrim, glmnet,
               kknn, naivebayes, kernlab, xgboost, gridExtra, rpart, future,
               ranger, rmarkdown, gt)


# Indlæsning af datasæt
subscription <- read_delim("subscription_v2.csv", delim = ";")
behavior <- read.csv("behavior.csv")
cancellation <- read.csv("cancellation.csv")

# Kontrol
# View(subscription)
# View(behavior)
# View(cancellation)

#Fix dato formater
subscription <- subscription %>%
  mutate(
    subscription_cancel_date = as.Date(subscription_cancel_date, format = "%d-%m-%Y"),
    order_date = as.Date(order_date),
    birthdate = as.Date(birthdate, format = "%d-%m-%Y"),
    usr_created = as.Date(usr_created, format = "%d-%m-%Y"),
    first_campaign_day = as.Date(first_campaign_day, format = "%d-%m-%Y"),
    last_campaign_day = as.Date(last_campaign_day, format = "%d-%m-%Y")
  )

cancellation <- cancellation %>%
  mutate(
    expiration_date = as.Date(expiration_date)
  )

#finder dubletter i datasæt sub+can
subscription %>%
  count(pseudo_id) %>%
  filter(n > 1)

cancellation %>%
  count(pseudo_id) %>%
  filter(n > 1)

#Filtrer cancellation ved at fjerne dubletter
cancellation <- cancellation %>%
  arrange(pseudo_id, desc(expiration_date)) %>%
  distinct(pseudo_id, .keep_all = TRUE)

# Joining af datasæt 
join_datasæt <- subscription |>
  dplyr::left_join(cancellation, by = "pseudo_id")

join_datasæt <- join_datasæt |>
  filter(!is.na(koen)) |>
  mutate(
    type = replace_na(type, "aktiv"),
    reason = replace_na(reason, "aktivt abonnement"),
    # Alder
    birthdate = as.Date(birthdate, format = "%d-%m-%Y"),
    age = floor(as.numeric(interval(birthdate, ymd("2025-01-01")) / years(1))),
    # Omdøb cancel date til aktive abonnementerr
    subscription_cancel_date = ifelse(
      subscription_cancel_date == as.Date("3000-01-01"),
      "aktive abonnementer",
      as.character(subscription_cancel_date)
    )
  )

#Lav variabel days_after_campaign
join_datasæt <- join_datasæt |>
  mutate(
    days_after_campaign = as.numeric(expiration_date - last_campaign_day))

#fjerne N/A, undtagen dem der opstår pga aktivt abonnement
cols_allow_na <- c("type", "reason", "expiration_date", 
                   "days_after_campaign")

join_datasæt <- join_datasæt %>%
  filter(
    rowSums(is.na(select(., -all_of(cols_allow_na)))) == 0
  )

#Fjerne dubletter pseudo_id og enkelt fejl observation
join_datasæt <- join_datasæt %>%
  distinct(pseudo_id, .keep_all = TRUE) %>%
  filter(pseudo_id != "add49e9f3efbc829a809336238069353")

#Tjek kolonner 
colSums(is.na(join_datasæt))

# Tæller hvor mange gange hver utm_source (trafikkilde) optræder i datasættet
join_datasæt |> 
  count(str_remove(str_extract(order_trackertag, "utm_source=[^&]+"), "utm_source="), sort = TRUE)


# Nye variabler i behavior df ---------------------------------------------
#Kig på behavior og laver struktur til page_url
# Artikeltype: laver en mellemversion af behavior med kategorier i page_url
behavior_type <- behavior %>%
  mutate(
    article_type = case_when(
      str_detect(page_url_clean, "politik") ~ "politik",
      str_detect(page_url_clean, "indland") ~ "indland",
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

# Aggrere behavior datasæt
# Så laver vi en ny dataframe, hvor kampagne er opdelt i tre perioder
behavior_periode <- behavior %>%
  left_join(
    join_datasæt %>% select(pseudo_id, first_campaign_day),
    by = "pseudo_id"
  ) %>%
  mutate(
    dt = as.Date(dt),
    day_number = as.numeric(dt - first_campaign_day) + 1,
    periode = case_when(
      day_number >= 1  & day_number <= 10 ~ "aktivitet_start_10",
      day_number >= 11 & day_number <= 20 ~ "aktivitet_midt_10",
      day_number >= 21 & day_number <= 30 ~ "aktivitet_slut_10",
      TRUE ~ NA_character_
    )
  ) %>%
  filter(day_number >= 1 & day_number <= 30)

# Vi tælle aktivitet for hver pseudo-id i hver periode
aktivitet_perioder <- behavior_periode %>%
  group_by(pseudo_id, periode) %>%
  summarise(
    aktivitet = n(),
    .groups = "drop"
  )

aktivitet_wide <- aktivitet_perioder %>%
  pivot_wider(
    names_from = periode,
    values_from = aktivitet,
    values_fill = 0
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

# view(behavior_agg)
# View(join_datasæt)

#Tjek NA
colSums(is.na(join_datasæt))
colSums(is.na(behavior_agg))

#Join behavior_agg og join_datasæt
final_df <- join_datasæt %>%
  left_join(behavior_agg, by = "pseudo_id")

# Se final dataframe
# view(final_df)

#Tjek NA
colSums(is.na(final_df))

# Filtrerer datasættet, så kun rækker med gyldig aktivitet beholdes,
# og fjerner observationer med account_active_days lig 6.
final_df <- final_df %>%
  filter(
    !is.na(aktivitet_total_30),
    account_active_days != 6
  )

final_df %>%
  mutate(source = str_extract(order_trackertag, "(?<=utm_source=)[^&]+")) %>%
  count(source, sort = TRUE)

final_df <- final_df %>%
  mutate(
    order_trackertag = case_when(
      str_detect(order_trackertag, "mail") ~ "mail",
      str_detect(order_trackertag, "jp.dk|jp") ~ "jp.dk",
      str_detect(order_trackertag, "adwords") ~ "adwords",
      TRUE ~ "andet"
    )
  )


# Lav churn variabel
data_model1 <- final_df |>
  mutate(
    # Opretter variabel der viser om kunden fortsætter efter kampagnen (0) eller churner (1)
    churn_ved_kampagne_udløb = case_when(
      is.na(expiration_date) ~ 0,               # Ingen slutdato → kunden er stadig aktiv → fortsætter
      expiration_date > last_campaign_day ~ 0,  # Slutter efter kampagnen → fortsætter
      expiration_date <= last_campaign_day ~ 1  # Slutter før eller på kampagnedag → churn
    )
  )


data_model2 <- data_model1 %>%
  filter(churn_ved_kampagne_udløb == 0) %>%
  mutate(
    days_after_campaign = as.numeric(expiration_date - last_campaign_day),
    churn_1måned_efter_forlængelse = case_when(
      is.na(expiration_date) ~ 0,            # Ingen slutdato → stadig aktiv → ikke early churn
      days_after_campaign <= 31 ~ 1,           # Stopper inden for 31 dage → early churn
      TRUE ~ 0                               # Ellers → bliver længere → ikke early churn
    )
  )


###### EVT. Gem til Power BI???????????
# view(data_model1)
# view(data_model2)


# Tidymodels workflow -----------------------------------------------------
model1_df <- data_model1 |> 
  select(
    -pseudo_id,
    -subscription_cancel_date,
    -order_date,
    -birthdate,
    -usr_created,
    -first_campaign_day,
    -last_campaign_day,
    -expiration_date,
    -type,
    -reason,
    -permission_given_today,
    -aktivitet_total_30,
    -days_after_campaign
  ) |> 
  mutate(
    churn_ved_kampagne_udløb = factor(
      churn_ved_kampagne_udløb,
      levels = c(1, 0),
      labels = c("Yes", "No")
    ),
    koen = as.factor(koen),
    permission_given_order = as.factor(permission_given_order),
    order_trackertag = as.factor(order_trackertag)
  )

colSums(is.na(model1_df))

# Opdeling i train og test samt folds
set.seed(7)
churn_split <- initial_split(model1_df, prop = 0.8, strata = churn_ved_kampagne_udløb)

churn_train <- training(churn_split)
churn_test  <- testing(churn_split)

churn_folds <- vfold_cv(churn_train, strata = churn_ved_kampagne_udløb)

# Laver en recipe
jp_recipe_1 <- 
  recipe(formula = churn_ved_kampagne_udløb ~ ., data = churn_train) |> 
  step_normalize(all_numeric_predictors()) |> 
  step_novel(all_nominal_predictors()) |> 
  step_dummy(all_nominal_predictors(), one_hot = TRUE) |> 
  step_zv(all_predictors())

# Decision tree
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
    preproc = list(rec = jp_recipe_1),
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

# Kør grid search på alle modeller i workflow_set
grid_results_1 <- jp_workflow_set %>%
  workflow_map(
    verbose = TRUE,                # TRUE viser hvad der sker undervejs, FALSE viser ingenting
    seed = 7,                      # Seed, som gør resultater reproducerbare
    resamples = churn_folds,       # cross-validation folds
    grid = 7,                      # antal parameter-kombinationer
    control = grid_ctrl,           # styring af tuning (gem predictions osv.)
    metrics = jp_metrics           # hvilke metrics der beregnes
  )

# RDS save
# ----------------------------
# saveRDS(grid_results, "grid_results.rds")
# grid_results <- readRDS("grid_results.rds")

# Slå parallel computing fra igen
plan(sequential)

# Rangér modellerne og se hvilke der performer bedst
grid_results_1 %>% 
  rank_results(select_best = TRUE) %>% 
  select(wflow_id, .metric, mean) %>% 
  pivot_wider(names_from = .metric, values_from = mean) %>% 
  arrange(-f_meas)

# Plot performance for bedste modeller
autoplot(grid_results_1, select_best = TRUE)

# Udvælg bedste hyperparameters for en specifik model
best_results_1 <- grid_results_1 |> 
  extract_workflow_set_result("rec_xgboost") |> 
  select_best(metric = "f_meas")
best_results_1

# Tag workflowet og indsæt de bedste parametre
final_wf_1 <- grid_results_1 |> 
  extract_workflow("rec_xgboost") |> 
  finalize_workflow(best_results_1)

# Træn modellen på træningsdata og test den på testdata
churn_last_fit_1 <- final_wf_1 |> 
  last_fit(churn_split, metrics = jp_metrics)

# Se model performance på testdata
collect_metrics(churn_last_fit_1)

# Hent predictioner fra modellen
test_preds_1 <- churn_last_fit_1 |> 
  collect_predictions()

# Lav confusion matrix (hvordan modellen klassificerer)
churn_last_fit_1 |>
  collect_predictions() |>
  conf_mat(estimate = .pred_class, truth = churn_ved_kampagne_udløb)

test_preds_1 <- test_preds_1 |> 
  mutate(
    fejltype = case_when(
      churn_ved_kampagne_udløb == "Yes" & .pred_class == "Yes" ~ "True Positive",
      churn_ved_kampagne_udløb == "No"  & .pred_class == "No"  ~ "True Negative",
      churn_ved_kampagne_udløb == "No"  & .pred_class == "Yes" ~ "False Positive",
      churn_ved_kampagne_udløb == "Yes" & .pred_class == "No"  ~ "False Negative"
    )
  )

test_preds_1 |> 
  count(fejltype)

final_model_1 <- fit(final_wf_1, model1_df)

final_model_1

# FEATURE IMPORTANCE (XGBOOST)
# Hent fitted xgboost model
xgb_fit_1 <- extract_fit_parsnip(final_model_1)$fit

# Vis vigtigste variabler
importance_tbl_1 <- xgb.importance(model = xgb_fit_1)

importance_tbl_1

# (valgfrit) plot
xgb.plot.importance(importance_tbl_1, top_n = 15)


# Klyngeanalyse 1 ---------------------------------------------------------
# Udvælger relevante variabler til klyngeanalyse
# Fokust er på adfærd og engagement
cluster_input <- data_model1 %>%
  select(
    pseudo_id,
    aktivitet_start_10,   # aktivitet i starten af perioden
    aktivitet_midt_10,    # aktivitet i midten
    aktivitet_slut_10,    # aktivitet i slutningen
    avg_scroll_depth,     # hvor langt brugeren scroller (engagement)
    forskellige_artikler, # antal forskellige artikler læst
    restricted_share,     # andel af låste artikler
    politik, erhverv, sport, kultur, andet, debat, indland, international, livsstil,  # præferencer for indholdstyper
    aktivitet_total_30,
    age,
    koen,
    churn_ved_kampagne_udløb
  ) %>%
  drop_na()  # fjerner observationer med manglende værdier


# Udvælger kun de variabler der skal bruges til selve klyngeanalysen
cluster_df <- cluster_input %>%
  select(
    aktivitet_start_10,   # aktivitet i starten af perioden
    aktivitet_midt_10,    # aktivitet i midten
    aktivitet_slut_10,    # aktivitet i slutningen
    avg_scroll_depth,     # hvor langt brugeren scroller (engagement)
    forskellige_artikler, # antal forskellige artikler læst
    restricted_share,     # andel af låste artikler
    politik, erhverv, sport, kultur, andet, debat, indland, international, livsstil  # præferencer for indholdstyper
  )


# Skalerer data fx scroll er 0.8 og aktivitet er 100, så vil aktivitets variablen fylde mere
cluster_scaled <- scale(cluster_df)


# Kører k-means clustering
# centers = 4 betyder vi opdeler i 4 klynger
# nstart = 25 betyder at modellen prøver flere forskellige startpunkter og vælger den bedste løsning
set.seed(7)
kmeans_model <- kmeans(cluster_scaled, centers = 4, nstart = 25)


# Valg af antal klynger (Elbow method)

# Beregner variation inden for klynger (WSS) for k = 1 til 10
wss <- map_dbl(1:10, function(k){
  kmeans(cluster_scaled, centers = k, nstart = 20)$tot.withinss
})

# Plotter WSS for at finde "knæk" (optimalt antal klynger)
plot(1:10, wss, type = "b",
     xlab = "Antal klynger",
     ylab = "Within-cluster sum of squares")


# Tilføjer klynger til datasæt
# Hver kunde tildeles en klynge
cluster_result <- cluster_input %>%
  mutate(cluster = factor(kmeans_model$cluster))


# Profilering af klynger

# Beregner gennemsnit for hver klynge så man kan se forskele mellem kundetyper
cluster_result %>%
  group_by(cluster) %>%
  summarise(
    n = n(),
    aktivitet = mean(aktivitet_total_30),
    scroll = mean(avg_scroll_depth),
    alder = mean(age),
    churn_rate = mean(churn_ved_kampagne_udløb),
    .groups = "drop"
  )


# Churn fordelt på klynger

# Tæller hvor mange der churner/ikke churner i hver klynge
cluster_result %>%
  count(cluster, churn_ved_kampagne_udløb) %>%
  group_by(cluster) %>%
  mutate(prop = n / sum(n))  # beregner andel

# Gem som csv fil, så den kan bruges i Power BI
write.csv(cluster_result, "cluster_result.csv", row.names = FALSE)


# Tabel med klynger
# 1. Lav en profil-tabel med gennemsnit for hver klynge
cluster_profile <- cluster_result %>%
  group_by(cluster) %>%
  summarise(
    n = n(),
    aktivitet_start_10 = mean(aktivitet_start_10),
    aktivitet_midt_10 = mean(aktivitet_midt_10),
    aktivitet_slut_10 = mean(aktivitet_slut_10),
    restricted = mean(restricted_share),
    alder = mean(age),
    avg_scroll_depth = mean(avg_scroll_depth),
    churn_rate = mean(churn_ved_kampagne_udløb),
    .groups = "drop"
  )

view(cluster_result)


# Tidymodels workflow til early churn. Model 2 ---------------------------------
# 1. DATA + TARGET
model2_df <- data_model2 %>%
  select(
    -pseudo_id,
    -subscription_cancel_date,
    -order_date,
    -birthdate,
    -usr_created,
    -first_campaign_day,
    -last_campaign_day,
    -expiration_date,
    -days_after_campaign,
    -type,
    -reason,
    -permission_given_today,
    -churn_ved_kampagne_udløb
  ) %>%
  mutate(
    churn_1måned_efter_forlængelse = factor(
      churn_1måned_efter_forlængelse,
      levels = c(1, 0),
      labels = c("Yes", "No")
    ),
    koen = as.factor(koen),
    permission_given_order = as.factor(permission_given_order),
    order_trackertag = as.factor(order_trackertag)
  )


# Tjekker fordeling af churn/ ikke churn
table(model2_df$churn_1måned_efter_forlængelse)
prop.table(table(model2_df$churn_1måned_efter_forlængelse))

# 2. SPLIT + CV
set.seed(7)
churn2_split <- initial_split(model2_df, prop = 0.8, strata = churn_1måned_efter_forlængelse)
churn2_train <- training(churn2_split)
churn2_test  <- testing(churn2_split)
churn2_folds <- vfold_cv(churn2_train, v = 5, strata = churn_1måned_efter_forlængelse)

# 3. RECIPE
jp_recipe2 <- 
  recipe(churn_1måned_efter_forlængelse ~ ., data = churn2_train) |> 
  step_normalize(all_numeric_predictors()) |> 
  step_novel(all_nominal_predictors()) |> 
  step_dummy(all_nominal_predictors(), one_hot = TRUE) |> 
  step_zv(all_predictors())

# 4. UPSAMPLING RECIPE
jp_recipe2_upsample <- 
  recipe(churn_1måned_efter_forlængelse ~ ., data = churn2_train) |> 
  step_normalize(all_numeric_predictors()) |> 
  step_novel(all_nominal_predictors()) |> 
  step_dummy(all_nominal_predictors(), one_hot = TRUE) |> 
  step_zv(all_predictors()) |>
  step_upsample(churn_1måned_efter_forlængelse, over_ratio = 1)

# 4. MODELLER
# Decision tree
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

# 5. WORKFLOW SET
workflow_set2 <- 
  workflow_set(
    preproc = list(rec = jp_recipe2),
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
workflow_set2


# 6. WORKFLOW SET: UPSAMPLING
workflow_set2_upsample <- 
  workflow_set(
    preproc = list(rec = jp_recipe2_upsample),
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


# Tuning
grid_ctrl <- control_grid(
  verbose = FALSE,
  save_pred = TRUE,
  parallel_over = "everything",
  save_workflow = TRUE
)

metrics <- metric_set(accuracy, roc_auc, f_meas, sens, yardstick::spec)

# Modeller for Baseline
grid_results2 <- workflow_set2 %>%
  workflow_map(
    seed = 7,
    resamples = churn2_folds,
    grid = 7,
    control = grid_ctrl,
    metrics = metrics
  )


# Modeller for upsampling
grid_results2_upsample <- workflow_set2_upsample %>%
  workflow_map(
    seed = 7,
    resamples = churn2_folds,
    grid = 7,
    control = grid_ctrl,
    metrics = metrics
  )


# Sammenligning af baseline modellerne
baseline_rank <- grid_results2 %>% 
  rank_results(select_best = TRUE) %>% 
  select(wflow_id, .metric, mean) %>% 
  pivot_wider(names_from = .metric, values_from = mean) %>% 
  arrange(-roc_auc)

baseline_rank


# Sammenligning af upsampling modellerne
upsample_rank <- grid_results2_upsample %>% 
  rank_results(select_best = TRUE) %>% 
  select(wflow_id, .metric, mean) %>% 
  pivot_wider(names_from = .metric, values_from = mean) %>% 
  arrange(-roc_auc)

upsample_rank



# Samlet sammenligning: Baseline vs upsampling
model2_sammenligning <- bind_rows(
  baseline_rank %>% mutate(version = "Baseline"),
  upsample_rank %>% mutate(version = "Upsampling")
) %>%
  arrange(-roc_auc)

model2_sammenligning


# Plot performance for bedste modeller for baseline
autoplot(grid_results2, select_best = TRUE)

# Plot performance for bedste modeller for upsampling
autoplot(grid_results2_upsample, select_best = TRUE)


# Baseline - Udvælg bedste hyperparameters for en specifik model
best_results2 <- grid_results2 |> 
  extract_workflow_set_result("rec_xgboost") |> 
  select_best(metric = "f_meas")

# Baseline - Tag workflowet og indsæt de bedste parametre
final_wf2 <- grid_results2 |> 
  extract_workflow("rec_xgboost") |> 
  finalize_workflow(best_results2)


# Upsampling - Udvælg bedste hyperparameters for en specifik model
best_results2_upsampling <- grid_results2_upsample |> 
  extract_workflow_set_result("rec_xgboost") |> 
  select_best(metric = "f_meas")

# Upsampling - Tag workflowet og indsæt de bedste parametre
final_wf2_upsampling <- grid_results2_upsample |> 
  extract_workflow("rec_xgboost") |> 
  finalize_workflow(best_results2_upsampling)

# Baseline - Test performence
churn2_last_fit <- final_wf2 |> 
  last_fit(churn2_split, metrics = metrics)

collect_metrics(churn2_last_fit)

# Upsampling - Test final_wf2_upsampling
churn2_last_fit_upsampling <- final_wf2_upsampling |> 
  last_fit(churn2_split, metrics = metrics)

collect_metrics(churn2_last_fit_upsampling)

# Baseline - CONFUSION MATRIX
churn2_last_fit |>
  collect_predictions() |>
  conf_mat(truth = churn_1måned_efter_forlængelse, estimate = .pred_class)

test_preds_2 <- churn2_last_fit |> 
  collect_predictions() |> 
  mutate(
    fejltype = case_when(
      churn_1måned_efter_forlængelse == "Yes" & .pred_class == "Yes" ~ "True Positive",
      churn_1måned_efter_forlængelse == "No"  & .pred_class == "No"  ~ "True Negative",
      churn_1måned_efter_forlængelse == "No"  & .pred_class == "Yes" ~ "False Positive",
      churn_1måned_efter_forlængelse == "Yes" & .pred_class == "No"  ~ "False Negative"
    )
  )

test_preds_2 |> 
  count(fejltype)


# Upsampling - CONFUSION MATRIX
churn2_last_fit_upsampling |>
  collect_predictions() |>
  conf_mat(truth = churn_1måned_efter_forlængelse, estimate = .pred_class)

test_preds_2_upsampling <- churn2_last_fit_upsampling |> 
  collect_predictions() |> 
  mutate(
    fejltype = case_when(
      churn_1måned_efter_forlængelse == "Yes" & .pred_class == "Yes" ~ "True Positive",
      churn_1måned_efter_forlængelse == "No"  & .pred_class == "No"  ~ "True Negative",
      churn_1måned_efter_forlængelse == "No"  & .pred_class == "Yes" ~ "False Positive",
      churn_1måned_efter_forlængelse == "Yes" & .pred_class == "No"  ~ "False Negative"
    )
  )

test_preds_2_upsampling |> 
  count(fejltype)


# Baseline -Feature importance (Xgboost)
final_model2 <- fit(final_wf2, model2_df)
xgb_fit_2 <- extract_fit_parsnip(final_model2)$fit
importance_tbl_2 <- xgboost::xgb.importance(model = xgb_fit_2)
importance_tbl_2
xgboost::xgb.plot.importance(importance_tbl_2, top_n = 15)


# Upsampling -Feature importance (Xgboost)
final_model2_upsampling <- fit(final_wf2_upsampling, model2_df)
xgb_fit_2_upsampling <- extract_fit_parsnip(final_model2_upsampling)$fit
importance_tbl_2_upsampling <- xgboost::xgb.importance(model = xgb_fit_2_upsampling)
importance_tbl_2_upsampling
xgboost::xgb.plot.importance(importance_tbl_2_upsampling, top_n = 15)





# Klyngeanalyse for model 2

#vi laver vores datasæt til klyngeanalysen med features der beskriver observationernes kendetegn
cluster_input2 <- data_model2 %>%
  select(
    pseudo_id,
    aktivitet_start_10,
    aktivitet_midt_10,
    aktivitet_slut_10,
    avg_scroll_depth,
    forskellige_artikler,
    restricted_share,
    politik, erhverv, sport, kultur, andet, debat, indland, international, livsstil,
    aktivitet_total_30,
    age,
    koen,
    churn_1måned_efter_forlængelse
  ) %>%
  drop_na()



# Vi vælger de variabler der skal bruges til selve klyngeanalysen
# Vi tager ikke chrun og alder med, da det skal være unsupervised. De skal bruges senere til profilering
cluster_df2 <- cluster_input2 %>%
  select(
    aktivitet_start_10,
    aktivitet_midt_10,
    aktivitet_slut_10,
    avg_scroll_depth,
    forskellige_artikler,
    restricted_share,
    politik, erhverv, sport, kultur, andet, debat, indland, international, livsstil
  )

# Alle variabler skal have samme vægt, så de standardiseres
cluster_scaled2 <- scale(cluster_df2)


# Vi skal anvende k-means clustering med 4 klynger og flere startpunkter (25)
# Den vælger k med den laveste wss
set.seed(7)
kmeans_model2 <- kmeans(cluster_scaled2, centers = 4, nstart = 25)


# Vi finder ud af hvor mange klynger vi skal bruge
wss2 <- map_dbl(1:10, function(k){
  kmeans(cluster_scaled2, centers = k, nstart = 20)$tot.withinss
})

# Vi finder ud af hvor mange klynger vi skal bruge
plot(1:10, wss2, type = "b",
     xlab = "Antal klynger",
     ylab = "WSS")


# Vi tilføjer klynger, altså hver observation tildeles en klynge
cluster_result2 <- cluster_input2 %>%
  mutate(cluster = factor(kmeans_model2$cluster))


#Vi lavr vores profilering af klyngerne
cluster_result2 %>%
  group_by(cluster) %>%
  summarise(
    n = n(),
    aktivitet = mean(aktivitet_total_30),
    scroll = mean(avg_scroll_depth),
    alder = mean(age),
    churn_1måned_efter_forlængelse_rate = mean(churn_1måned_efter_forlængelse == 1),
    .groups = "drop"
  )


# Vi finder procentdelen af dem der churner i hver klynge
cluster_result2 %>%
  count(cluster, churn_1måned_efter_forlængelse) %>%
  group_by(cluster) %>%
  mutate(prop = n / sum(n))

# Gem som csv fil, så den kan bruges i Power BI
write.csv(cluster_result2, "cluster_result2.csv", row.names = FALSE)

# Vi sætter en profil på hver klynge
cluster_profile2 <- cluster_result2 %>%
  group_by(cluster) %>%
  summarise(
    n = n(),
    aktivitet_start = mean(aktivitet_start_10),
    aktivitet_midt = mean(aktivitet_midt_10),
    aktivitet_slut = mean(aktivitet_slut_10),
    restricted = mean(restricted_share),
    alder = mean(age),
    scroll = mean(avg_scroll_depth),
    churn_1måned_efter_forlængelse_rate = mean(churn_1måned_efter_forlængelse),
    .groups = "drop"
  )

cluster_profile2

# Find den cluster ned højest chrun rate
cluster_result2 %>%
  group_by(cluster) %>%
  summarise(churn_rate = mean(churn_1måned_efter_forlængelse == 1))








# # Eksporter til cvs
# write.csv(model_df, "model_df.csv")
# 
# getwd()
# 
# view(cluster_result2)
# 
# 
# #Laver csv fil til colab
# write.csv(model_df, "model_df.csv")
