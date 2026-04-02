# Indlæsning af pakker ----------------------------------------------------
pacman::p_load(tidyverse, tidymodels, themis, table1, ggpubr, broom, ggfortify,
  GGally, PerformanceAnalytics, car, caret, skimr, discrim, glmnet,
  kknn, naivebayes, kernlab, xgboost, gridExtra, rpart, future,
  ranger, rmarkdown, rvest, httr, jsonlite, rlist, rjson,
  Rcrawler, hrbrthemes, knitr, hms, leaps, readxl, gbm,
  randomForest, stringr, vip)


# Indlæsning af datasæt ---------------------------------------------------
behavior <- read_csv("behavior.csv")
subscription <- read_csv("subscription.csv")
cancellation <- read_csv("cancellation.csv")

# Joining af datasæt ------------------------------------------------------
join_datasæt <- subscription |>
  left_join(cancellation, by = "pseudo_id")
 
view(join_datasæt)
str(join_datasæt)


# Rensning af data --------------------------------------------------------
  # Laver ny variabel, som kigger på abonnementernes længde
join_datasæt <- join_datasæt |> 
  mutate(
    abonnement_længde = as.numeric(expiration_date - first_campaign_day)
  )

# Fjerner de observationer som har haft abonnementet under de 2 måneder, som kampagnen tilbyder
join_datasæt <- join_datasæt |> 
  filter(abonnement_længde >=60)

