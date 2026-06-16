## ----setup, include=FALSE-----------------------------------------------------
knitr::opts_chunk$set(comment = "#>", eval = FALSE)


## ----setup-conn---------------------------------------------------------------
library(DSI); library(DSOpal); library(dsBaseClient); library(dsOMOPClient)

builder <- DSI::newDSLoginBuilder()
builder$append(server = "nairobi", url = "https://nairobi.datashield.live",
               user = "ethiopia", password = "P@ssw0rd", profile = "omop")
builder$append(server = "douala",  url = "https://douala.datashield.live",
               user = "ethiopia", password = "P@ssw0rd", profile = "omop")
builder$append(server = "dakar",   url = "https://dakar.datashield.live",
               user = "ethiopia", password = "P@ssw0rd", profile = "omop")
conns <- DSI::datashield.login(logins = builder$build())
ds.omop.connect(resource = "omop_demo.mimic", symbol = "omop", conns = conns)


## ----e1-----------------------------------------------------------------------
# STEP 1 — search the Condition domain for "alcohol abuse" to get its concept_id
ds.omop.concept.search("____", domain = "Condition", symbol = "omop", conns = conns)$pooled

# STEP 2 — find it in the pooled condition prevalence (ranked by distinct patients)
ds.omop.concept.prevalence("condition_occurrence", metric = "persons",
                           top_n = 15, scope = "pooled", symbol = "omop", conns = conns)$pooled


## ----e2-----------------------------------------------------------------------
# STEP 1 — disclosure-safe summary (pooled): n, mean, sd, p05-p95
ds.omop.column.stats("measurement", "value_as_number", concept_id = ____,
                     scope = "pooled", symbol = "omop", conns = conns)$pooled

# STEP 2 — its shape: pull the per-site bins, then draw them
h <- ds.omop.value.histogram("measurement", value_col = "value_as_number",
                             concept_id = ____, symbol = "omop", conns = conns)
# combine h$per_site (drop NA counts), put midpoints on a common grid, then barplot()


## ----e3-----------------------------------------------------------------------
# STEP — marital status is a CODED value, so it lives in value_as_concept_id. Count it, pooled.
ds.omop.value.counts("observation", "____", concept_id = 40766231,
                     scope = "pooled", symbol = "omop", conns = conns)$pooled


## ----e4-----------------------------------------------------------------------
# STEP 1 — write the recipe: fill the format of each variable and the age anchor year
rec <- omop_recipe(
  variables = list(
    omop_variable(table = "person", column = "gender_concept_id", format = "sex_mf", name = "sex"),
    omop_variable_age(name = "age", year = ____),
    omop_variable(table = "measurement", concept_id = 21492239, format = "____", name = "systolic_bp"),
    omop_variable(table = "condition_occurrence", concept_id = 433753, format = "____", name = "alcohol_abuse"),
    omop_variable(table = "visit_occurrence", format = "count", name = "n_visits")
  ),
  output = omop_output(name = "study", type = "wide"))

# STEP 2 — execute into a server symbol D, then RATIFY it was built as you expected
recipe_execute(rec, out = c(study = "D"), symbol = "omop", conns = conns)
ds.colnames("D", datasources = conns)
ds.dim("D", datasources = conns)

# STEP 3 — summarise the systolic_bp column
ds.summary("D$systolic_bp", datasources = conns)[[1]][["quantiles & mean"]]


## ----e5-----------------------------------------------------------------------
# STEP — add a filters = list(...) with a sex filter and an age filter
rec_sub <- omop_recipe(
  variables = list(
    omop_variable(table = "person", column = "gender_concept_id", format = "sex_mf", name = "sex"),
    omop_variable_age(name = "age", year = 2024),
    omop_variable(table = "condition_occurrence", concept_id = 433753, format = "binary", name = "alcohol_abuse")
  ),
  filters = list(
    male  = omop_filter_sex("____"),
    older = omop_filter_age(min = ____, year = 2024)
  ),
  output = omop_output(name = "study", type = "wide"))

recipe_execute(rec_sub, out = c(study = "DSUB"), symbol = "omop", conns = conns)
ds.dim("DSUB", datasources = conns)
as.data.frame(ds.table("DSUB$sex", datasources = conns)$output.list[["TABLES.COMBINED_all.sources_counts"]])


## ----e6-----------------------------------------------------------------------
# STEP 1 — recipe + execute into H
rec_h <- omop_recipe(
  variables = list(
    omop_variable(table = "person", column = "gender_concept_id", format = "sex_mf", name = "sex"),
    omop_variable_age(name = "age", year = 2024),
    omop_variable(table = "condition_occurrence", concept_id = 320128, format = "binary", name = "hypertension")
  ),
  output = omop_output(name = "study", type = "wide"))
recipe_execute(rec_h, out = c(study = "H"), symbol = "omop", conns = conns)

# STEP 2 — fit the logistic model: fill in the two predictors
fit <- ds.glm(formula = "H$hypertension ~ H$____ + H$____",
              family = "binomial", datasources = conns)
fit$coefficients


## ----challenge----------------------------------------------------------------
# YOUR CODE HERE


## ----logout-------------------------------------------------------------------
DSI::datashield.logout(conns)

