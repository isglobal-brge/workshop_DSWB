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


## ----ex1----------------------------------------------------------------------
# STEP 1 — search the Condition domain for the right concept (what term describes high cholesterol?)
ds.omop.concept.search()

# STEP 2 — once you have its concept_id, see how many patients have it
ds.omop.concept.prevalence()


## ----ex2----------------------------------------------------------------------
# STEP 1 — find the concept_id for a systolic blood pressure measurement
ds.omop.concept.search()

# STEP 2 — safe summary (mean, sd, p05-p95)
ds.omop.column.stats()

# STEP 3 — draw it (remember the histogram can plot itself)
ds.omop.value.histogram()


## ----ex3----------------------------------------------------------------------
# STEP 1 — find the concept_id that records marital status
ds.omop.concept.search()

# STEP 2 — count each recorded value (which column holds a CODED answer?)
ds.omop.value.counts()


## ----ex4----------------------------------------------------------------------
# STEP 1 — fill the recipe (choose formats: sex_mf / mean / binary / count ...)
omop_recipe(
  variables = list(
    omop_variable(),
    omop_variable_age(),
    omop_variable(),
    omop_variable()
  ),
  output = omop_output()
)

# STEP 2 — execute into a server symbol, then RATIFY (colnames + dim)
recipe_execute()

# STEP 3 — summarise: ds.summary() for numerics, ds.table() for categories/flags


## ----ex5----------------------------------------------------------------------
# STEP 1 — recipe with a filters = list(...) element, then execute
omop_recipe(
  variables = list(),
  filters = list(),
  output = omop_output()
)
recipe_execute()

# STEP 2 — fit the model and inspect $coefficients (P_OR is the odds ratio)
ds.glm()


## ----logout-------------------------------------------------------------------
DSI::datashield.logout(conns)

