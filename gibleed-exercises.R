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
ds.omop.connect(resource = "omop_demo.gibleed", symbol = "omop", conns = conns)


## ----ge1----------------------------------------------------------------------
# STEP 1 — pooled prevalence of the top drugs (by distinct patients)
ds.omop.concept.prevalence("____", metric = "persons", top_n = 12,
                           scope = "pooled", symbol = "omop", conns = conns)$pooled

# STEP 2 — confirm celecoxib's concept_id and how many patients take it
ds.omop.concept.lookup(1118084, symbol = "omop", conns = conns)$pooled


## ----ge2----------------------------------------------------------------------
rec <- omop_recipe(
  variables = list(
    omop_variable(table = "person", column = "gender_concept_id", format = "sex_mf", name = "sex"),
    omop_variable_age(name = "age", year = ____),
    omop_variable(table = "condition_occurrence", concept_id = 192671, format = "____", name = "gi_bleed"),
    omop_variable(table = "drug_exposure", concept_id = 1118084, format = "____", name = "celecoxib")
  ),
  output = omop_output(name = "study", type = "wide"))

recipe_execute(rec, out = c(study = "M"), symbol = "omop", conns = conns)
ds.colnames("M", datasources = conns); ds.dim("M", datasources = conns)
as.data.frame(ds.table("M$celecoxib", datasources = conns)$output.list[["TABLES.COMBINED_all.sources_counts"]])


## ----ge3----------------------------------------------------------------------
fit <- ds.glm(formula = "M$gi_bleed ~ M$age + M$sex + M$____",
              family = "binomial", datasources = conns)
fit$coefficients


## ----ge4----------------------------------------------------------------------
rec_fx <- omop_recipe(
  variables = list(
    omop_variable(table = "person", column = "gender_concept_id", format = "sex_mf", name = "sex"),
    omop_variable_age(name = "age", year = 2019),
    omop_variable(table = "condition_occurrence", concept_id = 4278672, format = "binary", name = "fx_forearm")
  ),
  output = omop_output(name = "study", type = "wide"))
recipe_execute(rec_fx, out = c(study = "FX"), symbol = "omop", conns = conns)

fit_fx <- ds.glm(formula = "FX$fx_forearm ~ FX$____ + FX$____",
                 family = "binomial", datasources = conns)
fit_fx$coefficients


## ----challenge----------------------------------------------------------------
# YOUR CODE HERE


## ----logout-------------------------------------------------------------------
DSI::datashield.logout(conns)

