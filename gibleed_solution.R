## ----setup, include=FALSE-----------------------------------------------------
knitr::opts_chunk$set(comment = "#>")
options(width = 200)   # print wide output on one line; the page scrolls it horizontally


## ----connect------------------------------------------------------------------
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


## ----ex1----------------------------------------------------------------------
ds.omop.concept.search("celecoxib", domain = "Drug",
                       symbol = "omop", conns = conns)

ds.omop.concept.prevalence("drug_exposure", metric = "persons", top_n = 12,
                           scope = "pooled", symbol = "omop", conns = conns)


## ----ex2----------------------------------------------------------------------
ds.omop.concept.search("gastrointestinal hemorrhage", domain = "Condition",
                       symbol = "omop", conns = conns)

rec <- omop_recipe(
  variables = list(
    omop_variable(table = "person", column = "gender_concept_id", format = "sex_mf", name = "sex"),
    omop_variable_age(name = "age", year = 2019),
    omop_variable(table = "condition_occurrence", concept_id = 192671, format = "binary", name = "gi_bleed"),
    omop_variable(table = "drug_exposure", concept_id = 1118084, format = "binary", name = "celecoxib")
  ),
  output = omop_output(name = "study", type = "wide"))
recipe_execute(rec, out = c(study = "M"), symbol = "omop", conns = conns)

fit <- ds.glm(formula = "M$gi_bleed ~ M$age + M$sex + M$celecoxib",
              family = "binomial", datasources = conns)
fit$coefficients


## ----ex3----------------------------------------------------------------------
ds.omop.concept.search("fracture of forearm", domain = "Condition",
                       symbol = "omop", conns = conns)

rec_fx <- omop_recipe(
  variables = list(
    omop_variable(table = "person", column = "gender_concept_id", format = "sex_mf", name = "sex"),
    omop_variable_age(name = "age", year = 2019),
    omop_variable(table = "condition_occurrence", concept_id = 4278672, format = "binary", name = "fx_forearm")
  ),
  output = omop_output(name = "study", type = "wide"))
recipe_execute(rec_fx, out = c(study = "FX"), symbol = "omop", conns = conns)

fit_fx <- ds.glm(formula = "FX$fx_forearm ~ FX$age + FX$sex", family = "binomial", datasources = conns)
fit_fx$coefficients


## ----logout-------------------------------------------------------------------
DSI::datashield.logout(conns)

