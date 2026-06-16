## ----setup, include=FALSE-----------------------------------------------------
knitr::opts_chunk$set(comment = "#>")


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

or_table <- function(fit) {
  co <- fit$coefficients
  data.frame(term = rownames(co),
             OR = round(co[, "P_OR"], 3),
             CI_low = round(co[, "low0.95CI.P_OR"], 3),
             CI_high = round(co[, "high0.95CI.P_OR"], 3),
             p_value = signif(co[, "p-value"], 3),
             row.names = NULL)
}


## ----ge1----------------------------------------------------------------------
ds.omop.concept.prevalence("drug_exposure", metric = "persons", top_n = 12,
                           scope = "pooled", symbol = "omop", conns = conns)$pooled

ds.omop.concept.lookup(1118084, symbol = "omop", conns = conns)$pooled


## ----ge2----------------------------------------------------------------------
rec <- omop_recipe(
  variables = list(
    omop_variable(table = "person", column = "gender_concept_id", format = "sex_mf", name = "sex"),
    omop_variable_age(name = "age", year = 2019),
    omop_variable(table = "condition_occurrence", concept_id = 192671, format = "binary", name = "gi_bleed"),
    omop_variable(table = "drug_exposure", concept_id = 1118084, format = "binary", name = "celecoxib")
  ),
  output = omop_output(name = "study", type = "wide"))

recipe_execute(rec, out = c(study = "M"), symbol = "omop", conns = conns)
ds.colnames("M", datasources = conns)
ds.dim("M", datasources = conns)
as.data.frame(ds.table("M$celecoxib", datasources = conns)$output.list[["TABLES.COMBINED_all.sources_counts"]])


## ----ge3, results='hide'------------------------------------------------------
fit <- ds.glm(formula = "M$gi_bleed ~ M$age + M$sex + M$celecoxib",
              family = "binomial", datasources = conns)


## ----ge3-table----------------------------------------------------------------
or_table(fit)


## ----ge4, results='hide'------------------------------------------------------
rec_fx <- omop_recipe(
  variables = list(
    omop_variable(table = "person", column = "gender_concept_id", format = "sex_mf", name = "sex"),
    omop_variable_age(name = "age", year = 2019),
    omop_variable(table = "condition_occurrence", concept_id = 4278672, format = "binary", name = "fx_forearm")
  ),
  output = omop_output(name = "study", type = "wide"))
recipe_execute(rec_fx, out = c(study = "FX"), symbol = "omop", conns = conns)
fit_fx <- ds.glm(formula = "FX$fx_forearm ~ FX$age + FX$sex", family = "binomial", datasources = conns)


## ----ge4-table----------------------------------------------------------------
or_table(fit_fx)


## ----logout-------------------------------------------------------------------
DSI::datashield.logout(conns)

