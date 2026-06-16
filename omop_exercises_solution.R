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
ds.omop.connect(resource = "omop_demo.mimic", symbol = "omop", conns = conns)


## ----ex1----------------------------------------------------------------------
ds.omop.concept.search("hyperlipidemia", domain = "Condition",
                       symbol = "omop", conns = conns)

prev <- ds.omop.concept.prevalence("condition_occurrence", metric = "persons",
                                   top_n = 15, scope = "pooled", symbol = "omop", conns = conns)$pooled
prev[prev$concept_id == 432867, ]


## ----ex2, fig.width=7, fig.height=4-------------------------------------------
ds.omop.column.stats("measurement", "value_as_number", concept_id = 21492239,
                     scope = "pooled", symbol = "omop", conns = conns)

ds.omop.value.histogram("measurement", value_col = "value_as_number", concept_id = 21492239,
                        nbins = 9, plot = TRUE, xlab = "Systolic BP (mmHg)",
                        main = "Systolic blood pressure (pooled)",
                        symbol = "omop", conns = conns)


## ----ex3----------------------------------------------------------------------
ds.omop.value.counts("observation", "value_as_concept_id", concept_id = 40766231,
                     scope = "pooled", symbol = "omop", conns = conns)


## ----ex4----------------------------------------------------------------------
rec <- omop_recipe(
  variables = list(
    omop_variable(table = "person", column = "gender_concept_id", format = "sex_mf", name = "sex"),
    omop_variable_age(name = "age", year = 2024),
    omop_variable(table = "measurement", concept_id = 21492239, format = "mean", name = "systolic_bp"),
    omop_variable(table = "condition_occurrence", concept_id = 432867, format = "binary", name = "hyperlipidemia")
  ),
  output = omop_output(name = "study", type = "wide"))

recipe_execute(rec, out = c(study = "D"), symbol = "omop", conns = conns)
ds.colnames("D", datasources = conns)
ds.dim("D", datasources = conns)
ds.summary("D$systolic_bp", datasources = conns)
ds.table("D$hyperlipidemia", datasources = conns)


## ----ex5----------------------------------------------------------------------
rec_h <- omop_recipe(
  variables = list(
    omop_variable(table = "person", column = "gender_concept_id", format = "sex_mf", name = "sex"),
    omop_variable_age(name = "age", year = 2024),
    omop_variable(table = "condition_occurrence", concept_id = 432867, format = "binary", name = "hyperlipidemia")
  ),
  filters = list(older = omop_filter_age(min = 50, year = 2024)),
  output = omop_output(name = "study", type = "wide"))
recipe_execute(rec_h, out = c(study = "H"), symbol = "omop", conns = conns)
ds.dim("H", datasources = conns)

fit <- ds.glm(formula = "H$hyperlipidemia ~ H$age + H$sex", family = "binomial", datasources = conns)
fit$coefficients


## ----logout-------------------------------------------------------------------
DSI::datashield.logout(conns)

