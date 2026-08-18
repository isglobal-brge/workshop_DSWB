## ----setup, include=FALSE-----------------------------------------------------
knitr::opts_chunk$set(comment = "#>", collapse = FALSE)
options(width = 200)   # print wide output on one line; the page scrolls it horizontally


## ----install, eval=FALSE------------------------------------------------------
# devtools::install_github(
#   "isglobal-brge/dsOMOPClient@2.7.1",
#   force = TRUE,
#   upgrade = "never"
# )


## ----libs---------------------------------------------------------------------
library(DSI)
library(DSOpal)
library(dsBaseClient)
library(dsOMOPClient)


## ----login--------------------------------------------------------------------
builder <- DSI::newDSLoginBuilder()
builder$append(server = "nairobi", url = "https://nairobi.datashield.live",
               user = "ethiopia", password = "P@ssw0rd", profile = "omop")
builder$append(server = "douala",  url = "https://douala.datashield.live",
               user = "ethiopia", password = "P@ssw0rd", profile = "omop")
builder$append(server = "dakar",   url = "https://dakar.datashield.live",
               user = "ethiopia", password = "P@ssw0rd", profile = "omop")
conns <- DSI::datashield.login(logins = builder$build())

ds.omop.connect(resource = "omop_demo.mimic", symbol = "omop", conns = conns)


## ----tables-------------------------------------------------------------------
tabs <- ds.omop.tables(symbol = "omop", conns = conns)[[1]]
tabs[tabs$schema_category == "CDM", c("table_name", "has_person_id")]


## ----columns------------------------------------------------------------------
ds.omop.columns("measurement", symbol = "omop", conns = conns)[[1]]


## ----gender-------------------------------------------------------------------
ds.omop.concept.prevalence("person", concept_col = "gender_concept_id",
                           scope = "pooled", symbol = "omop", conns = conns)


## ----topcond------------------------------------------------------------------
ds.omop.concept.prevalence("condition_occurrence", metric = "persons",
                           top_n = 10, scope = "pooled", symbol = "omop", conns = conns)


## ----search-------------------------------------------------------------------
ds.omop.concept.search("infarction / myocardial-acute", domain = "Condition",
                       limit = 5, symbol = "omop", conns = conns)


## ----lookup-------------------------------------------------------------------
ds.omop.concept.lookup(c(320128, 432867, 3027018), symbol = "omop", conns = conns)


## ----hr-stats-----------------------------------------------------------------
ds.omop.column.stats("measurement", "value_as_number", concept_id = 3027018,
                     scope = "pooled", symbol = "omop", conns = conns)


## ----hr-hist, fig.width=7, fig.height=4---------------------------------------
ds.omop.value.histogram("measurement", value_col = "value_as_number", concept_id = 3027018,
                        bins = 3, nbins = 3, plot = TRUE, xlab = "Heart rate (bpm)",
                        main = "Heart rate (pooled across sites)",
                        symbol = "omop", conns = conns)


## ----rhythm-------------------------------------------------------------------
ds.omop.value.counts("measurement", "value_as_concept_id", concept_id = 3022318,
                     scope = "pooled", symbol = "omop", conns = conns)


## ----recipe-skeleton, eval=FALSE----------------------------------------------
# omop_recipe(
#   variables = list(    # WHAT columns you want
#     omop_variable(),       # a person column, or something built from a concept_id
#     omop_variable_age(),   # the patient's age
#     omop_variable()        # ...add as many as you need
#   ),
#   filters = list(),    # OPTIONAL: restrict the population (Section 8)
#   output  = omop_output()  # the table SHAPE you want (usually wide = one row/patient)
# )
# recipe_execute()       # build it on each server into a named symbol


## ----recipe-simple------------------------------------------------------------
rec0 <- omop_recipe(
  variables = list(
    omop_variable(table = "person", column = "gender_concept_id", format = "sex_mf", name = "sex"),
    omop_variable_age(name = "age", year = 2024)
  ),
  output = omop_output(name = "study", type = "wide")
)
recipe_execute(rec0, out = c(study = "M0"), symbol = "omop", conns = conns)


## ----ratify-simple------------------------------------------------------------
ds.colnames("M0", datasources = conns)
ds.dim("M0", datasources = conns)


## ----m0-sex-------------------------------------------------------------------
ds.table("M0$sex", datasources = conns)


## ----m0-age-------------------------------------------------------------------
ds.summary("M0$age", datasources = conns)


## ----recipe-rich--------------------------------------------------------------
rec <- omop_recipe(
  variables = list(
    omop_variable(table = "person", column = "gender_concept_id", format = "sex_mf", name = "sex"),
    omop_variable_age(name = "age", year = 2024),
    omop_variable(table = "measurement", concept_id = 3027018, format = "mean", name = "heart_rate"),
    omop_variable(table = "condition_occurrence", concept_id = 320128, format = "binary", name = "hypertension"),
    omop_variable(table = "condition_occurrence", concept_id = 432867, format = "binary", name = "hyperlipidemia"),
    omop_variable(table = "visit_occurrence", format = "count", name = "n_visits")
  ),
  output = omop_output(name = "study", type = "wide")
)
recipe_execute(rec, out = c(study = "M"), symbol = "omop", conns = conns)
ds.colnames("M", datasources = conns)
ds.dim("M", datasources = conns)


## ----check-num----------------------------------------------------------------
ds.summary("M$age",        datasources = conns)
ds.summary("M$heart_rate", datasources = conns)
ds.summary("M$n_visits",   datasources = conns)


## ----check-sex----------------------------------------------------------------
ds.table("M$sex",            datasources = conns)


## ----check-htn----------------------------------------------------------------
ds.table("M$hypertension",   datasources = conns)


## ----check-hyperlip-----------------------------------------------------------
ds.table("M$hyperlipidemia", datasources = conns)


## ----recipe-multi-------------------------------------------------------------
rec_multi <- omop_recipe(
  variables = list(
    omop_variable(table = "person", column = "gender_concept_id", format = "sex_mf", name = "sex"),
    omop_variable_age(name = "age", year = 2024),
    omop_variable(table = "measurement", concept_id = 3027018, format = "mean", name = "heart_rate"),
    omop_variable(table = "condition_occurrence", concept_id = 320128, format = "binary", name = "hypertension")
  ),
  outputs = list(
    omop_output(name = "demographics", variables = c("sex", "age"), type = "wide"),
    omop_output(name = "clinical",     variables = c("heart_rate", "hypertension"), type = "wide")
  )
)
recipe_execute(rec_multi, out = c(demographics = "DEMO", clinical = "CLIN"),
               symbol = "omop", conns = conns)
ds.colnames("DEMO", datasources = conns)
ds.colnames("CLIN", datasources = conns)


## ----recipe-filtered----------------------------------------------------------
rec_sub <- omop_recipe(
  variables = list(
    omop_variable(table = "person", column = "gender_concept_id", format = "sex_mf", name = "sex"),
    omop_variable_age(name = "age", year = 2024)
  ),
  filters = list(
    older       = omop_filter_age(min = 50, year = 2024),
    with_lipids = omop_filter_has_concept(concept_id = 432867, table = "condition_occurrence")
  ),
  output = omop_output(name = "study", type = "wide")
)
recipe_execute(rec_sub, out = c(study = "SUB"), symbol = "omop", conns = conns)


## ----filtered-check-----------------------------------------------------------
ds.dim("SUB", datasources = conns)
ds.summary("SUB$age", datasources = conns)


## ----recipe-or----------------------------------------------------------------
rec_or <- omop_recipe(
  variables = list(
    omop_variable(table = "person", column = "gender_concept_id", format = "sex_mf", name = "sex"),
    omop_variable_age(name = "age", year = 2024)
  ),
  filters = list(
    cardiometabolic = omop_filter_group(
      omop_filter_has_concept(concept_id = 320128, table = "condition_occurrence"),  # hypertension
      omop_filter_has_concept(concept_id = 432867, table = "condition_occurrence"),  # hyperlipidemia
      operator = "OR"
    )
  ),
  output = omop_output(name = "study", type = "wide")
)
recipe_execute(rec_or, out = c(study = "ORC"), symbol = "omop", conns = conns)
ds.dim("ORC", datasources = conns)


## ----glm1---------------------------------------------------------------------
fit1 <- ds.glm(formula = "M$hyperlipidemia ~ M$age + M$sex",
               family = "binomial", datasources = conns)
fit1$coefficients


## ----glm2---------------------------------------------------------------------
fit2 <- ds.glm(formula = "M$hypertension ~ M$age + M$sex + M$hyperlipidemia",
               family = "binomial", datasources = conns)
fit2$coefficients


## ----logout-------------------------------------------------------------------
DSI::datashield.logout(conns)

