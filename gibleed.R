## ----setup, include=FALSE-----------------------------------------------------
knitr::opts_chunk$set(comment = "#>", collapse = FALSE)


## ----libs---------------------------------------------------------------------
library(DSI); library(DSOpal); library(dsBaseClient); library(dsOMOPClient)


## ----login--------------------------------------------------------------------
builder <- DSI::newDSLoginBuilder()
builder$append(server = "nairobi", url = "https://nairobi.datashield.live",
               user = "ethiopia", password = "P@ssw0rd", profile = "omop")
builder$append(server = "douala",  url = "https://douala.datashield.live",
               user = "ethiopia", password = "P@ssw0rd", profile = "omop")
builder$append(server = "dakar",   url = "https://dakar.datashield.live",
               user = "ethiopia", password = "P@ssw0rd", profile = "omop")
conns <- DSI::datashield.login(logins = builder$build())

ds.omop.connect(resource = "omop_demo.gibleed", symbol = "omop", conns = conns)


## ----no-numeric---------------------------------------------------------------
ds.omop.column.stats("measurement", "value_as_number",
                     scope = "pooled", symbol = "omop", conns = conns)$pooled


## ----sex----------------------------------------------------------------------
ds.omop.concept.prevalence("person", concept_col = "gender_concept_id",
                           scope = "pooled", symbol = "omop", conns = conns)$pooled


## ----cond-prev, fig.width=8, fig.height=4.5-----------------------------------
cond <- ds.omop.concept.prevalence("condition_occurrence", metric = "persons",
                                   top_n = 12, scope = "pooled", symbol = "omop", conns = conns)$pooled
cond <- cond[order(cond$n_persons), ]
par(mar = c(4, 17, 2, 1))
barplot(cond$n_persons, names.arg = cond$concept_name, horiz = TRUE, las = 1,
        col = "#4C72B0", border = NA, xlab = "patients",
        main = "Most common conditions in GiBleed")


## ----lookup-------------------------------------------------------------------
ds.omop.concept.lookup(c(192671, 4027663), symbol = "omop", conns = conns)$pooled
cond[cond$concept_id %in% c(192671, 4027663), ]


## ----recipe-------------------------------------------------------------------
rec <- omop_recipe(
  variables = list(
    omop_variable(table = "person", column = "gender_concept_id", format = "sex_mf", name = "sex"),
    omop_variable_age(name = "age", year = 2019),
    omop_variable(table = "condition_occurrence", concept_id = 192671,  format = "binary", name = "gi_bleed"),
    omop_variable(table = "condition_occurrence", concept_id = 4027663, format = "binary", name = "peptic_ulcer")
  ),
  output = omop_output(name = "study", type = "wide")
)
recipe_execute(rec, out = c(study = "M"), symbol = "omop", conns = conns)


## ----check--------------------------------------------------------------------
ds.colnames("M", datasources = conns)
ds.dim("M", datasources = conns)
as.data.frame(ds.table("M$gi_bleed",     datasources = conns)$output.list[["TABLES.COMBINED_all.sources_counts"]])
as.data.frame(ds.table("M$peptic_ulcer", datasources = conns)$output.list[["TABLES.COMBINED_all.sources_counts"]])


## ----glm, results='hide'------------------------------------------------------
fit <- ds.glm(
  formula = "M$gi_bleed ~ M$age + M$sex + M$peptic_ulcer",
  family  = "binomial", datasources = conns)


## ----glm-table----------------------------------------------------------------
co <- fit$coefficients
data.frame(
  term    = rownames(co),
  OR      = round(co[, "P_OR"], 3),
  CI_low  = round(co[, "low0.95CI.P_OR"], 3),
  CI_high = round(co[, "high0.95CI.P_OR"], 3),
  p_value = signif(co[, "p-value"], 3),
  row.names = NULL
)


## ----degenerate, warning=TRUE-------------------------------------------------
rec_oa <- omop_recipe(
  variables = list(
    omop_variable(table = "person", column = "gender_concept_id", format = "sex_mf", name = "sex"),
    omop_variable_age(name = "age", year = 2019),
    omop_variable(table = "condition_occurrence", concept_id = 80180, format = "binary", name = "osteoarthritis")
  ),
  output = omop_output(name = "study", type = "wide"))
recipe_execute(rec_oa, out = c(study = "OA"), symbol = "omop", conns = conns)

fit_oa <- ds.glm("OA$osteoarthritis ~ OA$age + OA$sex", family = "binomial", datasources = conns)
fit_oa$coefficients[, c("Estimate", "p-value")]


## ----logout-------------------------------------------------------------------
DSI::datashield.logout(conns)

