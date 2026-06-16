## ----setup, include=FALSE-----------------------------------------------------
knitr::opts_chunk$set(comment = "#>", collapse = FALSE)


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
                           scope = "pooled", symbol = "omop", conns = conns)$pooled


## ----topcond------------------------------------------------------------------
ds.omop.concept.prevalence("condition_occurrence", metric = "persons",
                           top_n = 10, scope = "pooled", symbol = "omop", conns = conns)$pooled


## ----search-------------------------------------------------------------------
hits <- ds.omop.concept.search("hyperlipidemia", domain = "Condition",
                               limit = 5, symbol = "omop", conns = conns)$pooled
hits[, c("concept_id", "concept_name", "domain_id", "vocabulary_id")]


## ----lookup-------------------------------------------------------------------
ds.omop.concept.lookup(c(320128, 432867, 3027018),
                       symbol = "omop", conns = conns)$pooled


## ----hr-stats-----------------------------------------------------------------
ds.omop.column.stats("measurement", "value_as_number", concept_id = 3027018,
                     scope = "pooled", symbol = "omop", conns = conns)$pooled


## ----hr-hist, fig.width=7, fig.height=4---------------------------------------
draw_hist <- function(concept_id, nbins, xlab, main, col = "#4C72B0", digits = 0) {
  h <- ds.omop.value.histogram("measurement", value_col = "value_as_number",
                               concept_id = concept_id, symbol = "omop", conns = conns)
  bins <- do.call(rbind, h$per_site)
  bins <- subset(bins, !is.na(count))
  mid  <- (bins$bin_start + bins$bin_end) / 2
  br   <- seq(min(mid), max(mid), length.out = nbins + 1)   # common equal-width bins
  grp  <- cut(mid, breaks = br, include.lowest = TRUE)      # re-bin each site onto them
  agg  <- tapply(bins$count, grp, sum); agg[is.na(agg)] <- 0
  ctr  <- round((head(br, -1) + tail(br, -1)) / 2, digits)
  barplot(as.numeric(agg), names.arg = ctr, las = 2, col = col, border = NA,
          xlab = xlab, ylab = "records", main = main)
}
draw_hist(3027018, nbins = 7, xlab = "Heart rate (bpm)",
          main = "Heart rate across the federation")


## ----creat-stats--------------------------------------------------------------
ds.omop.column.stats("measurement", "value_as_number", concept_id = 3016723,
                     scope = "pooled", symbol = "omop", conns = conns)$pooled


## ----creat-hist, fig.width=7, fig.height=4------------------------------------
draw_hist(3016723, nbins = 9, xlab = "Creatinine (mg/dL)",
          main = "Serum creatinine across the federation", col = "#C44E52", digits = 1)


## ----rhythm-------------------------------------------------------------------
ds.omop.value.counts("measurement", "value_as_concept_id",
                     concept_id = 3022318, scope = "pooled",
                     symbol = "omop", conns = conns)$pooled


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
ds.summary("M$heart_rate", datasources = conns)[[1]][["quantiles & mean"]]
ds.summary("M$n_visits",  datasources = conns)[[1]][["quantiles & mean"]]


## ----check-sex----------------------------------------------------------------
as.data.frame(ds.table("M$sex", datasources = conns)$output.list[["TABLES.COMBINED_all.sources_counts"]])


## ----check-htn----------------------------------------------------------------
as.data.frame(ds.table("M$hypertension", datasources = conns)$output.list[["TABLES.COMBINED_all.sources_counts"]])


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


## ----ratify-multi-------------------------------------------------------------
ds.colnames("DEMO", datasources = conns)
ds.colnames("CLIN", datasources = conns)


## ----recipe-filtered----------------------------------------------------------
rec_sub <- omop_recipe(
  variables = list(
    omop_variable(table = "person", column = "gender_concept_id", format = "sex_mf", name = "sex"),
    omop_variable_age(name = "age", year = 2024)
  ),
  filters = list(
    female        = omop_filter_sex("F"),
    middle_aged   = omop_filter_age(min = 50, year = 2024),
    with_hyperlip = omop_filter_has_concept(concept_id = 432867, table = "condition_occurrence")
  ),
  output = omop_output(name = "study", type = "wide")
)
recipe_execute(rec_sub, out = c(study = "SUB"), symbol = "omop", conns = conns)


## ----filtered-check-----------------------------------------------------------
ds.dim("SUB", datasources = conns)
ds.summary("SUB$age", datasources = conns)[[1]][["quantiles & mean"]]


## ----glm1, results='hide'-----------------------------------------------------
fit1 <- ds.glm(
  formula = "M$hyperlipidemia ~ M$age + M$sex",
  family  = "binomial", datasources = conns)


## ----glm1-table---------------------------------------------------------------
or_table <- function(fit) {
  co <- fit$coefficients
  data.frame(
    term    = rownames(co),
    OR      = round(co[, "P_OR"], 3),
    CI_low  = round(co[, "low0.95CI.P_OR"], 3),
    CI_high = round(co[, "high0.95CI.P_OR"], 3),
    p_value = signif(co[, "p-value"], 3),
    row.names = NULL
  )
}
or_table(fit1)


## ----glm2, results='hide'-----------------------------------------------------
fit2 <- ds.glm(
  formula = "M$hypertension ~ M$age + M$sex + M$hyperlipidemia",
  family  = "binomial", datasources = conns)


## ----glm2-table---------------------------------------------------------------
or_table(fit2)


## ----logout-------------------------------------------------------------------
DSI::datashield.logout(conns)

