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
ds.omop.connect(resource = "omop_demo.mimic", symbol = "omop", conns = conns)


## ----draw-hist, include=FALSE-------------------------------------------------
# Same federation-wide histogram helper as the tutorial.
draw_hist <- function(concept_id, nbins, xlab, main, col = "#4C72B0", digits = 0) {
  h <- ds.omop.value.histogram("measurement", value_col = "value_as_number",
                               concept_id = concept_id, symbol = "omop", conns = conns)
  bins <- do.call(rbind, h$per_site)
  bins <- subset(bins, !is.na(count))
  mid  <- (bins$bin_start + bins$bin_end) / 2
  br   <- seq(min(mid), max(mid), length.out = nbins + 1)
  grp  <- cut(mid, breaks = br, include.lowest = TRUE)
  agg  <- tapply(bins$count, grp, sum); agg[is.na(agg)] <- 0
  ctr  <- round((head(br, -1) + tail(br, -1)) / 2, digits)
  barplot(as.numeric(agg), names.arg = ctr, las = 2, col = col, border = NA,
          xlab = xlab, ylab = "records", main = main)
}


## ----e1-----------------------------------------------------------------------
ds.omop.concept.search("alcohol", domain = "Condition",
                       symbol = "omop", conns = conns)$pooled[, c("concept_id", "concept_name")]

prev <- ds.omop.concept.prevalence("condition_occurrence", metric = "persons",
                                   top_n = 15, scope = "pooled", symbol = "omop", conns = conns)$pooled
prev[prev$concept_id == 433753, ]


## ----e2, fig.width=7, fig.height=4--------------------------------------------
ds.omop.column.stats("measurement", "value_as_number", concept_id = 21492239,
                     scope = "pooled", symbol = "omop", conns = conns)$pooled

draw_hist(21492239, nbins = 9, xlab = "Systolic BP (mmHg)",
          main = "Systolic blood pressure across the federation")


## ----e3-----------------------------------------------------------------------
ds.omop.value.counts("observation", "value_as_concept_id", concept_id = 40766231,
                     scope = "pooled", symbol = "omop", conns = conns)$pooled


## ----e4-----------------------------------------------------------------------
rec <- omop_recipe(
  variables = list(
    omop_variable(table = "person", column = "gender_concept_id", format = "sex_mf", name = "sex"),
    omop_variable_age(name = "age", year = 2024),
    omop_variable(table = "measurement", concept_id = 21492239, format = "mean", name = "systolic_bp"),
    omop_variable(table = "condition_occurrence", concept_id = 433753, format = "binary", name = "alcohol_abuse"),
    omop_variable(table = "visit_occurrence", format = "count", name = "n_visits")
  ),
  output = omop_output(name = "study", type = "wide"))

recipe_execute(rec, out = c(study = "D"), symbol = "omop", conns = conns)
ds.colnames("D", datasources = conns)
ds.dim("D", datasources = conns)
ds.summary("D$systolic_bp", datasources = conns)[[1]][["quantiles & mean"]]


## ----e5-----------------------------------------------------------------------
rec_sub <- omop_recipe(
  variables = list(
    omop_variable(table = "person", column = "gender_concept_id", format = "sex_mf", name = "sex"),
    omop_variable_age(name = "age", year = 2024),
    omop_variable(table = "condition_occurrence", concept_id = 433753, format = "binary", name = "alcohol_abuse")
  ),
  filters = list(
    male  = omop_filter_sex("M"),
    older = omop_filter_age(min = 60, year = 2024)
  ),
  output = omop_output(name = "study", type = "wide"))

recipe_execute(rec_sub, out = c(study = "DSUB"), symbol = "omop", conns = conns)
ds.dim("DSUB", datasources = conns)
as.data.frame(ds.table("DSUB$sex", datasources = conns)$output.list[["TABLES.COMBINED_all.sources_counts"]])


## ----e6, results='hide'-------------------------------------------------------
rec_h <- omop_recipe(
  variables = list(
    omop_variable(table = "person", column = "gender_concept_id", format = "sex_mf", name = "sex"),
    omop_variable_age(name = "age", year = 2024),
    omop_variable(table = "condition_occurrence", concept_id = 320128, format = "binary", name = "hypertension")
  ),
  output = omop_output(name = "study", type = "wide"))
recipe_execute(rec_h, out = c(study = "H"), symbol = "omop", conns = conns)
fit <- ds.glm(formula = "H$hypertension ~ H$age + H$sex", family = "binomial", datasources = conns)


## ----e6-table-----------------------------------------------------------------
co <- fit$coefficients
data.frame(
  term    = rownames(co),
  OR      = round(co[, "P_OR"], 3),
  CI_low  = round(co[, "low0.95CI.P_OR"], 3),
  CI_high = round(co[, "high0.95CI.P_OR"], 3),
  p_value = signif(co[, "p-value"], 3),
  row.names = NULL
)


## ----logout-------------------------------------------------------------------
DSI::datashield.logout(conns)

