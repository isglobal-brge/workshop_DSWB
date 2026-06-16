## ----setup, include=FALSE-----------------------------------------------------
library(knitr)
knitr::opts_chunk$set(comment = "#>", collapse = FALSE)
# small helper: render a data.frame as a clean table
show_tbl <- function(x, ...) knitr::kable(x, ...)


## ----libs---------------------------------------------------------------------
library(DSI)
library(DSOpal)
library(dsBaseClient)
library(dsOMOPClient)


## ----login--------------------------------------------------------------------
builder <- DSI::newDSLoginBuilder()
for (s in c("nairobi", "douala", "dakar"))
  builder$append(server = s, url = paste0("https://", s, ".datashield.live"),
                 user = "ethiopia", password = "P@ssw0rd", profile = "omop")
conns <- DSI::datashield.login(logins = builder$build())

ds.omop.connect(resource = "omop_demo.mimic", symbol = "omop", conns = conns)


## ----tables-------------------------------------------------------------------
tabs <- ds.omop.tables(symbol = "omop", conns = conns)
show_tbl(head(tabs$nairobi[, c("table_name", "schema_category", "has_person_id")], 12))


## ----columns------------------------------------------------------------------
show_tbl(ds.omop.columns("measurement", symbol = "omop", conns = conns)$nairobi)


## ----tablestats---------------------------------------------------------------
st <- ds.omop.table.stats("condition_occurrence", symbol = "omop", conns = conns)$per_site$nairobi
show_tbl(data.frame(rows = st$rows, persons = st$persons))


## ----gender-------------------------------------------------------------------
g <- ds.omop.concept.prevalence("person", concept_col = "gender_concept_id",
                                scope = "pooled", symbol = "omop", conns = conns)
show_tbl(g$pooled)


## ----topcond------------------------------------------------------------------
pc <- ds.omop.concept.prevalence("condition_occurrence", metric = "persons",
                                 top_n = 10, scope = "pooled", symbol = "omop", conns = conns)
show_tbl(pc$pooled)


## ----search-------------------------------------------------------------------
show_tbl(ds.omop.concept.search("hypertension", domain = "Condition",
                                limit = 5, symbol = "omop", conns = conns))


## ----lookup-------------------------------------------------------------------
show_tbl(ds.omop.concept.lookup(c(320128, 432867, 3027018),
                                symbol = "omop", conns = conns))


## ----hr-stats-----------------------------------------------------------------
cs <- ds.omop.column.stats("measurement", "value_as_number", concept_id = 3027018,
                           scope = "pooled", symbol = "omop", conns = conns)
show_tbl(cs$pooled)


## ----hr-hist, fig.width=7, fig.height=4---------------------------------------
h <- ds.omop.value.histogram("measurement", value_col = "value_as_number",
                             concept_id = 3027018, symbol = "omop", conns = conns)
bins <- do.call(rbind, h$per_site)            # stack the three sites' bins
bins <- subset(bins, !is.na(count))           # drop suppressed bins
bins <- aggregate(count ~ bin_start, data = bins, FUN = sum)
bins <- bins[order(bins$bin_start), ]
barplot(bins$count, names.arg = round(bins$bin_start), las = 2,
        col = "#4C72B0", border = NA,
        xlab = "Heart rate (bpm)", ylab = "records",
        main = "Heart rate across the federation")


## ----rhythm-------------------------------------------------------------------
rh <- ds.omop.value.counts("measurement", "value_as_concept_id",
                           concept_id = 3022318, symbol = "omop", conns = conns)
show_tbl(rh$pooled)


## ----recipe-build-------------------------------------------------------------
rec <- omop_recipe(
  variables = list(
    omop_variable(table = "person", column = "gender_concept_id", format = "sex_mf", name = "sex"),
    omop_variable_age(name = "age"),
    omop_variable(table = "measurement", concept_id = 3027018, format = "mean",   name = "heart_rate"),
    omop_variable(table = "condition_occurrence", concept_id = 320128, format = "binary", name = "hypertension"),
    omop_variable(table = "condition_occurrence", concept_id = 432867, format = "binary", name = "hyperlipidemia")
  ),
  outputs = omop_output(name = "study", type = "wide")
)


## ----preview------------------------------------------------------------------
recipe_preview(rec, symbol = "omop", conns = conns)


## ----execute------------------------------------------------------------------
recipe_execute(rec, out = c(study = "M"), symbol = "omop", conns = conns)


## ----check-cols---------------------------------------------------------------
ds.colnames("M", datasources = conns)
ds.dim("M", datasources = conns)


## ----check-summaries----------------------------------------------------------
# numeric: a summary; categorical/binary: frequency tables
ds.summary("M$age", datasources = conns)[[1]][["quantiles & mean"]]


## ----check-tables-------------------------------------------------------------
tab_sex <- ds.table("M$sex", datasources = conns)$output.list[["TABLES.COMBINED_all.sources_counts"]]
show_tbl(as.data.frame(tab_sex))
tab_htn <- ds.table("M$hypertension", datasources = conns)$output.list[["TABLES.COMBINED_all.sources_counts"]]
show_tbl(as.data.frame(tab_htn))


## ----recipe-filtered----------------------------------------------------------
rec_old <- omop_recipe(
  variables = list(
    omop_variable(table = "person", column = "gender_concept_id", format = "sex_mf", name = "sex"),
    omop_variable_age(name = "age"),
    omop_variable(table = "condition_occurrence", concept_id = 320128, format = "binary", name = "hypertension")
  ),
  filters  = list(elderly = omop_filter_age(min = 65)),
  outputs  = omop_output(name = "study", type = "wide")
)
recipe_execute(rec_old, out = c(study = "E"), symbol = "omop", conns = conns)


## ----filtered-check-----------------------------------------------------------
ds.dim("E", datasources = conns)
ds.summary("E$age", datasources = conns)[[1]][["quantiles & mean"]]


## ----glm, results='hide'------------------------------------------------------
fit <- ds.glm(
  formula = "M$hypertension ~ M$age + M$sex + M$hyperlipidemia",
  family  = "binomial", datasources = conns)


## ----glm-table----------------------------------------------------------------
co <- fit$coefficients
res <- data.frame(
  term    = rownames(co),
  OR      = round(co[, "P_OR"], 2),
  CI_low  = round(co[, "low0.95CI.P_OR"], 2),
  CI_high = round(co[, "high0.95CI.P_OR"], 2),
  p_value = signif(co[, "p-value"], 3),
  row.names = NULL)
show_tbl(res)


## ----logout-------------------------------------------------------------------
DSI::datashield.logout(conns)

