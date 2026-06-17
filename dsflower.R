## ----setup, include=FALSE-----------------------------------------------------
knitr::opts_chunk$set(comment = "#>", collapse = FALSE, eval = FALSE)
options(width = 200)


## ----libs---------------------------------------------------------------------
library(DSI); library(DSOpal); library(dsBaseClient); library(dsFlowerClient)


## ----login--------------------------------------------------------------------
builder <- DSI::newDSLoginBuilder()
builder$append(server = "nairobi", url = "https://nairobi.datashield.live",
               user = "ethiopia", password = "P@ssw0rd",
               table = "dsflower_demo.breast_cancer")
builder$append(server = "dakar",   url = "https://dakar.datashield.live",
               user = "ethiopia", password = "P@ssw0rd",
               table = "dsflower_demo.breast_cancer")
builder$append(server = "douala",  url = "https://douala.datashield.live",
               user = "ethiopia", password = "P@ssw0rd",
               table = "dsflower_demo.breast_cancer")
conns <- DSI::datashield.login(logins = builder$build(), assign = TRUE, symbol = "D")


## ----dims---------------------------------------------------------------------
ds.dim("D", datasources = conns)


## ----features-----------------------------------------------------------------
features <- setdiff(ds.colnames("D", datasources = conns)[[1]], "malignant")
length(features)   # 30 predictors


## ----outcome------------------------------------------------------------------
ds.table("D$malignant", datasources = conns)


## ----torup--------------------------------------------------------------------
ds.flower.tor.up(conns)


## ----fit----------------------------------------------------------------------
fit <- ds.flower.fit(
  conns,
  symbol   = "D",
  target   = "malignant",
  features = features,
  model    = "sklearn_logreg",
  strategy = "fedavg",
  privacy  = "clinical_default",   # enforces Secure Aggregation (SecAgg+)
  rounds   = 5L,
  verbose  = TRUE
)


## ----predict------------------------------------------------------------------
new_patients <- read.csv("https://raw.githubusercontent.com/isglobal-brge/workshop_DSWB/main/data/breast_cancer_new_patients.csv")

prob <- ds.flower.predict(fit, new_patients[, features], type = "prob")

data.frame(
  actual      = new_patients$malignant,
  predicted   = as.integer(prob >= 0.5),
  P_malignant = round(prob, 3)
)


## ----teardown-----------------------------------------------------------------
ds.flower.tor.down(conns)
DSI::datashield.logout(conns)

