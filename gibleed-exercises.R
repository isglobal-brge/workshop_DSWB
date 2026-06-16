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


## ----ex1----------------------------------------------------------------------
# STEP 1 — search the Drug domain for an NSAID (e.g. one whose name you know)
ds.omop.concept.search()

# STEP 2 — how many patients are on it?
ds.omop.concept.prevalence()


## ----ex2----------------------------------------------------------------------
# STEP 1 — find the GI-haemorrhage condition concept_id
ds.omop.concept.search()

# STEP 2 — wide table of booleans (choose the right formats + the 2019 age anchor)
omop_recipe(
  variables = list(
    omop_variable(),
    omop_variable_age(),
    omop_variable(),
    omop_variable()
  ),
  output = omop_output()
)
recipe_execute()

# STEP 3 — logistic regression; is the drug a significant predictor? (P_OR in $coefficients)
ds.glm()


## ----ex3----------------------------------------------------------------------
# STEP 1 — find the forearm-fracture concept_id
ds.omop.concept.search()

# STEP 2 — table with sex + age + the fracture flag, then a logistic model on age + sex
omop_recipe(
  variables = list(
    omop_variable(),
    omop_variable_age(),
    omop_variable()
  ),
  output = omop_output()
)
recipe_execute()
ds.glm()


## ----logout-------------------------------------------------------------------
DSI::datashield.logout(conns)

