## ----setup, include=FALSE-----------------------------------------------------
knitr::opts_chunk$set(comment = "#>", collapse = FALSE)
options(width = 200)


## ----libs---------------------------------------------------------------------
library(DSI); library(DSOpal); library(dsBaseClient); library(dsFlowerClient)


## ----login--------------------------------------------------------------------
builder <- DSI::newDSLoginBuilder()
for (h in c("nairobi", "dakar", "douala")) {
  builder$append(server = h, url = paste0("https://", h, ".datashield.live"),
                 user = "ethiopia", password = "P@ssw0rd")
}
conns <- DSI::datashield.login(logins = builder$build())

DSI::datashield.assign.table(conns, "D",  "dsflower_demo.breast_cancer")  # tumours (binary)
DSI::datashield.assign.table(conns, "G",  "dsflower_demo.digits")          # digits 0-9
DSI::datashield.assign.table(conns, "G4", "dsflower_demo.digits4")         # digits 0-3


## ----connect------------------------------------------------------------------
ds.flower.link.up(conns)


## ----bc-explore---------------------------------------------------------------
ds.dim("D", datasources = conns)                 # rows per site + pooled
ds.table("D$malignant", datasources = conns)     # class balance


## ----bc-features--------------------------------------------------------------
bc_features <- setdiff(ds.colnames("D", datasources = conns)[[1]], "malignant")
length(bc_features)   # 30 predictors


## ----bc-train, message=TRUE---------------------------------------------------
bc_fit <- ds.flower.fit(
  conns, symbol = "D", target = "malignant", features = bc_features,
  model = "sklearn_logreg", strategy = "fedavg",
  privacy = "clinical_default", rounds = 5L, verbose = TRUE
)


## ----bc-predict---------------------------------------------------------------
bc_new <- read.csv("https://raw.githubusercontent.com/isglobal-brge/workshop_DSWB/main/data/breast_cancer_new_patients.csv")
bc_prob <- ds.flower.predict(bc_fit, bc_new[, bc_features], type = "prob")

data.frame(
  actual      = bc_new$malignant,
  predicted   = as.integer(bc_prob >= 0.5),
  P_malignant = round(bc_prob, 3)
)


## ----digits-explore-----------------------------------------------------------
ds.dim("G", datasources = conns)
ds.table("G$digit", datasources = conns)   # ~180 of each digit, balanced


## ----digits-images, fig.width=9, fig.height=1.6-------------------------------
dg_new <- read.csv("https://raw.githubusercontent.com/isglobal-brge/workshop_DSWB/main/data/digits_new_samples.csv")
px <- grep("^px_", colnames(dg_new), value = TRUE)
op <- par(mfrow = c(1, 10), mar = c(0, 0, 1.2, 0))
for (i in 1:10) {
  img <- matrix(as.numeric(dg_new[i, px]), 8, 8, byrow = TRUE)
  image(t(img[8:1, ]), col = grey.colors(16, 1, 0), axes = FALSE)
  title(main = dg_new$digit[i], line = 0.2)
}
par(op)


## ----digits-features----------------------------------------------------------
digit_features <- setdiff(ds.colnames("G", datasources = conns)[[1]], c("id", "digit"))
length(digit_features)   # 64 pixels


## ----dg-logreg-train, message=TRUE--------------------------------------------
dg_lr <- ds.flower.fit(
  conns, symbol = "G", target = "digit", features = digit_features,
  model = "sklearn_logreg", strategy = "fedavg",
  privacy = "clinical_default", rounds = 5L, verbose = TRUE
)


## ----dg-nn-train, message=TRUE------------------------------------------------
dg_nn <- ds.flower.fit(
  conns, symbol = "G", target = "digit", features = digit_features,
  model = ds.flower.model.pytorch_multiclass(n_classes = 10L, hidden_layers = c(64L, 32L)),
  strategy = "fedavg", privacy = "clinical_default", rounds = 3L, verbose = TRUE
)


## ----dg-predict---------------------------------------------------------------
lr_pred <- unlist(ds.flower.predict(dg_lr, dg_new[, digit_features], type = "response"))
nn_pred <- unlist(ds.flower.predict(dg_nn, dg_new[, digit_features], type = "response"))

data.frame(actual = dg_new$digit, logreg = lr_pred, neural_net = nn_pred)
cat("logistic regression accuracy:", round(mean(lr_pred == dg_new$digit), 3),
    " | neural network accuracy:", round(mean(nn_pred == dg_new$digit), 3), "\n")


## ----xgb-explore--------------------------------------------------------------
ds.dim("G4", datasources = conns)
ds.table("G4$digit", datasources = conns)


## ----xgb-train, message=TRUE--------------------------------------------------
xgb_fit <- ds.flower.fit(
  conns, symbol = "G4", target = "digit", features = digit_features,
  model = ds.flower.model.xgboost(objective = "multi:softmax", num_class = 4L,
                                  n_trees = 8L, eta = 0.5),
  strategy = "fedavg", privacy = "clinical_default", rounds = 2L, verbose = TRUE
)


## ----xgb-predict--------------------------------------------------------------
xgb_new <- read.csv("https://raw.githubusercontent.com/isglobal-brge/workshop_DSWB/main/data/digits4_new_samples.csv")
xgb_pred <- unlist(ds.flower.predict(xgb_fit, xgb_new[, digit_features], type = "response"))

data.frame(actual = xgb_new$digit, predicted = xgb_pred)
cat("xgboost accuracy:", round(mean(xgb_pred == xgb_new$digit), 3), "\n")


## ----teardown-----------------------------------------------------------------
ds.flower.link.down(conns)
DSI::datashield.logout(conns)

