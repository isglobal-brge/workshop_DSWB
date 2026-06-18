## ----setup, include=FALSE-----------------------------------------------------
# The code below is the real, runnable workflow. For this hand-out the chunks are
# shown but not executed live (each federated run takes a few minutes); the
# outputs printed underneath are from an actual run on the three-hospital
# federation. Download the .R to run it yourself.
knitr::opts_chunk$set(eval = FALSE, comment = "#>", collapse = FALSE)


## ----libs---------------------------------------------------------------------
library(DSI); library(DSOpal); library(dsBaseClient); library(dsFlowerClient)


## ----login--------------------------------------------------------------------
builder <- DSI::newDSLoginBuilder()
for (h in c("nairobi", "dakar", "douala")) {
  builder$append(server = h, url = paste0("https://", h, ".datashield.live"),
                 user = "ethiopia", password = "P@ssw0rd")
}
conns <- DSI::datashield.login(logins = builder$build())

DSI::datashield.assign.table(conns, "D", "dsflower_demo.breast_cancer")


## ----bc-dim-------------------------------------------------------------------
ds.dim("D", datasources = conns)              # rows per hospital + pooled total


## ----bc-balance---------------------------------------------------------------
ds.table("D$malignant", datasources = conns)  # benign (0) vs malignant (1)


## ----bc-sep-------------------------------------------------------------------
ds.asFactor("D$malignant", "mal_f", datasources = conns)
sep <- ds.meanSdGp(x = "D$worst_concave_points", y = "mal_f",
                   type = "combine", datasources = conns)
data.frame(
  diagnosis = c("benign", "malignant"),
  mean_worst_concave_points = round(as.numeric(sep$Mean_gp), 4),
  sd  = round(as.numeric(sep$StDev_gp), 4),
  n   = as.integer(sep$Nvalid_gp)
)


## ----bc-features--------------------------------------------------------------
bc_features <- setdiff(ds.colnames("D", datasources = conns)[[1]], "malignant")
length(bc_features)   # 30 predictors


## ----lr-up--------------------------------------------------------------------
ds.flower.link.up(conns)


## ----lr-train, message=TRUE---------------------------------------------------
fit_lr <- ds.flower.fit(
  conns, symbol = "D", target = "malignant", features = bc_features,
  model    = "sklearn_logreg",
  strategy = "fedavg",
  privacy  = "clinical_default",   # Secure Aggregation
  rounds   = 2L, verbose = TRUE
)
fit_lr


## ----lr-down------------------------------------------------------------------
ds.flower.link.down(conns)   # the link is only needed while training


## ----sgd-train, message=TRUE--------------------------------------------------
ds.flower.link.up(conns)

fit_sgd <- ds.flower.fit(
  conns, symbol = "D", target = "malignant", features = bc_features,
  model    = "sklearn_sgd",
  strategy = "fedavg",
  privacy  = "clinical_default",   # Secure Aggregation
  rounds   = 2L, verbose = TRUE
)
fit_sgd

ds.flower.link.down(conns)


## ----load-public--------------------------------------------------------------
bc <- read.csv("https://raw.githubusercontent.com/isglobal-brge/workshop_DSWB/main/data/breast_cancer_public.csv")
dim(bc)
table(diagnosis = ifelse(bc$malignant == 1, "malignant", "benign"))


## ----predict------------------------------------------------------------------
X <- bc[, bc_features]

p_lr  <- ds.flower.predict(fit_lr,  X, type = "prob")
p_sgd <- ds.flower.predict(fit_sgd, X, type = "prob")

head(data.frame(
  actual         = bc$malignant,
  P_malig_logreg = round(p_lr, 3),
  P_malig_sgd    = round(p_sgd, 3)
), 8)


## ----viz-sep, fig.width=8, fig.height=4---------------------------------------
op <- par(mfrow = c(1, 2))
for (m in list(c("Logistic regression", "p_lr"), c("SGD classifier", "p_sgd"))) {
  boxplot(get(m[2]) ~ bc$malignant, names = c("benign", "malignant"),
          col = c("#9ecae1", "#fc9272"), ylab = "predicted P(malignant)",
          xlab = "true diagnosis", main = m[1], ylim = c(0, 1))
}
par(op)


## ----validate-----------------------------------------------------------------
report <- function(prob, truth, name) {
  pred <- as.integer(prob >= 0.5)
  cm <- table(predicted = pred, actual = truth)
  cat("\n==", name, "==\n"); print(cm)
  cat(sprintf("accuracy: %.1f%%\n", 100 * mean(pred == truth)))
}
report(p_lr,  bc$malignant, "Logistic regression")
report(p_sgd, bc$malignant, "SGD classifier")


## ----cleanup------------------------------------------------------------------
DSI::datashield.logout(conns)

