## ----eval = FALSE-------------------------------------------------------------
# install.packages("devtools")
# library(devtools)
# devtools::session_info()


## -----------------------------------------------------------------------------
# libraries to connect with Opal databases
library(DSI)
library(DSOpal)

# DataSHIELD client libraries
library(dsBaseClient)
library(dsTidyverseClient)

# Library to perform meta-analyses
library(metafor)


## -----------------------------------------------------------------------------
devtools::session_info()


## ----insert_table_variables, echo=FALSE---------------------------------------
vars <- readr::read_delim("fig/table_variables_cnsim.txt", delim=",")
knitr::kable(vars)


## -----------------------------------------------------------------------------
builder <- DSI::newDSLoginBuilder()

# Server 1: France 
builder$append(
  server = 'France', 
  url = "https://opal-demo.obiba.org",
  user = "administrator", 
  password = "password",
  table = "CNSIM.CNSIM1",
  profile = "default"
)

# Server 2: Spain (ISGlobal)
builder$append(
  server = 'ISGlobal', 
  url = "https://opal.isglobal.org/repo",
  user = "invited", 
  password = "12345678Aa@",
  table = "CNSIM.CNSIM2",
  profile = "rock-inma"
)


## -----------------------------------------------------------------------------
logindata <- builder$build()
conns <- datashield.login(logins = logindata, assign = TRUE)


## -----------------------------------------------------------------------------
ds.ls()


## -----------------------------------------------------------------------------
ds.class(x = "D", datasources = conns)


## -----------------------------------------------------------------------------
ds.colnames("D")


## -----------------------------------------------------------------------------
ds.dim(x="D")


## -----------------------------------------------------------------------------
ds.class(x='D$LAB_HDL')
ds.length(x='D$LAB_HDL')
ds.mean(x='D$LAB_HDL')


## -----------------------------------------------------------------------------
?ds.quantileMean


## -----------------------------------------------------------------------------
ds.quantileMean(x='D$LAB_HDL')

ds.quantileMean(x='D$LAB_HDL', type = "split")


## -----------------------------------------------------------------------------
?ds.var


## -----------------------------------------------------------------------------
ds.var(x = 'D$LAB_HDL', type = "split")


## -----------------------------------------------------------------------------
a<-ds.var(x = 'D$LAB_HDL', type = "split")[[1]]
a
b<-ds.var(x = 'D$LAB_HDL', type = "split")[[1]][[1,1]]
b


## -----------------------------------------------------------------------------
ds.table("D$GENDER")


## -----------------------------------------------------------------------------
ds.mean("D$LAB_TRIG")
ds.var("D$LAB_TRIG")
ds.quantileMean("D$LAB_HDL")


## -----------------------------------------------------------------------------
ds.table("D$GENDER")
ds.table("D$DIS_CVA")
ds.table("D$DIS_DIAB")


## -----------------------------------------------------------------------------
for (v in c("LAB_TRIG", "LAB_HDL")) {
  print(ds.mean(paste0("D$", v)))
}


## -----------------------------------------------------------------------------
ds.ls()
ds.log(x='D$LAB_HDL', newobj='HDL_log')
ds.ls()
ds.mean(x="HDL_log")
ds.mean(x="D$LAB_HDL")


## -----------------------------------------------------------------------------
ds.sqrt(x='D$LAB_HDL', newobj='HDL_sqrt')
ds.ls()
ds.mean(x="HDL_sqrt")
ds.mean(x="D$LAB_HDL")


## -----------------------------------------------------------------------------
ds.dataFrame(c("D", "HDL_sqrt", "HDL_log"), newobj = "D")
ds.colnames("D")


## -----------------------------------------------------------------------------
# first find the column name you wish to refer to
ds.colnames(x="D")
# then check which levels you need to apply a boolean operator to:
ds.levels(x="D$GENDER")
?ds.dataFrameSubset


## -----------------------------------------------------------------------------
ds.dataFrameSubset(df.name = "D", V1.name = "D$GENDER", V2.name = "1", 
                   Boolean.operator = "==", newobj = "CNSIM.subset.Males")
ds.dataFrameSubset(df.name = "D", V1.name = "D$GENDER", V2.name = "0", 
                   Boolean.operator = "==", newobj = "CNSIM.subset.Females")


## -----------------------------------------------------------------------------
ds.completeCases(x1="D",newobj="D_without_NA")


## -----------------------------------------------------------------------------
ds.dataFrameSubset(df.name = "D",
  V1.name = "D$PM_BMI_CONTINUOUS",
  V2.name = "25",
  Boolean.operator = ">=",
  newobj = "subset.BMI.25.plus")


## -----------------------------------------------------------------------------
ds.quantileMean(x="subset.BMI.25.plus$PM_BMI_CONTINUOUS", type = "split")

ds.histogram(x="subset.BMI.25.plus$PM_BMI_CONTINUOUS")


## -----------------------------------------------------------------------------
ds.Boole(
  V1 = "D$PM_BMI_CONTINUOUS",
  V2 = "25",
  Boolean.operator = ">=",
  numeric.output = TRUE,
  newobj = "BMI.25.plus")

ds.Boole(
  V1 = "D$LAB_GLUC_ADJUSTED",
  V2 = "6",
  Boolean.operator = "<",
  numeric.output = TRUE,
  newobj = "GLUC.6.less")



## -----------------------------------------------------------------------------
?ds.make 

ds.make(toAssign = "BMI.25.plus+GLUC.6.less",
        newobj = "BMI.25.plus_GLUC.6.less")

# If BMI >= 25 and glucose < 6, then BMI.25.plus_GLUC.6.less=2
# If BMI >= 25 and glucose >= 6, then BMI.25.plus_GLUC.6.less=1
# If BMI < 25 and glucose < 6, then BMI.25.plus_GLUC.6.less=1
# If BMI < 25 and glucose >= 6, then BMI.25.plus_GLUC.6.less=0

ds.table(rvar= "BMI.25.plus_GLUC.6.less",
         datasources = conns)

ds.dataFrame(x=c("D", "BMI.25.plus_GLUC.6.less"), newobj = "D2")

ds.colnames("D2")

ds.dataFrameSubset(df.name = "D2",
  V1.name = "D2$BMI.25.plus_GLUC.6.less",
  V2.name = "2",
  Boolean.operator = "==",
  newobj = "subset2")

ds.dim("subset2")


## -----------------------------------------------------------------------------
ds.select(
  df.name = "D",
  tidy_expr = list(PM_BMI_CONTINUOUS, GENDER),
  newobj = "df_subset")

ds.colnames("df_subset")


## -----------------------------------------------------------------------------
ds.rename(
  df.name = "D",
  tidy_expr = list(bmi = PM_BMI_CONTINUOUS, sex = GENDER),
  newobj = "df_rename")

ds.colnames("df_rename")


## -----------------------------------------------------------------------------
?ds.histogram
ds.histogram(x='D$LAB_HDL')


## -----------------------------------------------------------------------------
ds.scatterPlot(x="D$LAB_HDL", y="D$PM_BMI_CONTINUOUS")


## -----------------------------------------------------------------------------
?ds.heatmapPlot
?ds.contourPlot
?ds.boxPlot


## -----------------------------------------------------------------------------
ds.cor(x='D$PM_BMI_CONTINUOUS', y='D$LAB_HDL')


## -----------------------------------------------------------------------------
ds.glm(formula = "D$LAB_HDL~D$PM_BMI_CONTINUOUS", 
       family="gaussian")


## -----------------------------------------------------------------------------
ds.glmSLMA(formula = "D$LAB_HDL~D$PM_BMI_CONTINUOUS", family="gaussian", 
           newobj = "workshop.obj")


## -----------------------------------------------------------------------------
ds.glmPredict(glmname = "workshop.obj", newobj = "workshop.prediction.obj")
ds.length("workshop.prediction.obj$fit", datasources=conns)
ds.length("D$LAB_HDL", datasources=conns)


## -----------------------------------------------------------------------------
ds.cbind(c('D$LAB_HDL', 'D$PM_BMI_CONTINUOUS'), newobj='vars')
ds.completeCases('vars', newobj='vars.complete')
ds.dim('vars.complete')


## -----------------------------------------------------------------------------
df1 <- ds.scatterPlot('D$PM_BMI_CONTINUOUS', "D$LAB_HDL", return.coords = TRUE)
df2 <- ds.scatterPlot('vars.complete$PM_BMI_CONTINUOUS', "workshop.prediction.obj$fit", 
                      return.coords = TRUE)
# then in native R
par(mfrow=c(2,2))
plot(as.data.frame(df1[[1]][[1]])$x,
     as.data.frame(df1[[1]][[1]])$y, xlab='Body Mass Index', ylab='HDL Cholesterol', main='Study 1')
lines(as.data.frame(df2[[1]][[1]])$x,as.data.frame(df2[[1]][[1]])$y, col='red')
plot(as.data.frame(df1[[1]][[2]])$x,as.data.frame(df1[[1]][[2]])$y, 
     xlab='Body Mass Index', ylab='HDL Cholesterol', main='Study 2')
lines(as.data.frame(df2[[1]][[2]])$x,as.data.frame(df2[[1]][[2]])$y, col='red')


## -----------------------------------------------------------------------------

glmslma <- ds.glmSLMA(formula = "vars.complete$LAB_HDL~vars.complete$PM_BMI_CONTINUOUS", family="gaussian", newobj = "workshop.obj")

ds.make(toAssign=paste0("(",glmslma$SLMA.pooled.ests.matrix[1,1],")+(", glmslma$SLMA.pooled.ests.matrix[2,1],"*vars.complete$PM_BMI_CONTINUOUS)"), 
        newobj = "predicted.values")

ds.make(toAssign = "vars.complete$LAB_HDL - predicted.values", 
        newobj = "residuals")

# and you can use those to run regression plot diagnostics  
ds.scatterPlot('predicted.values', "residuals")
ds.histogram("residuals")


## -----------------------------------------------------------------------------
ds.table("D$DIS_DIAB")


## -----------------------------------------------------------------------------
ds.class("D$DIS_DIAB")


## -----------------------------------------------------------------------------
glmSLMA_mod2<-ds.glmSLMA(formula="D$DIS_DIAB~D$PM_BMI_CONTINUOUS", family='binomial')


## -----------------------------------------------------------------------------
estimates <- c(glmSLMA_mod2$betamatrix.valid[2,])
se <- c(glmSLMA_mod2$sematrix.valid[2,])


## -----------------------------------------------------------------------------
res <- rma(estimates, sei=se)


## -----------------------------------------------------------------------------
forest(res, atransf=exp)


## -----------------------------------------------------------------------------
study_names <- c("France", "Spain")
weights <-  c(paste0(formatC(weights(res), format="f", digits=1, width=4), "%"))

forest(res, atransf=exp,
       xlab="Crude Odds Ratio", refline=log(1), xlim=c(-0.25,0.5), 
       at=log(c(0.95, 1, 1.1, 1.2, 1.3)),
       slab=cbind(paste0(study_names, " (", paste0(weights, ")"))), 
       mlab="RE model")
text(0.5, 4.5, pos=2, "Odds Ratio [95% CI]")
text(-0.25, 4.5, pos=4, "Study (weight)")


## -----------------------------------------------------------------------------
glm_mod1<-ds.glm(formula="D$DIS_DIAB~D$PM_BMI_CONTINUOUS+D$LAB_HDL*D$GENDER", family='binomial')


## -----------------------------------------------------------------------------
glmSLMA_mod2<-ds.glmSLMA(formula="D$DIS_DIAB~D$PM_BMI_CONTINUOUS+D$LAB_HDL*D$GENDER", family='binomial')


## -----------------------------------------------------------------------------
glm_mod1$coefficients
glmSLMA_mod2$SLMA.pooled.ests.matrix


## ----eval = FALSE-------------------------------------------------------------
# datashield.workspace_save(conns = conns, ws = "workspace2025")


## -----------------------------------------------------------------------------
datashield.logout(conns)


## ----eval = FALSE-------------------------------------------------------------
# conns <- datashield.login(logins = logindata,
#                           assign = TRUE, symbol = "D")
# ds.ls()
# datashield.logout(conns)
# 
# conns <- datashield.login(logins = logindata, restore = "workspace2025")
# ds.ls()

