## ----libraries, message=FALSE-------------------------------------------------
library(DSI)
library(DSOpal)
library(dsBaseClient)
library(dsOMOPClient)
library(dsOMOPHelper)
library(dsTidyverseClient)


## ----connection---------------------------------------------------------------
builder <- newDSLoginBuilder()
builder$append(server="opal-demo",
              url="https://opal-demo.obiba.org/",
              user="dsuser",
              password="P@ssw0rd",
              driver = "OpalDriver",
              profile = "omop")

logindata <- builder$build()
conns <- datashield.login(logins=logindata)


## ----setup--------------------------------------------------------------------
helper <- ds.omop.helper(
    connections = conns,
    resource = "omop_demo.mimiciv", 
    symbol = "mimiciv"
)


## ----ls-----------------------------------------------------------------------
ds.ls()


## ----summary------------------------------------------------------------------
ds.summary("mimiciv")


## ----tables-------------------------------------------------------------------
helper$tables()


## ----columns_condition_occurrence---------------------------------------------
helper$columns("condition_occurrence")


## ----columns_measurement------------------------------------------------------
helper$columns("measurement")


## ----columns_observation------------------------------------------------------
helper$columns("observation")


## ----concepts_condition_occurrence--------------------------------------------
condition_list <- helper$concepts("condition_occurrence")
condition_list


## ----concepts_measurement-----------------------------------------------------
measurement_list <- helper$concepts("measurement")
measurement_list


## ----concepts_observation-----------------------------------------------------
observation_list <- helper$concepts("observation")
observation_list


## ----save_concepts, eval=FALSE------------------------------------------------
# # Create a data directory if it doesn't exist
# dir.create("data", showWarnings = FALSE)
# 
# # Save the concept catalogs as CSV files
# write.csv(condition_list, file = "data/condition_list.csv")
# write.csv(measurement_list, file = "data/measurement_list.csv")
# write.csv(observation_list, file = "data/observation_list.csv")


## ----columns_measurement_2----------------------------------------------------
helper$columns("measurement")


## ----auto---------------------------------------------------------------------
helper$auto(
    table = "measurement",
    concepts = 3004249,
    columns = "value_as_number"
)


## ----summary_2----------------------------------------------------------------
ds.summary("mimiciv")


## ----data---------------------------------------------------------------------
ds.summary("mimiciv$systolic_blood_pressure.value_as_number")


## ----rename_sbp, warning=FALSE------------------------------------------------
ds.rename(
  df.name = "mimiciv",
  tidy_expr = list(sbp = systolic_blood_pressure.value_as_number),
  newobj = "mimiciv")


## ----colnames-----------------------------------------------------------------
ds.colnames("mimiciv")


## ----histogram_sbp------------------------------------------------------------
ds.histogram("mimiciv$sbp")


## ----columns_observation_2----------------------------------------------------
helper$columns("observation")


## ----auto_marital_status------------------------------------------------------
helper$auto(
    table = "observation",
    concepts = 40766231,
    columns = "value_as_concept_id"
)


## ----summary_3----------------------------------------------------------------
ds.summary("mimiciv")


## ----summary_marital_status---------------------------------------------------
ds.summary("mimiciv$marital_status_nhanes.value_as_concept_id")


## ----colnames_3---------------------------------------------------------------
ds.colnames("mimiciv")


## ----summary_gender-----------------------------------------------------------
ds.summary("mimiciv$gender_concept_id")


## ----rename_gender, warning=FALSE---------------------------------------------
ds.rename(
  df.name = "mimiciv",
  tidy_expr = list(gender = gender_concept_id),
  newobj = "mimiciv")


## ----filter_gender------------------------------------------------------------
ds.make(newobj = "gender_filter", toAssign = "c('female')")


## ----subset_gender------------------------------------------------------------
ds.dataFrameSubset(
  df.name = "mimiciv",
  V1.name = "mimiciv$gender",
  V2.name = "gender_filter",
  Boolean.operator = "==",
  newobj = "mimiciv",
  datasources = conns,
  notify.of.progress = FALSE
)


## ----summary_female-----------------------------------------------------------
ds.summary("mimiciv$gender")


## ----helper_2-----------------------------------------------------------------
helper <- ds.omop.helper(
    connections = conns,
    resource = "omop_demo.mimiciv", 
    symbol = "mimiciv"
)


## ----summary_4----------------------------------------------------------------
ds.summary("mimiciv")


## ----summary_female_2---------------------------------------------------------
ds.summary("mimiciv$gender_concept_id")


## ----concepts_condition_occurrence_2------------------------------------------
condition_list


## ----concepts_observation_2---------------------------------------------------
observation_list


## ----concepts_measurement_2---------------------------------------------------
measurement_list


## ----auto_copd----------------------------------------------------------------
helper$auto(
    table = "condition_occurrence",
    concepts = c(255573, 317009),
    columns = "condition_occurrence_id"
)


## ----auto_tobacco-------------------------------------------------------------
helper$auto(
    table = "observation",
    concepts = 4005823,
    columns = "observation_id"
)


## ----summary_5----------------------------------------------------------------
ds.summary("mimiciv")


## ----transform_copd-----------------------------------------------------------
# Convert COPD ID to numeric
ds.asNumeric(
    x.name = "mimiciv$chronic_obstructive_lung_disease.condition_occurrence_id",
    newobj = "copd_numeric",
    datasources = conns
)

# Convert numeric COPD to boolean
ds.Boole(
    V1 = "copd_numeric",
    V2 = 0,
    Boolean.operator = "!=",
    numeric.output = TRUE,
    na.assign = 0,
    newobj = "copd",
    datasources = conns
)


## ----table_copd_boolean-------------------------------------------------------
ds.table("copd")


## ----transform_tobacco--------------------------------------------------------
# Convert tobacco ID to numeric
ds.asNumeric(
    x.name = "mimiciv$tobacco_user.observation_id",
    newobj = "tobacco_numeric",
    datasources = conns
)

# Convert numeric tobacco to boolean 
ds.Boole(
    V1 = "tobacco_numeric",
    V2 = 0,
    Boolean.operator = "!=",
    numeric.output = TRUE,
    na.assign = 0,
    newobj = "tobacco",
    datasources = conns
)


## ----transform_asthma---------------------------------------------------------
# Convert asthma ID to numeric 
ds.asNumeric(
    x.name = "mimiciv$asthma.condition_occurrence_id",
    newobj = "asthma_numeric", 
    datasources = conns
)

# Convert numeric asthma to boolean
ds.Boole(
    V1 = "asthma_numeric",
    V2 = 0,
    Boolean.operator = "!=", 
    numeric.output = TRUE,
    na.assign = 0,
    newobj = "asthma",
    datasources = conns
)


## ----table_tobacco_boolean----------------------------------------------------
ds.table("tobacco")


## ----table_asthma_boolean-----------------------------------------------------
ds.table("asthma")


## ----glm_copd_tobacco---------------------------------------------------------
ds.glm(
    formula = "copd ~ tobacco + asthma",
    family = "binomial",
    datasources = conns
)


## ----logout-------------------------------------------------------------------
datashield.logout(conns)

