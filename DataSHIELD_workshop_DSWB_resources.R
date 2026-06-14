## ----cnsim_multiple-----------------------------------------------------------
library(DSOpal)
library(dsBaseClient)

# prepare login data and resources to assign
builder <- DSI::newDSLoginBuilder()
builder$append(server = "study1", url = "https://opal-demo.obiba.org", 
               user = "dsuser", password = "P@ssw0rd", 
               resource = "RSRC.CNSIM1", profile = "default")
#builder$append(server = "study2", url = "https://opal.isglobal.org/repo",
#               user = "invited",  password = "12345678Aa@",, 
#               resource = "CNSIM.CNSIM2", profile = "rock-inma")

logindata <- builder$build()

# login and assign resources
conns <- datashield.login(logins = logindata, assign = TRUE, symbol = "res")

# assigned objects are of class ResourceClient (and others)
ds.class("res")

# coerce ResourceClient objects to data.frames
# (DataSHIELD config allows as.resource.data.frame() assignment function for the purpose of the demo)
datashield.assign.expr(conns, symbol = "D", 
                       expr = quote(as.resource.data.frame(res, strict = TRUE)))
ds.class("D")
ds.colnames("D")

# do usual dsBase analysis
ds.summary('D$LAB_HDL')

# vector types are not necessarily the same depending on the data reader that was used
ds.class('D$GENDER')
ds.asFactor('D$GENDER', 'GENDER')
ds.summary('GENDER')

mod <- ds.glm("DIS_DIAB ~ LAB_TRIG + GENDER", data = "D" , family="binomial")
mod$coeff


datashield.logout(conns)


## ----install_dsSurvivalClient, eval=FALSE-------------------------------------
# devtools::install_github("neelsoumya/dsSurvivalClient")

