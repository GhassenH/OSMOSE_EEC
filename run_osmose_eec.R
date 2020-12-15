###########################################################################
# Script to run OSMOSE-EEC - V1.0 June 2020
# Ghassen Halouani
# OSMOSE 4.1
###########################################################################

# ## intall OSMOSE
# library(usethis)
# usethis::edit_r_environ()
# 
# library(devtools)
# install_github("osmose-model/osmose-private",   auth_token = "7ddaa9cb533e8a8eca0d883a378eee577a4aff31")



library("osmose")
library("beepr")

## Run the model
run_osmose("eec_all-parameters.csv", version = "4.3.0"); beep(3)

## Read OSMOSE-EEC configuration
osmConf = read_osmose(input="eec_all-parameters.csv")
summary(osmConf)

## Read OSMOSE-EEC outputs
outputs <- read_osmose("output")
plot(outputs)

sim = get_var(osmConf, what="simulation")
plot(osmConf, what="predation")
filename = system.file(package="osmose", "extdata", "master", "eec_all-parameters.csv")





demo = osmose_demo(path = "./", config="gog")

# run the osmose java core
run_osmose(demo$config_file)

