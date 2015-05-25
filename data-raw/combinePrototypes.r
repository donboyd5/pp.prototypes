


uf <- readRDS(paste0(draw, "underfunded.rds"))

str(uf)
uf$actives

#****************************************************************************************************
#                    Combine prototypes to create master dataframe of actives ####
#****************************************************************************************************

actives <- uf$actives
use_data(actives, overwrite = TRUE)

#****************************************************************************************************
#                    Combine prototypes to create master dataframe of retirees ####
#****************************************************************************************************
retirees <- uf$retirees
use_data(retirees, overwrite = TRUE)


#****************************************************************************************************
#                    Combine prototypes to create master dataframe of salary growth history ####
#****************************************************************************************************
salgrowth.hist <- uf$salgrowth.hist
use_data(salgrowth.hist, overwrite = TRUE)


