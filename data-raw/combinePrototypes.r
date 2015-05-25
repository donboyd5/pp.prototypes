


uf <- readRDS(paste0(draw, "underfunded.rds"))

str(uf)
uf$actives

#****************************************************************************************************
#                    Master dataframe of actives ####
#****************************************************************************************************

actives <- uf$actives


use_data(actives, overwrite = TRUE)
