

# Each prototype is a list with 4 data frames. Create 4 data frames, each of which has data for all prototypes.
protonames <- c("average", "underfunded")

getproto <- function(proto) readRDS(paste0(draw, paste0(proto, ".rds")))
biglist <- llply(protonames, getproto)

actives <- ldply(1:length(biglist), function(lnum) return(biglist[[lnum]]$actives))
retirees <- ldply(1:length(biglist), function(lnum) return(biglist[[lnum]]$retirees))
salgrowth.hist <- ldply(1:length(biglist), function(lnum) return(biglist[[lnum]]$salgrowth.hist))
salgrowth.assume <- ldply(1:length(biglist), function(lnum) return(biglist[[lnum]]$salgrowth.assume))

#****************************************************************************************************
#                    Now save the combined data frames ####
#****************************************************************************************************
use_data(actives, overwrite = TRUE)
use_data(retirees, overwrite = TRUE)
use_data(salgrowth.hist, overwrite = TRUE)
use_data(salgrowth.assume, overwrite = TRUE)


