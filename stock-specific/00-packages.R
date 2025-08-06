## check for missing packages and install if needed

if(!require(tidyverse))install.packages("tidyverse")
if(!require(sdmTMB))install.packages("sdmTMB", dependencies = TRUE)

if(!require(patchwork))install.packages("patchwork")
if(!require(aplot))install.packages("aplot") # for working with lists of plots
if(!require(gridGraphics))install.packages("gridGraphics")

if(!require(future))install.packages("future") # option for parallel running of condition models
if(!require(remotes))install.packages("remotes")

if(!require(gfdata))remotes::install_github("pbs-assess/gfdata")
if(!require(gfplot))remotes::install_github("pbs-assess/gfplot")

if(!require(ggsidekick))remotes::install_github("seananderson/ggsidekick")
if(!require(ggeffects))remotes::install_github("seananderson/ggeffects", ref = "sdmTMB")

# rosettafish for french translation of figures
# may need to be updated to contain species name or other stock lables before using
# if so may be better to install from a local clone, but if git version has been updated then this works
if(!require(rosettafish))remotes::install_github("pbs-assess/rosettafish")

