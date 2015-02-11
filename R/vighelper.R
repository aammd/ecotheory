library(devtools)
build_vignettes()
build()
install()

## see list of vignettes in the browser
browseVignettes(package = "ecotheory")

## see one vignette in the browser
vignette("Box1")

## see code in a little popup window
library("ecotheory")
edit(vignette("Box1"))

