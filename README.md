# Shiny apps

In this repository there are Shiny apps for different purposes. For further information on the different apps read the README-file in the particular directory.

To run the examples locally, you can install the **shiny** package in R, and
use the function `runGitHub()`. For example, to run the example `SPIN-analysis`:

```R
if (!require('shiny')) install.packages("shiny")
shiny::runGitHub("ShinyApps", "jasperschalla", subdir = "SPIN-analysis")
```

Or you can clone or download this repository, and use run
`shiny::runApp("SPIN-analysis")`. Alternatively, [SPIN-analysis](https://umwi.shinyapps.io/SPIN-analysis/) and [PRODCOM-analysis](https://umwi.shinyapps.io/PRODCOM-analysis/) can be run on the shiny server without the need for a local installation of R. However, the memory on the server is limited which can lead to problems with large data amounts.

