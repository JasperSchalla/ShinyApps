# Shiny Apps
============

In this repository there are Shiny-Apps for different purposes. For further information on the different apps read the README-file in the particular directory.

To run the examples locally, you can install the **shiny** package in R, and
use the function `runGitHub()`. For example, to run the example `SPIN-analysis`:

```R
if (!require('shiny')) install.packages("shiny")
shiny::runGitHub("ShinyApps", "JasperSchalla", subdir = "SPIN-analysis")
```

Or you can clone or download this repository, and use run
`shiny::runApp("SPIN-analysis")`.

