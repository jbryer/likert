
<!-- README.md is generated from README.Rmd. Please edit that file -->

### Analysis and Visualization of Likert Based Items

<img src="man/figures/likert.png" align="right" width="120" />

**Author: [Jason Bryer, Ph.D.](mailto:jason@bryer.org)**  
**Website: <https://jbryer.github.io/likert/>**

<!-- badges: start -->

[![](https://www.r-pkg.org/badges/version/likert?color=orange)](https://cran.r-project.org/package=likert)
[![](https://img.shields.io/badge/devel%20version-2.0.1-blue.svg)](https://github.com/jbryer/likert)
[![R build
status](https://github.com/jbryer/likert/workflows/R-CMD-check/badge.svg)](https://github.com/jbryer/likert/actions)
![Downloads](http://cranlogs.r-pkg.org/badges/likert)
<!-- badges: end -->

`likert` is an R package designed to help analyzing and visualizing
Likert type items. More information can be obtained at
<https://jbryer.github.io/likert>. Also, the [included
demo](https://github.com/jbryer/likert/blob/master/demo/likert.R)
demonstrates many of the features.

Download the 2013 useR! Conference
[abstract](https://github.com/jbryer/likert/raw/master/slides/useR%202013/Abstract/Speerschneider.Bryer.likert.pdf)
and
[slides](https://github.com/jbryer/likert/raw/master/slides/useR%202013/Slides/Slides.pdf).

![Reading Attitude](https://bryer.org/images/likert/centeredPlot1.png)
![Reading Attitude with
Histogram](https://bryer.org/images/likert/centeredPlot2.png)

The latest development version can be downloaded using the `devtools`
package.

    remotes::install_github('jbryer/likert')

To get started take a look at the [likert
demo](https://github.com/jbryer/likert/blob/master/demo/likert.R) or
from within R:

    demo('likert', package='likert')

Or run the [Shiny app](http://rstudio.com/shiny):

\``shinyLikert()`
