### Analysis and Visualization of Likert Based Items

<img src="man/figures/likert.png" align="right" width="120" />

**Author: [Jason Bryer, Ph.D.](mailto:jason@bryer.org)**  
**Website: <https://jbryer.github.io/likert/>**

[![Build Status](https://api.travis-ci.org/jbryer/likert.svg)](https://travis-ci.org/jbryer/likert?branch=master)
[![CRAN_Status_Badge](http://www.r-pkg.org/badges/version/likert)](http://cran.r-project.org/package=likert)
![Downloads](http://cranlogs.r-pkg.org/badges/likert)

`likert` is an R package designed to help analyzing and visualizing Likert type items. More information can be obtained at https://jbryer.github.io/likert. Also, the [included demo](https://github.com/jbryer/likert/blob/master/demo/likert.R) demonstrates many of the features.

Download the 2013 useR! Conference [abstract](https://github.com/jbryer/likert/raw/master/slides/useR%202013/Abstract/Speerschneider.Bryer.likert.pdf) and [slides](https://github.com/jbryer/likert/raw/master/slides/useR%202013/Slides/Slides.pdf).

![Reading Attitude](http://bryer.org/images/likert/centeredPlot1.png)
![Reading Attitude with Histogram](http://bryer.org/images/likert/centeredPlot2.png)

The latest development version can be downloaded using the `devtools` package.

```
remotes::install_github('jbryer/likert')
```

To get started take a look at the [likert demo](https://github.com/jbryer/likert/blob/master/demo/likert.R) or from within R:

```
demo('likert', package='likert')
```

Or run the [Shiny app](http://rstudio.com/shiny):

```
shinyLikert()
``
