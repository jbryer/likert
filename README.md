### Analysis and Visualization of Likert Based Items

[![Build Status](https://travis-ci.org/jbryer/likert?branch=master)](https://travis-ci.org/jbryer/likert)
[![CRAN_Status_Badge](http://www.r-pkg.org/badges/version/likert)](http://cran.r-project.org/package=likert)


Jason Bryer [jason@bryer.org](mailto:jason@bryer.org)  
Kim Speerschneider [kimkspeer@gmail.com](mailto:kimkspeer@gmail.com)

`likert` is an R package designed to help analyzing and visualizing Likert type items. More information can be optained at http://jason.bryer.org/likert. Also, the [included demo](https://github.com/jbryer/likert/blob/master/demo/likert.R) demonstrates many of the features.

Download the 2013 useR! Conference [abstract](https://github.com/jbryer/likert/blob/master/useR%202013/Abstract/Speerschneider.Bryer.likert.pdf?raw=true) and [slides](https://github.com/jbryer/likert/blob/master/useR%202013/Slides/Slides.pdf?raw=true).

![Reading Attitude](http://jason.bryer.org/images/likert/centeredPlot1.png)
![Reading Attitude with Histogram](http://jason.bryer.org/images/likert/centeredPlot2.png)

The latest development version can be downloaded using the `devtools` package.

	require(devtools)
	install_github('likert','jbryer')

To get started take a look at the [likert demo](https://github.com/jbryer/likert/blob/master/demo/likert.R) or from within R:

	demo('likert', package='likert')
	
Or run the [Shiny app](http://rstudio.com/shiny):

	shinyLikert()

