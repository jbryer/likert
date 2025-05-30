likert 2.0.1
=========

* Documentation fixes per CRAN request.
* likert.density.plot has been deprecated.
* Added likert_anchors dataset and vignette.

likert 2.0
=========

* Many fixed to be compatible with the latest version of ggplot2.
* Added digits parameter to plot for heatmaps.
* Now check to ensure grouping variable does not have any missing values.
* Print a warning if any variables are not factors.
* New Shiny app that allows for interactively manipulate Likert options.

likert 1.3.6
=========

* Fixed a bug where the labels were placed on the wrong side.
* Now require ggplot2 versions greater than 2.2.0.

likert 1.3.5
=========

* Fixed a bug where stacked bars were in the wrong order in grouped plots.
* Plotting options are now passed as a list. The likert.options returns the default set of plotting options.

likert 1.3.4
=========

* Fixed a bug where the neutral labels would be printed twice when there were an odd number of levels and plot.percents = TRUE.
* Fixed a bug where the results for a grouped analysis with missing levels may not be correct in the print function.

likert 1.3.3
=========

* Update for ggplot2 version 2.0

likert 1.3.2
=========

* Removed all use of the reshape package.
* Fixes issue 40 where the histogram bars may not match the likert bars.

likert 1.3.1
=========

* Fix to work with the latest version of the xtable package.

likert 1.3.0
=========

* Added ability to group items by factor.
* Converted all uses of reshape to reshape2.
* Other fixes to pass R CMD CHECK.

likert 1.2.1
=========

* Added colors parameter to plot.likert.bar to define custom colors for levels.
* Fixed bug where an unused factor level would not appear in the legend.

likert 1.2
=========

* Added initial support for conducting gap analysis. A demo, GapAnalysis has
  also been added.
* Removed dependency on a non-exported function in ggplot2.
* Can now create a likert object with a pre-summarized data frame.
* New Shiny app to demonstrate many of the features in the likert package. The
  shinyLikert() function will start the app.
* Fixed bug where text.color parameter was not being used.
* Fixed bug 16.
* Fixed bug where automatic conversion to factors would not work if there were
  missing levels in an item.
* Added text labels to the histogram (if include.histogram=TRUE) for the n.
* Fixed a bug where plot.likert(..., include.histogram=TRUE) would not work with a group and only one item (i.e. one column).
* Updates for newer version of R and Roxygen.

likert 1.1
=========

Released September 25, 2013.

* New density plot type plot(l, type='density').
* xtable function to create LaTeX and HTML tables.
* Added several parameters related to centering bar plots. Can now include the
  center level and split across zero, or specify which level, or between which
  levels, centering occurs.
* Plot a histogram of responses (n) alongside the barplot.
* Summary function now includes neutral column. Values will be NA if no level
  is the center.
* If there is a neutral category in the bar plot it will be labeled with its
  percentage.
* Added parameter to wrap text in plots for long item names.
* Changed the default colors.
* Added parameter to change legend title.
* Can now plot bar plot panels horizontally.
* Changed the center=TRUE as the default for bar plots to.
* Added panel.flip to flip the placement of grouping elements and items.
* Added legend.position parameter.
* Added panel.strip.color to change the background color for panel labels.
* Added group.order to reorder how groups are plotted. This also works for items in
  non-grouped items.
* Added parameters to turn off percent labels for low, high, and netural separately.
* Added a check to ensure all factors have the same number of levels.
* Items (i.e columns) that are not a factor will be converted to a factor using as.factor.
* Added reverse.levels function to reverse code factors. Useful when you wish to plot
  bars in reverse order (left-to-right).
* Two new demos, RecodeFactors and UnusedLevels show how to deal with common data
  issues before using the likert function.
* Added plot.percents parameter to label each category.
* Other bug fixes.
