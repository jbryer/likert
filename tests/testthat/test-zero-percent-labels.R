test_that("bar plots suppress percent signs on blank zero labels", {
  answers <- c(
    "Strongly disagree",
    "Disagree",
    "Neutral",
    "Agree",
    "Strongly agree"
  )
  items <- data.frame(
    first = factor(answers[c(4, 5, 4, 4)], levels = answers),
    second = factor(answers[c(1, 2, 2, 3)], levels = answers)
  )

  plot_labels <- function(fit, centered, plot.percents, zero.print) {
    plot <- likert.bar.plot(
      fit,
      centered = centered,
      plot.percents = plot.percents,
      zero.print = zero.print
    )
    layers <- suppressWarnings(ggplot2::ggplot_build(plot)$data)
    unlist(lapply(layers, `[[`, "label"), use.names = FALSE)
  }

  for (grouping in list(NULL, c("A", "A", "B", "B"))) {
    fit <- likert(items, grouping = grouping)
    for (centered in c(TRUE, FALSE)) {
      for (plot.percents in c(TRUE, FALSE)) {
        visible <- plot_labels(fit, centered, plot.percents, TRUE)
        hidden <- plot_labels(fit, centered, plot.percents, FALSE)

        expect_true(any(visible == "0%"))
        expect_equal(length(hidden), length(visible))
        expect_equal(
          hidden[grepl("[^[:space:]]", hidden)],
          visible[visible != "0%"]
        )
      }
    }
  }

  fit <- likert(items)
  visible <- plot_labels(fit, TRUE, TRUE, TRUE)
  expect_equal(plot_labels(fit, TRUE, TRUE, NULL), visible)
  expect_equal(plot_labels(fit, TRUE, TRUE, "-"), sub("^0%$", "-%", visible))
})
