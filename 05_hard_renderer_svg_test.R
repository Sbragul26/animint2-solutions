if ("package:ggplot2" %in% search()) {
  detach("package:ggplot2", unload = TRUE)
}

packages <- c("animint2", "xml2")
missing <- packages[!vapply(packages, requireNamespace, logical(1), quietly = TRUE)]

if (length(missing) > 0) {
  message("Missing packages: ", paste(missing, collapse = ", "))
  message("Install first, then re-run this script.")
  quit(save = "no", status = 0)
}

library(animint2)
library(xml2)

set.seed(42)

steps <- 1:24
groups <- c("ModelA", "ModelB", "ModelC")

data <- expand.grid(
  step = steps,
  grp = groups,
  KEEP.OUT.ATTRS = FALSE,
  stringsAsFactors = FALSE
)

data$grp <- factor(data$grp, levels = groups)
data$value <- with(
  data,
  ifelse(
    grp == "ModelA",
    10 + 0.6 * step + sin(step / 2),
    ifelse(
      grp == "ModelB",
      8 + 0.9 * step + cos(step / 3),
      12 + 0.4 * step + sin(step / 4)
    )
  )
)

data$label <- paste0(data$grp, " | step ", data$step)

counter <- data.frame(
  step = steps,
  x = 3,
  y = max(data$value) + 1,
  label = paste("Current step:", steps)
)

plot <- ggplot(data, aes(step, value)) +
  geom_line(
    aes(group = grp, key = grp),
    clickSelects = "grp",
    showSelected = "grp",
    size = 1,
    color = "#2c7fb8"
  ) +
  geom_point(
    aes(key = label),
    showSelected = c("step", "grp"),
    size = 2.8,
    alpha = 0.85,
    color = "#2c7fb8"
  ) +
  geom_text(
    aes(label = label, key = label),
    showSelected = c("step", "grp"),
    hjust = 0,
    vjust = 0,
    size = 2.8,
    color = "#2c7fb8"
  ) +
  geom_text(
    data = counter,
    aes(x = x, y = y, label = label, key = 1),
    showSelected = "step",
    inherit.aes = FALSE,
    size = 5,
    fontface = "bold"
  ) +
  theme_bw() +
  theme(legend.position = "bottom") +
  labs(
    title = "Hard Test: Renderer + SVG Validation",
    x = "Step",
    y = "Value"
  )

viz <- animint(
  hardcheck = plot + theme_animint(width = 900, height = 520),
  time = list(variable = "step", ms = 220),
  first = list(step = 1, grp = "ModelA")
)

animint2dir(viz, out.dir = "hard-renderer-svg-test", open.browser = FALSE)
indexpath <- file.path("hard-renderer-svg-test", "index.html")

if (!file.exists(indexpath)) {
  stop("Hard test failed: index.html was not generated.")
}

doc <- read_html(indexpath)
scripts <- xml_find_all(doc, ".//script")

if (length(scripts) < 1) {
  stop("Hard test failed: no script tags found in generated index.html.")
}

message("Hard test passed: script tags found = ", length(scripts))
message("Generated directory: hard-renderer-svg-test")
message("Generated HTML file: hard-renderer-svg-test/index.html")
