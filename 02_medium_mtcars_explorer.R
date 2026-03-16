if ("package:ggplot2" %in% search()) {
  detach("package:ggplot2", unload = TRUE)
}
library(animint2)

cars <- mtcars
cars$model <- rownames(mtcars)
cars$cylgroup <- factor(cars$cyl)
cars <- cars[order(cars$wt), ]
cars$frame <- seq_len(nrow(cars))

carscum <- do.call(rbind, lapply(cars$frame, function(frameid) {
  current <- cars[1:frameid, ]
  current$frame <- frameid
  current
}))

counter <- data.frame(
  frame = cars$frame,
  x = 4.8,
  y = 34,
  label = paste("Cars shown:", cars$frame)
)

plot <- ggplot() +
  geom_point(
    data = carscum,
    aes(wt, mpg, color = cylgroup, key = cylgroup),
    showSelected = "frame",
    clickSelects = "cylgroup",
    size = 3,
    alpha = 0.85,
    alpha_off = 0.15
  ) +
  geom_text(
    data = carscum,
    aes(wt, mpg, label = model, key = model),
    showSelected = c("frame", "cylgroup"),
    hjust = 0,
    vjust = 0,
    size = 2.8
  ) +
  geom_text(
    data = counter,
    aes(x = x, y = y, label = label, key = 1),
    showSelected = "frame",
    inherit.aes = FALSE,
    size = 5,
    fontface = "bold"
  ) +
  theme_bw() +
  theme(legend.position = "bottom") +
  labs(
    title = "Car Weight vs Mileage",
    x = "Weight (1000 lbs)",
    y = "Miles per Gallon",
    color = "Cylinders"
  )

viz <- animint(
  cars = plot + theme_animint(width = 900, height = 560),
  time = list(variable = "frame", ms = 170),
  first = list(frame = 1)
)

animint2dir(viz, out.dir = "medium-mtcars-explorer", open.browser = FALSE)
