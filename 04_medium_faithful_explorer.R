if ("package:ggplot2" %in% search()) {
  detach("package:ggplot2", unload = TRUE)
}
library(animint2)

data <- CO2
data$plant <- as.character(data$Plant)
data$type <- factor(data$Type)
data$treat <- factor(data$Treatment)
steps <- sort(unique(data$conc))
data$step <- match(data$conc, steps)

plants <- data
plants$top <- plants$uptake + 2

types <- aggregate(uptake ~ type + step + conc, data, mean)
types$top <- types$uptake + 1.5

treats <- aggregate(uptake ~ treat + step + conc, data, mean)
treats$top <- treats$uptake + 1.5

note <- data.frame(
  step = sort(unique(data$step)),
  x = min(data$conc) + 50,
  y = max(data$uptake) + 4
)
note$label <- paste("Concentration:", steps)

curve <- ggplot() +
  geom_line(
    data = data,
    aes(conc, uptake, group = plant, color = type, key = plant),
    alpha = 0.25,
    size = 0.8
  ) +
  geom_line(
    data = data,
    aes(conc, uptake, group = plant, key = plant),
    showSelected = "plant",
    color = "black",
    size = 1.2
  ) +
  geom_point(
    data = data,
    aes(conc, uptake, color = treat, key = plant),
    showSelected = "step",
    clickSelects = "plant",
    size = 2.8,
    alpha = 0.9,
    alpha_off = 0.1
  ) +
  geom_text(
    data = plants,
    aes(conc, top, label = plant, key = plant),
    showSelected = c("step", "plant"),
    size = 2.8
  ) +
  geom_text(
    data = note,
    aes(x = x, y = y, label = label, key = 1),
    showSelected = "step",
    inherit.aes = FALSE,
    size = 5,
    fontface = "bold"
  ) +
  guides(color = "none") +
  theme_bw() +
  labs(
    title = "CO2 Uptake Explorer",
    x = "Concentration",
    y = "Uptake"
  )

typeplot <- ggplot() +
  geom_bar(
    data = types,
    aes(type, uptake, fill = type, key = type),
    stat = "identity",
    position = "identity",
    showSelected = "step",
    width = 0.7,
    alpha = 0.9
  ) +
  geom_text(
    data = types,
    aes(type, top, label = round(uptake, 1), key = type),
    showSelected = "step",
    size = 3.5
  ) +
  guides(fill = "none") +
  theme_bw() +
  labs(
    title = "Average Uptake by Type",
    x = "Type",
    y = "Average Uptake"
  )

treatplot <- ggplot() +
  geom_bar(
    data = treats,
    aes(treat, uptake, fill = treat, key = treat),
    stat = "identity",
    position = "identity",
    showSelected = "step",
    width = 0.7,
    alpha = 0.9
  ) +
  geom_text(
    data = treats,
    aes(treat, top, label = round(uptake, 1), key = treat),
    showSelected = "step",
    size = 3.5
  ) +
  guides(fill = "none") +
  theme_bw() +
  labs(
    title = "Average Uptake by Treatment",
    x = "Treatment",
    y = "Average Uptake"
  )

viz <- animint(
  curve = curve + theme_animint(width = 900, height = 420),
  typeplot = typeplot + theme_animint(width = 520, height = 320),
  treatplot = treatplot + theme_animint(width = 520, height = 320),
  time = list(variable = "step", ms = 700),
  first = list(step = min(data$step), plant = data$plant[1])
)

animint2dir(viz, out.dir = "medium-co2-explorer", open.browser = FALSE)
