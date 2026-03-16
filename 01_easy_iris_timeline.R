if ("package:ggplot2" %in% search()) {
  detach("package:ggplot2", unload = TRUE)
}
library(animint2)

data <- ChickWeight
data$day <- data$Time
data$chick <- as.character(data$Chick)
data$diet <- factor(data$Diet)

bars <- aggregate(weight ~ diet + day, data, mean)
bars$top <- bars$weight + 10

marks <- data
marks$top <- marks$weight + 8

note <- data.frame(
  day = sort(unique(data$day)),
  x = 2,
  y = max(data$weight) + 25
)
note$label <- paste("Day:", note$day)

growth <- ggplot() +
  geom_line(
    data = data,
    aes(day, weight, group = chick, color = diet, key = chick),
    alpha = 0.25,
    size = 0.8
  ) + geom_line(
    data = data,
    aes(day, weight, group = chick, key = chick),
    showSelected = "chick",
    color = "black",
    size = 1.2
  ) + geom_point(
    data = data,
    aes(day, weight, color = diet, key = chick),
    showSelected = c("day", "diet"),
    clickSelects = "chick",
    size = 2.5,
    alpha = 0.9,
    alpha_off = 0.1
  ) + geom_text(
    data = marks,
    aes(day, top, label = chick, key = chick),
    showSelected = c("day", "chick"),
    size = 3
  ) + geom_text(
    data = note,
    aes(x = x, y = y, label = label, key = 1),
    showSelected = "day",
    inherit.aes = FALSE,
    size = 5,
    fontface = "bold"
  ) + guides(color = "none") +
  theme_bw() +
  labs(
    title = "Chick Weight Growth Explorer",
    x = "Day",
    y = "Weight"
  )

dietplot <- ggplot() +
  geom_bar(
    data = bars,
    aes(diet, weight, fill = diet, key = diet),
    stat = "identity",
    position = "identity",
    showSelected = "day",
    clickSelects = "diet",
    width = 0.7,
    alpha = 0.9
  ) + geom_text(
    data = bars,
    aes(diet, top, label = round(weight, 1), key = diet),
    showSelected = "day",
    size = 4
  ) + guides(fill = "none") +
  theme_bw() +
  labs(
    title = "Average Weight by Diet",
    x = "Diet",
    y = "Average Weight"
  )

viz <- animint(
  growth = growth + theme_animint(width = 900, height = 420),
  dietplot = dietplot + theme_animint(width = 500, height = 320),
  time = list(variable = "day", ms = 600),
  first = list(day = min(data$day), diet = levels(data$diet)[1], chick = data$chick[1])
)

animint2dir(viz, out.dir = "easy-chickweight-explorer", open.browser = FALSE)
