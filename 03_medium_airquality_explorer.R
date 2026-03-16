if ("package:ggplot2" %in% search()) {
  detach("package:ggplot2", unload = TRUE)
}

library(animint2)
data <- Orange
data$tree <- as.character(data$Tree)
ages <- sort(unique(data$age))
data$step <- match(data$age, ages)

bars <- data
bars$tree <- factor(bars$tree, levels = unique(bars$tree))
bars$top <- bars$circumference + 10

mean <- aggregate(circumference ~ step + age, data, mean)
mean$top <- mean$circumference + 12

note <- data.frame(
  step = sort(unique(data$step)),
  x = min(data$age) + 50,
  y = max(data$circumference) + 20
)
note$label <- paste("Age:", ages)

growth <- ggplot() +
  geom_line(
    data = data,
    aes(age, circumference, group = tree, color = tree, key = tree),
    alpha = 0.3,
    size = 0.8
  ) + geom_line(
    data = data,
    aes(age, circumference, group = tree, key = tree),
    showSelected = "tree",
    color = "black",
    size = 1.3
  ) +
  geom_point(
    data = data,
    aes(age, circumference, color = tree, key = tree),
    showSelected = c("step", "tree"),
    clickSelects = "tree",
    size = 3,
    alpha = 0.9,
    alpha_off = 0.1
  ) +  geom_text(
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
    title = "Orange Tree Growth Explorer",
    x = "Age",
    y = "Circumference"
  )

treeplot <- ggplot() +
  geom_bar(
    data = bars,
    aes(tree, circumference, fill = tree, key = tree),
    stat = "identity",
    position = "identity",
    showSelected = "step",
    clickSelects = "tree",
    width = 0.7,
    alpha = 0.9
  ) + geom_text(
    data = bars,
    aes(tree, top, label = round(circumference, 1), key = tree),
    showSelected = "step",
    size = 3.5
  ) + guides(fill = "none") +
  theme_bw() +
  labs(
    title = "Current Size by Tree",
    x = "Tree",
    y = "Circumference"
  )

meanplot <- ggplot() +
  geom_line(
    data = mean,
    aes(age, circumference, group = 1, key = 1),
    color = "#2c7fb8",
    size = 1.1
  ) + geom_point(
    data = mean,
    aes(age, circumference, key = 1),
    showSelected = "step",
    color = "#2c7fb8",
    size = 3
  ) + geom_text(
    data = mean,
    aes(age, top, label = round(circumference, 1), key = 1),
    showSelected = "step",
    size = 3.5
  ) + theme_bw() + labs(
    title = "Average Size Over Time",
    x = "Age",
    y = "Average Circumference"
  )

viz <- animint(
  growth = growth + theme_animint(width = 900, height = 420),
  treeplot = treeplot + theme_animint(width = 520, height = 320),
  meanplot = meanplot + theme_animint(width = 520, height = 320),
  time = list(variable = "step", ms = 800),
  first = list(step = min(data$step), tree = data$tree[1])
)

animint2dir(viz, out.dir = "medium-orange-explorer", open.browser = FALSE)
