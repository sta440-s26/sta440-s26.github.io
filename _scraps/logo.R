# Minimal dependencies
library(ggplot2)

set.seed(1)

n <- 60

# Two groups
x1 <- rnorm(n, mean = 2, sd = 0.5)
y1 <- 0.6 * x1 + rnorm(n, sd = 0.3) + 1

x2 <- rnorm(n, mean = 6, sd = 0.5)
y2 <- 0.6 * x2 + rnorm(n, sd = 0.3) - 2

dat <- rbind(
  data.frame(x = x1, y = y1, g = "A"),
  data.frame(x = x2, y = y2, g = "B")
)

ggplot(dat, aes(x, y)) +
  geom_point(aes(color = g), size = 2, alpha = 0.9) +
  geom_smooth(
    aes(color = g),
    method = "lm",
    se = FALSE,
    linewidth = 1
  ) +
  geom_smooth(
    method = "lm",
    se = FALSE,
    color = "black",
    linewidth = 1.2
  ) +
  scale_color_manual(values = c("red", "blue")) +
  theme_void() +
  theme(legend.position = "none")
