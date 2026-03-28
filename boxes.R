#' Script to find optimal box packing within a square

library(rectpacker)

cm <- 2.54


set.seed(1)
N <- 10
rect_widths <- c(21, 12,14, 19.5, 18.5, 18, 16, 21, 13.5, 11, 16.5, 21.5, 23, 21.5, 17, 9.5, 7, 9.5, 18, 14, 18.5, 19, 20, 17.5, 16.5)
rect_heights <- c(19.5, 17, 16, 18, 15, 18.5, 16.5, 16.5, 16.5, 16, 19, 17.5, 18.5, 17.5, 15, 18, 17.5, 18.5, 17.5, 15, 17, 16, 18, 18.5, 17.5)

rect_widths <- (rect_widths * cm) * 10
rect_heights <- (rect_heights * cm) * 10

rect_widths <- as.integer(round(rect_widths))
rect_heights <- as.integer(round(rect_heights))




box <- calc_small_box(rect_widths, rect_heights,
                      aspect_ratios = seq(0.5, 2, length.out = 20))
box

rectangles <- pack_rects(box$width, box$height, rect_widths, rect_heights)

rectangles <- as.data.frame(rectangles)

write.csv(rectangles, "rectangles.csv", row.names = FALSE)


library(readr)
library(dplyr)
library(ggplot2)

# Read your data
rects <- read.csv("rectangles.csv") %>%
  mutate(
    xmin = x,
    xmax = x + w,
    ymin = y,
    ymax = y + h,
    idx = idx + 1
  )

# Plot packed rectangles
ggplot(rects, aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax)) +
  geom_rect(aes(fill = factor(idx)), colour = "black", linewidth = 0.3, alpha = 0.85) +
  # label each rectangle in its center (optional)
  geom_text(
    aes(x = (xmin + xmax) / 2, y = (ymin + ymax) / 2, label = idx),
    size = 3, colour = "black"
  ) +
  coord_fixed() +
  labs(
    title = "Rectangle layout (from x/y coordinates)",
    x = "x (same units as w/h)",
    y = "y (same units as w/h)",
    fill = "idx"
  ) +
  theme_minimal()

length(rect_heights)




