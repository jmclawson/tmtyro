# "tmtyro" |>
#   strsplit("") |>
#   unlist() |>
#   data.frame() |>
#   setNames("letter") |>
#   mutate(
#     x = row_number()/1.5,
#     y = runif(min = 1, max = 2, n = 6)) |>
#   ggplot(aes(x, y,
#              color = factor(x),
#              label = toupper(letter))) +
#   geom_rect(
#     aes(xmin = x - 0.2,
#         xmax = x + 0.3,
#         ymin = y - 0.2,
#         ymax = y + 0.3),
#     fill = "white",
#     linewidth = 0,
#     color = "white") +
#   geom_rect(
#     aes(xmin = x - 0.25,
#         xmax = x + 0.25,
#         ymin = y - 0.25,
#         ymax = y + 0.25),
#     fill = "white",
#     linewidth = 1.5) +
#   geom_text(family = "Lithos Pro",
#             size = 6) +
#   # scale_y_continuous(limits = c(0.5, 2.5)) +
#   # scale_x_continuous(limits = c(0, 7)) +
#   theme_void() +
#   theme(
#     legend.position = "none",
#     panel.background = element_rect(fill = "#005588")) +
#   coord_equal()
#
# draw_cube <- function(
#     x, y,
#     width = 0.65,
#     offset = 0.4){
#   coords_x <- c(
#     x - width/2,
#     x - width/2,
#     x + width/2,
#     x + width/2,
#     x + width/2 + offset / sqrt(2),
#     x + width/2 + offset / sqrt(2),
#     x - width/2 + offset / sqrt(2)
#   )
#
#   coords_y <- c(
#     y + width/2,
#     y - width/2,
#     y - width/2,
#     y + width/2,
#     y - width/2 + offset / sqrt(2),
#     y + width/2 + offset / sqrt(2),
#     y + width/2 + offset / sqrt(2)
#   )
#
#   # df <- data.frame(
#   #   x_o = x,
#   #   x = coords_x,
#   #   y = coords_y
#   # )
#
#   df_all <-
#     rbind(
#       data.frame(
#         x_o = x,
#         side = "A",
#         x = coords_x[1:4],
#         y = coords_y[1:4]),
#       data.frame(
#         x_o = x,
#         side = "B",
#         x = coords_x[c(4, 3, 5, 6)],
#         y = coords_y[c(4, 3, 5, 6)]
#       ),
#       data.frame(
#         x_o = x,
#         side = "C",
#         x = coords_x[c(7, 1, 4, 6)],
#         y = coords_y[c(7, 1, 4, 6)]
#       )
#     )
#
#   rownames(df_all) <- NULL
#
#   df_all |>
#     filter(side %in% c("A", "B", "C"))
# }
#
# scaler <- 0.25
# xplus <- 0.12
# yplus <- 0.72
#
# "tmtyro" |>
#   strsplit("") |>
#   unlist() |>
#   data.frame() |>
#   setNames("letter") |>
#   mutate(
#     x_o = row_number(),
#     y_o = runif(min = 1, max = 2, n = 6)) |>
#   group_by(x_o) |>
#   mutate(
#     x = list(draw_cube(
#       x = cur_group_id(),
#       y = y_o)$x),
#     y = list(draw_cube(
#       x = cur_group_id(),
#       y = y_o)$y),
#     side = list(draw_cube(
#       x = cur_group_id(),
#       y = y_o)$side)
#   ) |>
#   unnest(cols = c(x, y, side)) |>
#   mutate(
#     x_factor = factor(x_o),
#     x_o = x_o * 0.25 + 0.11,
#     y_o = y_o * 0.25 + 0.74,
#     x = x * 0.25 + 0.11,
#     y = y * 0.25 + 0.72,
#   ) |>
#   ggplot(aes(x, y)) +
#   hexSticker::geom_hexagon(
#     fill = "#6F6F6F",
#     color = "white") +
#   geom_segment(
#     mapping = aes(
#       x = 0.13,
#       xend = 1.81,
#       y = 0.7,
#       yend = 0.7
#     ),
#     color = "white",
#     linewidth = 0.7,
#     arrow = arrow(
#       length = unit(0.1, "inches"),
#       angle = 20,
#       type = "closed")
#   ) +
#   geom_segment(
#     mapping = aes(
#       x = 0.24,
#       xend = 0.24,
#       y = 0.44,
#       yend = 1.5
#     ),
#     color = "white",
#     linewidth = 0.7,
#     arrow = arrow(
#       length = unit(0.1, "inches"),
#       angle = 20,
#       type = "closed")
#   ) +
#   # geom_vline(
#   #   xintercept = 0.24,
#   #   linewidth = 1.1,
#   #   color = "white") +
#   # geom_hline(
#   #   yintercept = 0.7,
#   #   linewidth = 1.1,
#   #   color = "white") +
#   geom_polygon(
#     aes(group = paste0(x_factor, side),
#         fill = x_factor),
#     color = "white",
#     linewidth = 0.7) +
#   geom_text(
#     aes(label = letter,
#         x = x_o, y = y_o - 0.01,
#         # color = x_factor
#         ),
#     color = "white",
#     family = "Lithos Pro",
#     size = 5.5) +
#   geom_smooth(
#     data = ~ select(.x, x_o, y_o) |>
#       distinct(),
#     mapping = aes(x = x_o,
#                   y = y_o - 0.2,
#                   group = "all"),
#     color = "blue",
#     method = "loess",
#     se = FALSE,
#     linewidth = 1.1
#   ) +
#   theme_void() +
#   theme(
#     legend.position = "none",
#     panel.background = element_rect(fill = "black")
#     ) +
#   coord_equal()
#
# ggsave("inst/logo.png",
#        width = 1200,
#        height = 1086,
#        units = "px",
#        dpi = 300)
#
# magick::image_read("inst/logo.png") |>
#   magick::image_scale(
#     geometry = magick::geometry_size_pixels(840, 760)) |>
#   magick::image_rotate(-30) |>
#   magick::image_crop("700x605+70+78") |>
#   magick::image_rotate(60) |>
#   magick::image_crop("700x605-0-0") |>
#   magick::image_rotate(-30) |>
#   magick::image_crop("605x698+48-47") |>
#   magick::image_fill(color = "transparent",
#         refcolor = "black",
#         fuzz = 70, point = "+1+1") |>
#   magick::image_fill(color = "transparent",
#         refcolor = "black",
#         fuzz = 70, point = "+604+697") |>
#   magick::image_fill(color = "transparent",
#         refcolor = "black",
#         fuzz = 70, point = "+1+697") |>
#   magick::image_fill(color = "transparent",
#         refcolor = "black",
#         fuzz = 70, point = "+604+1") |>
#   magick::image_write(path = "inst/tmtyro.png")
#
# usethis::use_logo("inst/figures/tmtyro.png",
#                   "360x417")
#
# #   if(file.exists("man/figures/logo.png")){
# #     file.remove("man/figures/logo.png")
# #   }
# #   use_logo("man/figures/stylo2gg.png")
# #   file.remove("man/figures/stylo2gg.png")
# # }
