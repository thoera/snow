library("tidyverse")
library("magick")

create_snow <- function(snowflake = 100, length = 100, speed = 0.025) {
  snow_in_the_sky <- vector(mode = "list", length = length)
  snow_on_the_ground <- vector(mode = "list", length = length)
  
  snow_init <- data_frame(
    x = runif(snowflake),
    y = runif(snowflake),
    size = runif(snowflake, min = 6, max = 14)
  )
  
  snow_in_the_sky[[1]] <- snow_init
  
  for (i in 2:length(snow_in_the_sky)) {
    snow_on_the_ground[[i]] <- bind_rows(
      snow_on_the_ground[[i - 1]],
      filter(snow_in_the_sky[[i - 1]], y <= speed)
    ) %>%
      mutate(y = 0)
    
    n <- nrow(filter(snow_in_the_sky[[i - 1]], y <= speed))
    more_snow_in_the_sky <- data_frame(
      x = runif(n),
      y = 1,
      size = runif(n, min = 6, max = 14)
    )
    
    snow_in_the_sky[[i]] <- snow_in_the_sky[[i - 1]] %>%
      filter(y > speed) %>%
      mutate(y = y - speed) %>%
      bind_rows(more_snow_in_the_sky)
  }
  return(list("snow_in_the_sky" = snow_in_the_sky,
              "snow_on_the_ground" = snow_on_the_ground))
}

plot_snow <- function(snow_in_the_sky, snow_on_the_ground) {
  p <- ggplot(data = snow_in_the_sky, aes(x = x, y = y, size = size)) +
    geom_point(color = "white", pch = 42, alpha = 0.75) +
    geom_point(data = snow_on_the_ground, aes(x = x, y = y, size = size),
               color = "white", pch = 45, alpha = 0.75) +
    scale_size_identity() +
    coord_cartesian(c(0, 1), c(0, 1)) +
    theme_void() +
    theme(panel.background = element_rect(fill = "black"),
          plot.background = element_rect(fill = "black"))
  print(p)
}

snow <- create_snow(snowflake = 200, length = 500, speed = 0.005)

img <- image_graph(width = 1920, height = 1080, res = 96)
snowstorm <- map(seq_along(snow[[1]]), function(x) {
  plot_snow(snow_in_the_sky = snow[["snow_in_the_sky"]][[x]],
            snow_on_the_ground = snow[["snow_on_the_ground"]][[x]])
})
dev.off()

animation <- image_animate(img, 25)

image_write(animation, "snowstorm.gif")

# parallel::mclapply(seq_along(snowstorm), function(x) {
#   ggsave(paste0("snow_", x, ".png"),
#          snowstorm[[x]], width = 16, height = 9, dpi = 120)
# }, mc.cores = parallel::detectCores())
