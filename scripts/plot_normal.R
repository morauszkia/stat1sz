plot_normal <- function(breaks, colored, mean = 0, sd = 1, 
                        save = FALSE, title = FALSE, 
                        color = c("darkgreen", "grey", "darkgreen")) {
  
  x <- seq(mean - 3*sd, mean + 3*sd, length.out = 10000)
  
  plot.data <- data.frame(
    x = x,
    y = dnorm(x, mean = mean, sd = sd)
  )
  
  normalcurve <- ggplot(plot.data, aes(x = x, y = y))+ 
    geom_line() + 
    theme_classic() + 
    ylab("") + 
    xlab("") +
    scale_x_continuous(breaks = breaks) + 
    scale_y_continuous(breaks = NULL) +
    theme(text = element_text(size = 15), plot.title = element_text(hjust = 0.5)) +
    geom_area(data = subset(plot.data, x < breaks[1]), aes(y = y), 
              fill = ifelse(1 %in% colored, color[1], color[2]), alpha = 0.7)
  if (length(breaks) == 1) {
    normalcurve <- normalcurve +
      geom_area(data = subset(plot.data, x >= breaks[1]), aes(y = y),
                fill = ifelse(2 %in% colored, color[1], color[2]), alpha = 0.7)
  } else {
    normalcurve <- normalcurve +
      geom_area(data = subset(plot.data, x >= breaks[1] & x <= breaks[2]), aes(y = y),
                fill = ifelse(2 %in% colored, color[1], color[2]), alpha = 0.7) +
      geom_area(data = subset(plot.data, x > breaks[2]), aes(y = y),
                fill = ifelse(3 %in% colored, color[3], color[2]), alpha = 0.7)
  }
  
  if (title) {
    normalcurve <- normalcurve +
      ggtitle(paste0("N(", mean, ";", sd,")"))
  }

  if (save) {
    ggsave(
      filename = paste0("N(", mean, ",", sd, ")_", 
                        paste(breaks, collapse = "-"), ".jpeg"),
      plot = normalcurve,
      device = "jpeg",
      width = 5,
      height = 3,
      units = "in"
    )
  } else {
    return(normalcurve)
  }
}