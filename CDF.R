normal_prob_CDF_plot <- function(lb, ub, mean = 0, sd = 1, limits = c(mean - 4 * sd, mean + 4 * sd)){
  if(is.null(limits[1]) || is.null(limits[2])) return ()
  x <- seq(limits[1], limits[2], length.out = 100)
  xmin <- max(lb, limits[1])
  xmax <- min(ub, limits[2])
  areax <- seq(xmin, xmax, length.out = 100)
  area <- data.frame(x = areax, ymin = 0, ymax = pnorm(areax, mean = mean, sd = sd))
  
  (ggplot()
    + xlab("x")
    + ylab("Cumulative Probability")
    + ggtitle("Normal Cumulative Distribution Function\n")
    + geom_line(data.frame(x = x, y = pnorm(x, mean = mean, sd = sd)),
                mapping = aes(x = x, y = y))
    + geom_ribbon(data = area, 
                  mapping = aes(x = x, ymin = ymin, ymax = ymax), 
                  fill = "#00BA38")
    + scale_x_continuous(limits = limits, breaks = 
                           if(limits[2] - limits[1] <= 15) seq(ceiling(limits[1]), ceiling(limits[2]), 1)
                         else seq(ceiling(limits[1]), ceiling(limits[2]), ceiling((limits[2] - limits[1])/15))
    )
    + scale_fill_manual(values=c("black"))
    + guides(fill=FALSE))
}
