normal_prob_area_plot <- function(lb, ub, mean = 0, sd = 1, limits = c(mean - 4 * sd, mean + 4 * sd), extreme = FALSE){
  if(is.null(limits[1]) || is.null(limits[2])) return ()
  x <- seq(limits[1], limits[2], length.out = 100)
  xmin <- max(lb, limits[1])
  xmax <- min(ub, limits[2])
  if(extreme == FALSE){
    areax1 <- seq(xmin, xmax, length.out = 100)
    areax2 <- 0 #No area
  } else{
    areax1 <- seq(ceiling(mean - 4 * sd), 
                  xmin, 
                  length.out = 100)
    areax2 <- seq(xmax, 
                  ceiling(mean + 4 * sd), 
                  length.out = 100)
  }
  
  area1 <- data.frame(x = areax1, ymin = 0, ymax = dnorm(areax1, mean = mean, sd = sd))
  area2 <- data.frame(x = areax2, ymin = 0, ymax = dnorm(areax2, mean = mean, sd = sd))
  (ggplot()
    + xlab("x")
    + ylab("Density")
    + ggtitle("Normal Probability Density Function\n")
    + geom_bar(stat="identity")
    + geom_line(data.frame(x = x, y = dnorm(x, mean = mean, sd = sd)),
                mapping = aes(x = x, y = y))
    + geom_ribbon(data = area1, 
                  mapping = aes(x = x, ymin = ymin, ymax = ymax), 
                  fill = "#00BA38")
    + geom_ribbon(data = area2, 
                  mapping = aes(x = x, ymin = ymin, ymax = ymax), 
                  fill = "#00BA38")
    + scale_x_continuous(limits = limits, breaks = 
                           if(limits[2] - limits[1] <= 15) seq(ceiling(limits[1]), ceiling(limits[2]), 1)
                         else seq(ceiling(limits[1]), ceiling(limits[2]), ceiling((limits[2] - limits[1])/15))
    )
    + scale_fill_manual(values=c("black"))
    + guides(fill=FALSE))
}
