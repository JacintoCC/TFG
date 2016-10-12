

#' @title Projection of 3-simplex points
#'
#' @export
#' @description This function projects 3-simplex points to a 2D triangle
#' @param points Simplex points to be projected
plotSimplex <- function(points){
  df.points <- data.frame(L = points[ ,1], rope = points[ ,2],
                          R = points[ ,3], d = densCols(points,
                colramp = colorRampPalette(heat.colors(100))))

  lines <- data.frame(x = c(0.5, 0, 0.5), y = c(0, 0.5, 0.5),
                      z = c(0.5, 0.5, 0), xend = c(1,1,1)/3,
                      yend = c(1,1,1)/3, zend = c(1,1,1)/3)
  borders <- data.frame(x = c(1,0,0), y=c(0,1,0), z=c(0,0,1),
                        xend = c(0,1,0), yend=c(0,0,1), zend=c(1,0,0))

  ggtern(data = df.points, aes(L, rope, R)) +
       geom_point(color = df.points$d) +
       geom_segment(data = lines,
                    aes(x = c(0.5, 0, 0.5), y = c(0, 0.5, 0.5),
                        z = c(0.5, 0.5, 0), xend = c(1,1,1)/3,
                        yend = c(1,1,1)/3, zend = c(1,1,1)/3),
                    color = 'orange', size = 0.5) +
       geom_segment(data = borders, aes(x = c(1,0,0), y=c(0,1,0), z=c(0,0,1),
                                        xend = c(0,1,0), yend=c(0,0,1),
                                        zend=c(1,0,0)),
                    color = 'orange', size = 1)
}

#' @title Plot of posterior distribution
#'
#' @export
#' @description This function plots the posterior distribution of the parameter
#' @param x Sequence in x-axis
#' @param y Values of difference distribution
#' @param names Names of the algorithms
#' @param dataset Names of the dataset
plotPosterior <- function (data, names, dataset,...) {
  qplot(data$x, data$y, geom = "line") +
    ggtitle(paste(names[1], "vs.", names[2], "\nDataset:", dataset)) +
    xlab("Difference") +
    ylab("Value") +
    geom_area(aes(fill="distribution"), fill = "lightblue") +
    geom_line(color="darkblue") +
    geom_vline(xintercept = -0.01, color = "orange") +
    geom_vline(xintercept = 0.01, color = "orange") +
    geom_segment(data = data.frame(x = -0.01, y = 0, xend=0.01, yend = 0),
                 aes(x = -0.01, y = 0, xend=0.01, yend = 0), color = "orange")

}
