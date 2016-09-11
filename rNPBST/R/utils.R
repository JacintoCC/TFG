#' @title Heaviside step function
#'
#' @description This function implements Heaviside step function
#' @param x Object to be evaluated
#' @param a Frontier value
heaviside <- function(x, a=0){
  return( (sign(x-a) + 1) / 2 )
}

#' @title Location of the maximum(s) in a vector
#'
#' @description Returns a vector with an 1 in the maximum position. If there are more than one then 1/num. maximums is situated in each position
#' @param x Vector where find the maximum(s)
locate.max <- function(x){
  vec <- sapply(x, function(y){y == max(x)})
  return(vec/sum(vec))
}

#' @title Projection of 3-simplex points
#'
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
