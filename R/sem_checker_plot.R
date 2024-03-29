#' Plots the assumptions of linear equations
#'
#' This functions plots the result of sem_checker.
#'
#' @usage sem_checker_plot(x, spatial=FALSE, what=1)
#' @param x a data.frame
#' @param spatial logical if TRUE evaluate spatial autocorrelation
#' @param what choose the regression to be evaluated
#' @return a plot
#' @examples
#' \donttest{
#' ## Not run:
#'
#' #Create or load a matrix
#' vals<-c(0,0,0,1,1,0,0,0,0,1,0,0,0,0,0,0)
#' mat<-matrix(data=vals, nrow=4, ncol=4, byrow = T)
#' colnames(mat)<-c("cover", "firesev", "age", "rich")
#' rownames(mat)<-c("cover", "firesev", "age", "rich")
#'
#' #Creates the psem object based on the matrix
#' sem_thinker(x=mat, dat=keeley)
#'
#' ##End
#' }
#' @keywords sem
#' @encoding UTF-8
#' @author Anderson Medina
#' @export sem_checker_plot



sem_checker_plot<-function(x, spatial=FALSE, what=1){

  if (class(x)!="list"){
    stop("Argument x must be a data.frame")
  }

  if (length(x)!=2){
    stop("Argument x must have two elements")
  }

  if (what>length(x[[2]])){
    stop("Argument what must be the position of the equation")
  }

  w<-x[[1]][[what]]

  if (spatial==TRUE){
    geomat<-as.matrix(w[, c("x", "y")])
    geokm <- fields::rdist.earth(geomat,
                               miles = FALSE, R = NULL)
  }

  norm<-x[[2]][[what]][[1]]
  homo<-x[[2]][[what]][[2]]
  mult<-x[[2]][[what]][[3]]
  if(!is.null(mult)){
    names(mult)<-substr(names(mult), 1, 10)
  }


  #texts:
  t.norm<-paste0("Normality: \nShapiro W = ",
                 round(norm[[1]], 2), "; p = ",
                 round(norm[[2]], 3))

  t.homo<-paste0("Heteroscedasticity: \nBreusch–Pagan\n Chi = ",
                 round(homo[[3]], 2), "; d.f. = ",
                 round(homo[[4]], 2), "; p = ",
                 round(homo[[5]], 3))

  par(mfrow = c(2, 2))
  #1
  plot(w$fit, w$res, main=t.homo,
       bty="l", pch=21, bg="gray",
       xlab="Fitted", ylab="Residuals")
  abline(h=0, lty=2)
  #2
  plot(w$quant, scale(w$res), main=t.norm,
       bty="l", pch=21, bg="gray",
       xlab="Theoretical Quantiles", ylab="Standardized Residuals")
  abline(a=0, b=1, lty=2)
  #3
  if (spatial==TRUE){
    letsR::lets.correl(scale(w$res), geokm, 10, T)
    title(main = "Spatial\n Correlogram")
  } else {
    plot(NULL, main="Spatial\n Correlogram",
         xlim=c(0,1), ylim=c(0,1),
         xlab="", ylab="", bty="n", axes=F)
  }

  #4
  if(!is.null(mult)){
  xmax<-max(10, mult)
  barplot(mult, main="Multicolinearity:",
          bty="l", ylim=c(0, xmax),
          xlab="Variables", ylab="VIF")
  box(bty="l")
  }else {
    plot(NULL, main="Multicolinearity:",
         xlim=c(0,1), ylim=c(0,1),
         xlab="", ylab="", bty="n", axes=F)
  }

  #return(res)
}

