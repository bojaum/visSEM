#' SEM response variable guesser
#'
#' Tries to find the response variable in a psem object. Uses network approach
#' to measure variables degrees (in and out). The response is selected using two
#' criteria:
#' 1) variables with smaller degrees out, in another words, variables that
#' predicts fewer or no variables. And if there are ties:
#' 2) variables with higher degrees in, in another words, variables that are
#' explained by more or all variables.
#'
#' @usage sem_guesser(sem)
#' @param sem a psem object
#' @return a character vector with the name of the response variable
#' @examples
#' \donttest{
#' ## Not run:
#' mod <- psem(
#' lm(rich ~ cover, data = keeley),
#' lm(cover ~ firesev, data = keeley),
#' lm(firesev ~ age, data = keeley),
#' data = keeley)
#'
#' sem_guesser(sem=mod)
#'
#' ##End
#' }
#' @keywords sem
#' @encoding UTF-8
#' @author Anderson Medina
#' @export sem_guesser


sem_guesser<-function(sem){

  if (class(sem)!="psem"){
    stop("Argument sem must be a psem object")
  }


  betas<-coefs(sem)
  ncorel<-grep("~~", betas$Response, invert=T)
  st.betas<-betas[ncorel,c(2,1,8,7)]
  p<-st.betas[,4]<0.05
  st.betas<-st.betas[,1:3]

  net<-graph_from_data_frame(st.betas, directed = TRUE)

  deg.in<-degree(net, mode="in")
  deg.out<-degree(net, mode="out")
  deg.dat<-data.frame(deg.in, deg.out)

  ord<-order(deg.out, -deg.in)
  deg.dat<-deg.dat[ord,]
  response<-row.names(deg.dat)[1]

  if (deg.dat[response,2]>0){
    warning("Selected response variable also explains other variable(s)")
  }

  return(response)
}
