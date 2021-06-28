#' SEM Layout maker
#'
#' Tries to find a layout matrix using a user provided "response" variable, in
#' another words, the most endogenous variable in the structural equation model.
#' Puts the "response" variable on the right and places the other variables from
#' right (second most endogenous) to the left (exogenous or least endogenous).
#'
#' @usage sem_layouter(sem, response="NULL")
#' @param sem a psem object
#' @param response a character vector with the name of the response variable
#' @return a matrix with xy coordinates in the columns and variables in the rows
#' @examples
#' \donttest{
#' ## Not run:
#' mod <- psem(
#' lm(rich ~ cover, data = keeley),
#' lm(cover ~ firesev, data = keeley),
#' lm(firesev ~ age, data = keeley),
#' data = keeley)
#'
#' sem_layouter(sem=mod, response="rich")
#'
#' ##End
#' }
#' @keywords sem
#' @encoding UTF-8
#' @author Anderson Medina
#' @export sem_layouter


sem_layouter<-function(sem, response=NULL){

  if (class(sem)!="psem"){
    stop("argument sem must be a psem object")
  }

  if (length(response)>1){
    stop("Argument response requires only one variable")
  }

  if (is.null(response)){
    warning("Argument response is NULL.
            Used sem_guesser to identify response variable")
    response<-sem_guesser(sem)
  }



  betas<-coefs(sem)
  ncorel<-grep("~~", betas$Response, invert=T)
  st.betas<-betas[ncorel,c(2,1,8,7)]
  p<-st.betas[,4]<0.05
  st.betas<-st.betas[,1:3]

  net<-graph_from_data_frame(st.betas, directed = TRUE)
  v<-names(V(net))
  endo<-grep(response, v)
  if (length(endo)==0){
    stop("Argument response does exist in psem object")
  }


  exo<-setdiff(1:length(v), endo)
  E(net)$pvalue<-p

  #output
  lay<-matrix(ncol=2, nrow=length(v))
  colnames(lay)<-c("x", "y")
  rownames(lay)<-v

  #finding x position
  lay[endo, 1]<-1 #response position x is fixed
  for(i in exo)
  {
    paths<-all_simple_paths(net, from=i, to = endo, mode = "out")
    lay[i,1]<-max(sapply(paths, length))
  }

  fre.tab<-table(lay[,1])
  n<-length(fre.tab)
  y<-numeric()
  for (i in n:1)
  {

    lout<-fre.tab[i]
    y<-c(y, seq(length.out=lout, from=n/(lout+1)))
  }
  lay[,2]<-y

  lay[,1]<-max(lay[,1])-lay[,1]+1

  return(lay)
}
