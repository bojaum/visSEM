#' Creates a psem object based on an adjacency matrix
#'
#' This functions creates a piecewise SEM based on two objects: a matrix
#' describing how variables are related to each other and a data.frame
#' containing the observed variables data.
#'
#' @usage sem_thinker(x, dat)
#' @param x a square matrix containing the variables relationships
#' @param dat a data.frame containing the measured variables
#' @return a piecewise object
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
#' @export sem_thinker


sem_thinker<-function(x, dat){

  if (class(x)[1]!="matrix"){
    stop("Argument x must be a matrix object")
  }

  if (class(dat)!="data.frame"){
    stop("Argument dat must be a data.frame object")
  }

  xisto<<-dat #GAMBIARRA

  p.color=c("blue", "red")

  net<-graph_from_adjacency_matrix(x, mode = "directed" , weighted = T)
  #Edges
  v1<-names(V(net))
  ed1<-round(E(net)$weight,2)
  E(net)$color<-ifelse(ed1>0, p.color[1], p.color[2])
  E(net)$width<-abs(ed1)
  E(net)$arrow.mode<-2
  E(net)$arrows<-c("to")

  #  print(visIgraph(net, idToLabel = T))

  dat.net<-as_data_frame(net)
  vars<-dat.net[,1:2]
  resp<-unique(vars[,2])
  n.equ<-length(resp)
  equ<-paste0("equ", 1:n.equ)
  ml<-list()

  for (i in 1:n.equ){
    temp.vars<-subset(vars, to==resp[i])
    response<-paste0(temp.vars[,1], collpase="+")
    response<-substr(response, 1, nchar(response)-1)
    predictor<-resp[i]
    #   form<-paste(predictor,"~",response)
    text<-paste(predictor,"~",paste(response, collapse=" + "))
    form<-as.formula(text)
    m<-lm(formula=form, data=xisto)

    #m<-lm(cover~rich, dat)
    ml[[i]]<-m
    names(ml)[i]<-text


  }

  ps<-piecewiseSEM:::formatpsem(ml)
  class(ps)<-"psem"

  return(ps)
}


