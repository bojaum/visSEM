#' SEM Conversion
#'
#' Converts a psem objet to a binary adjacency matrix
#' @usage sem_conver(sem=mod)
#' @param sem a psem object
#' @return an adjacency matrix
#' @examples
#' \donttest{
#' ## Not run:
#' #mod <- psem(
#' lm(rich ~ cover, data = keeley),
#' lm(cover ~ firesev, data = keeley),
#' lm(firesev ~ age, data = keeley),
#' data = keeley)
#'
#' sem_conver(sem=mod)
#'
#'
#' ##End
#' }
#' @keywords sem
#' @encoding UTF-8
#' @author Anderson Medina
#' @export sem_conver

sem_conver<-function(sem){
  if (class(sem)!="psem"){
    stop('Argument sem must be a psem object')
  }
  warning("sem_conver currently does not support correlated errors")

  betas<-coefs(sem)
  ncorel<-grep("~~", betas$Response, invert=T)
  corel<-grep("~~", betas$Response)

  st.betas<-betas[ncorel,c(2,1,8,7)]
  st.betas<-st.betas[,1:3]

  st.cor<-betas[corel,c(2,1,8,7)]
  st.cor<-st.cor[,1:3]
  st.cor[,1]<-gsub("~~", "", st.cor[,1])
  st.cor[,2]<-gsub("~~", "", st.cor[,2])

  #  st.dat<-rbind(st.betas, st.cor)
  st.dat<-st.betas
  net0<-graph_from_data_frame(st.dat, directed = TRUE)
  adj<-as_adjacency_matrix(net0)
  adj<-as.matrix(adj)
  return(adj)
}
