#' SEM Direct, Indirect and Total Effects
#'
#' Calculates direct, indirect and total effects from a piecewise SEM object.
#' Direct effects are the standardized coefficients, indirect effects are
#' calculated by multiplying the standardized coefficients of significant paths,
#' and total effects are the sum of the direct and indirect effects.
#' @usage sem_dittter(sem, response=NULL)
#' @param sem a psem object
#' @param response a character vector with the name of the response variable
#' @return a data.frame object in which rows are variables and columns are
#' direct, indirect and total effects.
#' @examples
#' \donttest{
#' ## Not run:
#' mod <- psem(
#' lm(rich ~ cover, data = keeley),
#' lm(cover ~ firesev, data = keeley),
#' lm(firesev ~ age, data = keeley),
#' data = keeley)
#'
#' sem_dittter(sem=mod, response="rich")
#'
#' #Forgetting to set response variable:
#' sem_dittter(sem=mod, response=NULL)
#'
#' ##End
#' }
#' @keywords sem
#' @encoding UTF-8
#' @author Anderson Medina
#' @export sem_ditter


sem_ditter<-function(sem, response=NULL){

  if (class(sem)!="psem"){
    stop('Argument sem must be a psem object')
  }

  if (length(response)>1){
    stop("Argument response requires only one variable")
  }

  if (is.null(response)){
    warning("Argument response is NULL.
            Used sem_guesser to identify response variable")
    response<-sem_guesser(sem=sem)
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
    warning("Argument response is not a variable in psem object.
            Used sem_guesser to identify response variable")
    response<-sem_guesser(sem=sem)
    endo<-grep(response, v)
  }

  exo<-setdiff(1:length(v), endo)
  E(net)$pvalue<-p

  #output
  dit<-matrix(ncol=3, nrow=length(exo))
  colnames(dit)<-c("Direct", "Indirect", "Total")
  rownames(dit)<-v[exo]

  for(i in exo)
  {
    paths<-all_simple_paths(net, from=i, to = endo, mode = "out")
    e<-lapply(paths, function(x) E(graph=net, path=x)$Std.Estimate)
    ps<-lapply(paths, function(x) E(graph=net, path=x)$pvalue)
    paths.sig<-grep("FALSE", ps, invert=T)

    paths.len<-sapply(e, length)[paths.sig]
    eff<-sapply(e, prod)[paths.sig]

    di<-ifelse(paths.len==1, "Direct", "Indirect")
    di<-factor(x=di, levels=c("Direct", "Indirect"))
    sum.di<-by(eff, di, sum)

    dit[v[exo][i], "Direct"]<-sum.di[1]
    dit[v[exo][i], "Indirect"]<-sum.di[2]
  }
  dit[, "Total"]<-rowSums(dit[,1:2], na.rm=T)

  return(dit)
}
