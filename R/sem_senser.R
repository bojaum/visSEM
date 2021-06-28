#' SEM sensivity analysis
#'
#' Visual inspection of the sensibility of the SEM to different groups found
#' on the dataset.
#' @usage sem_senser(x=NULL, dat=NULL, var.name=NULL, group=NULL, layout=NULL)
#' @param x a square matrix containing the variables relationships
#' @param dat a data.frame containing the measured variables
#' @param sem a psem object
#' @param var.name group name as a character vector with length 1
#' @param layout a coordinates matrix where each row is a variable
#' @param method a character to define the method used. method="split" manually
#' separates the dataset into the number of groups and runs a psem for each
#' group. method=multigroup" calls multigroup from \code{piecewiseSEM} that
#' includes an interaction term in each regression
#' @return a list with 1) the fitted psem models 2) \code{visNetwork} objects
#' @examples
#' \donttest{
#' ## Not run:
#' fac<-c(rep("A", 45), rep("B", 45))
#' keeley$group<-as.factor(fac)
#' sem <- psem(lm(rich ~ cover, data = keeley),
#'             lm(cover ~ firesev, data = keeley),
#'             lm(firesev ~ age, data = keeley),
#' data = keeley)
#'
#' mat1 <- matrix(ncol=2, nrow=4)
#' mat1[1,1] <-3
#' mat1[1,2] <-1
#' mat1[2,1] <-2
#' mat1[2,2] <-2
#' mat1[3,1] <-1
#' mat1[3,2] <-1
#' mat1[4,1] <-4
#' mat1[4,2] <-2
#'
#' # 1 Split with psem objet
#' sem_senser(sem=sem, var.name="group", method="split", layout=mat1)
#'
#' # 2 Split with raw data
#'
#' adj<-matrix(data=c(0,0,0,1,1,0,0,0,0,1,0,0,0,0,0,0), nrow=4, ncol=4, byrow = T)
#' colnames(adj)<-c("cover", "firesev", "age", "rich")
#' rownames(adj)<-c("cover", "firesev", "age", "rich")
#'
#' sem_senser(x=adj, dat=keeley, var.name="group", method="split", layout=mat1)
#'
#' # 3 Multigroup
#' sem_senser(sem=sem, var.name="group", method="multigroup", layout=mat1)
#'
#' ##End
#' }
#' @keywords sem
#' @encoding UTF-8
#' @author Anderson Medina
#' @export sem_senser


sem_senser<-function(x=NULL, dat=NULL, sem=NULL, var.name=NULL,
                     layout=NULL, method="split"){
  if (method=="split"){
    if (!is.null(sem)){
      if (class(sem)!="psem"){
        stop('Argument sem must be a psem object')
      }
      if (class(var.name)!="character"){
        stop('Argument var.name must be a character')
      }

      dat<-sem[[4]]
      var.x<-which(var.name==colnames(dat))

      if (length(var.x)==0){
        stop('Argument var.name not found on psem Data')
      }

      if (length(var.x)>1){
       stop('Argument var.name must be a single variable')
     }
    fac.x<-dat[,var.x]
    if (!class(fac.x)=="factor"){
      warning(paste(var.name, "converted to factor in the psem Data"))
      fac.x<-as.factor(fac.x)
      dat[,var.x]<-fac.x
    }
    fac.n<-levels(fac.x)
    f<-length(fac.n)


    d.split<-split(x=dat, f=fac.x)
    s.split<-list()
    n.split<-list()
    adj<-sem_conver(sem)
    for (i in 1:f){
      s.split[[i]]<-visSEM::sem_thinker(x=adj, dat=d.split[[i]])
      n.split[[i]]<-sem_plotter(sem=s.split[[i]], layout=layout)$vis
    }
    names(s.split)<-fac.n
    names(n.split)<-fac.n
    out.split<-list(psem=NULL, vis=NULL)
    out.split[[1]]<-s.split
    out.split[[2]]<-n.split

  } else {
    if (class(x)!="matrix"){
      stop('Argument x must be a adjacency matrix')
    }

    if (class(dat)!="data.frame"){
      stop('Argument dat must be a data.frame')
    }

    var.x<-which(var.name==colnames(dat))
    if (length(var.x)==0){
      stop('Argument var.name not found on psem Data')
    }

    if (length(var.x)>1){
      stop('Argument var.name must be a single variable')
    }
    fac.x<-dat[,var.x]
    if (!class(fac.x)=="factor"){
      warning(paste(var.name, "converted to factor in the psem Data"))
      fac.x<-as.factor(fac.x)
      dat[,var.x]<-fac.x
    }
    fac.n<-levels(fac.x)
    f<-length(fac.n)


    d.split<-split(x=dat, f=fac.x)
    s.split<-list()
    n.split<-list()

    for (i in 1:f){
      s.split[[i]]<-visSEM::sem_thinker(x=x, dat=d.split[[i]])
      n.split[[i]]<-sem_plotter(sem=s.split[[i]], layout=layout)$vis
    }

    names(s.split)<-fac.n
    names(n.split)<-fac.n
    out.split<-list(psem=NULL, vis=NULL)
    out.split[[1]]<-s.split
    out.split[[2]]<-n.split

  }
  return(out.split)
  }
if (method=="multigroup"){
  multi<-piecewiseSEM::multigroup(modelList=sem, group = var.name)
  out.split<-list(psem=NULL, vis=NULL)
  out.split[[1]]<-multi
  }
}
