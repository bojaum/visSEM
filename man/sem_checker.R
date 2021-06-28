#' Checks the assumptions of each linear equation of a SEM
#'
#' This functions extracts the residuals of each linear equation and evaluates
#' the assumptions of homoscedasticity, normality, spatial autocorrelation and
#' multicolinearity
#'
#' @usage sem_checker(x, dat, spatial=FALSE, coord=c("lon", "lat"))
#' @param x a square matrix containing the variables relationships
#' @param dat a data.frame containing the measured variables
#' @param spatial logical evaluate spatial autocorrelation?
#' @param coord a character with geographic coordinates column names
#' @return a list with original dataset and assumptions tests
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
#' sem_checker(x=mat, dat=keeley, spatial=F)
#'
#' ##End
#' }
#' @keywords sem
#' @encoding UTF-8
#' @author Anderson Medina
#' @export sem_checker


sem_checker<-function(x, dat, spatial=FALSE, coord=c("lon", "lat")){

  if (class(x)[1]!="matrix"){
    stop("Argument x must be a matrix object")
  }

  if (class(dat)!="data.frame"){
    stop("Argument dat must be a data.frame object")
  }
  lon<-NA
  lat<-NA
  if (spatial==TRUE){
    if (class(coord)!="character"){
      stop("Argument coord must be a character")
    }

    if (length(coord)!=2){
      stop("Argument coord must have two variables")
    }
    x.col<-intersect(coord, colnames(dat))
    if (length(x.col)!=2){
      stop("coord variables must be on the dataset")
    }
    lon<-dat[,coord[1]]
    lat<-dat[,coord[2]]
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

  res<-list()
  tes<-list()
  out<-list(residuals=NULL, tests=NULL)

  for (i in 1:n.equ){

    test<-list(norm=NULL, homo=NULL, mult=NULL)
    tes[[i]]<-test

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
    k<-qqnorm(m$residuals, plot.it = F)
    res[[i]]<-data.frame(x=lon, y=lat, fit=m$fitted.values, quant=k$x, res=m$residuals)
    tes[[i]][[1]]<-stats::shapiro.test(m$residuals)
    tes[[i]][[2]]<-car::ncvTest(m)
    if (length(response)>1){
      tes[[i]][[3]]<-car::vif(m)
    }

  }
  names(res)<-resp
  names(tes)<-resp
  out[[1]]<-res
  out[[2]]<-tes


  return(out)
}

