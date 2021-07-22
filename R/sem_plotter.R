#' SEM interactable Plot
#'
#' Plots a structural equation model using a psem object created with the
#' \code{piecewise} package.
#' @usage sem_plotter(sem, stand, layout, ...)
#' @param sem a psem object
#' @param stand Logical if TRUE uses standardized betas if FALSE uses raw betas
#' @param layout a coordinates matrix where each row is a variable
#' @param option a character vector of the type of graph. "full" plots paths
#' and correlated errors, "pred" omits correlated errors and "corr" plots only
#' the correlated errors
#' @param vertex.label a character vector to replace the variables names in the
#' plot. DEFAULT to NULL
#' @param vertex.color a character vector to define variable vertices colors
#' @param p.color character vector of length two with the colors of positive and
#' negative effects
#' @param c.color character vector of length one with the color of correlated
#' errors
#' @return a list with two dataframes (edges and vertices), an \code{igraph}
#' object and a \code{visNetwork} object
#' @examples
#' \donttest{
#' ## Not run:
#' mod <- psem(
#' lm(rich ~ cover, data = keeley),
#' lm(cover ~ firesev, data = keeley),
#' lm(firesev ~ age, data = keeley),
#' data = keeley)
#'
#' lay<-matrix(data=c(3,2,1,4,1,2,1,2), nrow=4, ncol=2)
#'
#' sem_plotter(sem=mod, layout=lay)
#'
#' ##End
#' }
#' @keywords sem
#' @encoding UTF-8
#' @author Anderson Medina
#' @export sem_plotter


sem_plotter<-function(sem, stand=TRUE, layout=NULL, option="full", vertex.label=NULL,
                      vertex.color="grey", p.color=c("blue", "red"),
                      c.color=c("darkgray"), vertex.shape="rectangle",
                      vertex.size=35, vertex.size2=15,
                      vertex.label.cex=0.8, vertex.label.color="black",
                      edge.arrow.size=0.5, edge.curved=F,
                      edge.label.color="black", edge.label.cex=0.6){

  if (class(sem)!="psem"){
    stop('Argument sem must be a psem object')
  }

  if (is.null(layout)){
    warning("Argument layout is missing")
    layout<-sem_layouter(sem, response=NULL)
  }

  if (all(option!=c("full", "corr", "pred"))){
    warning("Invalid option argument using 'full' instead")
    option<-"full"
  }

  if (length(p.color)!=2){
    stop("Two colors are required in the p.color argument")
  }

  if (length(c.color)!=1){
    stop("Only one color is required in the c.color argument")
  }

  stand<-as.logical(stand)
  if (!is.logical(stand)){
    stop("stand must be a LOGICAL vector")
  }

  s<-ifelse(stand==TRUE, 8, 3)

  betas<-coefs(sem)
  ncorel<-grep("~~", betas$Response, invert=T)
  corel<-grep("~~", betas$Response)

  st.betas<-betas[ncorel,c(2,1,s,7)]
  p1<-st.betas[,4]<0.05
  st.betas<-st.betas[,1:3]

  st.cor<-betas[corel,c(2,1,8,7)]
  p2<-st.cor[,4]<0.05
  st.cor<-st.cor[,1:3]
  st.cor[,1]<-gsub("~~", "", st.cor[,1])
  st.cor[,2]<-gsub("~~", "", st.cor[,2])

  st.dat<-rbind(st.betas, st.cor)

  net0<-graph_from_data_frame(st.dat, directed = TRUE)
  net1<-delete_edges(net0, corel)
  net2<-delete_edges(net0, ncorel)


  #Edges
  v1<-names(V(net1))
  ed1<-round(E(net1)$Std.Estimate,2)
  E(net1)$color<-ifelse(ed1>0, p.color[1], p.color[2])
  E(net1)$width<-abs(ed1)
  E(net1)$lty<-ifelse(p1, 1, 2)
  E(net1)$arrow.mode<-2
  E(net1)$dashes<-!p1
  #E(net1)$value<-abs(ed1)
  E(net1)$label<-ed1
  E(net1)$arrows<-c("to")

  v2<-names(V(net2))
  ed2<-round(E(net2)$Std.Estimate,2)
  E(net2)$color<-c.color
  E(net2)$width<-abs(ed2)
  E(net2)$lty<-ifelse(p2, 1, 2)
  E(net2)$arrow.mode<-3
  E(net2)$dashes<-!p2
  #E(net2)$value<-abs(ed2)
  E(net2)$label<-ed2
  E(net2)$arrows<-c("to;from")

  v0<-names(V(net0))
  E(net0)$color<-c(E(net1)$color, E(net2)$color)
  E(net0)$width<-c(E(net1)$width, E(net2)$width)
  E(net0)$lty<-c(E(net1)$lty, E(net2)$lty)
  E(net0)$arrow.mode<-c(E(net1)$arrow.mode, E(net2)$arrow.mode)
  E(net0)$dashes<-c(E(net1)$dashes, E(net2)$dashes)
  #E(net0)$value<-c(E(net1)$value, E(net2)$value)
  E(net0)$label<-c(E(net1)$label, E(net2)$label)
  E(net0)$arrows<-c(E(net1)$arrows, E(net2)$arrows)


  if (option=="full"){
    net<-net0
  }

  if (option=="corr"){
    net<-net2
  }

  if (option=="pred"){
    net<-net1
  }


  #Nodes

  V(net)$x<-layout[,1]
  V(net)$y<-layout[,2]

  nvar<-length(V(net))

  if (!is.null(vertex.label)){
    if (nvar!=vertex.label){
      warning("More labels supplied than variables. Ignoring labels.")
      vertex.label<-names(V(net))
    } else {
      V(net)$label<-vertex.label
    }
  } else {
    V(net)$label<-names(V(net))
  }


  if (length(vertex.color)==nvar | length(vertex.color)==1){
    V(net)$color<-vertex.color
  } else {
    warning("More colors than variables")
  }




  print(visIgraph(net, idToLabel = F))

  dat<-as_data_frame(x=net0, what = "both")
  dat$net<-net0
  dat$vis<-visIgraph(net, idToLabel = F)
  return(dat)

}
