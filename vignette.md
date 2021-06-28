---
### VisSEM
#### Visualization of Structural Equations Modelling using Networks
##### An r package to plot piecewise SEM objects as interactive networks  

#### How to install
```{r, message=F}
#If you do not have devtools installed
#install.packages("devtools")

#Then
library(devtools)
#devtools::install_github("bojaum/visSEM")

```
#### Loading
```{r, message=F}
library(igraph)
library(visNetwork)
library(piecewiseSEM)
library(visSEM)

```

#### Start building a piecewise SEM
```{r, message=F}
mod <- psem(lm(rich ~ cover, data = keeley),
            lm(cover ~ firesev, data = keeley),
            lm(firesev ~ age, data = keeley),
            data = keeley)
plot(mod)
```

#### Use visSEM to create an interactive plot. You can click and drag the variables to any position. 
```{r}
sem_plotter(sem=mod, layout=NULL)$vis
```

#### Note the warning when you do not specify a layout. You can specify a custom layout if you wish. The layout matrix has xy coordinates in columns and variables in the rows.
```{r}
mat1 <- matrix(ncol=2, nrow=4)
mat1[1,1] <-3
mat1[1,2] <-1
mat1[2,1] <-2
mat1[2,2] <-2
mat1[3,1] <-1
mat1[3,2] <-1
mat1[4,1] <-4
mat1[4,2] <-2
sem_plotter(sem=mod, layout=mat1)$vis
```

#### When you do not specify the layout, the package uses sem_layouter to find a layout.
```{r}
mat2<-sem_layouter(sem=mod, response="rich")
print(mat2)
```

#### In cases when you do not specify which is the response variable, function sem_guesser is called.
```{r}
sem_guesser(sem=mod)
```

#### You can customize your interactive SEM:
```{r}
cols<-c("#FF0000", "#80FF00", "#00FFFF", "#8000FF")
sem_plotter(sem=mod, layout=mat2, vertex.color=cols, p.color=c("black", "red"))$vis
```

#### You can calculate direct and indirect effects using sem_ditter
```{r}
sem_ditter(sem=mod, response="rich")
```

#### You can create a piecewise SEM using a matrix and a data.frame
```{r}

#Adjancecy matrix (First Model)
adj<-matrix(data=c(0,0,0,1,1,0,0,0,0,1,0,0,0,0,0,0), nrow=4, ncol=4, byrow = T)
colnames(adj)<-c("cover", "firesev", "age", "rich")
rownames(adj)<-c("cover", "firesev", "age", "rich")
print(adj)

#Adjancecy matrix (Second Model)
vals<-c(0,0,0,1,0,1,0,0,0,0,0,1,0,0,0,0,0,0,0,0,1,0,0,1,0)
adj2<-matrix(data=vals, nrow=5, ncol=5, byrow = T)
colnames(adj2)<-c("cover", "firesev", "age", "rich", "abiotic")
rownames(adj2)<-c("cover", "firesev", "age", "rich", "abiotic")
print(adj2)

#Data.frame
data(keeley)
#dat<-keeley

m1<-sem_thinker(x=adj, dat=keeley)
m2<-sem_thinker(x=adj2, dat=keeley)
```

#### Plot the interactive networks

```{r}
sem_plotter(sem=m1, layout=NULL)$vis
sem_plotter(sem=m2, layout=NULL)$vis
```

#### Function sem_plotter also creates a "static" network using igraph package.
```{r}
static<-sem_plotter(sem=mod, layout=mat1)$net
plot(static) #Default of igraph ignores layout
plot(static, layout=mat1) #using layout 1 (ziz-zag)
plot(static, layout=mat2) #using layout 2 (linear)
```

#### Sometimes you may evaluate the robustness of the SEM when considering different groups or subsets of the data.
##### Method "split" separates the dataset into multiple subsets and runs the piecewise SEM for each subset
```{r}
fac<-c(rep("A", 45), rep("B", 45))
keeley$group<-as.factor(fac)
mod <- psem(lm(rich ~ cover, data = keeley),
            lm(cover ~ firesev, data = keeley),
            lm(firesev ~ age, data = keeley),
      data = keeley)


sens1<-sem_senser(x=adj, dat=keeley, var.name="group", method="split", layout=mat1)
sens2<-sem_senser(sem=mod, var.name="group", method="split", layout=mat1)
```

##### Method "multigroup" uses the group analysis function from piecewiseSEM package
```{r}
sens<-sem_senser(sem=mod, var.name="group", method="multigroup", layout=mat1)
print(sens)
```

#### Group A
```{r}
sens2$vis[[1]]
```

#### Group B
```{r}
sens2$vis[[2]]
```

#### Checking for assumptions of the linear equations
```{r}
lon<-rnorm(90)
lat<-rnorm(90)
spatial.keeley<-cbind(keeley, lon, lat)
assump<-sem_checker(x=adj2, dat=spatial.keeley, spatial=T, coord=c("lon", "lat"))

sem_checker_plot(assump, spatial=T, what=1)
```
