### Visualize matrix population models in R
    # transition matrix as geometric transformation of population vector

rm(list=ls())

library(plotly)


# load functions -------
# N0=N0_2D;A=A_2D
doProj <- function(N0,A,t){
  N=list() 
  N[paste0("y",0:t)]=lapply(0:t,\(x) matrix(rep(0,length(N0)),ncol=1))
  N[[1]][,1] = N0 
  for(y in 1:t) N[[y+1]] = A%*%N[[y]] 
  lim= max(sapply(N,max))
  Ndf = as.data.frame(t(do.call("cbind",N))); names(Ndf) = paste0("N",1:length(N0))
  Ndf$label = paste0("year",0:t)
  Ndf
}

drawNvec2D <- function(Ndf,t=0){
  nstages=length(which(grepl("N",names(Ndf))))
  lim=max(sapply(Ndf[,1:nstages],max))
  thisdf = Ndf[Ndf$label%in%paste0("year",0:t),]
  alphas <- seq(0.4, 1, length.out = nrow(thisdf))
  if(nrow(thisdf)==1) alphas=1
  # Convert to rgba color strings
  arrow_colors <- sprintf("rgba(70, 130, 180, %.2f)", alphas)  # steelblue with varying alpha
  plot_ly() |> 
    config(
      scrollZoom = TRUE,    # enables scroll-to-zoom
      displayModeBar = TRUE # ensures toolbar is visible
    ) |> 
    add_trace(
      type = "scatter",
      mode = "text",
      x = thisdf$N1,
      y = thisdf$N2,
      text = thisdf$label,
      textposition = "top right"
    ) |>
    add_annotations(
      x    = thisdf$N1,
      y    = thisdf$N2,
      ax   = 0,
      ay   = 0,
      xref = "x",  axref = "x",
      yref = "y",  ayref = "y",
      text = "",
      showarrow = TRUE,
      arrowhead = 2,
      arrowsize = 1,
      arrowwidth = 2,
      arrowcolor = arrow_colors
    )|> 
    layout(
      xaxis = list(range = c(0, lim),fixedrange = FALSE),
      yaxis = list(range = c(0, lim),fixedrange = FALSE)
    ) |> 
    print()
}

 # t=10; Ndf=Ndf_3D
drawNvec3D <- function(Ndf,t=0){
  nstages=length(which(grepl("N",names(Ndf))))
  lim=max(sapply(Ndf[,1:nstages],max))
  thisdf = Ndf[Ndf$label%in%paste0("year",0:t),]
  # alphas <- seq(0.4, 1, length.out = nrow(thisdf))
  # if(nrow(thisdf)==1) alphas=1
  # # Convert to rgba color strings
  # arrow_colors <- sprintf("rgba(70, 130, 180, %.2f)", alphas)  # steelblue with varying alpha
  
  mat1 = as.matrix(thisdf[,1:nstages])
  mat0 = mat1; mat0[] = 0
  
  dir = mat1 - mat0 #direction vector
  dir = dir / sqrt(rowSums(dir^2)) #unit vector
  
  fig = plotly_empty()
  
  fig = fig |> 
    add_trace(
      type = "scatter3d",
      mode = "text",
      x = thisdf$N1,
      y = thisdf$N2,
      z = thisdf$N3,
      text = thisdf$label,
      textposition = "top right",
      showlegend = FALSE
    )
  
  # fig
  #Add P0
  # fig = fig %>%
  #   add_markers(type = "scatter3d", mode = "markers", size = 1,
  #               x = mat0[,1], y = mat0[,2], z = mat0[,3],
  #               color = rep(1, length(mat0[,1])) ,colors = c("#000000", "#ff0000"),
  #               showlegend=F)
  # fig
  #Add P1
  # fig = fig %>%
  #   add_markers(type = "scatter3d", mode = "markers", size = 1,
  #               x = mat1[,1], y = mat1[,2], z = mat1[,3],
  #               color = rep(2, length(mat1[,1])) ,colors = c("#000000", "#ff0000"))
  # fig
  #Add Lines from P0 to P1
  fig = fig %>%
    add_trace(type = "scatter3d", mode = "lines", split = rep(1:length(mat0[,1]), each = 2),
              x = c(rbind(mat0[,1],mat1[,1])), y = c(rbind(mat0[,2],mat1[,2])), z = c(rbind(mat0[,3],mat1[,3])),
              color = rep(1, length(mat0[,1])*2), colors = c("#000000", "#ff0000"))
  # fig
  
  for (i in seq_len(nrow(thisdf))) {
    fig <- fig |> 
      add_trace(type = "cone",
              x = mat1[i,1], y = mat1[i,2], z = mat1[i,3],
              u = dir[i,1], v = dir[i,2], w = dir[i,3], sizeref=3, # sizeref=1, # sizeref=0.3,sizemode = "absolute",
              color = rep(1, length(mat1[,1])), colors = c("#000000"),
              showscale = FALSE
      )
  }
  
  # #Add Cones
  # fig = fig %>%
  #   add_trace(type = "cone",
  #             x = mat1[,1], y = mat1[,2], z = mat1[,3],
  #             u = dir[,1], v = dir[,2], w = dir[,3],  sizeref=0.1*(nrow(mat0)-1)^2, # sizeref=0.3,sizemode = "absolute",
  #             color = rep(1, length(mat1[,1])), colors = c("#000000"),
  #             showscale = FALSE)
  
  # fig
  
  fig = fig |> 
    layout(
      scene = list(
        xaxis = list(range = c(0, lim),title="N1"),
        yaxis = list(range = c(0, lim),title="N2"),
        zaxis = list(range = c(0, lim),title="N3"),
        camera = list(
          eye = list(x = 1.5, y = -1.5, z = 0.5)  # adjust to taste
        )#,
        # aspectmode = "cube"
      ),
      showlegend=FALSE
    )
  
  print(fig)
}

# set model params and inits -------

N0_2D = c(100,0)   # 2D pop vector
A_2D = matrix(c(0.1,0.5,1.1,0.8),ncol=2)

popbio::lambda(A_2D)

N0_3D=c(100,0,0)    # 3D pop vector
A_3D = matrix(c(0,0.5,0,0,0.4,0.3,1.9,0,0.85),ncol=3)

popbio::lambda(A_3D)

nyears=10
allyears = 0:nyears

# simulate stage abundance over time using matrix ------ 

Ndf_2D = doProj(N0_2D,A_2D,t=nyears)

Ndf_3D = doProj(N0_3D,A_3D,t=nyears)

# draw population as vectors in space/plane

drawNvec2D(Ndf_2D,10)

drawNvec3D(Ndf_3D,3)
  
  
# animate population growth -------

# y=1
for(y in 0:nyears){
  drawNvec2D(Ndf_2D,y)
  Sys.sleep(0.1)
}

y=1
for(y in 0:nyears){
  drawNvec3D(Ndf_3D,y)
  Sys.sleep(1)
}


  
  



# END SCRIPT 









