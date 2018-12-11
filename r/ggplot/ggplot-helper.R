# empty theme
theme_empty <- function(title.spacing=0, subtitle.spacing=0, title.bold=F){
  thm <- theme(panel.grid.major = element_blank(), 
               panel.grid.minor = element_blank(), 
               panel.background = element_blank(),
               axis.line = element_blank(),
               axis.title = element_blank(),
               axis.ticks = element_blank(),
               axis.text = element_blank(),
               legend.key = element_blank(),
               plot.title = element_text(margin=margin(0,0,title.spacing,0)),
               plot.subtitle = element_text(margin=margin(0,0,subtitle.spacing,0))
               )
  if(isTRUE(title.bold)){
    thm + theme(plot.title = element_text(face = 'bold'))
  }
  return(thm)
}

theme_void <- function(grid='major', grid.color='lightgrey', title.spacing=0, subtitle.spacing=0, x.lab.spacing=0, y.lab.spacing=0){
  thm <- theme(
    panel.background = element_blank(),
    plot.title = element_text(face='bold', margin=margin(0,0,title.spacing,0)),
    plot.subtitle = element_text(margin=margin(0,0,subtitle.spacing,0)),
    axis.title.x = element_text(margin=margin(x.lab.spacing,0,0,0)),
    axis.title.y = element_text(margin=margin(0,y.lab.spacing,0,0))
    )
  
  if(identical(grid, 'major')){
    thm <- thm + theme(
      panel.grid.major = element_line(color = grid.color), 
      panel.grid.minor = element_blank()
    )
  }
  if(identical(grid, 'minor')){
    thm <- thm + theme(
      panel.grid.major = element_blank(),
      panel.grid.minor = element_line(color = grid.color)
    )
  }
  if(identical(grid, 'both')){
    thm <- thm + theme(
      panel.grid.major = element_line(color = grid.color),
      panel.grid.minor = element_line(color = grid.color)
    )
  }
  if(identical(grid, 'none')){
    thm <- thm + theme(
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank()
    )
  }
  return(thm)
}

theme_void_T <- theme(panel.background = element_blank(),
                    plot.title = element_text(face='bold'),
                    panel.grid.major = element_blank(), 
                    panel.grid.minor = element_blank())

##### option both doesn't work yet!!!!::::
null.expand <- function(gg.object=NULL,
                        axis='both', 
                        type.x='continuous', 
                        type.y='continuous',
                        add.opts=NULL){
  if(identical(axis, 'both')){
    eval(parse(text=paste('gg.object', 
                          '+', 
                          paste0('scale_x_', type.x, '(expand=c(0.05,0.05))'),
                          '+',
                          paste0('scale_y_', type.y, '(expand=c(0,0))'))))
  }
  if(identical(axis, 'x')){
    return(eval(parse(text=paste0('scale_x_', type.x, '(expand=c(0.05,0.05)', add.opts, ')'))))
  }
  if(identical(axis, 'y')){
    return(eval(parse(text=paste0('scale_y_', type.y, '(expand=c(0,0)', add.opts, ')'))))
  }
}

# crop legend from plot
g_legend<- function(a.gplot){
  tmp <- ggplot_gtable(ggplot_build(a.gplot))
  leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
  legend <- tmp$grobs[[leg]]
  return(legend)
}

# generate circular data
circleFun <- function(center = c(0,0),diameter = 1, npoints = 100){
  r = diameter / 2
  tt <- seq(0,2*pi,length.out = npoints)
  xx <- center[1] + r * cos(tt)
  yy <- center[2] + r * sin(tt)
  return(data.frame(x = xx, y = yy))
}

# rescaler function for continuous fills
center_around <- function(center=0) {
  function(x, to=NA, from=NA) {
    r <- max(abs(from-center))
    (x - (center-r)) / 2/r
  }
}

## Do not run
# scale_fill_gradientn(colors = c('#011A27', '#F0810F', '#E6DF44'), rescaler = center_around(2000))