
palette.summary<-function(){
for (i in seq_along(palette.list)){
if (i %in% c(1)){cat("Impressionism","\n")}
if (i %in% c(6)){cat("post-Impressionism","\n")}
if (i %in% c(10)){cat("Fauvism","\n")}
cat(noquote(paste(i, "artist:", names(palette.list)[i], sep = " ")),"\n")
cat(noquote("paintings:"),"\n")
for (p in 1:length(names(palette.list[[i]]))){
cat(paste(p, ":", " ", sep = ""), names(palette.list[[i]])[p], "\n", sep = "")
}
cat("\n")
}
}

see.palette<-function(artist, painting){

colores<-palette.list[[artist]][[painting]][["colors"]]

graphic<-matrix(ncol=2, nrow = length(colores))
graphic[,1]<-colores
graphic[,2]<-1

if (ncol(palette.list[[artist]][[painting]][["picture.matrix"]])>nrow(palette.list[[artist]][[painting]][["picture.matrix"]])){
horizontal<-TRUE} else {horizontal<-FALSE}

old.par <- par(no.readonly = TRUE)   
on.exit(par(old.par))

par(mfrow=c(2,1), mar=c(1.2,2,1.3,2))
plot.new()
mtext(paste(names(palette.list)[[artist]], names(palette.list[[artist]])[painting], sep = " - "), outer=FALSE,  cex=1, line=0)
if (horizontal==TRUE){
rasterImage(palette.list[[artist]][[painting]][["picture.matrix"]], ybottom = 0, ytop = 1, xleft = 0.25, xright = 0.75) #con esto bien, la imagen te queda siempre bien
}
if (horizontal==FALSE){
rasterImage(palette.list[[artist]][[painting]][["picture.matrix"]], ybottom = 0, ytop = 1, xleft = 0.35, xright = 0.65) #con esto bien, la imagen te queda siempre bien
}
barplot(as.numeric(graphic[,2]), col = graphic[,1], mgp = c(1,0,0), axes = FALSE, names.arg = 1:length(colores), cex.names = 0.9, xlab = "" #, font.lab=1, cex.lab=1.1
)
title(main = "palette", font.main=1, line=0.5, cex.main=1)
}

get.color<-function(artist,painting,color){
colors<-palette.list[[artist]][[painting]]$colors[color]
return(colors)
}

see.all.paintings<-function(){

  old.par <- par(no.readonly = TRUE)   
  on.exit(par(old.par))
  
  par(mfrow=c(6,4), mar=c(1.2,2,1.3,2))   #bottom,left,top,right
  
  for (artist in seq_along(palette.list)){
    for (painting in seq_along(palette.list[[artist]])){

      if (ncol(palette.list[[artist]][[painting]][["picture.matrix"]])>nrow(palette.list[[artist]][[painting]][["picture.matrix"]])){
        horizontal<-TRUE} else {horizontal<-FALSE}

      plot.new()
      mtext(paste(names(palette.list[[artist]])[painting]), outer=FALSE,  cex=0.5, line=0)

      if (horizontal==TRUE){
        rasterImage(palette.list[[artist]][[painting]][["picture.matrix"]], ybottom = 0, ytop = 1, xleft = 0.15, xright = 0.85) #con esto bien, la imagen te queda siempre bien
      }

      if (horizontal==FALSE){
        rasterImage(palette.list[[artist]][[painting]][["picture.matrix"]], ybottom = 0, ytop = 1, xleft = 0.27, xright = 0.73) #con esto bien, la imagen te queda siempre bien
      }

    }}
}



