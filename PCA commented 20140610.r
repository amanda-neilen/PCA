#This is the PCA anlaysis that was done for the LCM paper. It uses the rgl package to draw the 3d graphics. rgl is quite powerful, but a pain to get all the lines and lighting drawn correctly.

#Your working directory will of course vary
setwd("YOURDIRECTORY")
library("rgl")

#Load the expression values. These are RMA normalized expression values (linearized), averaged across bioreps. Each column is a different seed tissue.
#Assumes tab-delimited format, with rownames in the far left column and row names for all the samples.
exp.data=read.table("YOURDATATABLE", header= TRUE, row.names=1,sep="\t")

#take the expression data and divide every data point by 1000. This is just to reduce the size of the values returned by the PCA (which are unitless).
#This also selects the first 31 columns of the datatable for the samples to exclude the whole seed samples which confound clustering analyses. Other datasets will very likely differ.
exp.data.selected=exp.data[,1:31]/10000

#This sets up the colors used in the plot for the four groups identified in the hierarchical clustering analysis. Modify as desired to match your analysis.
mat=colors()[36]
zyg=colors()[81]
sc=colors()[132]
cze=colors()[76]

#assigning colors for each sample in the dataframe.
cols=c(zyg, zyg, zyg, zyg, mat, zyg, zyg, zyg, zyg, mat, mat, zyg, zyg, zyg, mat, mat, cze, cze, cze, cze, mat, sc, sc, sc, sc, sc, sc, sc, sc, sc, sc)

#Do the PCA on your dataframe 
exp.data.selected.pca=prcomp(t(exp.data.selected))

#Extract the first three principal components. This is what you will plot.
plotme=cbind(exp.data.selected.pca$x[,1:3])

#This is the code to draw the 3d plot. Once executed, the plot in the X11 window can be rotated by clicking and draging with your mouse.
plot3d(plotme,col=cols,type="s",size=1.5,xlim=c(-2,2),ylim=c(-2,2),zlim=c(-2,2),box=FALSE)
for(i in row.names(plotme)){
 segments3d(c(plotme[i,1],plotme[i,1]),c(plotme[i,2],2),c(plotme[i,3],plotme[i,3]),col="grey89",lwd=3)
 }
 
#This removes the default bounding box so it can be replaced with something more aesthetically pleasing
rgl.clear(type="bbox")

#Draw new bounding box walls. The range is 2 to -2, which may need to be adjusted based on the scale of the values returned by the PCA

#Wall PC1 PC3 +PC2
quads3d(c(2,2,-2,-2),c(2,2,2,2),c(2,-2,-2,2),"beige")
#Wall PC1 PC2 (-PC3)
quads3d(c(2,-2,-2,2),c(2,2,-2,-2),c(-2,-2,-2,-2),"beige")
#Wall PC2 PC3 (-PC1)
quads3d(c(-2,-2,-2,-2),c(2,-2,-2,2),c(2,2,-2,-2),"beige") 



#Parameters for drawing the grid on the bounding box walls. If you want to adjust the color or width, you can do so here.
gridwidth=2
gridcol="grey68"

#Draw the lines for the grids on the three walls. 
#If you have adjusted the range of the bounding box, you will need to adjust the coordinates for the lines as well.
segments3d(c(-1.99,1.99),c(1.99,1.99),c(1.99,1.99),col=gridcol,lwd=gridwidth)
segments3d(c(-1.99,1.99),c(1.99,1.99),c(1,1),col=gridcol,lwd=gridwidth)
segments3d(c(-1.99,1.99),c(1.99,1.99),c(0,0),col=gridcol,lwd=gridwidth)
segments3d(c(-1.99,1.99),c(1.99,1.99),c(-1,-1),      col=gridcol,lwd=gridwidth)
segments3d(c(-1.99,1.99),c(1.99,1.99),c(-1.99,-1.99),col=gridcol,lwd=gridwidth)

segments3d(c(1.99,1.99),c(1.99,1.99),c(-1.99,1.99),col=gridcol,lwd=gridwidth)
segments3d(c(1,1),c(1.99,1.99),c(-1.99,1.99),col=gridcol,lwd=gridwidth)
segments3d(c(0,0),c(1.99,1.99),c(-1.99,1.99),col=gridcol,lwd=gridwidth)
segments3d(c(-1,-1),c(1.99,1.99),c(-1.99,1.99),col=gridcol,lwd=gridwidth)
segments3d(c(-1.99,-1.99),c(1.99,1.99),c(-1.99,1.99),col=gridcol,lwd=gridwidth)

#BackGrid
segments3d(c(-1.99,-1.99),c(1.99,-1.99),c(-1.99,-1.99), col=gridcol,lwd=gridwidth)
segments3d(c(-1,-1),c(1.99,-1.99),c(-1.99,-1.99),col=gridcol,lwd=gridwidth)
segments3d(c(0,0),c(1.99,-1.99),c(-1.99,-1.99),col=gridcol,lwd=gridwidth)
segments3d(c(1,1),c(1.99,-1.99),c(-1.99,-1.99),col=gridcol,lwd=gridwidth)
segments3d(c(1.99,1.99),c(1.99,-1.99),c(-1.99,-1.99),col=gridcol,lwd=gridwidth)

segments3d(c(1.99,-1.99),c(-1.99,-1.99),c(-1.99,-1.99),col=gridcol,lwd=gridwidth)
segments3d(c(1.99,-1.99),c(-1,-1),c(-1.99,-1.99),col=gridcol,lwd=gridwidth)
segments3d(c(1.99,-1.99),c(0,0),c(-1.99,-1.99),col=gridcol,lwd=gridwidth)
segments3d(c(1.99,-1.99),c(1,1),c(-1.99,-1.99),col=gridcol,lwd=gridwidth)
segments3d(c(1.99,-1.99),c(1.99,1.99),c(-1.99,-1.99),col=gridcol,lwd=gridwidth)

#Right Grid
segments3d(c(-1.99,-1.99),c(1.99,-1.99),c(-1.99,-1.99),col=gridcol, lwd=gridwidth)
segments3d(c(-1.99,-1.99),c(1.99,-1.99),c(-1,-1),col=gridcol, lwd=gridwidth)
segments3d(c(-1.99,-1.99),c(1.99,-1.99),c(0,0),col=gridcol, lwd=gridwidth)
segments3d(c(-1.99,-1.99),c(1.99,-1.99),c(1,1),col=gridcol, lwd=gridwidth)
segments3d(c(-1.99,-1.99),c(1.99,-1.99),c(1.99,1.99),col=gridcol, lwd=gridwidth)

segments3d(c(-1.99,-1.99),c(-1.99,-1.99),c(-1.99,1.99),col=gridcol, lwd=gridwidth)
segments3d(c(-1.99,-1.99),c(-1,-1),c(-1.99,1.99),col=gridcol, lwd=gridwidth)
segments3d(c(-1.99,-1.99),c(0,0),c(-1.99,1.99),col=gridcol, lwd=gridwidth)
segments3d(c(-1.99,-1.99),c(1,1),c(-1.99,1.99),col=gridcol, lwd=gridwidth)
segments3d(c(-1.99,-1.99),c(1.99,1.99),c(-1.99,1.99),col=gridcol, lwd=gridwidth)

#lighting
#remove the default lighting

clear3d(type="lights")

#Add in lights. This requires fiddling. Best result in our case obtained using two light sources. 

light3d(theta=90,phi=355)
light3d(theta=0,phi=25)

#Write out your image

rgl.postscript("myplot.pdf",fmt="pdf")