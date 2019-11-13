# How to make nice figures with R using plot()
# Martin Leclerc


# ggplot/ggplot2 is useful 
# https://www.rstudio.com/wp-content/uploads/2015/03/ggplot2-cheatsheet.pdf
# but here we will focus on the base plot function

# Some nice cheatsheets are available 
# https://www.rstudio.com/resources/cheatsheets/
# http://publish.illinois.edu/johnrgallagher/files/2015/10/BaseGraphicsCheatsheet.pdf
# http://rpubs.com/SusanEJohnston/7953
# https://graphicsprinciples.github.io/

# Use smart colours! Colorblind safe + Print version in black & white
# http://colorbrewer2.org/#type=sequential&scheme=BuGn&n=3

# See also, Sean Anderson's website: seananderson.ca
# A goldmine of content he's put together in there:
# https://seananderson.ca/ggplot2-fish554/
# https://seananderson.ca/courses/11-multipanel/multipanel.pdf

# See paper titled "How can we make better graphs? An initiative to increase the graphical expertise and productivity of quantitative scientists" 
# https://doi.org/10.1002/pst.1912 

# There is a lot of information out there on data visualization
# https://serialmentor.com/dataviz/

# Let's start!
# par = set graphical parameters
# mar = margin (1 = bottom and clockwise)
# bty = box type "n","u","l","o"
# bg = background colour

# plot = plotting R object
# col = colour
# rgb code for colour -> couleur1=rgb(/255,/255,/255,/255) #(fraction of red, green, blue, transparent)
# pch = plotting shape
# axes = F , do not plot axes # We will use the function axis later (tick marks, colour, etc)

# Create some colour
colour1=rgb(0.9,0.9,0.9,0.9)
colour2=rgb(0.3,0.3,0.3,0.3)
colour3=rgb(0.9,0.4,0.9,0.9)
colour4=rgb(0.4,0.8,0.4,0.8)


# Create data
x= 1:50
y= rnorm(50,10,2) + x
y2= rnorm(50,10,2)+x+20
plot(y~x)

# Arguments pch, bg, bty
par (mfrow=c(1,2),mar=c(4,4,2,2), bg = "grey50")
plot(y~x)
plot(y~x,col="blue",pch=21,bg="green",bty="n")

# Argument mar
par (mfrow=c(1,2),mar=c(8,8,2,2),bty="n", bg = "grey50")
plot(y~x)
plot(y~x,col="blue",pch=21,bg="green",bty="n")

# Argument mfrow
par (mfrow=c(2,1),mar=c(4,4,2,2),bty="n", bg = "grey50")
plot(y~x)
plot(y~x,col="blue",pch=21,bg="green",bty="n")
dev.off()


# We can adjust the axes and tick marks 
xlim=c(0,50);ylim=c(0,80)								# xlim and ylim to define limits of x and y axes
plot(y~x,col=colour1,pch=21,bg=colour2,xlim=xlim,ylim=ylim,axes=F) 	# axes = F
axis(side=1,at=seq(0,50,10),col="blue",col.axis="red",lwd=6)

# Argument lwd, las
plot(y~x,col=colour1,pch=21,bg=colour2,xlim=xlim,ylim=ylim,axes=F)
axis(side=1,at=seq(0,50,10),col="blue",col.axis="red",lwd=2,las=2) 	# Adjust label orientation

# Argument tck and mtext()
plot(y~x,col=colour1,pch=21,bg=colour2,xlim=xlim,ylim=ylim,axes=F,ylab="",xlab="")	# ylab=""
axis(side=1,at=seq(0,50,10),col="blue",col.axis="red")
axis(side=2,at=seq(0,80,10),col="blue",col.axis="red",tck=0.02)		# Inside tick marks
axis(side=2,at=seq(0,80,10),col="blue",col.axis="red",tck=-0.02)		
mtext(side=2,"Variable Y",line=2.5)
mtext(side=2,"Variable Y",line=1.5)

# If the text is too long we can split on two rows
mtext(side=1,"Variable X is way toooooo long and I need \n\ at least two rows for this enormous text",line=3) 	# Use \n\ to specify where to split
# Adjust bottom margin if necessary

# mtext for margin text
# text() for text within the graph
text(x=35,y=10,"I love doing graph in R sooooo much")

# Add another set of points on the same Y axis
points(y2~x,col=colour3,pch=21,bg=colour4)

# Insert legend (bty = box type = "n","u","o","l")
legend ("topleft",legend= c("Y","Y2"), bty="n",pch=c(21,21),col=c(colour1,colour3),pt.bg=c(colour2,colour4))
legend (x=5,y=70,legend= c("Y","Y2"), bty="o",pch=c(21,21),col=c(colour1,colour3),pt.bg=c(colour2,colour4))


# Use lines instead of data points
plot(y~x,col=colour1,pch=21,bg=colour2,xlim=xlim,ylim=ylim,axes=F,ylab="",xlab="",type="l",lwd=3) 	# Note type = "l"
axis(side=1,at=seq(0,50,10),col="blue",col.axis="red")
axis(side=2,at=seq(0,80,10),col="blue",col.axis="red",tck=0.02)
mtext(side=2,"Variable Y",line=2.5)
mtext(side=1,"Variable X is way toooooo long and I need \n\ at least two rows for this enormous text",line=3)
lines(y2~x,col=colour3,lwd=3,lty=3)										# lty = line type
legend ("topleft",legend= c("Y","Y2"),bty="n",col=c(colour1,colour3),lty=c(1,3),lwd=3)


# But what if not on the same Y values. Maybe you want to add a 2nd Y axes?
par(mar=c(4,4,2,5))
plot(y~x,col="grey50",xlim=xlim,axes=F,ylab="",xlab="",type="l",lwd=1)
axis(side=1,at=seq(0,50,10),col="black",col.axis="black",tck=0.02,lwd=1)
axis(side=2,at=seq(10,60,10),col="grey50",col.axis="grey50",tck=0.02,lwd=1)
mtext(side=2,"Variable Y",line=2.5, col="grey50")
mtext(side=1,"Variable X",line=2.5)

par(new=T)												# Will overlay graph
plot(y2*-1~x,col="purple",lwd=2,lty=3,type="l", xlab="",ylab="",axes=F)
axis(side=4,at=seq(-80,-30,10),col="purple",col.axis="purple",tck=0.02,lwd=1)
mtext(side=4,"Variable Y2",line=2.5, col="purple")
legend (x=20,y=-30,legend= c("Y","Y2"), bty="n",col=c("grey","purple"),lty=c(1,3),lwd=c(1,2))



# Want to plot distribution?
x1=rnorm(100,10,1)
x2=rnorm(100,12,1)
hist(x1)

# You can transform your data in density
dens1=density(x1)
dens2=density(x2)
plot(dens1)
plot(dens2)
rug((x2))
dens3=density(x2,from=10) # You can also add From-To as arguments
plot(dens3)
rug(jitter(x2))

par(mar=c(4,4,3,3))
xlim=c(4,16);ylim=c(0,0.5)

# Plot distribution
plot(dens1$y~dens1$x,axes=F,xlab="",ylab="",main="",xlim=xlim,ylim=ylim,type = "n")
axis(1,at=seq(4,16,2))
axis(2,at=seq(0,0.5,0.1))
polygon(dens1,col=rgb(1,0,0,1),lty=1,lwd=1)
polygon(dens2,col=rgb(0,1,0,1),lty=1,lwd=1)
legend("topleft",legend=c("Team 1","Team 2"), fill=c(rgb(1,0,0,1),rgb(0,1,0,1)),bty="n",border=F)

# Add transparency
plot(dens1$y~dens1$x,axes=F,xlab="",ylab="",main="",xlim=xlim,ylim=ylim,type = "n")
axis(1,at=seq(4,16,2))
axis(2,at=seq(0,0.5,0.1))
mtext(side=1,"Number of goal per game",line=2.5)
mtext(side=2,"Density",line=2.5)
polygon(dens1,col=rgb(1,0,0,0.6),lty=1,lwd=1)
polygon(dens2,col=rgb(0,1,0,0.6),lty=1,lwd=1)
legend("topleft",legend=c("Team 1","Team 2"), fill=c(rgb(1,0,0,0.6),rgb(0,1,0,0.6)),bty="n",border=F)
text(x=7,y=0.45,"t = -12.04, df = 198, p-value < 0.001")


###
# How to save/export figures
# There is always save.as, but there is other options

tiff(".\\Base Plot in R\\Density_distribution.tiff",
     res=300,
     pointsize=8,
     height=12,
     width=12,
     units="cm")

# Plot the figure
plot(dens1$y~dens1$x,axes=F,xlab="",ylab="",main="",xlim=xlim,ylim=ylim,type = "n")
axis(1,at=seq(4,16,2))
axis(2,at=seq(0,0.5,0.1))
mtext(side=1,"Number of goal per game",line=2.5)
mtext(side=2,"Density",line=2.5)
polygon(dens1,col=rgb(1,0,0,0.6),lty=1,lwd=1)
polygon(dens2,col=rgb(0,1,0,0.6),lty=1,lwd=1)
legend("topleft",legend=c("Team 1","Team 2"), fill=c(rgb(1,0,0,0.6),rgb(0,1,0,0.6)),bty="n",border=F)
text(x=7,y=0.45,"t = -12.04, df = 198, p-value < 0.001")

dev.off()

# Possibility to save as .pdf .eps .png ... or play with the background, etc.




# Barplot
a=c(8,12,14,16,30,180)
barplot(a)

# Make it look better
barplot(sort(a,decreasing=T),names.arg=c("A","B","C","D","E","F"),ylab="")
mtext(side=2,"Frequency",line=2.5)
mtext(side=1,"Journals",line=2.5)

# Create dataframe
age=c(4,4,7,4,8,5,5,4,9,22,12,9,4,9,17,11,9,7,9,10,10,11,8,9,14,6,5,5,4,4,4,13,5,19,5,4,5,17,5,9,6,8,4,5,6,4,10,4,5,11,4,11,16,18,19,4,4,4,9,8,8,4,13,10,6,5,9,4,5,9,10,10,5,4,12,6,
      11,5,9,6,11,11,7,8,8,9,7,4,4,5,10,9,5,14,16,8,11,8,4,17,4,15,7,8,7,4,13,14,8,6,6,6,11,13,6,6,16,8,8,4,5,9,4,10,4,11,12,13,14,4,8,10,6,5,14,7,7,4,4,6,5,8,5,5,10,11,9,10,4,6,9,4,
      11,9,14,11,14,15,14,8,6,10,9,7,4,6,4,8,6,9,10,5,7,9,7,10,8,4,6,4,5,4,7,10,4,10,4,12,11,6,8,7,17,16,12,5,6,13,11,7,5,11,9,5,15,18,12,4,4,5,4,4,6,5,6,8,9,11,4,5,4,6,5,4,6,10,4,8,
      5,6,8,5,4,6,5,4,8,6,4,5,4,8,6,4,5,6,4,18,4,4,4,9,10,11,5,4,4,5,6,5,4,6,5,13,4,4,4,20,22,15,18,18,21,19,13,16,19,10,9,24,22,16,20,18,12,10,8,7,18,16,14,10,16,12,8,15,11,18,7,13,5,5,6,8,
      6,5,8,10,4,12,21,15,10,4,11,8,6,16,9,20,4,17,7,6,9,10,4,8,6,4,6,5,4,10,14,8,16,12,6,4,8,11,9,7,13,7,4,10,8,11,13,16,5,10,12,17,14,4,6,11,4,15)

status=status=c(rep("h",157),rep("m",205))
b=data.frame(cbind(age,status));b[,1]<-as.numeric(as.character(b[,1]))
head(b);str(b)

# Do histogram/barplot
hist(b[b$status=="m","age"],col="black",ylab="Frequency",xlab="Age of female",main=NA,ylim=c(0,100),xlim=c(4,25))
hist(b[b$status=="h","age"],col="red",ylab="",main=NA,add=T)  # add=T
legend(15,60, c("Monitored female", "Harvested female"), col = c("black","red"),
       text.col = "black", pch = c(15, 15),
       merge = F, bg = "white")

# add = T and transparency
hist(b[b$status=="m","age"],col=rgb(0,0,1,0.6),ylab="",xlab="",main=NA,ylim=c(0,100),xlim=c(4,25))
hist(b[b$status=="h","age"],col=rgb(1,0,0,0.6),ylab="",main=NA,add=T)
mtext(side=1,"Age of female", line=2.5)
mtext(side=2,"Frequency", line=2.5)
legend(15,60, c("Monitored female", "Harvested female"), col = c(rgb(0,0,1,0.6),rgb(1,0,0,0.6)),
       text.col = "black", pch = c(15, 15),
       merge = F, bg = "white")

# Make it look even better! 
# Argument beside
barplot(t(table(age, status)), main="", ylab="",
        xlab="", col=c("red","black"),
        legend = c("Hunted","Monitored"), las=2, axes=T, beside=T, ylim=c(0,41))		# Note beside=T
mtext(side=1,"Age of female", line=2.5)
mtext(side=2,"Frequency", line=2.5)
text (x=40,y=30,"Chi2 test says it's all good")




# Actograms
gr2=read.table(".\\Base Plot in R\\uSy.txt",header=T)
str(gr2);head(gr2)

plot(gr2$jd~gr2$time,type="n",ylim=c(0,50),xlim=c(0,24),ylab="",xlab="",bty="n",axes=F)
points(gr2$jd~gr2$time,cex=1,pch=20)  
axis(1,at=seq(0,24,4))
axis(2,at=seq(0,50,10))
mtext(side=1,"Time of day (h)",line=2,cex=1.05)
mtext(side=2,"Julian date",line=2,cex=1.05)

# Add colours
plot(gr2$jd~gr2$time,type="n",ylim=c(0,50),xlim=c(0,24),ylab="",xlab="",bty="n",axes=F)
rbPal <- colorRampPalette(c("blue","royalblue","springgreen","green","yellow","red"))
Color <- rbPal(49)[round(gr2$temp+0.51)]					# Color by temp
points(gr2$jd~gr2$time,col=Color,cex=1,pch=20)  
axis(1,at=seq(0,24,4))
axis(2,at=seq(0,50,10))
mtext(side=1,"Time of day (h)",line=2.5,cex=1.05)
mtext(side=2,"Julian date",line=2.5,cex=1.05)

# Add scale with layout() ; see Argument mar 
layout(matrix(1:2,1,2), widths=c(5,1)) 					# Layout options
par(mar=c(3,3,2,0.5), oma=rep(2, 4))
plot(gr2$jd~gr2$time,type="n",ylim=c(0,50),xlim=c(0,24),ylab="",xlab="",bty="n",axes=F)
rbPal <- colorRampPalette(c("blue","royalblue","springgreen","green","yellow","red"))
Color <- rbPal(49)[round(gr2$temp+0.51)]					# Color by temp
points(gr2$jd~gr2$time,col=Color,cex=1,pch=20)  
axis(1,at=seq(0,24,4))
axis(2,at=seq(0,50,10))
mtext(side=1,"Time of day (h)",line=2.5,cex=1.05)
mtext(side=2,"Julian date",line=2.5,cex=1.05)

par(mar=c(2,0.5,3,2))
seg=c(0:48)
image(1,seg,matrix(1:49,1,49),col=rbPal(49),axes=FALSE)
axis(4,seq(0,48,4),las=2)
dev.off()

# Plot predictions from model output and see if it match allEffects function
#install.packages("effects",dependencies=T)
library(effects)
mod.cowles <- glm(volunteer ~ sex + neuroticism*extraversion,data=Cowles, family=binomial)
plot(allEffects(mod.cowles))


# Plot predictions from model 
newdataH=read.table(".\\Base Plot in R\\newdataH.txt",header=T)
newdataM=read.table(".\\Base Plot in R\\newdataM.txt",header=T)
moni=read.table(".\\Base Plot in R\\moni.txt",header=T)
mass2=read.table(".\\Base Plot in R\\mass2.txt",header=T)
plot(prediction~year,newdataH,col="red",ylim=c(-3,3),type="n",axes=F,ylab="",xlab="")
axis(1,seq(1996,2013,4))
axis(2,seq(-3,3,1))
mtext(side=1,"Year",line=2.5)
mtext(side=2,"Scaled mass of adult females",line=2.5)
points(mass_scaled~jitter(year),moni,col="black",bg="black",pch=21,cex=0.7)
points(mass_scaled~jitter(year),mass2,col="red",bg="red",pch=21,cex=0.7)
polygon(c(rev(newdataM$year),newdataM$year),c(rev(newdataM$plo),newdataM$phi), col=rgb(0,0,0,0.15), border = NA)
polygon(c(rev(newdataH$year),newdataH$year),c(rev(newdataH$lowCI),newdataH$highCI), col=rgb(1,0,0,0.15), border = NA)
points(prediction~year,newdataH,col="red",type="l",lwd=3)
points(mass_scaled~year,newdataM, col="black",type="l",lwd=3)


# Insert expression on the axes-legends-text
plot((prediction+3*2)~year,newdataH,col="red",ylim=c(0,12),type="n",axes=F,ylab="",xlab="")
axis(1,seq(1996,2013,4))
axis(2,seq(0,12,4))
mtext(side=1,text = expression("Average Conc of S- on plates" ~ (mu ~ moles ~ cm^{-2} ~ "dry wt")),line=2.5)
mtext(side=2,text = expression(sqrt(Mass)),line=2.5)
points(mass_scaled+3*2~year,newdataM, col="black",type="l",lwd=3)
# See also the package latex2exp

# Add image (silhouette)
# http://phylopic.org/  # For silhouette, but any PNG file works
library("png")
ima<-readPNG(".\\Base Plot in R\\Ursus.png")
rasterImage(image=ima,xleft=1996,xright=2004,ytop=5,ybottom=0)


# Use silhouette adequately
plot(prediction~year,newdataH,col="red",ylim=c(-3,3),type="n",axes=F,ylab="",xlab="")
axis(1,seq(1996,2013,4))
axis(2,seq(-3,3,1))
mtext(side=1,"Year",line=2.5)
mtext(side=2,"Scaled mass of adult females",line=2.5)
points(prediction*-2~year,newdataH,col="red",type="l",lwd=3)
points(mass_scaled*-3~year,newdataM, col="black",type="l",lwd=3)
rasterImage(image=ima,xleft=1996,xright=1999,ytop=-2.3,ybottom=-3)

ima2<-readPNG(".\\Base Plot in R\\Bird.png")
rasterImage(image=ima2,xleft=1996,xright=1999,ytop=1,ybottom=0.3)


# If you want to have broken axes
# Explore library(plotrix)




# Example of predictions with a lot of data
library(mgcv);library(lme4);library(doBy);library(MuMIn);library(effects)

# Male
load(".\\Base Plot in R\\Movement_Male_model_11_REML.RData")

# Do predicitons
new50=data.frame(age=mean(movemale$age), dayfromhunting=mean(movemale$dayfromhunting), hour=seq(0,10,length=200),died.this.year="no")
new51=data.frame(age=mean(movemale$age), dayfromhunting=mean(movemale$dayfromhunting), hour=seq(0,10,length=200),died.this.year="yes")
pred50=predict.gam(mm[[11]]$gam,newdata=new50, type="response", se=TRUE)
pred51=predict.gam(mm[[11]]$gam,newdata=new51, type="response", se=TRUE)
new50$hour2=new50$hour+2		# Swedish time zone
new51$hour2=new51$hour+2		# Swedish time zone
movemale$hour2=movemale$hour+2	# Swedish time zone

new60 =data.frame(age=mean(movemale$age), dayfromhunting=seq(min(movemale[movemale$died.this.year == "no", "dayfromhunting"]),max(movemale[movemale$died.this.year == "no", "dayfromhunting"]),length=200), hour=mean(movemale$hour)+2,died.this.year="no")
new60a=data.frame(age=mean(movemale$age), dayfromhunting=seq(min(movemale[movemale$died.this.year == "yes", "dayfromhunting"]),max(movemale[movemale$died.this.year == "yes", "dayfromhunting"]),length=200), hour=mean(movemale$hour)+2,died.this.year="yes")
pred60=predict.gam(mm[[11]]$gam,newdata=new60, type="response", se=TRUE)
pred60a=predict.gam(mm[[11]]$gam,newdata=new60a, type="response", se=TRUE)
MM=movemale


# Do Figures
# 1 A
plot(pred50$fit~new50$hour2,col="black",ylab="", xlab="",ylim=c(0,8),xlim=c(1.7,12.3), type="n",lwd=2,bty="n",axes=F)
axis(1,at=seq(2,12,2))
axis(2,at=seq(0,8,2))
mtext(side=1,"Time of day (h)",line=2,cex=1.05)
mtext(side=2,"log (movement (m/h))",line=2,cex=1.05)
lines(pred50$fit~new50$hour2,col="black",lwd=3)
lines(pred50$fit+1.96*pred50$se~new50$hour2,col="black",lty=2,lwd=1)
lines(pred50$fit-1.96*pred50$se~new50$hour2,col="black",lty=2,lwd=1)
lines(pred51$fit~new51$hour2,col="red",lwd=3)
lines(pred51$fit+1.96*pred51$se~new51$hour2,col="red",lty=2,lwd=1)
lines(pred51$fit-1.96*pred51$se~new51$hour2,col="red",lty=2,lwd=1)
x=c(5.5,5.5,11,11)
y=c(7.4,7.7,7.7,7.4)
polygon(x,y,col="blue",border=NA)
text(x=8.25,y=7.55,"High mortality risk period",col="white",font=2,cex=1)
mtext(side=2,text="(a)",col="black",line=2.1,at=8,las=2,font=2)
legend(x=9,y=7,col=c("black","red"),lwd=2,legend=c("Survived","Died"),lty=1, bg=rgb(1,1,1,0.7),box.col=rgb(1,1,1,0.7))

points(MM$ldist~MM$hour2,pch=19,cex=0.02,col="black")

# Not really useful

# Use jitter
plot(pred50$fit~new50$hour2,col="black",ylab="", xlab="",ylim=c(0,8),xlim=c(1.7,12.3), type="n",lwd=2,bty="n",axes=F)
points(MM$ldist~jitter(MM$hour2),pch=19,cex=0.02,col="black")

# Use jitter by status
plot(pred50$fit~new50$hour2,col="black",ylab="", xlab="",ylim=c(0,8),xlim=c(1.7,12.3), type="n",lwd=2,bty="n",axes=F)
points(ldist~jitter(hour2),MM[MM$died.this.year=="no",],pch=19,cex=0.02,col=rgb(0,0,0,0.5))
points(ldist~jitter(hour2),MM[MM$died.this.year=="yes",],pch=19,cex=0.02,col=rgb(1,0,0,0.5))

# Use jitter by status and spaced out
plot(pred50$fit~new50$hour2,col="black",ylab="", xlab="",ylim=c(0,8),xlim=c(1.7,12.3), type="n",lwd=2,bty="n",axes=F)
points(ldist~I(jitter(hour2)-0.15),MM[MM$died.this.year=="no",],pch=19,cex=0.02,col=rgb(0,0,0,0.5))
points(ldist~I(jitter(hour2)+0.15),MM[MM$died.this.year=="yes",],pch=19,cex=0.02,col=rgb(1,0,0,0.5))



###
# Example of final figures and script
##



############################################################
############################################################
###################### Figures #############################
#####  Hunter selects for behavioral traits of bears  ######
#####   https://doi.org/10.1038/s41598-019-48853-3    ######
############################################################

########################################
############## Figure 1 ################
########################################

library(mgcv);library(lme4);library(doBy);library(MuMIn);library(effects);library(rptR)

# Male
load(".\\Base Plot in R\\Movement_Male_model_11_REML.RData")

# Do predicitons
new50=data.frame(age=mean(movemale$age), dayfromhunting=mean(movemale$dayfromhunting), hour=seq(0,10,length=200),died.this.year="no")
new51=data.frame(age=mean(movemale$age), dayfromhunting=mean(movemale$dayfromhunting), hour=seq(0,10,length=200),died.this.year="yes")
pred50=predict.gam(mm[[11]]$gam,newdata=new50, type="response", se=TRUE)
pred51=predict.gam(mm[[11]]$gam,newdata=new51, type="response", se=TRUE)
new50$hour2=new50$hour+2		# Swedish time zone
new51$hour2=new51$hour+2		# Swedish time zone
movemale$hour2=movemale$hour+2	# Swedish time zone

new60 =data.frame(age=mean(movemale$age), dayfromhunting=seq(min(movemale[movemale$died.this.year == "no", "dayfromhunting"]),max(movemale[movemale$died.this.year == "no", "dayfromhunting"]),length=200), hour=mean(movemale$hour)+2,died.this.year="no")
new60a=data.frame(age=mean(movemale$age), dayfromhunting=seq(min(movemale[movemale$died.this.year == "yes", "dayfromhunting"]),max(movemale[movemale$died.this.year == "yes", "dayfromhunting"]),length=200), hour=mean(movemale$hour)+2,died.this.year="yes")
pred60=predict.gam(mm[[11]]$gam,newdata=new60, type="response", se=TRUE)
pred60a=predict.gam(mm[[11]]$gam,newdata=new60a, type="response", se=TRUE)
MM=movemale

new150=data.frame(age=mean(movemale$age), dayfromhunting=mean(movemale$dayfromhunting), hour=seq(0,10,length=200),died.this.year="no")
new151=data.frame(age=mean(movemale$age), dayfromhunting=mean(movemale$dayfromhunting), hour=seq(0,10,length=200),died.this.year="yes")
pred150=predict.gam(mm[[11]]$gam,newdata=new150, type="response", se=TRUE)
pred151=predict.gam(mm[[11]]$gam,newdata=new151, type="response", se=TRUE)
new150$hour2=new150$hour+2		# Swedish time zone
new151$hour2=new151$hour+2		# Swedish time zone
movemale$hour2=movemale$hour+2	# Swedish time zone

# Adjust path
  tiff(".\\Base Plot in R\\Leclerc Hunters-selection.tiff",
	res=1200,
	pointsize=8,
	height=7,
	width=20,
	units="cm")

par(mfrow=c(1,3),mar=c(4,4,1.5,1.5))

# 1 A
plot(pred50$fit~new50$hour2,col="black",ylab="", xlab="",ylim=c(0,8),xlim=c(1.7,12.3), type="n",lwd=2,bty="n",axes=F)
axis(1,at=seq(2,12,2))
axis(2,at=seq(0,8,2))
mtext(side=1,"Time of day (h)",line=2,cex=1.05)
mtext(side=2,"log (movement (m/h))",line=2,cex=1.05)

a=density(MM[MM$died.this.year == "yes" & MM$hour2 == 2,"ldist"])
aa=data.frame(y=a$x,x1=1.8-(((a$y)-min(a$y))/(max(a$y)-min(a$y))/10000)*length(MM[MM$died.this.year == "yes" & MM$hour2 == 2 ,"ldist"]),x2=1.8+(((a$y)-min(a$y))/(max(a$y)-min(a$y))/10000)*length(MM[MM$died.this.year == "yes" & MM$hour2 == 2 ,"ldist"]))
b=density(MM[MM$died.this.year == "no" & MM$hour2 == 2,"ldist"])
bb=data.frame(y=b$x,x1=2.2-(((b$y)-min(b$y))/(max(b$y)-min(b$y))/10000)*length(MM[MM$died.this.year == "no" & MM$hour2 == 2 ,"ldist"]),x2=2.2+(((b$y)-min(b$y))/(max(b$y)-min(b$y))/10000)*length(MM[MM$died.this.year == "no" & MM$hour2 == 2 ,"ldist"]))
polygon(c(rev(aa$x1),aa$x2),c(rev(aa$y),aa$y),col=rgb(1,0,0,0.2),border=NA)
polygon(c(rev(bb$x1),bb$x2),c(rev(bb$y),bb$y),col=rgb(0,0,0,0.3),border=NA)
a=density(MM[MM$died.this.year == "yes" & MM$hour2 == 3,"ldist"])
aa=data.frame(y=a$x,x1=2.8-(((a$y)-min(a$y))/(max(a$y)-min(a$y))/10000)*length(MM[MM$died.this.year == "yes" & MM$hour2 == 3 ,"ldist"]),x2=2.8+(((a$y)-min(a$y))/(max(a$y)-min(a$y))/10000)*length(MM[MM$died.this.year == "yes" & MM$hour2 == 3 ,"ldist"]))
b=density(MM[MM$died.this.year == "no" & MM$hour2 == 3,"ldist"])
bb=data.frame(y=b$x,x1=3.2-(((b$y)-min(b$y))/(max(b$y)-min(b$y))/10000)*length(MM[MM$died.this.year == "no" & MM$hour2 == 3 ,"ldist"]),x2=3.2+(((b$y)-min(b$y))/(max(b$y)-min(b$y))/10000)*length(MM[MM$died.this.year == "no" & MM$hour2 == 3 ,"ldist"]))
polygon(c(rev(aa$x1),aa$x2),c(rev(aa$y),aa$y),col=rgb(1,0,0,0.2),border=NA)
polygon(c(rev(bb$x1),bb$x2),c(rev(bb$y),bb$y),col=rgb(0,0,0,0.3),border=NA)
a=density(MM[MM$died.this.year == "yes" & MM$hour2 == 4,"ldist"])
aa=data.frame(y=a$x,x1=3.8-(((a$y)-min(a$y))/(max(a$y)-min(a$y))/10000)*length(MM[MM$died.this.year == "yes" & MM$hour2 == 4 ,"ldist"]),x2=3.8+(((a$y)-min(a$y))/(max(a$y)-min(a$y))/10000)*length(MM[MM$died.this.year == "yes" & MM$hour2 == 4 ,"ldist"]))
b=density(MM[MM$died.this.year == "no" & MM$hour2 == 4,"ldist"])
bb=data.frame(y=b$x,x1=4.2-(((b$y)-min(b$y))/(max(b$y)-min(b$y))/10000)*length(MM[MM$died.this.year == "no" & MM$hour2 == 4 ,"ldist"]),x2=4.2+(((b$y)-min(b$y))/(max(b$y)-min(b$y))/10000)*length(MM[MM$died.this.year == "no" & MM$hour2 == 4 ,"ldist"]))
polygon(c(rev(aa$x1),aa$x2),c(rev(aa$y),aa$y),col=rgb(1,0,0,0.2),border=NA)
polygon(c(rev(bb$x1),bb$x2),c(rev(bb$y),bb$y),col=rgb(0,0,0,0.3),border=NA)
a=density(MM[MM$died.this.year == "yes" & MM$hour2 == 5,"ldist"])
aa=data.frame(y=a$x,x1=4.8-(((a$y)-min(a$y))/(max(a$y)-min(a$y))/10000)*length(MM[MM$died.this.year == "yes" & MM$hour2 == 5 ,"ldist"]),x2=4.8+(((a$y)-min(a$y))/(max(a$y)-min(a$y))/10000)*length(MM[MM$died.this.year == "yes" & MM$hour2 == 5 ,"ldist"]))
b=density(MM[MM$died.this.year == "no" & MM$hour2 == 5,"ldist"])
bb=data.frame(y=b$x,x1=5.2-(((b$y)-min(b$y))/(max(b$y)-min(b$y))/10000)*length(MM[MM$died.this.year == "no" & MM$hour2 == 5 ,"ldist"]),x2=5.2+(((b$y)-min(b$y))/(max(b$y)-min(b$y))/10000)*length(MM[MM$died.this.year == "no" & MM$hour2 == 5 ,"ldist"]))
polygon(c(rev(aa$x1),aa$x2),c(rev(aa$y),aa$y),col=rgb(1,0,0,0.2),border=NA)
polygon(c(rev(bb$x1),bb$x2),c(rev(bb$y),bb$y),col=rgb(0,0,0,0.3),border=NA)
a=density(MM[MM$died.this.year == "yes" & MM$hour2 == 6,"ldist"])
aa=data.frame(y=a$x,x1=5.8-(((a$y)-min(a$y))/(max(a$y)-min(a$y))/10000)*length(MM[MM$died.this.year == "yes" & MM$hour2 == 6 ,"ldist"]),x2=5.8+(((a$y)-min(a$y))/(max(a$y)-min(a$y))/10000)*length(MM[MM$died.this.year == "yes" & MM$hour2 == 6 ,"ldist"]))
b=density(MM[MM$died.this.year == "no" & MM$hour2 == 6,"ldist"])
bb=data.frame(y=b$x,x1=6.2-(((b$y)-min(b$y))/(max(b$y)-min(b$y))/10000)*length(MM[MM$died.this.year == "no" & MM$hour2 == 6 ,"ldist"]),x2=6.2+(((b$y)-min(b$y))/(max(b$y)-min(b$y))/10000)*length(MM[MM$died.this.year == "no" & MM$hour2 == 6 ,"ldist"]))
polygon(c(rev(aa$x1),aa$x2),c(rev(aa$y),aa$y),col=rgb(1,0,0,0.2),border=NA)
polygon(c(rev(bb$x1),bb$x2),c(rev(bb$y),bb$y),col=rgb(0,0,0,0.3),border=NA)
a=density(MM[MM$died.this.year == "yes" & MM$hour2 == 7,"ldist"])
aa=data.frame(y=a$x,x1=6.8-(((a$y)-min(a$y))/(max(a$y)-min(a$y))/10000)*length(MM[MM$died.this.year == "yes" & MM$hour2 == 7 ,"ldist"]),x2=6.8+(((a$y)-min(a$y))/(max(a$y)-min(a$y))/10000)*length(MM[MM$died.this.year == "yes" & MM$hour2 == 7 ,"ldist"]))
b=density(MM[MM$died.this.year == "no" & MM$hour2 == 7,"ldist"])
bb=data.frame(y=b$x,x1=7.2-(((b$y)-min(b$y))/(max(b$y)-min(b$y))/10000)*length(MM[MM$died.this.year == "no" & MM$hour2 == 7 ,"ldist"]),x2=7.2+(((b$y)-min(b$y))/(max(b$y)-min(b$y))/10000)*length(MM[MM$died.this.year == "no" & MM$hour2 == 7 ,"ldist"]))
polygon(c(rev(aa$x1),aa$x2),c(rev(aa$y),aa$y),col=rgb(1,0,0,0.2),border=NA)
polygon(c(rev(bb$x1),bb$x2),c(rev(bb$y),bb$y),col=rgb(0,0,0,0.3),border=NA)
a=density(MM[MM$died.this.year == "yes" & MM$hour2 == 8,"ldist"])
aa=data.frame(y=a$x,x1=7.8-(((a$y)-min(a$y))/(max(a$y)-min(a$y))/10000)*length(MM[MM$died.this.year == "yes" & MM$hour2 == 8 ,"ldist"]),x2=7.8+(((a$y)-min(a$y))/(max(a$y)-min(a$y))/10000)*length(MM[MM$died.this.year == "yes" & MM$hour2 == 8 ,"ldist"]))
b=density(MM[MM$died.this.year == "no" & MM$hour2 == 8,"ldist"])
bb=data.frame(y=b$x,x1=8.2-(((b$y)-min(b$y))/(max(b$y)-min(b$y))/10000)*length(MM[MM$died.this.year == "no" & MM$hour2 == 8 ,"ldist"]),x2=8.2+(((b$y)-min(b$y))/(max(b$y)-min(b$y))/10000)*length(MM[MM$died.this.year == "no" & MM$hour2 == 8 ,"ldist"]))
polygon(c(rev(aa$x1),aa$x2),c(rev(aa$y),aa$y),col=rgb(1,0,0,0.2),border=NA)
polygon(c(rev(bb$x1),bb$x2),c(rev(bb$y),bb$y),col=rgb(0,0,0,0.3),border=NA)
a=density(MM[MM$died.this.year == "yes" & MM$hour2 == 9,"ldist"])
aa=data.frame(y=a$x,x1=8.8-(((a$y)-min(a$y))/(max(a$y)-min(a$y))/10000)*length(MM[MM$died.this.year == "yes" & MM$hour2 == 9 ,"ldist"]),x2=8.8+(((a$y)-min(a$y))/(max(a$y)-min(a$y))/10000)*length(MM[MM$died.this.year == "yes" & MM$hour2 == 9 ,"ldist"]))
b=density(MM[MM$died.this.year == "no" & MM$hour2 == 9,"ldist"])
bb=data.frame(y=b$x,x1=9.2-(((b$y)-min(b$y))/(max(b$y)-min(b$y))/10000)*length(MM[MM$died.this.year == "no" & MM$hour2 == 9 ,"ldist"]),x2=9.2+(((b$y)-min(b$y))/(max(b$y)-min(b$y))/10000)*length(MM[MM$died.this.year == "no" & MM$hour2 == 9 ,"ldist"]))
polygon(c(rev(aa$x1),aa$x2),c(rev(aa$y),aa$y),col=rgb(1,0,0,0.2),border=NA)
polygon(c(rev(bb$x1),bb$x2),c(rev(bb$y),bb$y),col=rgb(0,0,0,0.3),border=NA)
a=density(MM[MM$died.this.year == "yes" & MM$hour2 == 10,"ldist"])
aa=data.frame(y=a$x,x1=9.8-(((a$y)-min(a$y))/(max(a$y)-min(a$y))/10000)*length(MM[MM$died.this.year == "yes" & MM$hour2 == 10 ,"ldist"]),x2=9.8+(((a$y)-min(a$y))/(max(a$y)-min(a$y))/10000)*length(MM[MM$died.this.year == "yes" & MM$hour2 == 10 ,"ldist"]))
b=density(MM[MM$died.this.year == "no" & MM$hour2 == 10,"ldist"])
bb=data.frame(y=b$x,x1=10.2-(((b$y)-min(b$y))/(max(b$y)-min(b$y))/10000)*length(MM[MM$died.this.year == "no" & MM$hour2 == 10 ,"ldist"]),x2=10.2+(((b$y)-min(b$y))/(max(b$y)-min(b$y))/10000)*length(MM[MM$died.this.year == "no" & MM$hour2 ==  10 , "ldist"]))
polygon(c(rev(aa$x1),aa$x2),c(rev(aa$y),aa$y),col=rgb(1,0,0,0.2),border=NA)
polygon(c(rev(bb$x1),bb$x2),c(rev(bb$y),bb$y),col=rgb(0,0,0,0.3),border=NA)
a=density(MM[MM$died.this.year == "yes" & MM$hour2 == 11,"ldist"])
aa=data.frame(y=a$x,x1=10.8-(((a$y)-min(a$y))/(max(a$y)-min(a$y))/10000)*length(MM[MM$died.this.year == "yes" & MM$hour2 == 11 ,"ldist"]),x2=10.8+(((a$y)-min(a$y))/(max(a$y)-min(a$y))/10000)*length(MM[MM$died.this.year == "yes" & MM$hour2 == 11 ,"ldist"]))
b=density(MM[MM$died.this.year == "no" & MM$hour2 == 11,"ldist"])
bb=data.frame(y=b$x,x1=11.2-(((b$y)-min(b$y))/(max(b$y)-min(b$y))/10000)*length(MM[MM$died.this.year == "no" & MM$hour2 == 11 ,"ldist"]),x2=11.2+(((b$y)-min(b$y))/(max(b$y)-min(b$y))/10000)*length(MM[MM$died.this.year == "no" & MM$hour2 == 11 ,"ldist"]))
polygon(c(rev(aa$x1),aa$x2),c(rev(aa$y),aa$y),col=rgb(1,0,0,0.2),border=NA)
polygon(c(rev(bb$x1),bb$x2),c(rev(bb$y),bb$y),col=rgb(0,0,0,0.3),border=NA)
a=density(MM[MM$died.this.year == "yes" & MM$hour2 == 12,"ldist"])
aa=data.frame(y=a$x,x1=11.8-(((a$y)-min(a$y))/(max(a$y)-min(a$y))/10000)*length(MM[MM$died.this.year == "yes" & MM$hour2 == 12 ,"ldist"]),x2=11.8+(((a$y)-min(a$y))/(max(a$y)-min(a$y))/10000)*length(MM[MM$died.this.year == "yes" & MM$hour2 == 12 ,"ldist"]))
b=density(MM[MM$died.this.year == "no" & MM$hour2 == 12,"ldist"])
bb=data.frame(y=b$x,x1=12.2-(((b$y)-min(b$y))/(max(b$y)-min(b$y))/10000)*length(MM[MM$died.this.year == "no" & MM$hour2 == 12 ,"ldist"]),x2=12.2+(((b$y)-min(b$y))/(max(b$y)-min(b$y))/10000)*length(MM[MM$died.this.year == "no" & MM$hour2 == 12 ,"ldist"]))
polygon(c(rev(aa$x1),aa$x2),c(rev(aa$y),aa$y),col=rgb(1,0,0,0.2),border=NA)
polygon(c(rev(bb$x1),bb$x2),c(rev(bb$y),bb$y),col=rgb(0,0,0,0.3),border=NA)
lines(pred50$fit~new50$hour2,col="black",lwd=3)
lines(pred50$fit+1.96*pred50$se~new50$hour2,col="black",lty=2,lwd=1)
lines(pred50$fit-1.96*pred50$se~new50$hour2,col="black",lty=2,lwd=1)
lines(pred51$fit~new51$hour2,col="red",lwd=3)
lines(pred51$fit+1.96*pred51$se~new51$hour2,col="red",lty=2,lwd=1)
lines(pred51$fit-1.96*pred51$se~new51$hour2,col="red",lty=2,lwd=1)
x=c(5.5,5.5,11,11)
y=c(7.4,7.7,7.7,7.4)
polygon(x,y,col="blue",border=NA)
text(x=8.25,y=7.55,"High mortality risk period",col="white",font=2,cex=1)
mtext(side=2,text="(a)",col="black",line=2.1,at=8,las=2,font=2)
legend(x=9,y=7,col=c("black","red"),lwd=2,legend=c("Survived","Died"),lty=1, bg=rgb(1,1,1,0.7),box.col=rgb(1,1,1,0.7))


# 1 B
plot(pred150$fit~new150$hour2,col="black",ylab="", xlab="",ylim=c(0,8), type="l",lwd=2,bty="n",axes=F)
axis(1,at=seq(2,12,2))
axis(2,at=seq(0,8,2))
mtext(side=1,"Time of day (h)",line=2,cex=1.05)
mtext(side=2,"log (movement (m/h))",line=2,cex=1.05)
a=density(movemale[movemale$died.this.year == "yes" & movemale$hour2 == 2,"ldist"])
aa=data.frame(y=a$x,x1=1.8-(((a$y)-min(a$y))/(max(a$y)-min(a$y))/10000)*length(movemale[movemale$died.this.year == "yes" & movemale$hour2 == 2 ,"ldist"]),x2=1.8+(((a$y)-min(a$y))/(max(a$y)-min(a$y))/10000)*length(movemale[movemale$died.this.year == "yes" & movemale$hour2 == 2 ,"ldist"]))
b=density(movemale[movemale$died.this.year == "no" & movemale$hour2 == 2,"ldist"])
bb=data.frame(y=b$x,x1=2.2-(((b$y)-min(b$y))/(max(b$y)-min(b$y))/10000)*length(movemale[movemale$died.this.year == "no" & movemale$hour2 == 2 ,"ldist"]),x2=2.2+(((b$y)-min(b$y))/(max(b$y)-min(b$y))/10000)*length(movemale[movemale$died.this.year == "no" & movemale$hour2 == 2 ,"ldist"]))
polygon(c(rev(aa$x1),aa$x2),c(rev(aa$y),aa$y),col=rgb(1,0,0,0.2),border=NA)
polygon(c(rev(bb$x1),bb$x2),c(rev(bb$y),bb$y),col=rgb(0,0,0,0.3),border=NA)
a=density(movemale[movemale$died.this.year == "yes" & movemale$hour2 == 3,"ldist"])
aa=data.frame(y=a$x,x1=2.8-(((a$y)-min(a$y))/(max(a$y)-min(a$y))/10000)*length(movemale[movemale$died.this.year == "yes" & movemale$hour2 == 3 ,"ldist"]),x2=2.8+(((a$y)-min(a$y))/(max(a$y)-min(a$y))/10000)*length(movemale[movemale$died.this.year == "yes" & movemale$hour2 == 3 ,"ldist"]))
b=density(movemale[movemale$died.this.year == "no" & movemale$hour2 == 3,"ldist"])
bb=data.frame(y=b$x,x1=3.2-(((b$y)-min(b$y))/(max(b$y)-min(b$y))/10000)*length(movemale[movemale$died.this.year == "no" & movemale$hour2 == 3 ,"ldist"]),x2=3.2+(((b$y)-min(b$y))/(max(b$y)-min(b$y))/10000)*length(movemale[movemale$died.this.year == "no" & movemale$hour2 == 3 ,"ldist"]))
polygon(c(rev(aa$x1),aa$x2),c(rev(aa$y),aa$y),col=rgb(1,0,0,0.2),border=NA)
polygon(c(rev(bb$x1),bb$x2),c(rev(bb$y),bb$y),col=rgb(0,0,0,0.3),border=NA)
a=density(movemale[movemale$died.this.year == "yes" & movemale$hour2 == 4,"ldist"])
aa=data.frame(y=a$x,x1=3.8-(((a$y)-min(a$y))/(max(a$y)-min(a$y))/10000)*length(movemale[movemale$died.this.year == "yes" & movemale$hour2 == 4 ,"ldist"]),x2=3.8+(((a$y)-min(a$y))/(max(a$y)-min(a$y))/10000)*length(movemale[movemale$died.this.year == "yes" & movemale$hour2 == 4 ,"ldist"]))
b=density(movemale[movemale$died.this.year == "no" & movemale$hour2 == 4,"ldist"])
bb=data.frame(y=b$x,x1=4.2-(((b$y)-min(b$y))/(max(b$y)-min(b$y))/10000)*length(movemale[movemale$died.this.year == "no" & movemale$hour2 == 4 ,"ldist"]),x2=4.2+(((b$y)-min(b$y))/(max(b$y)-min(b$y))/10000)*length(movemale[movemale$died.this.year == "no" & movemale$hour2 == 4 ,"ldist"]))
polygon(c(rev(aa$x1),aa$x2),c(rev(aa$y),aa$y),col=rgb(1,0,0,0.2),border=NA)
polygon(c(rev(bb$x1),bb$x2),c(rev(bb$y),bb$y),col=rgb(0,0,0,0.3),border=NA)
a=density(movemale[movemale$died.this.year == "yes" & movemale$hour2 == 5,"ldist"])
aa=data.frame(y=a$x,x1=4.8-(((a$y)-min(a$y))/(max(a$y)-min(a$y))/10000)*length(movemale[movemale$died.this.year == "yes" & movemale$hour2 == 5 ,"ldist"]),x2=4.8+(((a$y)-min(a$y))/(max(a$y)-min(a$y))/10000)*length(movemale[movemale$died.this.year == "yes" & movemale$hour2 == 5 ,"ldist"]))
b=density(movemale[movemale$died.this.year == "no" & movemale$hour2 == 5,"ldist"])
bb=data.frame(y=b$x,x1=5.2-(((b$y)-min(b$y))/(max(b$y)-min(b$y))/10000)*length(movemale[movemale$died.this.year == "no" & movemale$hour2 == 5 ,"ldist"]),x2=5.2+(((b$y)-min(b$y))/(max(b$y)-min(b$y))/10000)*length(movemale[movemale$died.this.year == "no" & movemale$hour2 == 5 ,"ldist"]))
polygon(c(rev(aa$x1),aa$x2),c(rev(aa$y),aa$y),col=rgb(1,0,0,0.2),border=NA)
polygon(c(rev(bb$x1),bb$x2),c(rev(bb$y),bb$y),col=rgb(0,0,0,0.3),border=NA)
a=density(movemale[movemale$died.this.year == "yes" & movemale$hour2 == 6,"ldist"])
aa=data.frame(y=a$x,x1=5.8-(((a$y)-min(a$y))/(max(a$y)-min(a$y))/10000)*length(movemale[movemale$died.this.year == "yes" & movemale$hour2 == 6 ,"ldist"]),x2=5.8+(((a$y)-min(a$y))/(max(a$y)-min(a$y))/10000)*length(movemale[movemale$died.this.year == "yes" & movemale$hour2 == 6 ,"ldist"]))
b=density(movemale[movemale$died.this.year == "no" & movemale$hour2 == 6,"ldist"])
bb=data.frame(y=b$x,x1=6.2-(((b$y)-min(b$y))/(max(b$y)-min(b$y))/10000)*length(movemale[movemale$died.this.year == "no" & movemale$hour2 == 6 ,"ldist"]),x2=6.2+(((b$y)-min(b$y))/(max(b$y)-min(b$y))/10000)*length(movemale[movemale$died.this.year == "no" & movemale$hour2 == 6 ,"ldist"]))
polygon(c(rev(aa$x1),aa$x2),c(rev(aa$y),aa$y),col=rgb(1,0,0,0.2),border=NA)
polygon(c(rev(bb$x1),bb$x2),c(rev(bb$y),bb$y),col=rgb(0,0,0,0.3),border=NA)
a=density(movemale[movemale$died.this.year == "yes" & movemale$hour2 == 7,"ldist"])
aa=data.frame(y=a$x,x1=6.8-(((a$y)-min(a$y))/(max(a$y)-min(a$y))/10000)*length(movemale[movemale$died.this.year == "yes" & movemale$hour2 == 7 ,"ldist"]),x2=6.8+(((a$y)-min(a$y))/(max(a$y)-min(a$y))/10000)*length(movemale[movemale$died.this.year == "yes" & movemale$hour2 == 7 ,"ldist"]))
b=density(movemale[movemale$died.this.year == "no" & movemale$hour2 == 7,"ldist"])
bb=data.frame(y=b$x,x1=7.2-(((b$y)-min(b$y))/(max(b$y)-min(b$y))/10000)*length(movemale[movemale$died.this.year == "no" & movemale$hour2 == 7 ,"ldist"]),x2=7.2+(((b$y)-min(b$y))/(max(b$y)-min(b$y))/10000)*length(movemale[movemale$died.this.year == "no" & movemale$hour2 == 7 ,"ldist"]))
polygon(c(rev(aa$x1),aa$x2),c(rev(aa$y),aa$y),col=rgb(1,0,0,0.2),border=NA)
polygon(c(rev(bb$x1),bb$x2),c(rev(bb$y),bb$y),col=rgb(0,0,0,0.3),border=NA)
a=density(movemale[movemale$died.this.year == "yes" & movemale$hour2 == 8,"ldist"])
aa=data.frame(y=a$x,x1=7.8-(((a$y)-min(a$y))/(max(a$y)-min(a$y))/10000)*length(movemale[movemale$died.this.year == "yes" & movemale$hour2 == 8 ,"ldist"]),x2=7.8+(((a$y)-min(a$y))/(max(a$y)-min(a$y))/10000)*length(movemale[movemale$died.this.year == "yes" & movemale$hour2 == 8 ,"ldist"]))
b=density(movemale[movemale$died.this.year == "no" & movemale$hour2 == 8,"ldist"])
bb=data.frame(y=b$x,x1=8.2-(((b$y)-min(b$y))/(max(b$y)-min(b$y))/10000)*length(movemale[movemale$died.this.year == "no" & movemale$hour2 == 8 ,"ldist"]),x2=8.2+(((b$y)-min(b$y))/(max(b$y)-min(b$y))/10000)*length(movemale[movemale$died.this.year == "no" & movemale$hour2 == 8 ,"ldist"]))
polygon(c(rev(aa$x1),aa$x2),c(rev(aa$y),aa$y),col=rgb(1,0,0,0.2),border=NA)
polygon(c(rev(bb$x1),bb$x2),c(rev(bb$y),bb$y),col=rgb(0,0,0,0.3),border=NA)
a=density(movemale[movemale$died.this.year == "yes" & movemale$hour2 == 9,"ldist"])
aa=data.frame(y=a$x,x1=8.8-(((a$y)-min(a$y))/(max(a$y)-min(a$y))/10000)*length(movemale[movemale$died.this.year == "yes" & movemale$hour2 == 9 ,"ldist"]),x2=8.8+(((a$y)-min(a$y))/(max(a$y)-min(a$y))/10000)*length(movemale[movemale$died.this.year == "yes" & movemale$hour2 == 9 ,"ldist"]))
b=density(movemale[movemale$died.this.year == "no" & movemale$hour2 == 9,"ldist"])
bb=data.frame(y=b$x,x1=9.2-(((b$y)-min(b$y))/(max(b$y)-min(b$y))/10000)*length(movemale[movemale$died.this.year == "no" & movemale$hour2 == 9 ,"ldist"]),x2=9.2+(((b$y)-min(b$y))/(max(b$y)-min(b$y))/10000)*length(movemale[movemale$died.this.year == "no" & movemale$hour2 == 9 ,"ldist"]))
polygon(c(rev(aa$x1),aa$x2),c(rev(aa$y),aa$y),col=rgb(1,0,0,0.2),border=NA)
polygon(c(rev(bb$x1),bb$x2),c(rev(bb$y),bb$y),col=rgb(0,0,0,0.3),border=NA)
a=density(movemale[movemale$died.this.year == "yes" & movemale$hour2 == 10,"ldist"])
aa=data.frame(y=a$x,x1=9.8-(((a$y)-min(a$y))/(max(a$y)-min(a$y))/10000)*length(movemale[movemale$died.this.year == "yes" & movemale$hour2 == 10 ,"ldist"]),x2=9.8+(((a$y)-min(a$y))/(max(a$y)-min(a$y))/10000)*length(movemale[movemale$died.this.year == "yes" & movemale$hour2 == 10 ,"ldist"]))
b=density(movemale[movemale$died.this.year == "no" & movemale$hour2 == 10,"ldist"])
bb=data.frame(y=b$x,x1=10.2-(((b$y)-min(b$y))/(max(b$y)-min(b$y))/10000)*length(movemale[movemale$died.this.year == "no" & movemale$hour2 == 10 ,"ldist"]),x2=10.2+(((b$y)-min(b$y))/(max(b$y)-min(b$y))/10000)*length(movemale[movemale$died.this.year == "no" & movemale$hour2 == 10 ,"ldist"]))
polygon(c(rev(aa$x1),aa$x2),c(rev(aa$y),aa$y),col=rgb(1,0,0,0.2),border=NA)
polygon(c(rev(bb$x1),bb$x2),c(rev(bb$y),bb$y),col=rgb(0,0,0,0.3),border=NA)
a=density(movemale[movemale$died.this.year == "yes" & movemale$hour2 == 11,"ldist"])
aa=data.frame(y=a$x,x1=10.8-(((a$y)-min(a$y))/(max(a$y)-min(a$y))/10000)*length(movemale[movemale$died.this.year == "yes" & movemale$hour2 == 11 ,"ldist"]),x2=10.8+(((a$y)-min(a$y))/(max(a$y)-min(a$y))/10000)*length(movemale[movemale$died.this.year == "yes" & movemale$hour2 == 11 ,"ldist"]))
b=density(movemale[movemale$died.this.year == "no" & movemale$hour2 == 11,"ldist"])
bb=data.frame(y=b$x,x1=11.2-(((b$y)-min(b$y))/(max(b$y)-min(b$y))/10000)*length(movemale[movemale$died.this.year == "no" & movemale$hour2 == 11 ,"ldist"]),x2=11.2+(((b$y)-min(b$y))/(max(b$y)-min(b$y))/10000)*length(movemale[movemale$died.this.year == "no" & movemale$hour2 == 11 ,"ldist"]))
polygon(c(rev(aa$x1),aa$x2),c(rev(aa$y),aa$y),col=rgb(1,0,0,0.2),border=NA)
polygon(c(rev(bb$x1),bb$x2),c(rev(bb$y),bb$y),col=rgb(0,0,0,0.3),border=NA)
a=density(movemale[movemale$died.this.year == "yes" & movemale$hour2 == 12,"ldist"])
aa=data.frame(y=a$x,x1=11.8-(((a$y)-min(a$y))/(max(a$y)-min(a$y))/10000)*length(movemale[movemale$died.this.year == "yes" & movemale$hour2 == 12 ,"ldist"]),x2=11.8+(((a$y)-min(a$y))/(max(a$y)-min(a$y))/10000)*length(movemale[movemale$died.this.year == "yes" & movemale$hour2 == 12 ,"ldist"]))
b=density(movemale[movemale$died.this.year == "no" & movemale$hour2 == 12,"ldist"])
bb=data.frame(y=b$x,x1=12.2-(((b$y)-min(b$y))/(max(b$y)-min(b$y))/10000)*length(movemale[movemale$died.this.year == "no" & movemale$hour2 == 12 ,"ldist"]),x2=12.2+(((b$y)-min(b$y))/(max(b$y)-min(b$y))/10000)*length(movemale[movemale$died.this.year == "no" & movemale$hour2 == 12 ,"ldist"]))
polygon(c(rev(aa$x1),aa$x2),c(rev(aa$y),aa$y),col=rgb(1,0,0,0.2),border=NA)
polygon(c(rev(bb$x1),bb$x2),c(rev(bb$y),bb$y),col=rgb(0,0,0,0.3),border=NA)
lines(pred150$fit~new150$hour2,col="black",lwd=3)
lines(pred150$fit+1.96*pred150$se~new150$hour2,col="black",lty=2,lwd=1)
lines(pred150$fit-1.96*pred150$se~new150$hour2,col="black",lty=2,lwd=1)
lines(pred151$fit~new151$hour2,col="red",lwd=3)
lines(pred151$fit+1.96*pred151$se~new151$hour2,col="red",lty=2,lwd=1)
lines(pred151$fit-1.96*pred151$se~new151$hour2,col="red",lty=2,lwd=1)
x=c(5.5,5.5,11,11)
y=c(7.4,7.7,7.7,7.4)
polygon(x,y,col="blue",border=NA)
text(x=8.25,y=7.55,"High mortality risk period",col="white",font=2,cex=1)
mtext(side=2,text="(b)",col="black",line=2.1,at=8,las=2,font=2)
legend(x=9,y=7,col=c("black","red"),lwd=2,legend=c("Survived","Died"),lty=1, bg=rgb(1,1,1,0.7),box.col=rgb(1,1,1,0.7))


# 1 C
plot(pred60$fit~new60$dayfromhunting,col="black",ylab="", xlab="",ylim=c(0,8),xlim=c(-20.3,40.3), type="l",lwd=2,bty="n",axes=F)
axis(1,at=seq(-20,40,10))
axis(2,at=seq(0,8,2))
mtext(side=1,"Julian date",line=2,cex=1.05)
mtext(side=2,"log (movement (m/h))",line=2,cex=1.05)
c=MM[MM$dayfromhunting <= -11 & MM$dayfromhunting >= -20 & MM$died.this.year == "no","ldist"]
a=density(c)
aa=data.frame(y=a$x,x1=-13-(((a$y)-min(a$y))/(max(a$y)-min(a$y))/2000)*length(c),x2= (-13)+(((a$y)-min(a$y))/(max(a$y)-min(a$y))/2000)*length(c))
polygon(c(rev(aa$x1),aa$x2),c(rev(aa$y),aa$y),col=rgb(0,0,0,0.3),border=NA)
c=MM[MM$dayfromhunting <= -1 & MM$dayfromhunting >= -10 & MM$died.this.year == "no","ldist"]
a=density(c)
aa=data.frame(y=a$x,x1=-3-(((a$y)-min(a$y))/(max(a$y)-min(a$y))/2000)*length(c),x2= (-3)+(((a$y)-min(a$y))/(max(a$y)-min(a$y))/2000)*length(c))
polygon(c(rev(aa$x1),aa$x2),c(rev(aa$y),aa$y),col=rgb(0,0,0,0.3),border=NA)
c=MM[MM$dayfromhunting <= 9 & MM$dayfromhunting >= 0 & MM$died.this.year == "no","ldist"]
a=density(c)
aa=data.frame(y=a$x,x1=7-(((a$y)-min(a$y))/(max(a$y)-min(a$y))/2000)*length(c),x2= (7)+(((a$y)-min(a$y))/(max(a$y)-min(a$y))/2000)*length(c))
polygon(c(rev(aa$x1),aa$x2),c(rev(aa$y),aa$y),col=rgb(0,0,0,0.3),border=NA)
c=MM[MM$dayfromhunting <= 19 & MM$dayfromhunting >= 10 & MM$died.this.year == "no","ldist"]
a=density(c)
aa=data.frame(y=a$x,x1=17-(((a$y)-min(a$y))/(max(a$y)-min(a$y))/2000)*length(c),x2= (17)+(((a$y)-min(a$y))/(max(a$y)-min(a$y))/2000)*length(c))
polygon(c(rev(aa$x1),aa$x2),c(rev(aa$y),aa$y),col=rgb(0,0,0,0.3),border=NA)
c=MM[MM$dayfromhunting <= 29 & MM$dayfromhunting >= 20 & MM$died.this.year == "no","ldist"]
a=density(c)
aa=data.frame(y=a$x,x1=27-(((a$y)-min(a$y))/(max(a$y)-min(a$y))/2000)*length(c),x2= (27)+(((a$y)-min(a$y))/(max(a$y)-min(a$y))/2000)*length(c))
polygon(c(rev(aa$x1),aa$x2),c(rev(aa$y),aa$y),col=rgb(0,0,0,0.3),border=NA)
c=MM[MM$dayfromhunting <= 40 & MM$dayfromhunting >= 30 & MM$died.this.year == "no","ldist"]
a=density(c)
aa=data.frame(y=a$x,x1=37-(((a$y)-min(a$y))/(max(a$y)-min(a$y))/2000)*length(c),x2= (37)+(((a$y)-min(a$y))/(max(a$y)-min(a$y))/2000)*length(c))
polygon(c(rev(aa$x1),aa$x2),c(rev(aa$y),aa$y),col=rgb(0,0,0,0.3),border=NA)
c=MM[MM$dayfromhunting <= -11 & MM$dayfromhunting >= -20 & MM$died.this.year == "yes","ldist"]
a=density(c)
aa=data.frame(y=a$x,x1=-17-(((a$y)-min(a$y))/(max(a$y)-min(a$y))/2000)*length(c),x2= (-17)+(((a$y)-min(a$y))/(max(a$y)-min(a$y))/2000)*length(c))
polygon(c(rev(aa$x1),aa$x2),c(rev(aa$y),aa$y),col=rgb(1,0,0,0.3),border=NA)
c=MM[MM$dayfromhunting <= -1 & MM$dayfromhunting >= -10 & MM$died.this.year == "yes","ldist"]
a=density(c)
aa=data.frame(y=a$x,x1=-7-(((a$y)-min(a$y))/(max(a$y)-min(a$y))/2000)*length(c),x2= (-7)+(((a$y)-min(a$y))/(max(a$y)-min(a$y))/2000)*length(c))
polygon(c(rev(aa$x1),aa$x2),c(rev(aa$y),aa$y),col=rgb(1,0,0,0.3),border=NA)
c=MM[MM$dayfromhunting <= 9 & MM$dayfromhunting >= 0 & MM$died.this.year == "yes","ldist"]
a=density(c)
aa=data.frame(y=a$x,x1=3-(((a$y)-min(a$y))/(max(a$y)-min(a$y))/2000)*length(c),x2= (3)+(((a$y)-min(a$y))/(max(a$y)-min(a$y))/2000)*length(c))
polygon(c(rev(aa$x1),aa$x2),c(rev(aa$y),aa$y),col=rgb(1,0,0,0.3),border=NA)
c=MM[MM$dayfromhunting <= 19 & MM$dayfromhunting >= 10 & MM$died.this.year == "yes","ldist"]
a=density(c)
aa=data.frame(y=a$x,x1=13-(((a$y)-min(a$y))/(max(a$y)-min(a$y))/2000)*length(c),x2= (13)+(((a$y)-min(a$y))/(max(a$y)-min(a$y))/2000)*length(c))
polygon(c(rev(aa$x1),aa$x2),c(rev(aa$y),aa$y),col=rgb(1,0,0,0.3),border=NA)
c=MM[MM$dayfromhunting <= 29 & MM$dayfromhunting >= 20 & MM$died.this.year == "yes","ldist"]
a=density(c)
aa=data.frame(y=a$x,x1=23-(((a$y)-min(a$y))/(max(a$y)-min(a$y))/1000)*length(c),x2= (23)+(((a$y)-min(a$y))/(max(a$y)-min(a$y))/1000)*length(c))
polygon(c(rev(aa$x1),aa$x2),c(rev(aa$y),aa$y),col=rgb(1,0,0,0.3),border=NA)
#c=MM[MM$dayfromhunting <= 40 & MM$dayfromhunting >= 30 & MM$died.this.year == "yes","ldist"]
#a=density(c)
#aa=data.frame(y=a$x,x1=33-(((a$y)-min(a$y))/(max(a$y)-min(a$y))/2500)*length(c),x2= (33)+(((a$y)-min(a$y))/(max(a$y)-min(a$y))/2000)*length(c))
#polygon(c(rev(aa$x1),aa$x2),c(rev(aa$y),aa$y),col=rgb(1,0,0,0.3),border=NA)
lines(pred60$fit~new60$dayfromhunting,col="black",lwd=3)
lines(pred60$fit+1.96*pred60$se~new60$dayfromhunting,col="black",lty=2,lwd=1)
lines(pred60$fit-1.96*pred60$se~new60$dayfromhunting,col="black",lty=2,lwd=1)
lines(pred60a$fit~new60a$dayfromhunting,col="red",lwd=3)
lines(pred60a$fit+1.96*pred60a$se~new60a$dayfromhunting,col="red",lty=2,lwd=1)
lines(pred60a$fit-1.96*pred60a$se~new60a$dayfromhunting,col="red",lty=2,lwd=1)
mtext(side=2,text="(c)",col="black",line=2.1,at=8,las=2,font=2)
legend(x=20,y=7,col=c("black","red"),lwd=2,legend=c("Survived","Died"),lty=1, bg=rgb(1,1,1,0.7),box.col=rgb(1,1,1,0.7))
dev.off()



 