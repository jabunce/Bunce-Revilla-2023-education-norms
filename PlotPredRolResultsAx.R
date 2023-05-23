
##
post <- post5
str(post)


abils_grd_sch_yr <- list(
                        # Comm year 1                            rol     sch     yr                                             sch      yr                                             sch      yr    grade
                        post$muInt + pull(post, paste("off_Sch[", 2, ",", 1, ",", 1, "]", sep="")) + pull(post, paste("bGrade[", 1, ",", 1, "]", sep=""))*pull(post, paste("CumuGrade[", 1, ",", 1, ",", 1, "]", sep="")), #Comm G1 yr1     1
                        post$muInt + pull(post, paste("off_Sch[", 2, ",", 1, ",", 1, "]", sep="")) + pull(post, paste("bGrade[", 1, ",", 1, "]", sep=""))*pull(post, paste("CumuGrade[", 1, ",", 1, ",", 2, "]", sep="")), #G2              2
                        post$muInt + pull(post, paste("off_Sch[", 2, ",", 1, ",", 1, "]", sep="")) + pull(post, paste("bGrade[", 1, ",", 1, "]", sep=""))*pull(post, paste("CumuGrade[", 1, ",", 1, ",", 3, "]", sep="")), #G3              3
                        post$muInt + pull(post, paste("off_Sch[", 2, ",", 1, ",", 1, "]", sep="")) + pull(post, paste("bGrade[", 1, ",", 1, "]", sep=""))*pull(post, paste("CumuGrade[", 1, ",", 1, ",", 4, "]", sep="")), #G4              4
                        post$muInt + pull(post, paste("off_Sch[", 2, ",", 1, ",", 1, "]", sep="")) + pull(post, paste("bGrade[", 1, ",", 1, "]", sep=""))*pull(post, paste("CumuGrade[", 1, ",", 1, ",", 5, "]", sep="")), #G5              5    
                        # Comm year 2
                        post$muInt + pull(post, paste("off_Sch[", 2, ",", 1, ",", 2, "]", sep="")) + pull(post, paste("bGrade[", 1, ",", 2, "]", sep=""))*pull(post, paste("CumuGrade[", 1, ",", 2, ",", 1, "]", sep="")), #Comm G1 yr2     6
                        post$muInt + pull(post, paste("off_Sch[", 2, ",", 1, ",", 2, "]", sep="")) + pull(post, paste("bGrade[", 1, ",", 2, "]", sep=""))*pull(post, paste("CumuGrade[", 1, ",", 2, ",", 2, "]", sep="")), #G2              7
                        post$muInt + pull(post, paste("off_Sch[", 2, ",", 1, ",", 2, "]", sep="")) + pull(post, paste("bGrade[", 1, ",", 2, "]", sep=""))*pull(post, paste("CumuGrade[", 1, ",", 2, ",", 3, "]", sep="")), #G3              8
                        post$muInt + pull(post, paste("off_Sch[", 2, ",", 1, ",", 2, "]", sep="")) + pull(post, paste("bGrade[", 1, ",", 2, "]", sep=""))*pull(post, paste("CumuGrade[", 1, ",", 2, ",", 4, "]", sep="")), #G4              9
                        post$muInt + pull(post, paste("off_Sch[", 2, ",", 1, ",", 2, "]", sep="")) + pull(post, paste("bGrade[", 1, ",", 2, "]", sep=""))*pull(post, paste("CumuGrade[", 1, ",", 2, ",", 5, "]", sep="")), #G5              10


                        # SecN year 1
                        post$muInt + pull(post, paste("off_Sch[", 2, ",", 2, ",", 1, "]", sep="")) + pull(post, paste("bGrade[", 2, ",", 1, "]", sep=""))*pull(post, paste("CumuGrade[", 2, ",", 1, ",", 1, "]", sep="")), #SecN G1 yr1     11
                        post$muInt + pull(post, paste("off_Sch[", 2, ",", 2, ",", 1, "]", sep="")) + pull(post, paste("bGrade[", 2, ",", 1, "]", sep=""))*pull(post, paste("CumuGrade[", 2, ",", 1, ",", 2, "]", sep="")), #G2              12
                        post$muInt + pull(post, paste("off_Sch[", 2, ",", 2, ",", 1, "]", sep="")) + pull(post, paste("bGrade[", 2, ",", 1, "]", sep=""))*pull(post, paste("CumuGrade[", 2, ",", 1, ",", 3, "]", sep="")), #G3              13
                        post$muInt + pull(post, paste("off_Sch[", 2, ",", 2, ",", 1, "]", sep="")) + pull(post, paste("bGrade[", 2, ",", 1, "]", sep=""))*pull(post, paste("CumuGrade[", 2, ",", 1, ",", 4, "]", sep="")), #G4              14
                        post$muInt + pull(post, paste("off_Sch[", 2, ",", 2, ",", 1, "]", sep="")) + pull(post, paste("bGrade[", 2, ",", 1, "]", sep=""))*pull(post, paste("CumuGrade[", 2, ",", 1, ",", 5, "]", sep="")), #G5              15
                        # SecN year 2
                        post$muInt + pull(post, paste("off_Sch[", 2, ",", 2, ",", 2, "]", sep="")) + pull(post, paste("bGrade[", 2, ",", 2, "]", sep=""))*pull(post, paste("CumuGrade[", 2, ",", 2, ",", 1, "]", sep="")), #SecN G1 yr2     16
                        post$muInt + pull(post, paste("off_Sch[", 2, ",", 2, ",", 2, "]", sep="")) + pull(post, paste("bGrade[", 2, ",", 2, "]", sep=""))*pull(post, paste("CumuGrade[", 2, ",", 2, ",", 2, "]", sep="")), #G2              17
                        post$muInt + pull(post, paste("off_Sch[", 2, ",", 2, ",", 2, "]", sep="")) + pull(post, paste("bGrade[", 2, ",", 2, "]", sep=""))*pull(post, paste("CumuGrade[", 2, ",", 2, ",", 3, "]", sep="")), #G3              18
                        post$muInt + pull(post, paste("off_Sch[", 2, ",", 2, ",", 2, "]", sep="")) + pull(post, paste("bGrade[", 2, ",", 2, "]", sep=""))*pull(post, paste("CumuGrade[", 2, ",", 2, ",", 4, "]", sep="")), #G4              19
                        post$muInt + pull(post, paste("off_Sch[", 2, ",", 2, ",", 2, "]", sep="")) + pull(post, paste("bGrade[", 2, ",", 2, "]", sep=""))*pull(post, paste("CumuGrade[", 2, ",", 2, ",", 5, "]", sep="")), #G5              20
                        

                        # SecF year 1
                        post$muInt + pull(post, paste("off_Sch[", 2, ",", 3, ",", 1, "]", sep="")) + pull(post, paste("bGrade[", 3, ",", 1, "]", sep=""))*pull(post, paste("CumuGrade[", 3, ",", 1, ",", 1, "]", sep="")), #SecF G1 yr1     21
                        post$muInt + pull(post, paste("off_Sch[", 2, ",", 3, ",", 1, "]", sep="")) + pull(post, paste("bGrade[", 3, ",", 1, "]", sep=""))*pull(post, paste("CumuGrade[", 3, ",", 1, ",", 2, "]", sep="")), #G2              22
                        post$muInt + pull(post, paste("off_Sch[", 2, ",", 3, ",", 1, "]", sep="")) + pull(post, paste("bGrade[", 3, ",", 1, "]", sep=""))*pull(post, paste("CumuGrade[", 3, ",", 1, ",", 3, "]", sep="")), #G3              23
                        post$muInt + pull(post, paste("off_Sch[", 2, ",", 3, ",", 1, "]", sep="")) + pull(post, paste("bGrade[", 3, ",", 1, "]", sep=""))*pull(post, paste("CumuGrade[", 3, ",", 1, ",", 4, "]", sep="")), #G4              24
                        post$muInt + pull(post, paste("off_Sch[", 2, ",", 3, ",", 1, "]", sep="")) + pull(post, paste("bGrade[", 3, ",", 1, "]", sep=""))*pull(post, paste("CumuGrade[", 3, ",", 1, ",", 5, "]", sep="")), #G5              25
                        # SecF year 2
                        post$muInt + pull(post, paste("off_Sch[", 2, ",", 3, ",", 2, "]", sep="")) + pull(post, paste("bGrade[", 3, ",", 2, "]", sep=""))*pull(post, paste("CumuGrade[", 3, ",", 2, ",", 1, "]", sep="")), #SecF G1 yr2     26
                        post$muInt + pull(post, paste("off_Sch[", 2, ",", 3, ",", 2, "]", sep="")) + pull(post, paste("bGrade[", 3, ",", 2, "]", sep=""))*pull(post, paste("CumuGrade[", 3, ",", 2, ",", 2, "]", sep="")), #G2              27
                        post$muInt + pull(post, paste("off_Sch[", 2, ",", 3, ",", 2, "]", sep="")) + pull(post, paste("bGrade[", 3, ",", 2, "]", sep=""))*pull(post, paste("CumuGrade[", 3, ",", 2, ",", 3, "]", sep="")), #G3              28
                        post$muInt + pull(post, paste("off_Sch[", 2, ",", 3, ",", 2, "]", sep="")) + pull(post, paste("bGrade[", 3, ",", 2, "]", sep=""))*pull(post, paste("CumuGrade[", 3, ",", 2, ",", 4, "]", sep="")), #G4              29
                        post$muInt + pull(post, paste("off_Sch[", 2, ",", 3, ",", 2, "]", sep="")) + pull(post, paste("bGrade[", 3, ",", 2, "]", sep=""))*pull(post, paste("CumuGrade[", 3, ",", 2, ",", 5, "]", sep="")), #G5              30
                        

                        # RelF year 1
                        post$muInt + pull(post, paste("off_Sch[", 2, ",", 4, ",", 1, "]", sep="")) + pull(post, paste("bGrade[", 4, ",", 1, "]", sep=""))*pull(post, paste("CumuGrade[", 4, ",", 1, ",", 1, "]", sep="")), #RelF G1 yr1     31
                        post$muInt + pull(post, paste("off_Sch[", 2, ",", 4, ",", 1, "]", sep="")) + pull(post, paste("bGrade[", 4, ",", 1, "]", sep=""))*pull(post, paste("CumuGrade[", 4, ",", 1, ",", 2, "]", sep="")), #G2              32
                        post$muInt + pull(post, paste("off_Sch[", 2, ",", 4, ",", 1, "]", sep="")) + pull(post, paste("bGrade[", 4, ",", 1, "]", sep=""))*pull(post, paste("CumuGrade[", 4, ",", 1, ",", 3, "]", sep="")), #G3              33
                        post$muInt + pull(post, paste("off_Sch[", 2, ",", 4, ",", 1, "]", sep="")) + pull(post, paste("bGrade[", 4, ",", 1, "]", sep=""))*pull(post, paste("CumuGrade[", 4, ",", 1, ",", 4, "]", sep="")), #G4              34
                        post$muInt + pull(post, paste("off_Sch[", 2, ",", 4, ",", 1, "]", sep="")) + pull(post, paste("bGrade[", 4, ",", 1, "]", sep=""))*pull(post, paste("CumuGrade[", 4, ",", 1, ",", 5, "]", sep="")), #G5              35
                        # RelF year 2
                        post$muInt + pull(post, paste("off_Sch[", 2, ",", 4, ",", 2, "]", sep="")) + pull(post, paste("bGrade[", 4, ",", 2, "]", sep=""))*pull(post, paste("CumuGrade[", 4, ",", 2, ",", 1, "]", sep="")), #RelF G1 yr2     36
                        post$muInt + pull(post, paste("off_Sch[", 2, ",", 4, ",", 2, "]", sep="")) + pull(post, paste("bGrade[", 4, ",", 2, "]", sep=""))*pull(post, paste("CumuGrade[", 4, ",", 2, ",", 2, "]", sep="")), #G2              37
                        post$muInt + pull(post, paste("off_Sch[", 2, ",", 4, ",", 2, "]", sep="")) + pull(post, paste("bGrade[", 4, ",", 2, "]", sep=""))*pull(post, paste("CumuGrade[", 4, ",", 2, ",", 3, "]", sep="")), #G3              38
                        post$muInt + pull(post, paste("off_Sch[", 2, ",", 4, ",", 2, "]", sep="")) + pull(post, paste("bGrade[", 4, ",", 2, "]", sep=""))*pull(post, paste("CumuGrade[", 4, ",", 2, ",", 4, "]", sep="")), #G4              39
                        post$muInt + pull(post, paste("off_Sch[", 2, ",", 4, ",", 2, "]", sep="")) + pull(post, paste("bGrade[", 4, ",", 2, "]", sep=""))*pull(post, paste("CumuGrade[", 4, ",", 2, ",", 5, "]", sep="")), #G5              40
                        

                        # Comm teachers year 1 and 2
                        post$muInt + pull(post, paste("off_Sch[", 1, ",", 1, ",", 1, "]", sep="")), #teacher year 1     41
                        post$muInt + pull(post, paste("off_Sch[", 1, ",", 1, ",", 2, "]", sep="")), #teacher year 2     42
                        # SecN teachers year 1 and 2
                        post$muInt + pull(post, paste("off_Sch[", 1, ",", 2, ",", 1, "]", sep="")), #teacher year 1     43
                        post$muInt + pull(post, paste("off_Sch[", 1, ",", 2, ",", 2, "]", sep="")), #teacher year 2     44
                        # SecF teachers year 1 and 2
                        post$muInt + pull(post, paste("off_Sch[", 1, ",", 3, ",", 1, "]", sep="")), #teacher year 1     45
                        post$muInt + pull(post, paste("off_Sch[", 1, ",", 3, ",", 2, "]", sep="")), #teacher year 2     46
                        # RelF teachers year 1 and 2
                        post$muInt + pull(post, paste("off_Sch[", 1, ",", 4, ",", 1, "]", sep="")), #teacher year 1     47
                        post$muInt + pull(post, paste("off_Sch[", 1, ",", 4, ",", 2, "]", sep=""))  #teacher year 2     48
                    )

names(abils_grd_sch_yr) <- c("G1CommY1","G2CommY1","G3CommY1","G4CommY1","G5CommY1",
                            "G1CommY2","G2CommY2","G3CommY2","G4CommY2","G5CommY2",

                            "G1SecNY1","G2SecNY1","G3SecNY1","G4SecNY1","G5SecNY1",
                            "G1SecNY2","G2SecNY2","G3SecNY2","G4SecNY2","G5SecNY2",

                            "G1SecFY1","G2SecFY1","G3SecFY1","G4SecFY1","G5SecFY1",
                            "G1SecFY2","G2SecFY2","G3SecFY2","G4SecFY2","G5SecFY2",

                            "G1RelFY1","G2RelFY1","G3RelFY1","G4RelFY1","G5RelFY1",
                            "G1RelFY2","G2RelFY2","G3RelFY2","G4RelFY2","G5RelFY2",

                            "TCommY1", "TCommY2",
                            "TSecNY1", "TSecNY2",
                            "TSecFY1", "TSecFY2",
                            "TRelFY1", "TRelFY2"
                            )

str(abils_grd_sch_yr)




######## Plot colors

colorlist <- hcl.colors(n=11, palette="Blue-Yellow 2", #"Blue-Red",
                        alpha=0.7)
names(colorlist) <- c("1.1","1.2","1.3","1.4","1.5", "neutral",
                      "2.5","2.4","2.3","2.2","2.1")
#pie(rep(1, 11), col = colorlist)


colorlist2 <- hcl.colors(n=11, palette="RdPu", #"Green-Brown",
                        alpha=0.7)
names(colorlist2) <- c("1.1","1.2","1.3","1.4","1.5", "neutral",
                      "2.5","2.4","2.3","2.2","2.1")
#pie(rep(1, 11), col = colorlist2)


# area and line colors
Y1Line_lwd <- 4.5
T1Line_lwd <- 4.5
Y1Line_col <- colorlist["2.1"] # red
T1Line_col <- "black" #colorlist2["1.1"] #brown

Y2Line_lwd <- 4.5
T2Line_lwd <- 4.5
Y2Line_col <- colorlist["1.1"] # blue
T2Line_col <- colorlist2["1.1"] # green

Y1Area_col <- colorlist["2.2"]
T1Area_col <- rgb(1,1,1,alpha=0) #colorlist2["1.3"]

Y2Area_col <- colorlist["1.3"]
T2Area_col <- colorlist2["1.3"]

ConLine_lwd <- 3
ConLine_col <- "black"

ConArea_col <- grey(0.5)

ZerLine_lwd <- 2
ZerLine_lty <- "11"       #lty: first number in string is dash length, second is white space length
ZerLine_col <- "black"

cex_axis <- 1.2 # size of x-axis labels






################ Inverted Compare Grades ##################################################################################


pdf(file="./Plots/Compare_invert.pdf",
    height=6, width=5)
layout( matrix(data=c( 1, 2, 3, 4, 5, 6, 7, 8 ),
        nrow=4, ncol=2, byrow = FALSE),
        heights=rep(1,times=4),
        widths=c(3,1)
      )
par(mar = c(0, 0, 0, 0.8), oma = c(5, 5, 6, 1)) #margins for indiv plot, oma for outer margins (bottom, left, top, right)

par(xpd=FALSE)


# Comm

Comm_G1G3_plot <- denschart6( abils_grd_sch_yr[c(1,8,42,41)],
            #labels=rev(quest_names)
            labels="",
            adjust=1,
            color=c( NA,
                     NA,
                     NA,
                     NA
                    ),
            colorHPDI=c( Y1Area_col,
                         Y2Area_col,
                         T2Area_col,
                         T1Area_col
                        ),
            polyborder=c( Y1Line_col,
                          Y2Line_col,
                          T2Line_col,
                          T1Line_col
                        ),
            polyborderHPDI=c( Y1Line_col,
                              Y2Line_col,
                              T2Line_col,
                              T1Line_col
                            ),
            HPDI=0.9,
            border=NA, yaxt="n",
            cex=0.8, height=0.5,
            #clip(-0.5, 21, 0, 2), #clip(x1, x2, y1, y2) clips drawing beyond the rectangle
            xlim=range( -2.1, 0.5 ),
            yvals = c(0,
                      0,
                      0,
                      0)
 )
lines(x=c( mean(unlist(abils_grd_sch_yr[1])), mean(unlist(abils_grd_sch_yr[1])) ),
      y=c(0,0.6), col=Y1Line_col, lwd=Y1Line_lwd, lty=1, lend=0)
lines(x=c( mean(unlist(abils_grd_sch_yr[8])), mean(unlist(abils_grd_sch_yr[8])) ),
      y=c(0,0.6), col=Y2Line_col, lwd=Y2Line_lwd, lty=1, lend=0)
lines(x=c( mean(unlist(abils_grd_sch_yr[41])), mean(unlist(abils_grd_sch_yr[41])) ),
      y=c(0,0.6), col=T1Line_col, lwd=T1Line_lwd, lty=1, lend=0)
lines(x=c( mean(unlist(abils_grd_sch_yr[42])), mean(unlist(abils_grd_sch_yr[42])) ),
      y=c(0,0.6), col=T2Line_col, lwd=T2Line_lwd, lty=1, lend=0)

# lines in legend
par(xpd=NA) # plotting clipped to device region
lines(x=c(-2.85, -0.17), y=c(1.4,1.4), col="Black", lwd=1, lty=1, lend=1)
lines(x=c(-0.06, 1.7), y=c(1.4,1.4), col="Black", lwd=1, lty=1, lend=1)

lines(x=c(-2.77, -2.585), y=c(1.735,1.735), col="Black", lwd=9, lty=1, lend=0)
par(xpd=FALSE) # plotting clipped to plot region

par(new=TRUE) #add to existing plot


Comm_G3G5_plot <- denschart5( abils_grd_sch_yr[c(3,10)],
            #labels=rev(quest_names)
            labels="",
            adjust=1,
            color=c( NA,
                     NA
                    ),
            colorHPDI=c( Y1Area_col,
                         Y2Area_col
                        ),
            polyborder=c( Y1Line_col,
                          Y2Line_col
                        ),
            polyborderHPDI=c( Y1Line_col,
                              Y2Line_col
                            ),
            HPDI=0.9,
            border=NA, yaxt="n",
            cex=0.8, height=0.5,
            #clip(-0.5, 21, 0, 2), #clip(x1, x2, y1, y2) clips drawing beyond the rectangle
            xlim=range( -2.1, 0.5 ),
            yvals = c(0,
                      0,
                      0,
                      0)
 )

lines(x=c( mean(unlist(abils_grd_sch_yr[3])), mean(unlist(abils_grd_sch_yr[3])) ),
      y=c(0,-0.6), col=Y1Line_col, lwd=Y1Line_lwd, lty=1, lend=0)
lines(x=c( mean(unlist(abils_grd_sch_yr[10])), mean(unlist(abils_grd_sch_yr[10])) ),
      y=c(0,-0.6), col=Y2Line_col, lwd=Y2Line_lwd, lty=1, lend=0)


axislwd <- 1
lines(x=c(-1.5, 0.5), y=c(0,0), col="black", lwd=axislwd, lty=1, lend=1)

lines(x=c(-1.5,-1.5), y=c(0.05,-0.05), col="black", lwd=axislwd, lty=1, lend=1)
lines(x=c(-1,-1), y=c(0.05,-0.05), col="black", lwd=axislwd, lty=1, lend=1)
lines(x=c(-0.5,-0.5), y=c(0.05,-0.05), col="black", lwd=axislwd, lty=1, lend=1)
lines(x=c(0,0), y=c(0.05,-0.05), col="black", lwd=axislwd, lty=1, lend=1)
lines(x=c(0.5,0.5), y=c(0.05,-0.05), col="black", lwd=axislwd, lty=1, lend=1)




#set up new plot for legend placement
par(new=TRUE) #add to existing plot
plot( x=0, y=8, type="n", ylim=c(0,10), xlim=c(0,10), axes=FALSE, ylab=NA, xlab=NA)
par(xpd=NA) # plotting clipped to device region


# legend
legtext <- c( " 2017 Teachers", " 2019 Teachers",
              " 2017 Grade 1", " 2019 Grade 3",
              "", " Contrast Grade 3 - 1")
xcoords <- c(0, 0,
             7.2, 10.8,
             15, 18) # coordinates of legend items
secondvector <- (1:length(legtext))-1
textwidths <- xcoords/secondvector # this works for all but the first element
textwidths[1] <- 0 # so replace element 1 with a finite number (any will do)

  legend(x=-2, y=20,
         #inset=c(0,-0.5),       # inset is distance from x and y margins
         text.width=textwidths,  
         legend=legtext,
         bty="n",
         bg="white",
         col=c("white", T2Area_col,
               Y1Area_col, Y2Area_col,
               rgb(1,1,1,alpha=0), ConArea_col ),
         lty=c(1,1,1,1,1,1), 
         lwd=c(6,9,9,9,9,9),
         cex=1.2,
         x.intersp=0.5,
         seg.len=1.5,
         #horiz=TRUE,
         ncol=3) # ncol=number of columns of legend items)

  rect(xleft = -2.2,
       ybottom = 11.9,
       xright = 14.6,
       ytop = 19.6,
       lwd=1)

# row labels
mtext(expression(bold("Community school")), side = 1, outer = T, cex = 1, adj=-0.215, line = -25 )
mtext(expression(bold("Secular nearby")),   side = 1, outer = T, cex = 1, adj=-0.195, line = -18.5 )
mtext(expression(bold("Secular far")),      side = 1, outer = T, cex = 1, adj=-0.175, line = -12 )
mtext(expression(bold("Religious far")),    side = 1, outer = T, cex = 1, adj=-0.185, line = -5 )

par(xpd=FALSE)


par(new=TRUE) #add to existing plot
plot( x=0, y=8, type="n", ylim=c(0,10), xlim=c(0,10), axes=FALSE, ylab=NA, xlab=NA)
par(xpd=NA) # plotting clipped to device region


# legend
legtext <- c( "", "",
              " 2017 Grade 3", " 2019 Grade 5",
              " Contrast Grade 5 - 3", "")
xcoords <- c(0, 0,
             7.2, 10.8,
             15, 18) # coordinates of legend items
secondvector <- (1:length(legtext))-1
textwidths <- xcoords/secondvector # this works for all but the first element
textwidths[1] <- 0 # so replace element 1 with a finite number (any will do)

  legend(x=-2, y=16.5,
         #inset=c(0,-0.5),       # inset is distance from x and y margins
         text.width=textwidths,  
         legend=legtext,
         bty="n",
         bg="white",
         col=c(rgb(1,1,1,alpha=0), rgb(1,1,1,alpha=0),
               Y1Area_col, Y2Area_col,
               ConArea_col, rgb(1,1,1,alpha=0) ),
         lty=c(1,1,1,1,1,1), 
         lwd=c(9,9,9,9,9,9),
         cex=1.2,
         x.intersp=0.5,
         seg.len=1.5,
         #horiz=TRUE,
         ncol=3) # ncol=number of columns of legend items)

par(xpd=FALSE)


# SecN

SecN_G1G3_plot <- denschart6( abils_grd_sch_yr[c(11,18,44,43)],
            #labels=rev(quest_names)
            labels="",
            adjust=1,
            color=c( NA,
                     NA,
                     NA,
                     NA
                    ),
            colorHPDI=c( Y1Area_col,
                         Y2Area_col,
                         T2Area_col,
                         T1Area_col
                        ),
            polyborder=c( Y1Line_col,
                          Y2Line_col,
                          T2Line_col,
                          T1Line_col
                        ),
            polyborderHPDI=c( Y1Line_col,
                              Y2Line_col,
                              T2Line_col,
                              T1Line_col
                            ),
            HPDI=0.9,
            border=NA, yaxt="n",
            cex=0.8, height=0.5,
            #clip(-0.5, 21, 0, 2), #clip(x1, x2, y1, y2) clips drawing beyond the rectangle
            xlim=range( -2.1, 0.5),
            yvals = c(0,
                      0,
                      0,
                      0)
 )

lines(x=c( mean(unlist(abils_grd_sch_yr[11])), mean(unlist(abils_grd_sch_yr[11])) ),
      y=c(0,0.6), col=Y1Line_col, lwd=Y1Line_lwd, lty=1)
lines(x=c( mean(unlist(abils_grd_sch_yr[18])), mean(unlist(abils_grd_sch_yr[18])) ),
      y=c(0,0.6), col=Y2Line_col, lwd=Y2Line_lwd, lty=1)
lines(x=c( mean(unlist(abils_grd_sch_yr[43])), mean(unlist(abils_grd_sch_yr[43])) ),
      y=c(0,0.6), col=T1Line_col, lwd=T1Line_lwd, lty=1)
lines(x=c( mean(unlist(abils_grd_sch_yr[44])), mean(unlist(abils_grd_sch_yr[44])) ),
      y=c(0,0.6), col=T2Line_col, lwd=T2Line_lwd, lty=1)

par(new=TRUE) #add to existing plot

SecN_G3G5_plot <- denschart5( abils_grd_sch_yr[c(13,20)],
            #labels=rev(quest_names)
            labels="",
            adjust=1,
            color=c( NA,
                     NA
                    ),
            colorHPDI=c( Y1Area_col,
                         Y2Area_col
                        ),
            polyborder=c( Y1Line_col,
                          Y2Line_col
                        ),
            polyborderHPDI=c( Y1Line_col,
                              Y2Line_col
                            ),
            HPDI=0.9,
            border=NA, yaxt="n",
            cex=0.8, height=0.5,
            #clip(-0.5, 21, 0, 2), #clip(x1, x2, y1, y2) clips drawing beyond the rectangle
            xlim=range( -2.1, 0.5),
            yvals = c(0,
                      0,
                      0,
                      0)
 )

lines(x=c( mean(unlist(abils_grd_sch_yr[13])), mean(unlist(abils_grd_sch_yr[13])) ),
      y=c(0,-0.6), col=Y1Line_col, lwd=Y1Line_lwd, lty=1, lend=0)
lines(x=c( mean(unlist(abils_grd_sch_yr[20])), mean(unlist(abils_grd_sch_yr[20])) ),
      y=c(0,-0.6), col=Y2Line_col, lwd=Y2Line_lwd, lty=1, lend=0)

lines(x=c(-1.5, 0.5), y=c(0,0), col="black", lwd=axislwd, lty=1, lend=1)

lines(x=c(-1.5,-1.5), y=c(0.05,-0.05), col="black", lwd=axislwd, lty=1, lend=1)
lines(x=c(-1,-1), y=c(0.05,-0.05), col="black", lwd=axislwd, lty=1, lend=1)
lines(x=c(-0.5,-0.5), y=c(0.05,-0.05), col="black", lwd=axislwd, lty=1, lend=1)
lines(x=c(0,0), y=c(0.05,-0.05), col="black", lwd=axislwd, lty=1, lend=1)
lines(x=c(0.5,0.5), y=c(0.05,-0.05), col="black", lwd=axislwd, lty=1, lend=1)

par(xpd=FALSE)




# SecF 

SecF_G1G3_plot <- denschart6( abils_grd_sch_yr[c(21,28,46,45)],
            #labels=rev(quest_names)
            labels="",
            adjust=1,
            color=c( NA,
                     NA,
                     NA,
                     NA
                    ),
            colorHPDI=c( Y1Area_col,
                         Y2Area_col,
                         T2Area_col,
                         T1Area_col
                        ),
            polyborder=c( Y1Line_col,
                          Y2Line_col,
                          T2Line_col,
                          T1Line_col
                        ),
            polyborderHPDI=c( Y1Line_col,
                              Y2Line_col,
                              T2Line_col,
                              T1Line_col
                            ),
            HPDI=0.9,
            border=NA, yaxt="n",
            cex=0.8, height=0.5,
            #clip(-0.5, 21, 0, 2), #clip(x1, x2, y1, y2) clips drawing beyond the rectangle
            xlim=range( -2.1, 0.5),
            yvals = c(0,
                      0,
                      0,
                      0)
 )

lines(x=c( mean(unlist(abils_grd_sch_yr[21])), mean(unlist(abils_grd_sch_yr[21])) ),
      y=c(0,0.6), col=Y1Line_col, lwd=Y1Line_lwd, lty=1)
lines(x=c( mean(unlist(abils_grd_sch_yr[28])), mean(unlist(abils_grd_sch_yr[28])) ),
      y=c(0,0.6), col=Y2Line_col, lwd=Y2Line_lwd, lty=1)
lines(x=c( mean(unlist(abils_grd_sch_yr[45])), mean(unlist(abils_grd_sch_yr[45])) ),
      y=c(0,0.6), col=T1Line_col, lwd=T1Line_lwd, lty=1)
lines(x=c( mean(unlist(abils_grd_sch_yr[46])), mean(unlist(abils_grd_sch_yr[46])) ),
      y=c(0,0.6), col=T2Line_col, lwd=T2Line_lwd, lty=1)

par(new=TRUE) #add to existing plot

SecF_G3G5_plot <- denschart5( abils_grd_sch_yr[c(23,30)],
            #labels=rev(quest_names)
            labels="",
            adjust=1,
            color=c( NA,
                     NA
                    ),
            colorHPDI=c( Y1Area_col,
                         Y2Area_col
                        ),
            polyborder=c( Y1Line_col,
                          Y2Line_col
                        ),
            polyborderHPDI=c( Y1Line_col,
                              Y2Line_col
                            ),
            HPDI=0.9,
            border=NA, yaxt="n",
            cex=0.8, height=0.5,
            #clip(-0.5, 21, 0, 2), #clip(x1, x2, y1, y2) clips drawing beyond the rectangle
            xlim=range( -2.1, 0.5),
            yvals = c(0,
                      0,
                      0,
                      0)
 )

lines(x=c( mean(unlist(abils_grd_sch_yr[23])), mean(unlist(abils_grd_sch_yr[23])) ),
      y=c(0,-0.6), col=Y1Line_col, lwd=Y1Line_lwd, lty=1)
lines(x=c( mean(unlist(abils_grd_sch_yr[30])), mean(unlist(abils_grd_sch_yr[30])) ),
      y=c(0,-0.6), col=Y2Line_col, lwd=Y2Line_lwd, lty=1)

lines(x=c(-1.5, 0.5), y=c(0,0), col="black", lwd=axislwd, lty=1, lend=1)

lines(x=c(-1.5,-1.5), y=c(0.05,-0.05), col="black", lwd=axislwd, lty=1, lend=1)
lines(x=c(-1,-1), y=c(0.05,-0.05), col="black", lwd=axislwd, lty=1, lend=1)
lines(x=c(-0.5,-0.5), y=c(0.05,-0.05), col="black", lwd=axislwd, lty=1, lend=1)
lines(x=c(0,0), y=c(0.05,-0.05), col="black", lwd=axislwd, lty=1, lend=1)
lines(x=c(0.5,0.5), y=c(0.05,-0.05), col="black", lwd=axislwd, lty=1, lend=1)

par(xpd=FALSE)


# RelF

RelF_G1G3_plot <- denschart6( abils_grd_sch_yr[c(31,38,48,47)],
            #labels=rev(quest_names)
            labels="",
            adjust=1,
            color=c( NA,
                     NA,
                     NA,
                     NA
                    ),
            colorHPDI=c( Y1Area_col,
                         Y2Area_col,
                         T2Area_col,
                         T1Area_col
                        ),
            polyborder=c( Y1Line_col,
                          Y2Line_col,
                          T2Line_col,
                          T1Line_col
                        ),
            polyborderHPDI=c( Y1Line_col,
                              Y2Line_col,
                              T2Line_col,
                              T1Line_col
                            ),
            HPDI=0.9,
            border=NA, yaxt="n",
            cex=0.8, height=0.5,
            #clip(-0.5, 21, 0, 2), #clip(x1, x2, y1, y2) clips drawing beyond the rectangle
            xlim=range( -2.1, 0.5),
            yvals = c(0,
                      0,
                      0,
                      0)
 )

lines(x=c( mean(unlist(abils_grd_sch_yr[31])), mean(unlist(abils_grd_sch_yr[31])) ),
      y=c(0,0.6), col=Y1Line_col, lwd=Y1Line_lwd, lty=1)
lines(x=c( mean(unlist(abils_grd_sch_yr[38])), mean(unlist(abils_grd_sch_yr[38])) ),
      y=c(0,0.6), col=Y2Line_col, lwd=Y2Line_lwd, lty=1)
lines(x=c( mean(unlist(abils_grd_sch_yr[47])), mean(unlist(abils_grd_sch_yr[47])) ),
      y=c(0,0.6), col=T1Line_col, lwd=T1Line_lwd, lty=1)
lines(x=c( mean(unlist(abils_grd_sch_yr[48])), mean(unlist(abils_grd_sch_yr[48])) ),
      y=c(0,0.6), col=T2Line_col, lwd=T2Line_lwd, lty=1)

par(new=TRUE) #add to existing plot

RelF_G3G5_plot <- denschart5( abils_grd_sch_yr[c(33,40)],
            #labels=rev(quest_names)
            labels="",
            adjust=1,
            color=c( NA,
                     NA
                    ),
            colorHPDI=c( Y1Area_col,
                         Y2Area_col
                        ),
            polyborder=c( Y1Line_col,
                          Y2Line_col
                        ),
            polyborderHPDI=c( Y1Line_col,
                              Y2Line_col
                            ),
            HPDI=0.9,
            border=NA, yaxt="n",
            cex=0.8, height=0.5,
            #clip(-0.5, 21, 0, 2), #clip(x1, x2, y1, y2) clips drawing beyond the rectangle
            xlim=range( -2.1, 0.5),
            yvals = c(0,
                      0,
                      0,
                      0)
 )

lines(x=c( mean(unlist(abils_grd_sch_yr[33])), mean(unlist(abils_grd_sch_yr[33])) ),
      y=c(0,-0.6), col=Y1Line_col, lwd=Y1Line_lwd, lty=1, lend=0)
lines(x=c( mean(unlist(abils_grd_sch_yr[40])), mean(unlist(abils_grd_sch_yr[40])) ),
      y=c(0,-0.6), col=Y2Line_col, lwd=Y2Line_lwd, lty=1, lend=0)

lines(x=c(-1.5, 0.5), y=c(0,0), col="black", lwd=axislwd, lty=1, lend=1)

lines(x=c(-1.5,-1.5), y=c(0.05,-0.05), col="black", lwd=axislwd, lty=1, lend=1)
lines(x=c(-1,-1), y=c(0.05,-0.05), col="black", lwd=axislwd, lty=1, lend=1)
lines(x=c(-0.5,-0.5), y=c(0.05,-0.05), col="black", lwd=axislwd, lty=1, lend=1)
lines(x=c(0,0), y=c(0.05,-0.05), col="black", lwd=axislwd, lty=1, lend=1)
lines(x=c(0.5,0.5), y=c(0.05,-0.05), col="black", lwd=axislwd, lty=1, lend=1)

axis(side=1,
    at=c(-1.5,-1,-0.5,0,0.5), labels=c(-1.5,-1,-0.5,0,0.5),
    #at=c(-1,0,1), labels=c(-1,0,1),
    cex.axis=cex_axis)

par(xpd=NA)

text(x=-0.3, y=-1.5, labels="Independence", adj=0, cex=1.2)
Arrows(-1.95, -1.5,  # x0,y0
       -2.45, -1.5, # x1,y1
       code=2,arr.type="curved",lend="round",lwd=2,arr.lwd=1, arr.width=0.2, arr.length=0.3) 

text(x=-0.7, y=-1.5, labels="Inter-dependence", adj=1, cex=1.2)
Arrows(0.7  , -1.5,  # x0,y0
       1.2, -1.5, # x1,y1
       code=2,arr.type="curved",lend="round",lwd=2,arr.lwd=1, arr.width=0.2, arr.length=0.3) 


#### contrasts

par(xpd=FALSE)

# Comm contrast

Comm_G1G3_contr <- denschart6( list(unlist(abils_grd_sch_yr[8]) -
                                    unlist(abils_grd_sch_yr[1])),
            #labels=rev(quest_names)
            labels="",
            adjust=1,
            color=NA,
            colorHPDI=ConArea_col,
            polyborder=ConLine_col,
            polyborderHPDI=ConLine_col,
            HPDI=0.9,
            border=NA, yaxt="n",
            cex=0.8, height=0.5,
            #clip(-0.5, 21, 0, 2), #clip(x1, x2, y1, y2) clips drawing beyond the rectangle
            xlim=range(-0.5,0.5),
                #range( min(contr_param_list$ber.matK1), max(contr_param_list$ber.matK1) ),
            yvals = 0
 )

lines(x=c( mean( unlist(abils_grd_sch_yr[8]) - unlist(abils_grd_sch_yr[1]) ),
           mean( unlist(abils_grd_sch_yr[8]) - unlist(abils_grd_sch_yr[1]) ) ),
      y=c(0,0.6), col=ConLine_col, lwd=ConLine_lwd, lty=1)

par(new=TRUE) #add to existing plot

Comm_G3G5_contr <- denschart5( list(unlist(abils_grd_sch_yr[10]) -
                                    unlist(abils_grd_sch_yr[3])),
            #labels=rev(quest_names)
            labels="",
            adjust=1,
            color=NA,
            colorHPDI=ConArea_col,
            polyborder=ConLine_col,
            polyborderHPDI=ConLine_col,
            HPDI=0.9,
            border=NA, yaxt="n",
            cex=0.8, height=0.5,
            clip(0.1, 1, 0, 2), #clip(x1, x2, y1, y2) clips drawing beyond the rectangle
            xlim=range(-0.5,0.5),
                #range( min(contr_param_list$ber.matK1), max(contr_param_list$ber.matK1) ),
            yvals = 0
 )

lines(x=c( mean( unlist(abils_grd_sch_yr[10]) - unlist(abils_grd_sch_yr[3]) ),
           mean( unlist(abils_grd_sch_yr[10]) - unlist(abils_grd_sch_yr[3]) ) ),
      y=c(0,-0.6), col=ConLine_col, lwd=ConLine_lwd, lty=1)

par(xpd=FALSE) # plotting clipped to device region

lines(x=c(-1, 1), y=c(0,0), col="black", lwd=axislwd, lty=1, lend=1)

lines(x=c(-0.5,-0.5), y=c(0.05,-0.05), col="black", lwd=axislwd, lty=1, lend=1)
lines(x=c(0.5,0.5), y=c(0.05,-0.05), col="black", lwd=axislwd, lty=1, lend=1)

par(xpd=NA) # plotting clipped to device region
lines(x=c(0,0),
      y=c(-0.7,0.7), col=ZerLine_col, lwd=ZerLine_lwd, lty=ZerLine_lty)
par(xpd=FALSE) # plotting clipped to plot region





# SecN contrast

SecN_G1G3_contr <- denschart6( list(unlist(abils_grd_sch_yr[18]) -
                                    unlist(abils_grd_sch_yr[11])),
            #labels=rev(quest_names)
            labels="",
            adjust=1,
            color=NA,
            colorHPDI=ConArea_col,
            polyborder=ConLine_col,
            polyborderHPDI=ConLine_col,
            HPDI=0.9,
            border=NA, yaxt="n",
            cex=0.8, height=0.5,
            #clip(-0.5, 21, 0, 2), #clip(x1, x2, y1, y2) clips drawing beyond the rectangle
            xlim=range(-0.5,0.5),
                #range( min(contr_param_list$ber.matK1), max(contr_param_list$ber.matK1) ),
            yvals = 0
 )

lines(x=c( mean( unlist(abils_grd_sch_yr[18]) - unlist(abils_grd_sch_yr[11]) ),
           mean( unlist(abils_grd_sch_yr[18]) - unlist(abils_grd_sch_yr[11]) ) ),
      y=c(0,0.6), col=ConLine_col, lwd=ConLine_lwd, lty=1)

par(new=TRUE) #add to existing plot

SecN_G3G5_contr <- denschart5( list(unlist(abils_grd_sch_yr[20]) -
                                    unlist(abils_grd_sch_yr[13])),
            #labels=rev(quest_names)
            labels="",
            adjust=1,
            color=NA,
            colorHPDI=ConArea_col,
            polyborder=ConLine_col,
            polyborderHPDI=ConLine_col,
            HPDI=0.9,
            border=NA, yaxt="n",
            cex=0.8, height=0.5,
            clip(0.1, 1, 0, 2), #clip(x1, x2, y1, y2) clips drawing beyond the rectangle
            xlim=range(-0.5,0.5),
                #range( min(contr_param_list$ber.matK1), max(contr_param_list$ber.matK1) ),
            yvals = 0
 )

lines(x=c( mean( unlist(abils_grd_sch_yr[20]) - unlist(abils_grd_sch_yr[13]) ),
           mean( unlist(abils_grd_sch_yr[20]) - unlist(abils_grd_sch_yr[13]) ) ),
      y=c(0,-0.6), col=ConLine_col, lwd=ConLine_lwd, lty=1)

par(xpd=FALSE) # plotting clipped to device region
lines(x=c(-1, 1), y=c(0,0), col="black", lwd=axislwd, lty=1, lend=1)

lines(x=c(-0.5,-0.5), y=c(0.05,-0.05), col="black", lwd=axislwd, lty=1, lend=1)
lines(x=c(0.5,0.5), y=c(0.05,-0.05), col="black", lwd=axislwd, lty=1, lend=1)

par(xpd=NA) # plotting clipped to device region
lines(x=c(0,0),
      y=c(-0.7,0.7), col=ZerLine_col, lwd=ZerLine_lwd, lty=ZerLine_lty)
par(xpd=FALSE) # plotting clipped to plot region





# SecF contrast

SecF_G1G3_contr <- denschart6( list(unlist(abils_grd_sch_yr[28]) -
                                    unlist(abils_grd_sch_yr[21])),
            #labels=rev(quest_names)
            labels="",
            adjust=1,
            color=NA,
            colorHPDI=ConArea_col,
            polyborder=ConLine_col,
            polyborderHPDI=ConLine_col,
            HPDI=0.9,
            border=NA, yaxt="n",
            cex=0.8, height=0.5,
            #clip(-0.5, 21, 0, 2), #clip(x1, x2, y1, y2) clips drawing beyond the rectangle
            xlim=range(-0.5,0.5),
                #range( min(contr_param_list$ber.matK1), max(contr_param_list$ber.matK1) ),
            yvals = 0
 )

lines(x=c( mean( unlist(abils_grd_sch_yr[28]) - unlist(abils_grd_sch_yr[21]) ),
           mean( unlist(abils_grd_sch_yr[28]) - unlist(abils_grd_sch_yr[21]) ) ),
      y=c(0,0.6), col=ConLine_col, lwd=ConLine_lwd, lty=1)

par(new=TRUE) #add to existing plot

SecF_G3G5_contr <- denschart5( list(unlist(abils_grd_sch_yr[30]) -
                                    unlist(abils_grd_sch_yr[23])),
            #labels=rev(quest_names)
            labels="",
            adjust=1,
            color=NA,
            colorHPDI=ConArea_col,
            polyborder=ConLine_col,
            polyborderHPDI=ConLine_col,
            HPDI=0.9,
            border=NA, yaxt="n",
            cex=0.8, height=0.5,
            clip(0, 1, 0, 2), #clip(x1, x2, y1, y2) clips drawing beyond the rectangle
            xlim=range(-0.5,0.5),
                #range( min(contr_param_list$ber.matK1), max(contr_param_list$ber.matK1) ),
            yvals = 0
 )

lines(x=c( mean( unlist(abils_grd_sch_yr[30]) - unlist(abils_grd_sch_yr[23]) ),
           mean( unlist(abils_grd_sch_yr[30]) - unlist(abils_grd_sch_yr[23]) ) ),
      y=c(0,-0.6), col=ConLine_col, lwd=ConLine_lwd, lty=1)

par(xpd=FALSE) # plotting clipped to device region
lines(x=c(-1, 1), y=c(0,0), col="black", lwd=axislwd, lty=1, lend=0)

lines(x=c(-0.5,-0.5), y=c(0.05,-0.05), col="black", lwd=axislwd, lty=1, lend=1)
lines(x=c(0.5,0.5), y=c(0.05,-0.05), col="black", lwd=axislwd, lty=1, lend=1)

par(xpd=NA) # plotting clipped to device region
lines(x=c(0,0),
      y=c(-0.7,0.7), col=ZerLine_col, lwd=ZerLine_lwd, lty=ZerLine_lty)
par(xpd=FALSE) # plotting clipped to plot region





# RelF contrast

RelF_G1G3_contr <- denschart6( list(unlist(abils_grd_sch_yr[38]) -
                                    unlist(abils_grd_sch_yr[31])),
            #labels=rev(quest_names)
            labels="",
            adjust=1,
            color=NA,
            colorHPDI=ConArea_col,
            polyborder=ConLine_col,
            polyborderHPDI=ConLine_col,
            HPDI=0.9,
            border=NA, yaxt="n",
            cex=0.8, height=0.5,
            #clip(-0.5, 21, 0, 2), #clip(x1, x2, y1, y2) clips drawing beyond the rectangle
            xlim=range(-0.5,0.5),
                #range( min(contr_param_list$ber.matK1), max(contr_param_list$ber.matK1) ),
            yvals = 0
 )

lines(x=c( mean( unlist(abils_grd_sch_yr[38]) - unlist(abils_grd_sch_yr[31]) ),
           mean( unlist(abils_grd_sch_yr[38]) - unlist(abils_grd_sch_yr[31]) ) ),
      y=c(0,0.6), col=ConLine_col, lwd=ConLine_lwd, lty=1)

par(new=TRUE) #add to existing plot

RelF_G3G5_contr <- denschart5( list(unlist(abils_grd_sch_yr[40]) -
                                    unlist(abils_grd_sch_yr[33])),
            #labels=rev(quest_names)
            labels="",
            adjust=1,
            color=NA,
            colorHPDI=ConArea_col,
            polyborder=ConLine_col,
            polyborderHPDI=ConLine_col,
            HPDI=0.9,
            border=NA, yaxt="n",
            cex=0.8, height=0.5,
            clip(0, 1, 0, 2), #clip(x1, x2, y1, y2) clips drawing beyond the rectangle
            xlim=range(-0.5,0.5),
                #range( min(contr_param_list$ber.matK1), max(contr_param_list$ber.matK1) ),
            yvals = 0
 )

lines(x=c( mean( unlist(abils_grd_sch_yr[40]) - unlist(abils_grd_sch_yr[33]) ),
           mean( unlist(abils_grd_sch_yr[40]) - unlist(abils_grd_sch_yr[33]) ) ),
      y=c(0,-0.6), col=ConLine_col, lwd=ConLine_lwd, lty=1)

par(xpd=FALSE) # plotting clipped to device region
lines(x=c(-1, 1), y=c(0,0), col="black", lwd=axislwd, lty=1, lend=1)

lines(x=c(-0.5,-0.5), y=c(0.05,-0.05), col="black", lwd=axislwd, lty=1, lend=1)
lines(x=c(0.5,0.5), y=c(0.05,-0.05), col="black", lwd=axislwd, lty=1, lend=1)

par(xpd=NA) # plotting clipped to device region
lines(x=c(0,0),
      y=c(-1,0.7), col=ZerLine_col, lwd=ZerLine_lwd, lty=ZerLine_lty)
par(xpd=FALSE) # plotting clipped to plot region

axis(side=1,
    at=c(-0.5,0.5), labels=c(-0.5,0.5),
    cex.axis=cex_axis)

graphics.off()







################### Response probabilities: religious school ##############################################################

num_samp <- length(post$lp__)

quest_domain.sch <- c("1. Marriage", "2. Parenting", "3. Inheritance",
               "4. Education", "5. Education", "6. Healthcare", "7. Healthcare",
               "8. Fairness", "9. Religion", "10. Religion",
               "11. Labor", "12. Commerce", "13. Marriage",
               "14. Parenting", "15. Labor")

# matrices to hold posterior distributions of probabilities
probs.teach.RelF <- matrix( data=0, ncol=K, nrow=num_samp, dimnames=list(1:num_samp,quest_domain.sch) )

probs.stud2017.1.RelF <- matrix( data=0, ncol=K, nrow=num_samp, dimnames=list(1:num_samp,quest_domain.sch) )

probs.stud2019.3.RelF <- matrix( data=0, ncol=K, nrow=num_samp, dimnames=list(1:num_samp,quest_domain.sch) )


#mean posterior estimates of probability of each person answering each question as positive
for (k in 1:K) {

    #teacher RelF 2019

    tGamma <- exp( post$muGamma + pull(post, paste("off_Gamma[", k, ",", 1, "]", sep="")) ) # [quest, role]
    Beta <- pull(post, paste("beta[", k, "]", sep="")) # constant across individuals
    tAlpha <- unlist(abils_grd_sch_yr[48]) # from beginning of script

    probs.teach.RelF[,k] <- inv.logit( tGamma*( tAlpha - Beta ) )


    #student RelF year 1 2017
    
    s1Gamma <- exp( post$muGamma + pull(post, paste("off_Gamma[", k, ",", 2, "]", sep="")) ) # [quest, role]
    s1Alpha <- unlist(abils_grd_sch_yr[31])

    probs.stud2017.1.RelF[,k] <- inv.logit( s1Gamma*( s1Alpha - Beta ) )


    #student RelF year 3 2019
    
    s3Gamma <- exp( post$muGamma + pull(post, paste("off_Gamma[", k, ",", 2, "]", sep="")) ) # [quest, role]
    s3Alpha <- unlist(abils_grd_sch_yr[38])

    probs.stud2019.3.RelF[,k] <- inv.logit( s3Gamma*( s3Alpha - Beta ) )

} # for k


# order by teacher probs
tprobs <- colMeans(probs.teach.RelF)
dec.order <- order(tprobs, decreasing = FALSE)
#tprobs[dec.order]

probs.teach.RelF.ord <- probs.teach.RelF[,dec.order]
probs.stud2017.1.RelF.ord <- probs.stud2017.1.RelF[,dec.order]
probs.stud2019.3.RelF.ord <- probs.stud2019.3.RelF[,dec.order]

quest_domain.sch.ord <- quest_domain.sch[dec.order]



cex_axis <- 0.8

#### density plot of responses

pdf(file="./Plots/response_probability.pdf", 
height=6, width=5)
par(mfcol=c(1,1))
par(mar = c(0, 0, 0, 0), oma = c(5, 8, 4, 2)) #margins for indiv plot, oma for outer margins (bottom, left, top, right)

# teachers 2019
denschart4( rev(split(probs.teach.RelF.ord, col(probs.teach.RelF.ord))),
          #labels=rev(quest_names)
          labels="",
          adjust=1,
          color= NA,
          colorHPDI= T2Area_col,
          polyborder=T2Line_col,
          polyborderHPDI=T2Line_col,
          HPDI=0.9,
          meanlines=TRUE,
          meanlinesCol=T2Line_col,
          meanlinesLwd=T2Line_lwd,
          border=NA, yaxt="n",
          cex=0.8, height=0.5,
          xlim=range( 0, 1),
          clip(0,1,0,16) #x1, x2, y1, y2    clip at x=0  
 )
text(x=0.5, y=16, labels="", cex=0.75)
axis(side=2,
    col="white",
    at=c(1:K), 
    labels=rev(quest_domain.sch.ord), las=1, cex.axis=cex_axis)   #left
axis(side=1, at=c(0,0.5,1), labels=c(0,0.5,1), cex.axis=cex_axis)

lines(x=list( x=c(0.5,0.5), y=c(0,15.5) ), col=ZerLine_col, lwd=ZerLine_lwd, lty=ZerLine_lty)


#set up new plot for legend placement
par(new=TRUE) #add to existing plot
plot( x=0, y=8, type="n", ylim=c(0,10), xlim=c(0,10), axes=FALSE, ylab=NA, xlab=NA)
par(xpd=NA) # plotting clipped to device region


# legend
legtext <- c(" 2019 Teachers", " 2019 Grade 3", " 2017 Grade 1")
xcoords <- c(0, 3.3, 5) # coordinates of legend items
secondvector <- (1:length(legtext))-1
textwidths <- xcoords/secondvector # this works for all but the first element
textwidths[1] <- 0 # so replace element 1 with a finite number (any will do)

  legend(x=-3, y=11.3,
         #inset=c(0,-0.5),       # inset is distance from x and y margins
         text.width=textwidths,  
         legend=legtext,
         bty="n",
         bg="white",
         col=c(T2Area_col, Y2Area_col, Y1Area_col),
         lty=c(1,1,1), 
         lwd=c(9,9,9),
         cex=cex_axis,
         x.intersp=0.5,
         seg.len=1.5,
         horiz=TRUE)

  rect(xleft = -3.2,
       ybottom = 10.5,
       xright = 10.3,
       ytop = 11.3,
       lwd=1)


par(new=TRUE) # overlay plot on previous plot

# students 2017 grade 1
denschart4( rev(split(probs.stud2017.1.RelF.ord, col(probs.stud2017.1.RelF.ord))),
          labels="",
          adjust=1,
          color= NA,
          colorHPDI= Y1Area_col,
          polyborder=Y1Line_col,
          polyborderHPDI=Y1Line_col,
          HPDI=0.9,
          meanlines=TRUE,
          meanlinesCol=Y1Line_col,
          meanlinesLwd=Y1Line_lwd,
          border=NA, yaxt="n",
          cex=0.8, height=0.5,
          xlim=range( 0, 1),
          clip(0,1,0,16) #x1, x2, y1, y2    clip at x=0   
 )

par(new=TRUE) # overlay plot on previous plot

# students 2019 grade 3
denschart4( rev(split(probs.stud2019.3.RelF.ord, col(probs.stud2019.3.RelF.ord))),
          #labels=rev(quest_names)
          labels="",
          adjust=1,
          color= NA,
          colorHPDI= Y2Area_col,
          polyborder=Y2Line_col,
          polyborderHPDI=Y2Line_col,
          HPDI=0.9,
          meanlines=TRUE,
          meanlinesCol=Y2Line_col,
          meanlinesLwd=Y2Line_lwd,
          border=NA, yaxt="n",
          cex=0.8, height=0.5,
          xlim=range( 0, 1),
          clip(0,1,-20,16) #x1, x2, y1, y2    clip at x=0  
 )

#par(xpd=NA) # plotting clipped to device region

mtext("Probability Response = 1", side = 1, outer = TRUE, cex = cex_axis, line = 2.5)

graphics.off()

