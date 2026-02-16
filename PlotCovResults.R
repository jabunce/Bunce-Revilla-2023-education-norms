
#plot logistic response functions for a given question and latent trait estimates of indivs

questions <- quest_names.sch #from PrepareData
post <- post2


J <- length(unique(d.school.r$newID))    # number of people
K <- length(unique(d.school.r$QID))      # number of questions
A <- length(unique(d.school.r$intYear))  # number of years
N <- nrow(d.school.r)                    # total number of responses

#####

pdf(file="./Plots/m1_logistic_response_cov_years.pdf", 
  height=10, width=12)
par(mfrow=c(4,4), oma=c(5,7,5,5), mar=c(3,2,2,2))

#ROLE <- 1 # 1=teachers, 2=students

for (Quest in 1:K) {


  par(mar=c(3, 3, 3, 3)) #c(bottom, left, top, right)
  plot( x=0, y=0.5, type="n", ylim=c(0,1), xlim=c(-2,2), axes=F, ylab=NA, xlab=NA, main=c(questions[Quest]))
  box(which = "plot", lty = "solid")
  axis( side=1, at=c(-2,0,2), labels = TRUE )
  axis( side=2, at=c(0,0.5,1), labels = TRUE )


  YEAR <- 2017
  year <- 1
  off_Int <- mean( pull(post, paste("off_Int[", d.school.r[which(d.school.r$QID == Quest & d.school.r$intYear == YEAR), "newID"][1], ",", year, "]", sep="")) )
  for ( p in 2:length(d.school.r[which(d.school.r$QID == Quest & d.school.r$intYear == YEAR), "newID"]) ) {

    off_Int <- c( off_Int, mean( pull(post, paste("off_Int[", d.school.r[which(d.school.r$QID == Quest & d.school.r$intYear == YEAR), "newID"][p], ",", year, "]", sep="")) ) )

  } # for p

  points( x= 0 + off_Int,
          y= d.school.r[which(d.school.r$QID == Quest & d.school.r$intYear == YEAR), "ResponseFlipped"],
          lwd=1, col="red", cex=1, pch=1 )


  YEAR <- 2019
  year <- 2
  off_Int <- mean( pull(post, paste("off_Int[", d.school.r[which(d.school.r$QID == Quest & d.school.r$intYear == YEAR), "newID"][1], ",", year, "]", sep="")) )
  for ( p in 2:length(d.school.r[which(d.school.r$QID == Quest & d.school.r$intYear == YEAR), "newID"]) ) {

    off_Int <- c( off_Int, mean( pull(post, paste("off_Int[", d.school.r[which(d.school.r$QID == Quest & d.school.r$intYear == YEAR), "newID"][p], ",", year, "]", sep="")) ) )

  } # for p

  points( x= 0 + off_Int,
          y= d.school.r[which(d.school.r$QID == Quest & d.school.r$intYear == YEAR), "ResponseFlipped"],
          lwd=1, col="blue", cex=1, pch=1 )


  InverseLogit <- function(x) 1/(1+exp(-1*x)) # logit^-1 = logistic, undoing the logit function in the model
  gamma <- mean( pull(post, paste("gamma[", Quest, "]", sep="")) )  
  beta <-  mean( pull(post, paste("beta[", Quest, "]", sep="")) )

  curve( InverseLogit( gamma*(x - beta) ),
    from=-2.5, to=2.5, add=T )
  abline( v=beta, #Bafumi et al. 2005, pg 174-175
          col=col.alpha("black",0.2) )
} #for Quest


mtext(text="Prob(Response = 1)", side=2, outer=TRUE, line=2, cex=1.5, las=3, adj=0.5)
mtext(text="Latent Axis", side=1, outer=TRUE, line=1, cex=1.5, las=1, adj=0.5)

graphics.off()








##########################################################################



#makes a vector of length J with 1 or 0 depending on whether the individual has the given characteristic
# Matsi
machiID <- unique(d.school.r[which(d.school.r$Machi == 1), "newID"])
length(machiID) #number of machis in dataset
mestizoID <- unique(d.school.r[which(d.school.r$Machi == 0), "newID"])
length(mestizoID) #number of mestizos in dataset
machi <- matrix(data= cbind(machiID, 1), nrow <- length(machiID), ncol <- 2)
mestizo <- matrix(data= cbind(mestizoID, 0), nrow <- length(mestizoID), ncol <- 2)
machi_dum <- rbind(machi, mestizo)
machi_dum <- machi_dum[order(machi_dum[,1]),] #order based on newID
machi <- machi_dum[,2]

# teacher
TeacherID <- unique(d.school.r[which(d.school.r$Role == 1), "newID"]) # teacher=1, student=2
notTeacherID <- unique(d.school.r[which(d.school.r$Role != 1), "newID"])
Teacher <- matrix(data= cbind(TeacherID, 1), nrow <- length(TeacherID), ncol <- 2)
notTeacher <- matrix(data= cbind(notTeacherID, 0), nrow <- length(notTeacherID), ncol <- 2)
Teacher_dum <- rbind(Teacher, notTeacher)
Teacher_dum <- Teacher_dum[order(Teacher_dum[,1]),]
Teacher <- Teacher_dum[,2]
#Teacher <- c( rep(NA, length(unique(d.community$newID))), Teacher ) #add NAs for non-teacher adults

# interviewed in 2017
int2017ID <- unique(d.school.r[which(d.school.r$intYear == 2017), "newID"])
notint2017ID <- unique(d.school.r[which(d.school.r$intYear != 2017), "newID"])
for (x in 1:length(notint2017ID) ){
  if (notint2017ID[x] %in% int2017ID) {
    notint2017ID[x] <- NA
  } #if
} # for x
notint2017ID <- notint2017ID[!is.na(notint2017ID)]
int2017 <- matrix(data= cbind(int2017ID, 1), nrow <- length(int2017ID), ncol <- 2)
notint2017 <- matrix(data= cbind(notint2017ID, 0), nrow <- length(notint2017ID), ncol <- 2)
int2017_dum <- rbind(int2017, notint2017)
int2017_dum <- int2017_dum[order(int2017_dum[,1]),]
int2017 <- int2017_dum[,2]

# interviewed in 2019
int2019ID <- unique(d.school.r[which(d.school.r$intYear == 2019), "newID"])
notint2019ID <- unique(d.school.r[which(d.school.r$intYear != 2019), "newID"])
for (x in 1:length(notint2019ID) ){
  if (notint2019ID[x] %in% int2019ID) {
    notint2019ID[x] <- NA
  } #if
} # for x
notint2019ID <- notint2019ID[!is.na(notint2019ID)]
int2019 <- matrix(data= cbind(int2019ID, 1), nrow <- length(int2019ID), ncol <- 2)
notint2019 <- matrix(data= cbind(notint2019ID, 0), nrow <- length(notint2019ID), ncol <- 2)
int2019_dum <- rbind(int2019, notint2019)
int2019_dum <- int2019_dum[order(int2019_dum[,1]),]
int2019 <- int2019_dum[,2]

# dropped out of school
DroppedID <- unique(d.school.r[which(d.school.r$dropped == 1), "newID"])
notDroppedID <- unique(d.school.r[which(d.school.r$dropped != 1), "newID"])
Dropped <- matrix(data= cbind(DroppedID, 1), nrow <- length(DroppedID), ncol <- 2)
notDropped <- matrix(data= cbind(notDroppedID, 0), nrow <- length(notDroppedID), ncol <- 2)
Dropped_dum <- rbind(Dropped, notDropped)
Dropped_dum <- Dropped_dum[order(Dropped_dum[,1]),]
Dropped <- Dropped_dum[,2]



#newIDs of people interviewed twice
twice <- NULL
measured <- NULL

for (x in 1:nrow(d.school.r)) {
  if (d.school.r[x,"intYear"] == 2017) {
    measured <- c(measured, d.school.r[x,"ID"])
  } # if
} # for x

for (x in 1:nrow(d.school.r)) {
  if (d.school.r[x,"intYear"] == 2019 & d.school.r[x,"ID"] %in% measured) {
    twice <- c(twice, d.school.r[x,"ID"])
  } # if
} # for x

twice <- unique(twice) # vector of IDs of people interviewed twice
length(twice)
length(unique(d.school.r$ID))

# measured twice
RepeatsID <- unique(d.school.r[which(d.school.r$ID %in% twice), "newID"])
notRepeatsID <- unique(d.school.r[which( !(d.school.r$ID %in% twice) ), "newID"])
Repeats <- matrix(data= cbind(RepeatsID, 1), nrow <- length(RepeatsID), ncol <- 2)
notRepeats <- matrix(data= cbind(notRepeatsID, 0), nrow <- length(notRepeatsID), ncol <- 2)
Repeats_dum <- rbind(Repeats, notRepeats)
Repeats_dum <- Repeats_dum[order(Repeats_dum[,1]),]
Repeats <- Repeats_dum[,2]



###### dataset with all people, but just the grades that twice-measured people were in in 2019

d.school.r.once <- NULL

for (x in 1:nrow(d.school.r)) {
  if ( !(d.school.r[x,"ID"] %in% twice) | (d.school.r[x,"intYear"] == 2019) ) {
    d.school.r.once <- rbind( d.school.r.once, d.school.r[x,] )
  } # if
} # for x





S4ID <- unique(d.school.r.once[which(d.school.r.once$sch.num == 4), "newID"]) ############################################## S4 Religious
notS4ID <- unique(d.school.r.once[which(d.school.r.once$sch.num != 4), "newID"])
S4 <- matrix(data= cbind(S4ID, 1), nrow <- length(S4ID), ncol <- 2)
notS4 <- matrix(data= cbind(notS4ID, 0), nrow <- length(notS4ID), ncol <- 2)
S4_dum <- rbind(S4, notS4)
S4_dum <- S4_dum[order(S4_dum[,1]),]
S4 <- S4_dum[,2]
#S4 <- c( rep(NA, length(unique(d.community$newID))), S4 ) #add NAs for non-teacher adults

S3ID <- unique(d.school.r.once[which(d.school.r.once$sch.num == 3), "newID"]) ############################################## S3 Sec far
notS3ID <- unique(d.school.r.once[which(d.school.r.once$sch.num != 3), "newID"])
S3 <- matrix(data= cbind(S3ID, 1), nrow <- length(S3ID), ncol <- 2)
notS3 <- matrix(data= cbind(notS3ID, 0), nrow <- length(notS3ID), ncol <- 2)
S3_dum <- rbind(S3, notS3)
S3_dum <- S3_dum[order(S3_dum[,1]),]
S3 <- S3_dum[,2]


S2ID <- unique(d.school.r.once[which(d.school.r.once$sch.num == 2), "newID"]) ############################################## S2 Sec near
notS2ID <- unique(d.school.r.once[which(d.school.r.once$sch.num != 2), "newID"])
S2 <- matrix(data= cbind(S2ID, 1), nrow <- length(S2ID), ncol <- 2)
notS2 <- matrix(data= cbind(notS2ID, 0), nrow <- length(notS2ID), ncol <- 2)
S2_dum <- rbind(S2, notS2)
S2_dum <- S2_dum[order(S2_dum[,1]),]
S2 <- S2_dum[,2]


S1ID <- unique(d.school.r.once[which(d.school.r.once$sch.num == 1), "newID"]) ############################################## S1 Comm
notS1ID <- unique(d.school.r.once[which(d.school.r.once$sch.num != 1), "newID"])
S1 <- matrix(data= cbind(S1ID, 1), nrow <- length(S1ID), ncol <- 2)
notS1 <- matrix(data= cbind(notS1ID, 0), nrow <- length(notS1ID), ncol <- 2)
S1_dum <- rbind(S1, notS1)
S1_dum <- S1_dum[order(S1_dum[,1]),]
S1 <- S1_dum[,2]


Grade1ID <- unique(d.school.r.once[which(d.school.r.once$Grade == 1), "newID"])
notGrade1ID <- unique(d.school.r.once[which(d.school.r.once$Grade != 1), "newID"])
Grade1 <- matrix(data= cbind(Grade1ID, 1), nrow <- length(Grade1ID), ncol <- 2)
notGrade1 <- matrix(data= cbind(notGrade1ID, 0), nrow <- length(notGrade1ID), ncol <- 2)
Grade1_dum <- rbind(Grade1, notGrade1)
Grade1_dum <- Grade1_dum[order(Grade1_dum[,1]),]
Grade1 <- Grade1_dum[,2]


Grade2ID <- unique(d.school.r.once[which(d.school.r.once$Grade == 2), "newID"])
notGrade2ID <- unique(d.school.r.once[which(d.school.r.once$Grade != 2), "newID"])
Grade2 <- matrix(data= cbind(Grade2ID, 1), nrow <- length(Grade2ID), ncol <- 2)
notGrade2 <- matrix(data= cbind(notGrade2ID, 0), nrow <- length(notGrade2ID), ncol <- 2)
Grade2_dum <- rbind(Grade2, notGrade2)
Grade2_dum <- Grade2_dum[order(Grade2_dum[,1]),]
Grade2 <- Grade2_dum[,2]


Grade3ID <- unique(d.school.r.once[which(d.school.r.once$Grade == 3), "newID"])
notGrade3ID <- unique(d.school.r.once[which(d.school.r.once$Grade != 3), "newID"])
Grade3 <- matrix(data= cbind(Grade3ID, 1), nrow <- length(Grade3ID), ncol <- 2)
notGrade3 <- matrix(data= cbind(notGrade3ID, 0), nrow <- length(notGrade3ID), ncol <- 2)
Grade3_dum <- rbind(Grade3, notGrade3)
Grade3_dum <- Grade3_dum[order(Grade3_dum[,1]),]
Grade3 <- Grade3_dum[,2]


Grade4ID <- unique(d.school.r.once[which(d.school.r.once$Grade == 4), "newID"])
notGrade4ID <- unique(d.school.r.once[which(d.school.r.once$Grade != 4), "newID"])
Grade4 <- matrix(data= cbind(Grade4ID, 1), nrow <- length(Grade4ID), ncol <- 2)
notGrade4 <- matrix(data= cbind(notGrade4ID, 0), nrow <- length(notGrade4ID), ncol <- 2)
Grade4_dum <- rbind(Grade4, notGrade4)
Grade4_dum <- Grade4_dum[order(Grade4_dum[,1]),]
Grade4 <- Grade4_dum[,2]


Grade5ID <- unique(d.school.r.once[which(d.school.r.once$Grade == 5), "newID"])
notGrade5ID <- unique(d.school.r.once[which(d.school.r.once$Grade != 5), "newID"])
Grade5 <- matrix(data= cbind(Grade5ID, 1), nrow <- length(Grade5ID), ncol <- 2)
notGrade5 <- matrix(data= cbind(notGrade5ID, 0), nrow <- length(notGrade5ID), ncol <- 2)
Grade5_dum <- rbind(Grade5, notGrade5)
Grade5_dum <- Grade5_dum[order(Grade5_dum[,1]),]
Grade5 <- Grade5_dum[,2]


MaleID <- unique(d.school.r.once[which(d.school.r.once$sex.1male == 1), "newID"])
notMaleID <- unique(d.school.r.once[which(d.school.r.once$sex.1male == 0), "newID"])
Male <- matrix(data= cbind(MaleID, 1), nrow <- length(MaleID), ncol <- 2)
notMale <- matrix(data= cbind(notMaleID, 0), nrow <- length(notMaleID), ncol <- 2)
Male_dum <- rbind(Male, notMale)
Male_dum <- Male_dum[order(Male_dum[,1]),]
Male <- Male_dum[,2]




# get individual offsets for each year
off_Int_2017 <- mean( pull(post, paste("off_Int[", 1, ",", 1, "]", sep="")) )
off_Int_2019 <- mean( pull(post, paste("off_Int[", 1, ",", 2, "]", sep="")) )
for (i in 2:J) {
  off_Int_2017 <- c( off_Int_2017, mean( pull(post, paste("off_Int[", i, ",", 1, "]", sep="")) ) )
  off_Int_2019 <- c( off_Int_2019, mean( pull(post, paste("off_Int[", i, ",", 2, "]", sep="")) ) )
}


abils2017 <- 0 +  off_Int_2017
abils2019 <- 0 +  off_Int_2019
labs <- c(1:J)
ID <- unique(d.school.r.once[,"ID"])

#data matrix for plotting
all_1 <- as.data.frame( cbind(labs, ID,
                              abils2017, abils2019,
                              machi,
                              Teacher,
                              S4, S3, S2, S1, 
                              Grade1, Grade2, Grade3, Grade4, Grade5,
                              Male, Dropped, Repeats
                            ) ) 

#sort by abils
all <- all_1[order(all_1$abils2017),]
all <- cbind(all, c(1:J))
names(all)
colnames(all)[length(colnames(all))] <- c("ord")

# just people interviewed twice
repeats <- all[which(all$ID %in% twice),]


############ jitter for plotting
jit <- jitter( rep(0,nrow(all)), factor=0, amount=0.15 )
all_jit <- all
all_jit$machi_jit <- all_jit$machi + jit
all$machi[1:10]
all_jit$machi_jit[1:10]

jit <- jitter( rep(0,nrow(repeats)), factor=0, amount=0.15 )
repeats_jit <- repeats
repeats_jit$machi_jit <- repeats_jit$machi + jit
repeats$machi[1:10]
repeats_jit$machi_jit[1:10]




##########################################################


colorlist <- hcl.colors(n=11, palette="Blue-Red 3",
                        alpha=0.75)
names(colorlist) <- c("1.1","1.2","1.3","1.4","1.5", "neutral",
                      "2.5","2.4","2.3","2.2","2.1")
#pie(rep(1, 11), col = colorlist)


colorlist2 <- hcl.colors(n=11, palette="Green-Brown",
                        alpha=0.75)
names(colorlist2) <- c("1.1","1.2","1.3","1.4","1.5", "neutral",
                      "2.5","2.4","2.3","2.2","2.1")
#pie(rep(1, 11), col = colorlist2)


# point and line colors and sizes
BerPoint_lwd <- 0.5
BerPoint_col <- "black" #colorlist["2.3"]
BerPoint_cex <- 0.75
BerPoint_pch <- 16 #1

MatPoint_lwd <- 1
MatPoint_col <- "blue" #colorlist["1.2"]
MatPoint_cex <- 0.75
MatPoint_pch <- 1


BerIndivTraj_lwd <- 0.25
BerIndivTraj_col <- colorlist["2.4"]
BerIndivTraj_lty <- 1

MatIndivTraj_lwd <- 0.25
MatIndivTraj_col <- grey(0.8) #colorlist["1.4"]
MatIndivTraj_lty <- 1


BerMeanTraj_lwd <- 3
BerMeanTraj_col <- "blue" # colorlist["2.1"]
BerMeanTraj_lty <- 1

MatMeanTraj_lwd <- 3
MatMeanTraj_col <- "black" # colorlist["1.1"]
MatMeanTraj_lty <- 1


BerVelTraj_lwd <- 2
BerVelTraj_col <- colorlist2["2.2"]
BerVelTrajArea_col <- colorlist2["2.3"]
BerVelTraj_lty <- 1

MatVelTraj_lwd <- 2
MatVelTraj_col <- colorlist2["1.2"]
MatVelTrajArea_col <- colorlist2["1.4"]
MatVelTraj_lty <- 1



##### Attrition

pdf(file="./Plots/attrition.pdf",
    height=3.5, width=3.5)

#set up plot
par(mar=c(2, 4, 4, 2)) #c(bottom, left, top, right)
plot( x=0, y=2, type="n", ylim=c(-3.5,2), xlim=c(-0.6,2.6), axes=F, ylab=NA, xlab=NA)
box(which = "plot", lty = "solid")
#axis( side=1, at=seq( 0, 25, by=5 ), labels = TRUE ) # bottom
axis( side=2, at=seq( -3, 2, by=1 ), labels = TRUE, cex.axis=0.5 ) # left


#teachers
# 2017
points( x=all_jit$machi_jit[all_jit$Teacher==1 & all_jit$machi==0],
        y=all_jit$abils2017[all_jit$Teacher==1 & all_jit$machi==0],
        lwd=MatPoint_lwd, col="black", cex=MatPoint_cex, pch=MatPoint_pch )

lines(x = c(-0.4, 0.4),
      y = c(
            mean(all_jit$abils2017[all_jit$Teacher==1 & all_jit$machi==0], na.rm=TRUE),
            mean(all_jit$abils2017[all_jit$Teacher==1 & all_jit$machi==0], na.rm=TRUE)
           ),
      col="black", lwd=MatVelTraj_lwd, lty=1)

# 2019
points( x=all_jit$machi_jit[all_jit$Teacher==1 & all_jit$machi==0],
        y=all_jit$abils2019[all_jit$Teacher==1 & all_jit$machi==0],
        lwd=MatPoint_lwd, col="blue", cex=MatPoint_cex, pch=MatPoint_pch )

lines(x = c(-0.4, 0.4),
      y = c(
            mean(all_jit$abils2019[all_jit$Teacher==1 & all_jit$machi==0], na.rm=TRUE),
            mean(all_jit$abils2019[all_jit$Teacher==1 & all_jit$machi==0], na.rm=TRUE)
           ),
      col="blue", lwd=MatVelTraj_lwd, lty=1)

####


# S2 Manu grade 1 
# abils 2017 dropped
points( x=all_jit$machi_jit[all_jit$machi==1 & all_jit$S2==1 & all_jit$Grade1==1 & all_jit$Dropped==1 & all_jit$Teacher==0], 
        y=all_jit$abils2017[all_jit$machi==1 & all_jit$S2==1 & all_jit$Grade1==1 & all_jit$Dropped==1 & all_jit$Teacher==0],
        lwd=MatPoint_lwd, col="black", cex=MatPoint_cex, pch=MatPoint_pch )

lines(x = c(0.6, 1.4),
      y = c(
            mean(all_jit$abils2017[all_jit$machi==1 & all_jit$S2==1 & all_jit$Grade1==1 & all_jit$Dropped==1 & all_jit$Teacher==0], na.rm=TRUE),
            mean(all_jit$abils2017[all_jit$machi==1 & all_jit$S2==1 & all_jit$Grade1==1 & all_jit$Dropped==1 & all_jit$Teacher==0], na.rm=TRUE)
           ),
      col="black", lwd=MatVelTraj_lwd, lty=1)

# abils 2019 not dropped
points( x=all_jit$machi_jit[all_jit$machi==1 & all_jit$S2==1 & all_jit$Grade1==1 & all_jit$Dropped==0 & all_jit$Teacher==0],
        y=all_jit$abils2019[all_jit$machi==1 & all_jit$S2==1 & all_jit$Grade1==1 & all_jit$Dropped==0 & all_jit$Teacher==0],
        lwd=MatPoint_lwd, col="blue", cex=MatPoint_cex, pch=MatPoint_pch )

lines(x = c(0.6, 1.4),
      y = c(
            mean(all_jit$abils2019[all_jit$machi==1 & all_jit$S2==1 & all_jit$Grade1==1 & all_jit$Dropped==0 & all_jit$Teacher==0], na.rm=TRUE),
            mean(all_jit$abils2019[all_jit$machi==1 & all_jit$S2==1 & all_jit$Grade1==1 & all_jit$Dropped==0 & all_jit$Teacher==0], na.rm=TRUE)
           ),
      col="blue", lwd=MatVelTraj_lwd, lty=1)


#S2 Manu grade 2
# abils 2017 dropped
points( x=all_jit$machi_jit[all_jit$machi==1 & all_jit$S2==1 & all_jit$Grade2==1 & all_jit$Dropped==1 & all_jit$Teacher==0] + 1,
        y=all_jit$abils2017[all_jit$machi==1 & all_jit$S2==1 & all_jit$Grade2==1 & all_jit$Dropped==1 & all_jit$Teacher==0],
        lwd=MatPoint_lwd, col="black", cex=MatPoint_cex, pch=MatPoint_pch )

lines(x = c(1.6, 2.4),
      y = c(
            mean(all_jit$abils2017[all_jit$machi==1 & all_jit$S2==1 & all_jit$Grade2==1 & all_jit$Dropped==1 & all_jit$Teacher==0], na.rm=TRUE),
            mean(all_jit$abils2017[all_jit$machi==1 & all_jit$S2==1 & all_jit$Grade2==1 & all_jit$Dropped==1 & all_jit$Teacher==0], na.rm=TRUE)
           ),
      col="black", lwd=MatVelTraj_lwd, lty=1)

# abils 2019 not dropped
points( x=all_jit$machi_jit[all_jit$machi==1 & all_jit$S2==1 & all_jit$Grade2==1 & all_jit$Dropped==0 & all_jit$Teacher==0] + 1,
        y=all_jit$abils2019[all_jit$machi==1 & all_jit$S2==1 & all_jit$Grade2==1 & all_jit$Dropped==0 & all_jit$Teacher==0],
        lwd=MatPoint_lwd, col="blue", cex=MatPoint_cex, pch=MatPoint_pch )

lines(x = c(1.6, 2.4),
      y = c(
            mean(all_jit$abils2019[all_jit$machi==1 & all_jit$S2==1 & all_jit$Grade2==1 & all_jit$Dropped==0 & all_jit$Teacher==0], na.rm=TRUE),
            mean(all_jit$abils2019[all_jit$machi==1 & all_jit$S2==1 & all_jit$Grade2==1 & all_jit$Dropped==0 & all_jit$Teacher==0], na.rm=TRUE)
           ),
      col="blue", lwd=MatVelTraj_lwd, lty=1)




# labels and lines
par(xpd=NA) # plotting clipped to device region
text("Latent axis", x=-1.5, y=-0.5, srt=90, las=3, cex=0.75)

yadj <- 0 #-0.7 #y adjustment for machi labels
cex <- 0.7

text("Teachers", x=0, y=-3, cex=cex)
text("Matsi Students", x=1.5, y=-2.5 + yadj, cex=cex)
text("G1", x=1, y=-1.9 + yadj, cex=cex)
text("G2", x=2, y=-1.9 + yadj, cex=cex)

lines( x=c(0.7, 2.3),
       y=c(-2.2 + yadj, -2.2 + yadj),
       col="black", lwd=2, lty=1)

lines( x=c(0.7, 0.7),
       y=c(-2.2 + yadj, -2.1 + yadj),
       col="black", lwd=2, lty=1)

lines( x=c(2.3, 2.3),
       y=c(-2.2 + yadj, -2.1 + yadj),
       col="black", lwd=2, lty=1)




#set up new plot for legend placement
par(new=TRUE) #add to existing plot
plot( x=0, y=8, type="n", ylim=c(0,10), xlim=c(0,10), axes=FALSE, ylab=NA, xlab=NA)
par(xpd=NA) # plotting clipped to device region

# legend
  # set horizontal spacing for legend text

  legtext <-c("2017",
              "2019" 
               )
  xcoords <- c(0,
               3 
               )
  secondvector <- (1:length(legtext))-1
  textwidths <- xcoords/secondvector # this works for all but the first element
  textwidths[1] <- 3 # so replace element 1 with a finite number (any will do)


  legend(x=0.4, y=12.2,
         ncol=4,
         cex=0.75,
         text.width=textwidths,
         legend=legtext,
         bty="n",
         col=c("black",
               "blue" 
              ),
         merge=FALSE,
         pch=c(NA, 
               NA 
               ),
         lty=c(1,
               1 
               ),
         lwd=c(MatVelTraj_lwd,
               MatVelTraj_lwd #
               ),
         seg.len=2 )




  rect(xleft = -0.4,
       ybottom = 10.7,
       xright = 10.4,
       ytop = 12.2,
       lwd=1)



graphics.off()







##### Gender

pdf(file="./Plots/gender.pdf",
    height=3.5, width=3.5)

#set up plot
par(mar=c(1, 4, 3, 2)) #c(bottom, left, top, right)
plot( x=0, y=2, type="n", ylim=c(-3.5,1.5), xlim=c(-0.6,4.6), axes=F, ylab=NA, xlab=NA)
box(which = "plot", lty = "solid")
axis( side=2, at=seq( -3, 2, by=1 ), labels = TRUE, cex.axis=0.5 ) # left


#male teachers
# 2017
points( x=all_jit$machi_jit[all_jit$Teacher==1 & all_jit$machi==0 & all_jit$Male==1],
        y=all_jit$abils2017[all_jit$Teacher==1 & all_jit$machi==0 & all_jit$Male==1],
        lwd=MatPoint_lwd, col="black", cex=MatPoint_cex, pch=16 )

lines(x = c(-0.4, 0.4),
      y = c(
            mean(all_jit$abils2017[all_jit$Teacher==1 & all_jit$machi==0 & all_jit$Male==1], na.rm=TRUE),
            mean(all_jit$abils2017[all_jit$Teacher==1 & all_jit$machi==0 & all_jit$Male==1], na.rm=TRUE)
           ),
      col="black", lwd=MatVelTraj_lwd, lty=1)

# 2019
points( x=all_jit$machi_jit[all_jit$Teacher==1 & all_jit$machi==0 & all_jit$Male==1],
        y=all_jit$abils2019[all_jit$Teacher==1 & all_jit$machi==0 & all_jit$Male==1],
        lwd=MatPoint_lwd, col="blue", cex=MatPoint_cex, pch=16 )

lines(x = c(-0.4, 0.4),
      y = c(
            mean(all_jit$abils2019[all_jit$Teacher==1 & all_jit$machi==0 & all_jit$Male==1], na.rm=TRUE),
            mean(all_jit$abils2019[all_jit$Teacher==1 & all_jit$machi==0 & all_jit$Male==1], na.rm=TRUE)
           ),
      col="blue", lwd=MatVelTraj_lwd, lty=1)


#female teachers
# 2017
points( x=all_jit$machi_jit[all_jit$Teacher==1 & all_jit$machi==0 & all_jit$Male==0],
        y=all_jit$abils2017[all_jit$Teacher==1 & all_jit$machi==0 & all_jit$Male==0],
        lwd=MatPoint_lwd, col="black", cex=MatPoint_cex, pch=1 )

lines(x = c(-0.4, 0.4),
      y = c(
            mean(all_jit$abils2017[all_jit$Teacher==1 & all_jit$machi==0 & all_jit$Male==0], na.rm=TRUE),
            mean(all_jit$abils2017[all_jit$Teacher==1 & all_jit$machi==0 & all_jit$Male==0], na.rm=TRUE)
           ),
      col="black", lwd=MatVelTraj_lwd, lty="11")

# 2019
points( x=all_jit$machi_jit[all_jit$Teacher==1 & all_jit$machi==0 & all_jit$Male==0],
        y=all_jit$abils2019[all_jit$Teacher==1 & all_jit$machi==0 & all_jit$Male==0],
        lwd=MatPoint_lwd, col="blue", cex=MatPoint_cex, pch=1 )

lines(x = c(-0.4, 0.4),
      y = c(
            mean(all_jit$abils2019[all_jit$Teacher==1 & all_jit$machi==0 & all_jit$Male==0], na.rm=TRUE),
            mean(all_jit$abils2019[all_jit$Teacher==1 & all_jit$machi==0 & all_jit$Male==0], na.rm=TRUE)
           ),
      col="blue", lwd=MatVelTraj_lwd, lty="11")

####


#Male students 
# abils 2017, for repeats who are in G3 in 2019 (from d.school.r.once) 
points( x=all_jit$machi_jit[all_jit$machi==1 & all_jit$Male==1 & all_jit$Grade3==1 & all_jit$Repeats==1],
        y=all_jit$abils2017[all_jit$machi==1 & all_jit$Male==1 & all_jit$Grade3==1 & all_jit$Repeats==1],
        lwd=MatPoint_lwd, col="black", cex=MatPoint_cex, pch=16 )

lines(x = c(0.6, 1.4),
      y = c(
            mean(all_jit$abils2017[all_jit$machi==1 & all_jit$Male==1 & all_jit$Grade3==1 & all_jit$Repeats==1], na.rm=TRUE),
            mean(all_jit$abils2017[all_jit$machi==1 & all_jit$Male==1 & all_jit$Grade3==1 & all_jit$Repeats==1], na.rm=TRUE)
           ),
      col="black", lwd=MatVelTraj_lwd, lty=1)

# abils 2019 G3
points( x=all_jit$machi_jit[all_jit$machi==1 & all_jit$Male==1 & all_jit$Grade3==1 & all_jit$Repeats==1],
        y=all_jit$abils2019[all_jit$machi==1 & all_jit$Male==1 & all_jit$Grade3==1 & all_jit$Repeats==1],
        lwd=MatPoint_lwd, col="blue", cex=MatPoint_cex, pch=16 )

lines(x = c(0.6, 1.4),
      y = c(
            mean(all_jit$abils2019[all_jit$machi==1 & all_jit$Male==1 & all_jit$Grade3==1 & all_jit$Repeats==1], na.rm=TRUE),
            mean(all_jit$abils2019[all_jit$machi==1 & all_jit$Male==1 & all_jit$Grade3==1 & all_jit$Repeats==1], na.rm=TRUE)
           ),
      col="blue", lwd=MatVelTraj_lwd, lty=1)


#Female students G3
# abils 2017
points( x=all_jit$machi_jit[all_jit$machi==1 & all_jit$Male==0 & all_jit$Grade3==1 & all_jit$Repeats==1],
        y=all_jit$abils2017[all_jit$machi==1 & all_jit$Male==0 & all_jit$Grade3==1 & all_jit$Repeats==1],
        lwd=MatPoint_lwd, col="black", cex=MatPoint_cex, pch=1 )

lines(x = c(0.6, 1.4),
      y = c(
            mean(all_jit$abils2017[all_jit$machi==1 & all_jit$Male==0 & all_jit$Grade3==1 & all_jit$Repeats==1], na.rm=TRUE),
            mean(all_jit$abils2017[all_jit$machi==1 & all_jit$Male==0 & all_jit$Grade3==1 & all_jit$Repeats==1], na.rm=TRUE)
           ),
      col="black", lwd=MatVelTraj_lwd, lty="11")

# abils 2019
points( x=all_jit$machi_jit[all_jit$machi==1 & all_jit$Male==0 & all_jit$Grade3==1 & all_jit$Repeats==1],
        y=all_jit$abils2019[all_jit$machi==1 & all_jit$Male==0 & all_jit$Grade3==1 & all_jit$Repeats==1],
        lwd=MatPoint_lwd, col="blue", cex=MatPoint_cex, pch=1 )

lines(x = c(0.6, 1.4),
      y = c(
            mean(all_jit$abils2019[all_jit$machi==1 & all_jit$Male==0 & all_jit$Grade3==1 & all_jit$Repeats==1], na.rm=TRUE),
            mean(all_jit$abils2019[all_jit$machi==1 & all_jit$Male==0 & all_jit$Grade3==1 & all_jit$Repeats==1], na.rm=TRUE)
           ),
      col="blue", lwd=MatVelTraj_lwd, lty="11")



#####

#Male S4 students G3
# abils 2017
points( x=all_jit$machi_jit[all_jit$S4==1 & all_jit$machi==1 & all_jit$Male==1 & all_jit$Grade3==1 & all_jit$Repeats==1] + 1, 
        y=all_jit$abils2017[all_jit$S4==1 & all_jit$machi==1 & all_jit$Male==1 & all_jit$Grade3==1 & all_jit$Repeats==1],
        lwd=MatPoint_lwd, col="black", cex=MatPoint_cex, pch=16 )

lines(x = c(1.6, 2.4),
      y = c(
            mean(all_jit$abils2017[all_jit$S4==1 & all_jit$machi==1 & all_jit$Male==1 & all_jit$Grade3==1 & all_jit$Repeats==1], na.rm=TRUE),
            mean(all_jit$abils2017[all_jit$S4==1 & all_jit$machi==1 & all_jit$Male==1 & all_jit$Grade3==1 & all_jit$Repeats==1], na.rm=TRUE)
           ),
      col="black", lwd=MatVelTraj_lwd, lty=1)

# abils 2019
points( x=all_jit$machi_jit[all_jit$S4==1 & all_jit$machi==1 & all_jit$Male==1 & all_jit$Grade3==1 & all_jit$Repeats==1] + 1,
        y=all_jit$abils2019[all_jit$S4==1 & all_jit$machi==1 & all_jit$Male==1 & all_jit$Grade3==1 & all_jit$Repeats==1],
        lwd=MatPoint_lwd, col="blue", cex=MatPoint_cex, pch=16 )

lines(x = c(1.6, 2.4),
      y = c(
            mean(all_jit$abils2019[all_jit$S4==1 & all_jit$machi==1 & all_jit$Male==1 & all_jit$Grade3==1 & all_jit$Repeats==1], na.rm=TRUE),
            mean(all_jit$abils2019[all_jit$S4==1 & all_jit$machi==1 & all_jit$Male==1 & all_jit$Grade3==1 & all_jit$Repeats==1], na.rm=TRUE)
           ),
      col="blue", lwd=MatVelTraj_lwd, lty=1)


#Female S4 students G3
# abils 2017
points( x=all_jit$machi_jit[all_jit$S4==1 & all_jit$machi==1 & all_jit$Male==0 & all_jit$Grade3==1 & all_jit$Repeats==1] + 1,
        y=all_jit$abils2017[all_jit$S4==1 & all_jit$machi==1 & all_jit$Male==0 & all_jit$Grade3==1 & all_jit$Repeats==1],
        lwd=MatPoint_lwd, col="black", cex=MatPoint_cex, pch=1 )

lines(x = c(1.6, 2.4),
      y = c(
            mean(all_jit$abils2017[all_jit$S4==1 & all_jit$machi==1 & all_jit$Male==0 & all_jit$Grade3==1 & all_jit$Repeats==1], na.rm=TRUE),
            mean(all_jit$abils2017[all_jit$S4==1 & all_jit$machi==1 & all_jit$Male==0 & all_jit$Grade3==1 & all_jit$Repeats==1], na.rm=TRUE)
           ),
      col="black", lwd=MatVelTraj_lwd, lty="11")

# abils 2019
points( x=all_jit$machi_jit[all_jit$S4==1 & all_jit$machi==1 & all_jit$Male==0 & all_jit$Grade3==1 & all_jit$Repeats==1] + 1,
        y=all_jit$abils2019[all_jit$S4==1 & all_jit$machi==1 & all_jit$Male==0 & all_jit$Grade3==1 & all_jit$Repeats==1],
        lwd=MatPoint_lwd, col="blue", cex=MatPoint_cex, pch=1 )

lines(x = c(1.6, 2.4),
      y = c(
            mean(all_jit$abils2019[all_jit$S4==1 & all_jit$machi==1 & all_jit$Male==0 & all_jit$Grade3==1 & all_jit$Repeats==1], na.rm=TRUE),
            mean(all_jit$abils2019[all_jit$S4==1 & all_jit$machi==1 & all_jit$Male==0 & all_jit$Grade3==1 & all_jit$Repeats==1], na.rm=TRUE)
           ),
      col="blue", lwd=MatVelTraj_lwd, lty="11")



####


#Male students G5
# abils 2017
points( x=all_jit$machi_jit[all_jit$machi==1 & all_jit$Male==1 & all_jit$Grade5==1 & all_jit$Repeats==1] + 2,
        y=all_jit$abils2017[all_jit$machi==1 & all_jit$Male==1 & all_jit$Grade5==1 & all_jit$Repeats==1],
        lwd=MatPoint_lwd, col="black", cex=MatPoint_cex, pch=16 )

lines(x = c(2.6, 3.4),
      y = c(
            mean(all_jit$abils2017[all_jit$machi==1 & all_jit$Male==1 & all_jit$Grade5==1 & all_jit$Repeats==1], na.rm=TRUE),
            mean(all_jit$abils2017[all_jit$machi==1 & all_jit$Male==1 & all_jit$Grade5==1 & all_jit$Repeats==1], na.rm=TRUE)
           ),
      col="black", lwd=MatVelTraj_lwd, lty=1)

# abils 2019
points( x=all_jit$machi_jit[all_jit$machi==1 & all_jit$Male==1 & all_jit$Grade5==1 & all_jit$Repeats==1] + 2,
        y=all_jit$abils2019[all_jit$machi==1 & all_jit$Male==1 & all_jit$Grade5==1 & all_jit$Repeats==1],
        lwd=MatPoint_lwd, col="blue", cex=MatPoint_cex, pch=16 )

lines(x = c(2.6, 3.4),
      y = c(
            mean(all_jit$abils2019[all_jit$machi==1 & all_jit$Male==1 & all_jit$Grade5==1 & all_jit$Repeats==1], na.rm=TRUE),
            mean(all_jit$abils2019[all_jit$machi==1 & all_jit$Male==1 & all_jit$Grade5==1 & all_jit$Repeats==1], na.rm=TRUE)
           ),
      col="blue", lwd=MatVelTraj_lwd, lty=1)


#Female students G5
# abils 2017
points( x=all_jit$machi_jit[all_jit$machi==1 & all_jit$Male==0 & all_jit$Grade5==1 & all_jit$Repeats==1] + 2,
        y=all_jit$abils2017[all_jit$machi==1 & all_jit$Male==0 & all_jit$Grade5==1 & all_jit$Repeats==1],
        lwd=MatPoint_lwd, col="black", cex=MatPoint_cex, pch=1 )

lines(x = c(2.6, 3.4),
      y = c(
            mean(all_jit$abils2017[all_jit$machi==1 & all_jit$Male==0 & all_jit$Grade5==1 & all_jit$Repeats==1], na.rm=TRUE),
            mean(all_jit$abils2017[all_jit$machi==1 & all_jit$Male==0 & all_jit$Grade5==1 & all_jit$Repeats==1], na.rm=TRUE)
           ),
      col="black", lwd=MatVelTraj_lwd, lty="11")

# abils 2019
points( x=all_jit$machi_jit[all_jit$machi==1 & all_jit$Male==0 & all_jit$Grade5==1 & all_jit$Repeats==1] + 2,
        y=all_jit$abils2019[all_jit$machi==1 & all_jit$Male==0 & all_jit$Grade5==1 & all_jit$Repeats==1],
        lwd=MatPoint_lwd, col="blue", cex=MatPoint_cex, pch=1 )

lines(x = c(2.6, 3.4),
      y = c(
            mean(all_jit$abils2019[all_jit$machi==1 & all_jit$Male==0 & all_jit$Grade5==1 & all_jit$Repeats==1], na.rm=TRUE),
            mean(all_jit$abils2019[all_jit$machi==1 & all_jit$Male==0 & all_jit$Grade5==1 & all_jit$Repeats==1], na.rm=TRUE)
           ),
      col="blue", lwd=MatVelTraj_lwd, lty="11")



#####

#Male S4 students G5
# abils 2017
points( x=all_jit$machi_jit[all_jit$S4==1 & all_jit$machi==1 & all_jit$Male==1 & all_jit$Grade5==1 & all_jit$Repeats==1] + 3, 
        y=all_jit$abils2017[all_jit$S4==1 & all_jit$machi==1 & all_jit$Male==1 & all_jit$Grade5==1 & all_jit$Repeats==1],
        lwd=MatPoint_lwd, col="black", cex=MatPoint_cex, pch=16 )

lines(x = c(3.6, 4.4),
      y = c(
            mean(all_jit$abils2017[all_jit$S4==1 & all_jit$machi==1 & all_jit$Male==1 & all_jit$Grade5==1 & all_jit$Repeats==1], na.rm=TRUE),
            mean(all_jit$abils2017[all_jit$S4==1 & all_jit$machi==1 & all_jit$Male==1 & all_jit$Grade5==1 & all_jit$Repeats==1], na.rm=TRUE)
           ),
      col="black", lwd=MatVelTraj_lwd, lty=1)

# abils 2019
points( x=all_jit$machi_jit[all_jit$S4==1 & all_jit$machi==1 & all_jit$Male==1 & all_jit$Grade5==1 & all_jit$Repeats==1] + 3,
        y=all_jit$abils2019[all_jit$S4==1 & all_jit$machi==1 & all_jit$Male==1 & all_jit$Grade5==1 & all_jit$Repeats==1],
        lwd=MatPoint_lwd, col="blue", cex=MatPoint_cex, pch=16 )

lines(x = c(3.6, 4.4),
      y = c(
            mean(all_jit$abils2019[all_jit$S4==1 & all_jit$machi==1 & all_jit$Male==1 & all_jit$Grade5==1 & all_jit$Repeats==1], na.rm=TRUE),
            mean(all_jit$abils2019[all_jit$S4==1 & all_jit$machi==1 & all_jit$Male==1 & all_jit$Grade5==1 & all_jit$Repeats==1], na.rm=TRUE)
           ),
      col="blue", lwd=MatVelTraj_lwd, lty=1)


#Female S4 students G5
# abils 2017
points( x=all_jit$machi_jit[all_jit$S4==1 & all_jit$machi==1 & all_jit$Male==0 & all_jit$Grade5==1 & all_jit$Repeats==1] + 3,
        y=all_jit$abils2017[all_jit$S4==1 & all_jit$machi==1 & all_jit$Male==0 & all_jit$Grade5==1 & all_jit$Repeats==1],
        lwd=MatPoint_lwd, col="black", cex=MatPoint_cex, pch=1 )

lines(x = c(3.6, 4.4),
      y = c(
            mean(all_jit$abils2017[all_jit$S4==1 & all_jit$machi==1 & all_jit$Male==0 & all_jit$Grade5==1 & all_jit$Repeats==1], na.rm=TRUE),
            mean(all_jit$abils2017[all_jit$S4==1 & all_jit$machi==1 & all_jit$Male==0 & all_jit$Grade5==1 & all_jit$Repeats==1], na.rm=TRUE)
           ),
      col="black", lwd=MatVelTraj_lwd, lty="11")

# abils 2019
points( x=all_jit$machi_jit[all_jit$S4==1 & all_jit$machi==1 & all_jit$Male==0 & all_jit$Grade5==1 & all_jit$Repeats==1] + 3,
        y=all_jit$abils2019[all_jit$S4==1 & all_jit$machi==1 & all_jit$Male==0 & all_jit$Grade5==1 & all_jit$Repeats==1],
        lwd=MatPoint_lwd, col="blue", cex=MatPoint_cex, pch=1 )

lines(x = c(3.6, 4.4),
      y = c(
            mean(all_jit$abils2019[all_jit$S4==1 & all_jit$machi==1 & all_jit$Male==0 & all_jit$Grade5==1 & all_jit$Repeats==1], na.rm=TRUE),
            mean(all_jit$abils2019[all_jit$S4==1 & all_jit$machi==1 & all_jit$Male==0 & all_jit$Grade5==1 & all_jit$Repeats==1], na.rm=TRUE)
           ),
      col="blue", lwd=MatVelTraj_lwd, lty="11")



# labels and lines
par(xpd=NA) # plotting clipped to device region
text("Latent axis", x=-2, y=-1.25, srt=90, las=3, cex=0.75)

yadj <- 0 #y adjustment for machi labels
cex <- 0.7

text("Teachers", x=0, y=-3, cex=cex)
text("Matsi Students", x=2.5, y=-2.5 + yadj, cex=cex)
text("G1-3", x=1, y=-1.6 + yadj, cex=cex)
text("all", x=1, y=-1.9 + yadj, cex=cex)
text("G1-3", x=2, y=-1.6 + yadj, cex=cex)
text("rel", x=2, y=-1.9 + yadj, cex=cex)

text("G3-5", x=3, y=-1.6 + yadj, cex=cex)
text("all", x=3, y=-1.9 + yadj, cex=cex)
text("G3-5", x=4, y=-1.6 + yadj, cex=cex)
text("rel", x=4, y=-1.9 + yadj, cex=cex)

lines( x=c(0.7, 4.3),
       y=c(-2.2 + yadj, -2.2 + yadj),
       col="black", lwd=2, lty=1)

lines( x=c(0.7, 0.7),
       y=c(-2.2 + yadj, -2.1 + yadj),
       col="black", lwd=2, lty=1)

lines( x=c(4.3, 4.3),
       y=c(-2.2 + yadj, -2.1 + yadj),
       col="black", lwd=2, lty=1)




#set up new plot for legend placement
par(new=TRUE) #add to existing plot
plot( x=0, y=8, type="n", ylim=c(0,10), xlim=c(0,10), axes=FALSE, ylab=NA, xlab=NA)
par(xpd=NA) # plotting clipped to device region

# legend
  # set horizontal spacing for legend text

  legtext <- c("Female indiv",
               "", 
               "Male indiv"
               )
  xcoords <- c(0,
               1,#1.7, #1.5 #4.5,  #4 #8.5 # moves third item right
               3.1 #4.6 #4 #8.5#, #7.5 #24.2 # moves last item right
               #15  #36.8 # moves last three items together right
               )
  secondvector <- (1:length(legtext))-1
  textwidths <- xcoords/secondvector # this works for all but the first element
  textwidths[1] <- 6 #2 # moves last three items right  # so replace element 1 with a finite number (any will do)



  legend(x=0.4, #-3.25, #-2.4
         y=12.5,
         ncol=3,
         cex=0.5, #1.3
         text.width=textwidths,
         legend=legtext,
         bty="n",

         col=c(grey(0.5),
               "white", 
               grey(0.5) 
              ),
         merge=FALSE,
         pch=c(1,
               MatPoint_pch, 
               16 
               ),
         lty=c(0,
               0,
               0 
               ),
         lwd=c(BerPoint_lwd,
               MatPoint_lwd,
               MatPoint_lwd 
               ),
         seg.len=2 )


  legtext <-c("Female mean",
              "Male mean" 
               )
  xcoords <- c(0,
               4.5 
               )
  secondvector <- (1:length(legtext))-1
  textwidths <- xcoords/secondvector # this works for all but the first element
  textwidths[1] <- 3 # so replace element 1 with a finite number (any will do)

linetype <- "11"

  legend(x=0.4, y=12,
         ncol=4,
         cex=0.5,
         text.width=textwidths,
         legend=legtext,
         bty="n",
         col=c(grey(0.5),
               grey(0.5) 
              ),
         merge=FALSE,
         pch=c(NA, 
               NA 
               ),
         lty=c(3,
               1 
               ),
         lwd=c(MatVelTraj_lwd,
               MatVelTraj_lwd 
               ),
         seg.len=2 )




  legtext <-c("2017",
              "2019" #
               )
  xcoords <- c(0,
               4.5 
               )
  secondvector <- (1:length(legtext))-1
  textwidths <- xcoords/secondvector # this works for all but the first element
  textwidths[1] <- 3 # so replace element 1 with a finite number (any will do)


  legend(x=0.4, y=11.5,
         ncol=4,
         cex=0.5,
         text.width=textwidths,
         legend=legtext,
         bty="n",
         #bg="white",
         col=c("black",
               "blue" #
              ),
         merge=FALSE,
         pch=c(NA, 
               NA 
               ),
         lty=c(1,
               1 
               ),
         lwd=c(MatVelTraj_lwd,
               MatVelTraj_lwd 
               ),
         seg.len=2 )




  rect(xleft = -0.4,
       ybottom = 10.7,
       xright = 10.4,
       ytop = 12.5,
       lwd=1)



graphics.off()








