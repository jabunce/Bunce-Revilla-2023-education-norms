
# load school and community data

d <- read.csv(
  file=
  "./data/SchoolComm_data.csv",
  header=TRUE)

#Check the variable names and dimensions in the data frame
names(d)
dim(d)





J <- length(unique(d$newID))	#number of people
K <- length(unique(d$QID)) 		#number of questions
N <- nrow(d)				          #total number of responses
jj <- d$newID			            #vector of person IDs
kk <- d$QID				            #vector of question IDs
y <- d$ResponseFlipped			  #vector of responses


quest_names <- c("1.wife hunts/works", "2.daughter babysits", "3.not wear dead hat",
               "4.hit students", "5.no questions", "6.post flu", "7.not post chest",
               "8.pot to needy", "9.good nonbaptized no heaven", "10.bad baptized heaven",
               "11.stop work to visit", "12.cheap mean store", "13.xcousin marriage",
               "14.arranged marriage", "15.laborer party")




####### base model: m1, rand effect for person ############################

rstan_options(auto_write = TRUE) #to let stan make a copy of the model and use multiple cores
options(mc.cores = parallel::detectCores())

model_file <- list.files(path="./Code/", pattern="*.stan", full.names=TRUE) #list of paths to Stan models



data_list_1 <- list(
	J = length(unique(d$newID)),	#number of people
	K = length(unique(d$QID)),		#number of questions
	N = nrow(d),			            #total number of responses
	jj = d$newID,			            #vector of person IDs
	kk = d$QID,			              #vector of question IDs
	y = d$ResponseFlipped			    #vector of responses
)

length(jj)
length(kk)
length(unique(jj))
length(y)


start_list_1 <- list(
  b0=0, b1 = as.array(rep(0, times=J)), 
  beta = as.array(rep(0, times=K)),
  gamma= as.array(rep(1, times=K)),
  sigma_beta=1, sigma_gamma=1
)

# samps <- 3000 #3000
# num_chains <- 4 #4



model <- cmdstan_model(model_file[1])

m1 <- model$sample(
        seed=1,
        data=data_list_1, 
        iter_warmup=samps/2,
        save_warmup = TRUE,
        iter_sampling=samps/2,
        chains=num_chains,
        init=rep(list(start_list_1), num_chains),
        max_treedepth=10, #default treedepth is 10
        adapt_delta=0.99
      )

m1$save_object(file = "m1_fit.RDS") #save stan output so you don't have re-run model
#m1 <- readRDS("m1_fit.RDS")


# look at posterior estimates
print(n=300, as_tibble(
    m1$summary(c(
                "lp__", "b0","b1","beta", "gamma",
                "sigma_beta", "sigma_gamma"
               ))
))

post1 <- m1$draws(format = "df", , inc_warmup = FALSE)
#str(post1)
saveRDS(post1, "post1.RDS")
#post1 <- readRDS("post1.RDS")
#post1$muInt


fitmod <- m1$draws(format = "array", inc_warmup = TRUE) # array format keeps chains separate for plotting
str(fitmod)

color_scheme_set("mix-blue-red")

#look at all traces, in Plots folder
pdf(file="./Plots/traces_m1.pdf",
  height=3, width=8)
par(mfrow=c(2,1))


    print(mcmc_trace(fitmod, pars="b0", iter1 = samps/2))
    print(mcmc_trace(fitmod, pars="sigma_beta", iter1 = samps/2))
    print(mcmc_trace(fitmod, pars="sigma_gamma", iter1 = samps/2))
    print(mcmc_trace(fitmod, pars="lp__", iter1 = samps/2))

    for ( z2 in 1:J ){
        print(mcmc_trace(fitmod, pars=paste("b1[", z2, "]", sep=""), iter1 = samps/2))
    }

    for ( z2 in 1:K ){
        print(mcmc_trace(fitmod, pars=paste("beta[", z2, "]", sep=""), iter1 = samps/2))
    }

    for ( z2 in 1:K ){
        print(mcmc_trace(fitmod, pars=paste("gamma[", z2, "]", sep=""), iter1 = samps/2))
    }

graphics.off()







############################ Plots


#Makes a vector of length J with 1 or 0 depending on whether the individual has the given characteristic
machiID <- unique(d[which(d$Machi == 1), "newID"])
length(machiID) #number of machis in dataset
mestizoID <- unique(d[which(d$Machi == 0), "newID"])
length(mestizoID) #number of mestizos in dataset
machi <- matrix(data= cbind(machiID, 1), nrow <- length(machiID), ncol <- 2)
mestizo <- matrix(data= cbind(mestizoID, 0), nrow <- length(mestizoID), ncol <- 2)
machi_dum <- rbind(machi, mestizo)
machi_dum <- machi_dum[order(machi_dum[,1]),] #order based on newID
machi <- machi_dum[,2]


TeacherID <- unique(d[which(d$Grade == 9), "newID"])
notTeacherID <- unique(d[which(d$Grade != 9), "newID"])
Teacher <- matrix(data= cbind(TeacherID, 1), nrow <- length(TeacherID), ncol <- 2)
notTeacher <- matrix(data= cbind(notTeacherID, 0), nrow <- length(notTeacherID), ncol <- 2)
Teacher_dum <- rbind(Teacher, notTeacher)
Teacher_dum <- Teacher_dum[order(Teacher_dum[,1]),]
Teacher <- Teacher_dum[,2]
Teacher <- c( rep(NA, length(unique(d[which(is.na(d$Grade)),"newID"]))), Teacher ) #add NAs for non-teacher adults



mb0 <- mean( pull(post1, "b0") )
mb1 <- mean( pull(post1, paste("b1[", 1, "]", sep="")) )
for (i in 2:J) {
  mb1 <- c( mb1, mean( pull(post1, paste("b1[", i, "]", sep="")) ) )
}


abils <- mb0 + mb1 #mean(b0_samp) +  colMeans(b1_samp)
o <- c(J:1)
abils[o]  #inverse order for plotting
labs <- c(1:J)
ID <- unique(d[,"newID"])

#data matrix for plotting
all_1 <- as.data.frame( cbind(labs, ID, abils,
                            machi,
                            #School.mest, WorkMest, ComMest,
                            Teacher
                            ) ) 

#sort by abils
all <- all_1[order(all_1$abils),]
all <- cbind(all, c(1:J))
names(all)
colnames(all)[6] <- c("ord")


############################### Individual location by characteristic
jit <- jitter( rep(0,nrow(all)), factor=0, amount=0.15 )
all_jit <- all
all_jit$machi_jit <- all_jit$machi + jit
all$machi[1:10]
all_jit$machi_jit[1:10]




##################Teachers and Students
pdf(file="./plots/Dotplot_mest_matsi.pdf", 
    height=3.5, width=3.5)

xyplot( all_jit$abils ~ all_jit$machi,
  xlim=c(-0.8, 3.8),
  ylim=c(-3.5, 1.7),
  cex=0.4,
  xlab="",
  ylab="",
  scales=list( draw=T, tck=c(1,0), cex=0.6, #alternating=1,
              x=list(draw=F)
              ),
  panel = function (x, y) {
          #mestizo non-teachers
          panel.xyplot( x=all_jit$machi_jit[is.na(all_jit$Teacher) & all_jit$machi==0],
                        y=all_jit$abils[is.na(all_jit$Teacher) & all_jit$machi==0],
                        pch = 1, cex=0.5, col = "black",
                        jitter.x = F, jitter.y = FALSE,
                        amount = 0.1)

          #mestizo teachers
          panel.xyplot( x=all_jit$machi_jit[all_jit$Teacher==1 & all_jit$machi==0] + 1,
                        y=all_jit$abils[all_jit$Teacher==1 & all_jit$machi==0],
                        pch = 1, cex=0.5, col = "black",
                        jitter.x = F, jitter.y = FALSE,
                        amount = 0.1)


          #adult machis
          panel.xyplot( x=all_jit$machi_jit[is.na(all_jit$Teacher) & all_jit$machi==1] + 1,
                        y=all_jit$abils[is.na(all_jit$Teacher) & all_jit$machi==1],
                        pch = 16, cex=0.5, col = "black",
                        jitter.x = F, jitter.y = FALSE,
                        amount = 0.1)



          #current machi students
          panel.xyplot( x=all_jit$machi_jit[all_jit$Teacher==0 & all_jit$machi==1] + 2,
                        y=all_jit$abils[all_jit$Teacher==0 & all_jit$machi==1],
                        pch = 16, cex=0.5, col = "black",
                        jitter.x = F, jitter.y = FALSE,
                        amount = 0.1)



          #bottom labels and lines
          ltext(x=0,
                 y=-2.6,
                 labels="Non-Teachers", pos=1, offset=0, cex=0.5, col="black")
          ltext(x=1,
                 y=-2.6,
                 labels="Teachers", pos=1, offset=0, cex=0.5, col="black")
          ltext(x=0.5,
                 y=-3,
                 labels="Non-Matsigenka", pos=1, offset=0, cex=0.5, col="black")

          ltext(x=2.6,
                 y=-2.3,
                 labels="Matsigenka", pos=1, offset=0, cex=0.5, col="black")
          ltext(x=2,
                 y=-1.9,
                 labels="Adults", pos=1, offset=0, cex=0.5, col="black")
          ltext(x=3,
                 y=-1.9,
                 labels="Students", pos=1, offset=0, cex=0.5, col="black")


          panel.segments( x0=-0.5,
                          y0=-2.9,
                          x1=1.4, 
                          y1=-2.9, col="black", lwd=1 )
          panel.segments( x0=-0.5,
                          y0=-2.9,
                          x1=-0.5, 
                          y1=-2.8, col="black", lwd=1 )
          panel.segments( x0=1.4,
                          y0=-2.9,
                          x1=1.4, 
                          y1=-2.8, col="black", lwd=1 )

          panel.segments( x0=1.7,
                          y0=-2.2,
                          x1=3.5, 
                          y1=-2.2, col="black", lwd=1 )
          panel.segments( x0=1.7,
                          y0=-2.2,
                          x1=1.7, 
                          y1=-2.1, col="black", lwd=1 )
          panel.segments( x0=3.5,
                          y0=-2.2,
                          x1=3.5, 
                          y1=-2.1, col="black", lwd=1 )

          #mean lines
          panel.segments( x0=-0.4,
                          y0=mean(all_jit$abils[is.na(all_jit$Teacher) & all_jit$machi==0], na.rm=TRUE),
                          x1=0.4, 
                          y1=mean(all_jit$abils[is.na(all_jit$Teacher) & all_jit$machi==0], na.rm=TRUE),
                          col="black", lwd=0.5 )
          panel.segments( x0=0.6,
                          y0=mean(all_jit$abils[all_jit$Teacher==1 & all_jit$machi==0], na.rm=TRUE),
                          x1=1.4, 
                          y1=mean(all_jit$abils[all_jit$Teacher==1 & all_jit$machi==0], na.rm=TRUE),
                          col="black", lwd=0.5 )
          panel.segments( x0=1.6,
                          y0=mean(all_jit$abils[is.na(all_jit$Teacher) & all_jit$machi==1], na.rm=TRUE),
                          x1=2.4, 
                          y1=mean(all_jit$abils[is.na(all_jit$Teacher) & all_jit$machi==1], na.rm=TRUE),
                          col="black", lwd=0.5 )
          panel.segments( x0=2.6,
                          y0=mean(all_jit$abils[all_jit$Teacher==0 & all_jit$machi==1], na.rm=TRUE),
                          x1=3.4, 
                          y1=mean(all_jit$abils[all_jit$Teacher==0 & all_jit$machi==1], na.rm=TRUE),
                          col="black", lwd=0.5 )         

  } #panel
) #xyplot


vp2 <- viewport(x = 0.05, y = 0.47, width = 0.08, height = 0.84, just = c("center", "center"))
pushViewport(vp2)
#grid.rect(gp = gpar(vol = "blue")) # just to see dimensions/position of the viewport
#grid.text("Eje Latente", gp = gpar(cex = 0.7), rot=90)

grid.text(x=0.5, y=0.87, just=c("right","center"), label="Inter-dependence", gp = gpar(cex = 0.6), rot=90)
grid.lines( x = c(0.55,0.55), y = c(0.89,0.99), gp = gpar(col = "black", lwd=1),
            arrow = arrow(angle = 20, length = unit(0.08, "inches"), ends = "last", type = "open") )

grid.text(x=0.5, y=0.23, just=c("left","center"), label="Independence", gp = gpar(cex = 0.6), rot=90)
grid.lines( x = c(0.55,0.55), y = c(0.21,0.11), gp = gpar(col = "black", lwd=1),
            arrow = arrow(angle = 20, length = unit(0.08, "inches"), ends = "last", type = "open") )


graphics.off()



