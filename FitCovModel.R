
############################# STAN analysis ##############################################################

rstan_options(auto_write = TRUE) #to let stan make a copy of the model and use multiple cores
options(mc.cores = parallel::detectCores())



#load models and data and starting conditions

model_file <- list.files(path="./Code/", pattern="*.stan", full.names=TRUE) #list of paths to Stan models



# model with just random effect for person

J <- length(unique(d.school.r$newID))    # number of people
K <- length(unique(d.school.r$QID))      # number of questions
A <- length(unique(d.school.r$intYear))  # number of years
N <- nrow(d.school.r)                    # total number of responses


data_list_2 <- list(
    J = length(unique(d.school.r$newID)),                 #number of people
    K = length(unique(d.school.r$QID)),                   #number of questions
    A = length(unique(d.school.r$intYear)),               #number of years
    N = nrow(d.school.r),                                 #total number of responses
    R = length(unique(d.school.r$Role)),                  # number of roles (teacher or student)
    jj = d.school.r$newID,                                #vector of person IDs
    kk = d.school.r$QID,                                  #vector of question IDs
    aa = ifelse( d.school.r$intYear == 2017, 1, 2 ),      #vector of interview years (2017=1, 2019=2)
    rr = d.school.r$Role,                                 # vector of roles for each observation: 2=student, 1=teacher
    y = d.school.r$ResponseFlipped                        #vector of responses
)


start_list_2 <- list(
  #muInt = 0,
  zInt = matrix(0, nrow=J, ncol=A),
  sigmaInt = as.array(rep(1, times=A)),
  L_R = array(data=0, dim=c(A,A)),
  beta = as.array(rep(0, times=K)),
  gamma = as.array(rep(1, times=K))
)



# samps <- 3000
# num_chains <- 6

model <- cmdstan_model(model_file[2])

m2 <- model$sample(
        seed=1,
        data=data_list_2, 
        iter_warmup=samps/2,
        save_warmup = TRUE,
        iter_sampling=samps/2,
        chains=num_chains,
        init=rep(list(start_list_2), num_chains),
        max_treedepth=10, #default treedepth is 10
        adapt_delta=0.99
      )

m2$save_object(file = "m2_fit.RDS") #save stan output so you don't have re-run model
#m2 <- readRDS("m2_fit.RDS")


# look at posterior estimates
print(n=100, as_tibble(
    m2$summary(c(
            "sigmaInt",
            "beta",
            "gamma",
            "lp__",
            "off_Int"
            ))
))

post2 <- m2$draws(format = "df", , inc_warmup = FALSE)
#str(post2)
saveRDS(post2, "post2.RDS")
#post2 <- readRDS("post2.RDS")
#post2$muInt


fitmod <- m2$draws(format = "array", inc_warmup = TRUE) # array format keeps chains separate for plotting
str(fitmod)

color_scheme_set("mix-blue-red")

#look at all traces, in Plots folder
pdf(file="./Plots/traces_m2.pdf",
  height=3, width=8)
par(mfrow=c(2,1))


    print(mcmc_trace(fitmod, pars="muInt", iter1 = samps/2))
    print(mcmc_trace(fitmod, pars="muGamma", iter1 = samps/2))
    print(mcmc_trace(fitmod, pars="sigma_beta", iter1 = samps/2))
    print(mcmc_trace(fitmod, pars="sigma_gamma", iter1 = samps/2))
    print(mcmc_trace(fitmod, pars="lp__", iter1 = samps/2))

    for ( z1 in 1:J ){
        for (z2 in 1:A) {
            print(mcmc_trace(fitmod, pars=paste("off_Int[", z1, ",", z2, "]", sep=""), iter1 = samps/2))
        } #z2
    } #z1

    for ( z2 in 1:A ){
        print(mcmc_trace(fitmod, pars=paste("sigmaInt[", z2, "]", sep=""), iter1 = samps/2))
    }

    for ( z2 in 1:K ){
        print(mcmc_trace(fitmod, pars=paste("beta[", z2, "]", sep=""), iter1 = samps/2))
    }

    for ( z1 in 1:K ){
        for (z2 in 1:R) {
            print(mcmc_trace(fitmod, pars=paste("off_Gamma[", z1, ",", z2, "]", sep=""), iter1 = samps/2))
        }
    }
    

graphics.off()




