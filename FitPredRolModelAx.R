
############################# STAN analysis ##############################################################

rstan_options(auto_write = TRUE) #to let stan make a copy of the model and use multiple cores
options(mc.cores = parallel::detectCores())



#load models and data and starting conditions

model_file <- list.files(path="./Code/", pattern="*.stan", full.names=TRUE) #list of paths to Stan models



# model with just random effect for person and school and predictor for grade

J <- length(unique(d.school.r$newID))               # number of people
K <- length(unique(d.school.r$QID))                     # number of questions
A <- length(unique(d.school.r$intYear))                 # number of years
N <- nrow(d.school.r)                                   # total number of responses
S <- length(unique(d.school.r$sch.num))                 # number of schools
G <- length(unique(d.school.r$Grade))                   # number of grade levels
R <- length(unique(d.school.r$Role))                    # number of roles (teacher or student)
jj <- d.school.r$newID                                  # vector of person IDs
kk <- d.school.r$QID                                    # vector of question IDs
y <- d.school.r$ResponseFlipped                         # vector of responses
ss <- d.school.r$sch.num                                # vector of school IDs
aa <- ifelse( d.school.r$intYear == 2017, 1, 2 )        # vector of interview years (2017=1, 2019=2)
rr <- d.school.r$Role                                   # vector of roles for each observation: 2=student, 1=teacher

data_list_5 <- list(
    J = length(unique(d.school.r$newID)),                   # number of people
    K = length(unique(d.school.r$QID)),                     # number of questions
    A = length(unique(d.school.r$intYear)),                 # number of years
    N = nrow(d.school.r),                                   # total number of responses
    S = length(unique(d.school.r$sch.num)),                 # number of schools
    R = length(unique(d.school.r$Role)),                    # number of roles (teacher or student)
    jj = d.school.r$newID,                                  # vector of person IDs
    kk = d.school.r$QID,                                    # vector of question IDs
    y = d.school.r$ResponseFlipped,                         # vector of responses
    ss = d.school.r$sch.num,                                # vector of school IDs
    aa = ifelse( d.school.r$intYear == 2017, 1, 2 ),        # vector of interview years (2017=1, 2019=2)
    rr = d.school.r$Role,                                   # vector of roles for each observation: 2=student, 1=teacher

    G = length(unique(d.school.r$Grade)),     #number of grade levels
    Grade = d.school.r$Grade                  #vector of grades for each observation
)


start_list_5 <- list(
  muInt = 0,
  
  zInt = matrix(0, nrow=J, ncol=A),
  sigmaInt = as.array(rep(1, times=A)),
  L_R = array(data=0, dim=c(A,A)),

  zSch = array(data=0, dim=c(R,S,A)),
  sigmaSch = matrix(1, nrow=R, ncol=A),
  L_S = array(data=0, dim=c(R,A,A)),

  X = array(data=1/G, dim=c(G,S,A)),
  bGrade = matrix(0, nrow=S, ncol=A),

  beta = as.array(rep(0, times=K)),
  muGamma = 0,
  off_Gamma= matrix(0, nrow=K, ncol=R),
  sigma_beta=1,
  sigma_gamma=1
)



samps <- number_samps
num_chains <- number_chains

model <- cmdstan_model(model_file[1])

m5 <- model$sample(
        seed=1,
        data=data_list_5, 
        iter_warmup=samps/2,
        save_warmup = TRUE,
        iter_sampling=samps/2,
        chains=num_chains,
        init=rep(list(start_list_5), num_chains),
        max_treedepth=10, #default treedepth is 10
        adapt_delta=0.999
      )

m5$save_object(file = "m5_fit.RDS") #save stan output so you don't have re-run model
#m5 <- readRDS("m5_fit.RDS")


# look at posterior estimates
print(n=500, as_tibble(
    m5$summary(c(
            "lp__",
            "muInt",
            "X",
            "bGrade",
            "CumuGrade",
            "sigmaInt",
            "sigma_beta",
            "sigma_gamma",
            "beta",
            "muGamma",
            "off_Gamma",
            "off_Sch",
            "off_Int"
            ))
))#[,1:2])

post5 <- m5$draws(format = "df", , inc_warmup = FALSE)
str(post5, list.len = 754)
saveRDS(post5, "post5.RDS")
#post5 <- readRDS("post5.RDS")



fitmod <- m5$draws(format = "draws_array", inc_warmup = TRUE) # array format keeps chains separate for plotting
str(fitmod)

color_scheme_set("mix-blue-red")

#look at all traces, in Plots folder
pdf(file="./Plots/traces_m5.pdf",
  height=3, width=8)
par(mfrow=c(2,1))


    print(mcmc_trace(fitmod, pars="lp__", iter1 = samps/2))
    print(mcmc_trace(fitmod, pars="sigma_beta", iter1 = samps/2))
    print(mcmc_trace(fitmod, pars="sigma_gamma", iter1 = samps/2))
    print(mcmc_trace(fitmod, pars="muInt", iter1 = samps/2))
    print(mcmc_trace(fitmod, pars="muGamma", iter1 = samps/2))


    for ( z1 in 1:J ){
        for (z2 in 1:A) {
            print(mcmc_trace(fitmod, pars=paste("off_Int[", z1, ",", z2, "]", sep=""), iter1 = samps/2))
        } #z2
    } #z1

    for ( z1 in 1:R ){
        for ( z2 in 1:S ){
            for (z3 in 1:A) {
                print(mcmc_trace(fitmod, pars=paste("off_Sch[", z1, ",", z2, ",", z3, "]", sep=""), iter1 = samps/2))
            }
        }
    }

    for ( z1 in 1:S ){
        for (z2 in 1:A) {
            print(mcmc_trace(fitmod, pars=paste("bGrade[", z1, ",", z2, "]", sep=""), iter1 = samps/2))
        }
    }

    for ( z1 in 1:S ){
        for (z2 in 1:A) {
            for ( z3 in 1:G ){
                print(mcmc_trace(fitmod, pars=paste("X[", z1, ",", z2, ",", z3, "]", sep=""), iter1 = samps/2))
            }
        }
    }

    for ( z2 in 1:A ){
        print(mcmc_trace(fitmod, pars=paste("sigmaInt[", z2, "]", sep=""), iter1 = samps/2))
    }


    for ( z1 in 1:R ){
        for ( z2 in 1:A ){
            print(mcmc_trace(fitmod, pars=paste("sigmaSch[", z1, ",", z2, "]", sep=""), iter1 = samps/2))
        }
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




