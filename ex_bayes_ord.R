#This is just a test
# Ok. this is fine
#Great job
#set a branch
library(rstan)
library(slurmR)
library(simstudy)
library(data.table)

### Function to be execute on array of cores (12 iterations per core)

iter <- function(iternum, defA, baseprobs, nobs, sm) {
  
    dT <- genData(nobs)
    dT <- trtAssign(dT, grpName = "exposed")
    dT <- addColumns(defA, dT)
    
    dT <- genOrdCat(dT, adjVar = "z", baseprobs, catVar = "r")
    
    K <- length(dT[, levels(r)])
    N <- dT[, .N]
    x <- as.matrix(dT[, .(exposed)])
    D <- ncol(x)
    y <- as.vector(dT[, as.numeric(r)])
    
    sampdat <- list(K = K, N = N, D= D, y = y, x = x)
    
    fit <-  sampling(sm, data=sampdat, seed = 3327, 
                     iter = 3000, warmup = 500, show_messages = FALSE, 
                     cores = 4, refresh = 0)
    
    p.eff <- mean(extract(fit, pars = "OR")[[1]] < 1)
    p.clinic <- mean(extract(fit, pars = "OR")[[1]] < 0.8)
    
    data.table(iternum, p.eff, p.clinic)
}
  
### Compile Stan model (just once in this case)

rt <- stanc("~/r/ordmodel.stan")
sm <- stan_model(stanc_ret = rt, verbose=FALSE)

## Define data (only 7 categories here)

defA <- defDataAdd(varname = "z", formula = "0.30 * exposed", dist = "nonrandom")
baseprobs <-  c(.15, .20, .35, .15, .10, .05)

## Create 1080 data sets and fit model for each 
## With 90 cores, that means 12 data sets and model fits per core
## Should take around 6 to 8 minutes. 
## Performance may actually improve if njobs is reduced

job <- Slurm_lapply(1:1080,
             iter, 
             defA = defA,
             baseprobs = baseprobs,
             nobs = 250,
             sm = sm,
             njobs = 90, 
             mc.cores = 4,
             tmp_path = "/gpfs/scratch/goldfk01",
             job_name = "bayes_ord",
             sbatch_opt = list(time = "00:30:00"),
             plan = "wait")

### Save data

res_bayesord <- Slurm_collect(job) # data is a list
res_bayesord <- rbindlist(res_bayesord) # converting list to data.table

date_stamp <- gsub("-", "", Sys.Date())
dir.create(file.path("~/r/", date_stamp), showWarnings = FALSE)
save(res_bayesord, file = paste0("~/r/", date_stamp, "/bayes_ord.rda"))
