R"""
#### FUNCTIONS USED FOR DATA SIMULATION ####

# 1. SOME USEFUL FUNCTIONS #
### Function to transform counts as binary data
binary = function(x) ifelse(x>0, 1, 0)

### Random Sampling with a Zero-Truncated Poisson
rtpois <- function(N, lambda) qpois(runif(N, dpois(0, lambda), 1), lambda)


#################################################################################################################

# 2. Function to simulate data #
sim_data <- function(I, J, psi, p11, p10, lam, ome) {
  
  # I = number of sites
  # J = number of occasions
  # psi = probability of site occupancy
  # p11 = site level probability of TRUE detection
  # p10 = site level probability of FALSE detection
  # lam = lambda = parameter of the POISSON process for TRUE detection counts 
  # ome = omega = parameter of the POISSON process for FALSE detection counts
  

  ## STEP 1: TRUE OCCUPANCY PROCESS : Define Occupancy State of all sites
  z = rep(0, I)
  occupied = sample(I, psi*I)
  z[occupied] <- 1
  if( mean(z) != psi)  print("### ERROR in psi / z status ###")
  
  ## Define Detection Parameters
  p = rep(0, I)
  p <- z*p11 + (1-z)*p10 # p equals p11 if z=1, p10 if z=0
  
  ## STEP 2 : LONGITUDINAL BINARY DETECTION PROCESS : Sample the Binary Data 'y'
  y = matrix(NA,I,J)
  for(j in 1:J){
    y[,j] = rbinom(I,1,p)
  } # j
  
  ## Redefine the parameter lambda conditionally on z
  lamZ = z*lam# lambda = 0 when z = 0
  w <- binary( apply(y,1,sum) )
  
  ## Sample K, N and get Q
  N = K = NA
  for(i in 1:I){
    if(w[i] == 0){ N[i]=0 ;  K[i]=0 }else{
      N[i] <- rtpois(1,lamZ[i]+ome)
      pK = dpois(0:N[i], lamZ[i])
      K[i] = (sample(N[i]+1, size=1, prob=pK)-1)
    } # end if
  } # i
  
  return( list(y=y, tp=K, N=N, z.real=z) )
  
} # END FUNCTION


#################################################################################################################

# 3. Function for the "Data Validation Process" -- works for all models #

### STEP 4 : PARTIAL VERIFICATION PROCESS

### Notes: 
# N = data matrix of ALL detection counts (TP + FP)
# tp = data matrix of true positive (TP) detection counts

# n.valid = NUMBER of detections to be validated (if prop.valid=FALSE)
# n.valid = PROPORTION of detections to be validated (if prop.valid=TRUE)

valid_data <- function(N, tp, n.valid, prop.valid=FALSE) {
  
  if(prop.valid==FALSE){
    n.valid=n.valid 
  }else{
    n.valid = round(n.valid*sum(N))
  } # end if
  
  II = length(N)
  III = length(N[N>0])
  
  if(n.valid < III){ 	### if less validatd data than sites, randomly choose sites to choose from
    sel = which(N>0)
    sites = sample(sel, size=n.valid, replace = FALSE)
    
    ## Objects
    k = rep(NA, II)
    n = rep(0,II)
    
    for(i in 1:length(sites)){
      tp.pos.i = sample.int(n=N[sites[i]], size = tp[sites[i]], replace = FALSE)
      spl.N.i = sample.int(n=N[sites[i]], size = 1, replace = FALSE)
      n[sites[i]] = 1
      k[sites[i]] = length( which(spl.N.i %in% tp.pos.i) )
      k[is.na(k)] <- 0
    } # "i"
    
  }else{	## if n.valid >= III
    
    
    n.max = trunc( n.valid / III ) 
    
    if(n.max == 0){ print("#### ERROR: NEEDS TO VALIDATE AT LEAST 1 DETECTION PER SITE") }else{
      if(n.valid > sum(N)){ print("#### ERROR: n.valid > N") }else{
        
        ## Objects
        cum = 0
        i.done = rep(0,II)
        tp.pos = list()
        s.i = list()
        k = NA
        n = rep(0,II)
        
        ### Start Validation 
        for(i in 1:II){
          
          # --2a-- If N is NULL
          if(N[i] == 0){
            k[i] = n[i] = 0
            i.done[i] = -1 	# this means it can be skipped on next round
            
            ## --2b-- If N is NOT NULL
          }else{
            
            # --3a-- If NO TP
            if(tp[i] == 0){
              k[i] = 0
              n[i] = min(N[i], n.max)
              cum = cum + n[i]
              if(n[i] < N[i]) i.done[i] = n[i] else i.done[i] = -1
              
              ## --3b-- If SOME TP (tp > 0)
            }else{
              nn = min(N[i], n.max)
              tp.pos[[i]] = sample.int(n=N[i], size = tp[i], replace = FALSE)
              
              s.ii = sample.int(n=N[i], size = nn, replace = FALSE)
              s.i[[i]] = s.ii
              n[i] = nn 
              if( nn != length( s.ii ) ) print ("#######  PB nn #######")
              k[i] = length( which(s.i[[i]] %in% tp.pos[[i]]) )
              
              cum = cum + n[i]
              if(n[i] == N[i]) i.done[i] = -1 else 
                if(n[i] < N[i]) i.done[i] = n[i] else print ("#######  PB nn #######")   
              
              #### end the 'IF' statements --2-- and --3--
            } # end if 3 -- tp is NOT NULL
          } 	# end if 2 -- N is NOT NULL
          
        } # i
        
        ### KEEP GOING if n.valid NOT REACHED...
        while(cum < n.valid){	
          
          for(i in 1:II){
            
            # --1--
            if(i.done[i] == -1){ # DO NOTHING if it's a site with no detection or a site for which we already validated all detections
            }else{
              
              # --4a-- If NO TP	# validate 1 more (n+1), and it's necess. a FP, so still k=0
              if(tp[i] == 0){		
                add = ifelse(cum < n.valid, 1, 0)
                k[i] = k[i] + 0
                n[i] = n[i] + add
                cum = cum + add
                if(n[i] < N[i]) i.done[i] = n[i] else i.done[i] = -1
                
                ## --4b-- If SOME TP (tp > 0)	# validate 1 more (n+1) -- could be a FP or TP
              }else{
                add = ifelse(cum < n.valid, 1, 0)
                n[i] = n[i] + add
                if(add > 0){ 
                  s.ii = sample(x=(1:N[i])[-s.i[[i]]], size = 1, replace = FALSE)
                  s.i[[i]] = c(s.i[[i]], s.ii)
                  k[i] = k[i] + length( which(s.ii %in% tp.pos[[i]]) )
                  
                }else{} # end if add > 0
                
                cum = cum + add
                if(n[i] == N[i]) i.done[i] = -1 else 
                  if(n[i] < N[i]) i.done[i] = n[i] else print ("#######  PB nn #######")   
                
              } ## end IF --4b-- 
            } ## end IF --1-- 
            
          } # i
        } # end "while"
      } # end if n.valid NOT > N
    } # end if n.max NOT = 0
    
  }# end if n.valid >= III
  
  return(list(n=n,k=k))
  
}  ### END FUNCTION ###


"""