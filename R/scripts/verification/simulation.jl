using Distributions, Random, Turing, Plots, StatsBase





I, J, ψ, p11, p10, λ, ω = 20, 5, 0.8, 0.8, 0.3, 2,3
#function sim_data(I, J, ψ, p11, p10, λ, ω)
    z = zeros(I)
    occupied = sample(1:I, floor(Int64, ψ * I), replace = false)
    z[occupied] .= 1
   
   ## STEP 1: TRUE OCCUPANCY PROCESS : Define Occupancy State of all sites
    mean(z) == ψ  
    sample(1:20, 16, replace = false)
    
    ## Define Detection Parameters
    p = zeros(0, I)
    p = z .* p11 .+ (1 .- z) .* p10 # p equals p11 if z=1, p10 if z=0    

    ## STEP 2 : LONGITUDINAL BINARY DETECTION PROCESS : Sample the Binary Data 'y'
    # y = matrix(NA,I,J)
    y = zeros(I,J)
    for i in 1:I
        for j in 1:J
            y[i,j] = rand(Binomial(1,p[i]),1)[1]
        end
    end


    ## Redefine the parameter lambda conditionally on z
    λZ = z .* λ# lambda = 0 when z = 0
    
    w = zeros(I)
    for i in 1:I
        if sum(y[3,:]) > 0
            w[i] = 1
        end
    end
    w

    ## Sample K, N and get Q
    N = K = NA
    for i in 1:I
        if w[i] == 0 
            N[i]=0 
            K[i]=0 
        else{
            N[i] <- rtpois(1,lamZ[i]+ome)
            pK = dpois(0:N[i], lamZ[i])
            K[i] = (sample(N[i]+1, size=1, prob=pK)-1)
        end
    end
y, K, N, z

end