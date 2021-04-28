using Distributions, Random, Turing, Plots, StatsBase
using RCall
include("r_sim.jl") # sim functions from Chambert

# Use sim functions from Chambert et al 2017
R"""
# returns:
# y, tp, N, z_real
data <- sim_data(20,  # I - sites/images
                 5,   # J - surveys
                 0.8, # ψ - probability object is in image
                 0.8, # p11 - true positive
                 0.1, # p10 - false positive
                 2,   # λ - poisson intensity parm for true detections
                 3)   # ω - poisson intensity parm for false detections

# returns:
# n, k
valid <- valid_data(data[["N"]],
                    data[["tp"]],
                    n.valid = 0.2,
                    prop.valid = TRUE)
"""


df = @rget data
valid = @rget data

valid[:y]
valid[:N]
@model model(y) = begin
# Priors
    ψ   ~ Uniform(0,1)	# psi = Pr(Occupancy)
    p₁₀ ~ Uniform(0,1)	# p10 = Pr(y = 1 | z = 0)
    p₁₁ ~ Uniform(0,1)	# p11 = Pr(y = 1 | z = 1)

    λ ~ Uniform(0,1000)	# Poisson process parameter for TRUE Detections
    ω ~ Uniform(0,1000)	# Poisson process parameter for FALSE Detections

    I = size(y,1)
    J = size(y,2)
    p = zeros(I)
    v = zeros(I)
    w = zeros(I)
    
# Likelihood
    for i in 1:I
        p[i] = z[i].*p₁₁ .+ (1-z[i]).*p₁₀ 	# p: 'detection' parameter for the next Bernoulli Process: p equals p11 if z=1, p10 if z=0
        z[i] ~ Bernoulli(ψ)		# Latent Variable z: Occupancy status of image 'i' -- Bernoulli process

        for j in 1:J 
            y[i,j] ~ Bernoulli.(p[i]) # Observed Variable y -- Bernoulli process
        end

        v[i] = sum(y[i,:]) 	# Total number of occasions with at least one detection at that site -- takes value between 0 and T {0,..,T}
        if v[i] > 0
            w[i] = 1
        else
            w[i] = 0
        end 		# Binary transformation of v[i] =>  = 1 if there was >=1 detection, 0 if no detection -- Takes values {0/1} 
        
        
        ### probsN is the probability array used for the total number of detections N[i]
        ## Define the probability that N = 0 (No detection at all)
        probsN[i,1] = 1 - w[i]		# Pr(N=0) is directly determined by w[i]
        ## The next 5 lines define the probability cells corresponding to the ZERO-TRUNCATED Poisson Process used for N[i], when w=1 -- i.e., for Pr(N>0)
        for r in 2:R 
            probsN[i,r] = w[i]*(exp(-(λ*z[i]+λ))*(pow((λ*z[i]+λ),x[r])) / ( exp(logfact(x[r]))*(1-exp(-(λ*z[i]+λ)))))# r
            probsN[i,R+1] <- 1 - sum(probsN[i,1:R])	# Pr(N>R) // R is the highest value taken by N in the data + 1
        end
  
    end
end








@model BayesHmm(y, K) = begin
    # Get observation length.
    N = length(y)

    # State sequence.
    s = tzeros(Int, N)

    # Emission matrix.
    m = Vector(undef, K)

    # Transition matrix.
    T = Vector{Vector}(undef, K)

    # Assign distributions to each element
    # of the transition matrix and the
    # emission matrix.
    for i = 1:K
        T[i] ~ Dirichlet(ones(K)/K)
        m[i] ~ Normal(i, 0.5)
    end

    # Observe each point of the input.
    s[1] ~ Categorical(K)
    y[1] ~ Normal(m[s[1]], 0.1)

    for i = 2:N
        s[i] ~ Categorical(vec(T[s[i-1]]))
        y[i] ~ Normal(m[s[i]], 0.1)
    end
end;