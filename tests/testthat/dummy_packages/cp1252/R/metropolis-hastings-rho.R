# Copied from the spatialprobit package https://raw.githubusercontent.com/cran/spatialprobit/master/R/metropolis-hastings-rho.R
# This file is encoded in Cp-1252

# @param beta parameter vektor (k x 1)
# @param z imputed observed variables before truncation (n x 1)
# @param W spatial weight matrix (n x n)
# @param X design matrix (n x k)
density_rho <- function(rho, beta, z, I_n, W, X, type=c("SAR","SEM")) {
  A <- (I_n - rho * W)          # n x n
  if (type == "SAR") {
    S <- A %*% z - X %*% beta     # n x 1; Residuals of SAR
  } else {
    S <- as.numeric(A %*% (z - X %*% beta))   # n x 1; Residuals of SEM
  }
  as.numeric(det(A) * exp(-0.5 * t(S) %*% S))
  #as.vector(exp(log(det(A)) - 0.5 * t(S) %*% S))
}

# Metropolis-Hastings-Chain with tuned acceptance rate, see LeSage (2009)
#
# proposal density is normal distribution
# g(rho* | rho_t) = rho_t + c * N(0, 1)
#
# @param type "SEM" or "SAR",
# @param n number of samples to draw
# @param start.value start value for rho
# @param c tuning parameter
draw_rho_metropolis <- function(type="SEM", n, z, I_n, W, X, burn.in=100, start.value=0.2, c=1) {
n <- n + burn.in
u <- runif(n=n, 0, 1)  # samples for M-H-ratio
s <- rnorm(n=n, 0, 1)  # realisations from proposal density
rho_t <- numeric(n)    # vector for rho within the chain
rho_t[1] <- start.value              # start value
p_t      <- density_rho(rho_t[1], beta=beta, z=z, I_n=I_n, W=W, X=X, type=type)  # f(rho_t | beta, z)
i    <- 2 # number of accepted proposals / length of chain
acceptance_rate <- numeric(n) # running acceptance rate
num_accepted <- 0
while (i <= n) {

  # create proposal rho_p from proposal density g(rho_p | rho_{t-1}) ~ N(rho_{t-1}, c^2)
  rho_p <- rho_t[i-1] + c * s[i]   # proposal rho = letztes akzeptiertes Rho + normalverteilte Zufallsstreuung s

  # Berechnung der Dichte f(rho_p) für das Proposal rho_p
  p_p <- density_rho(rho_p, beta=beta, z=z, I_n=I_n, W=W, X=X, type=type)
  # SW: Berechnung der Dichte ist teuer, daher besser für ein Grid vorher rechnen?
  # Jain, Dichte p(rho|t,beta( hängt von z, beta ab. Aber zumindestens die Log-Determinante kann man vorher berechnen
  # Berechnung Dichte f(rho_{t-1}) für letzten Wert der Chain
  # p_t <- p(rho_t[i-1], beta=beta, z=z, W=W, X=X, type=type)  # SW: Kann ich die alte Dichte nicht merken, dann muss ich die nicht neu berechnen

  # Wegen Symmetrie der Normalverteilung als Proposal-Dichte g(rho_p | rho_t) = g(rho_t | rho_p)
  # vereinfacht sich das M-H-Ratio von [ f(rho_p) * g(rho_t | rho_p) ] / [ f(rho_t) * g(rho_p | rho_t) ]
  # auf f(rho_p) / f(rho_t)!
  # determine M-H-Ratio R(rho_p, rho_t) = min[ 1 ; p(rho_p) / p(rho_t) ]
  # see LeSage (2009), eqn (5.27)
  R <- min(1, p_p / p_t)
  if (u[i] <= R) {    # see Givens/Hoeting, p.184, eqn (7.2)
    # accept proposal
    rho_t[i] <- rho_p
    p_t <- p_p  # save density
    num_accepted <- num_accepted + 1
  } else {
    # stay with the current value
    rho_t[i] <- rho_t[i-1]
  }
  acceptance_rate[i] <- num_accepted / i
  if (acceptance_rate[i] < 0.4) c <- c / 1.1   # Wenn Akzeptanzrate zu klein, dann verringere (?) Streuungsparameter "c"
  if (acceptance_rate[i] > 0.6) c <- c * 1.1
  i <- i + 1
}
return(list(rho_t=rho_t,acceptance_rate=acceptance_rate))
}
