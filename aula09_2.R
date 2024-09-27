#Integracao de Monte Carlo

f <- function(x) exp(-x^2)
curve(f,-10,10)

w <- runif(1000000,-10,10)

#integral: e^(-x^2) dx = (b-a)integral:e^(-x^2)*1/(b-a)dx = (b-a)*E(e^(-x^2))

mean(f(w))*(10-(-10))
integrate(f,-10,10)



