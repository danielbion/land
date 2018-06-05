library('MASS')

x = as.matrix(iris[,c(1,3)]) #150 x 2

mu = mean(x) # Média estimada
cov = cov(x) # Matriz de covariância estimada

t = 0

S = dim(x)[1] # Numero de observações
N = S
D = dim(x)[2] # Numero of atributos
Z = sqrt(((2*pi)^D) * det(cov)) # Constante de normalização da distribuição normal Euclidiana


# Equação 7: Estimar M para estimar C
sigma = 0.5
w = matrix(0, S, S)
for(i in 1:S){
    for(j in 1:S){
        w[i, j] = kernel(x[i,], x[j,], sigma)
    }
}

p = 0.01
M = list()
for(s in 1:S){
    tempM = matrix(0, D, D)        
    for(d in 1:D){
        for(n in 1:N){        
            tempM[d, d] = tempM[d, d] + (w[s, n] * ((x[s,d] - x[n,d])^2) + p)
        }
    }
    M[[i]] = solve(tempM)
}

# Equação 10: Estimar C
v = mvrnorm(S, rep(0, D), cov) # Normal com média 0 e matriz de covariância estimada

summ = 0
for(s in 1:S){
    summ = summ + m(mu, v[s], M)
}
C = Z / S * summ

# Equação 12: Estimar d para ajustar a média
summ = 0
for(n in 1:N){    
    for(s in 1:S){
        summ = summ + Log(mu, x[n]) - ((Z / (C * S)) * m(mu, v[s], M) * v[s])
    }
}
delta = solve(-cov) %*% (summ/N)
d = -cov %*% delta

# Quem é alfa?
mu = Exp(alfa_mu * d)

m = function(mu, v, M){
    det(M %*% Exp(mu, v))^0.5
}

kernel = function(x1, x2, sigma){
    summ = 0
    for(d in 1:length(x1)){
        summ = summ + (x1[d] - x2[d])^2
    }
    return ( exp(-summ / ((2 * sigma)^2) ))
}

Exp = function(mu, v){
    # ???
}

Log = function(mu, v){
    # ???
}