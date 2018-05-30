library('MASS')

x = as.matrix(iris[,c(1,3)]) #150 x 2

mu = mean(x) # Média estimada
cov = cov(x) # Matriz de covariância estimada

t = 0

S = dim(x)[1] # Numero de observações
D = dim(x)[2] # Numero of atributos
Z = sqrt(((2*pi)^D) * det(cov)) # Constante de normalização da distribuição normal Euclidiana


# Equação 7
sigma = 0.5
w = matrix(0, S, S)
for(i in 1:S){
    for(j in 1:S){
        w[i, j] = kernel(x[i,], x[j,], sigma)
    }
}

p = 0.01
M = list()
for(i in 1:S){
    tempM = matrix(0, D, D)        
    for(d in 1:D){
        for(j in 1:S){        
            tempM[d, d] = tempM[d, d] + (w[i, j] * ((x[i,d] - x[j,d])^2) + p)
        }
    }
    M[[i]] = solve(tempM)
}

# Equação 10
v = mvrnorm(S, rep(0, D), cov) # Normal com média 0 e matriz de covariância estimada

summ = 0
for(s in 1:S){
    summ = summ + m(mu, v[s], M)
}
C = Z / S * summ

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