
#partial directed coherence
#sameshima baccala 1999, 2001
#biological cybernetics

################################################################################
## Com rank

file_name <- "/Volumes/GoogleDrive/My Drive/Projeto-rank/aplicacao/teste1_fase3_.csv"
data <- read.csv(file_name)
data <- as.matrix(data)

## Rank transform
data[,1] <- rank(data[,1])
data[,2] <- rank(data[,2])
#data[,3] <- rank(data[,3])
#data[,4] <- rank(data[,4])

#pdf(file="/Volumes/GoogleDrive/My Drive/Projeto-rank/figuras/timeseries-rank.pdf", width=4, height=4)
#plot(data[,1], xlab="", ylab="", type="l", lwd=2)
#dev.off()

## Seleciona a ordem do VAR/PDC usando o pico da coorrelação cruzada
## ccf(data[,1], data[,2])
tmp <- ccf(data[,1], data[,2], plot=FALSE)
p <- abs(tmp$lag[which(abs(tmp$acf) == max(abs(tmp$acf)))])

## Analise de Granger usando VAR
#VAR(data[,c(1,2,3)], p=p)$p.value[1,2]  ## se < 0.05, Granger do player 1 para o 2
#VAR(data[,c(1,2,4)], p=p)$p.value[2,1]  ## se < 0.05, Granger do player 2 para o 1


#boot.var(data[,c(1,2,3)], p=p, B=1000)$p.value[1,2]
#boot.var(data[,c(1,2,4)], p=p, B=1000)$p.value[2,1]

## Analise de Granger usando PDC
tmp <- PDC(data[,c(1,2)], p=p, srate= 1, maxBoot=1000, plot=TRUE)
tmp$p.value[1,2] ## se < 0.05, Granger do player 1 para o 2
tmp$pdc[1,2]

tmp$p.value[2,1] ## se < 0.05, Granger do player 2 para o 1
tmp$pdc[2,1]

## Analise de Granger usando PDC
#tmp <- PDC(data[,c(1,2,3)], p=p, srate= 1, maxBoot=1000, plot=TRUE) ## se < 0.05, Granger do player 1 para o 2
#tmp$p.value[1,2]
#tmp$pdc[1,2]

#tmp <- PDC(data[,c(1,2,4)], p=p, srate=1, maxBoot=1000, plot=TRUE) ## se < 0.05, Granger do player 2 para o 1
#tmp$p.value[2,1]
#tmp$pdc[2,1]


## Analise usando correlcao
delta <- 0
data <- read.csv(file_name)
data <- as.matrix(data[(delta:(nrow(data)-delta)),])

## Covariando pela posicao da raquete do joogo
#data[,1] <- lm(data[,1]~data[,4])$res
#data[,2] <- lm(data[,2]~data[,3])$res

data[,1] <- rank(data[,1])
data[,2] <- rank(data[,2])
#data[,3] <- rank(data[,3])
#data[,4] <- rank(data[,4])

correlationts(data[,1], data[,2], nboot=1000)
#correlationts(data[,3], data[,4], nboot=1000)


################################################################################
## Funções

## Calcula a correlação entre duas séries temporais e o p-valor via block bootstrap
correlationts <- function(x, y, block=10, nboot=500) {
	
	orig <- abs(cor(x,y))
	
	distr <- array(0, nboot)
	for(boot in 1:nboot) {
		nblock <- length(x)/block
		tmpx <- sample(seq(1:(length(x)-block)), size=nblock, replace=TRUE)
		tmpy <- sample(seq(1:(length(x)-block)), size=nblock, replace=TRUE)
		x.b <- 0
		y.b <- 0
		for(i in 1:nblock) {
			x.b <- c(x.b, x[tmpx[i]:(tmpx[i]+block-1)])
			y.b <- c(y.b, y[tmpy[i]:(tmpy[i]+block-1)])
		}
		distr[boot] <- abs(cor(x.b, y.b))
	}
	p <- length(which(distr > orig))/nboot
	return (p)
}


VAR <- function(x, p=1) {

    T <- dim(x)[1]
    K <- dim(x)[2]


    for (k in 1:K) {
        x[,k] <- (x[,k] - mean(x[,k])) # / sd(x[,k])
    }

    Y <- x[(1+p):T, ]

    pp <- 1
    Z <- as.matrix(x[(p-pp+1):(T-p+p-pp), ])
    if (p > 1) {
        for (pp in 2:p) {
            Z <- cbind(Z, x[(p-pp+1):(T-p+p-pp), ])
        }
    }

    B <- qr.solve(t(Z)%*%Z)%*%t(Z)%*%Y

    u <- Y - Z %*% B

    pvalue <- matrix(0, K, K)
    Sigma <- (t(u) %*% u) / (nrow(Z) - ncol(Z))    ####

    for (target in 1:K) {
        for (source in 1:K) {
            C <- matrix(0, p, nrow(B))
            jj <- source
            for (pp in 1:p) {
                C[pp,jj] <- 1
                jj <- jj + K
            }
            BB <- B[,target]
            wald <- (t(C %*% BB) %*% qr.solve(C %*% qr.solve(t(Z) %*% Z) %*% t(C)) %*% (C %*% BB)) / Sigma[target, target]

           pvalue[source, target] <- 1-pchisq(wald, df=nrow(C))
        }
    }

    res <- list()

    res$ar <- array(0, c(p, K, K))
    j <- 1
    for (r in 1:p) {
        res$ar[r,,] <- t(B[j:(j+K-1), 1:K])
        j <- j + K
    }

    res$p.value <- pvalue
    res$coef <- B
    res$order <- p
    res$var.pred <- Sigma
    return(res)
}


#'    This function computes the partial directed coherence for graphs.
#'
#'    @param x a four-dimensional (T x K x N x N) matrix. T: time series length. K: number of features (graphs). N: size of the graph (number of nodes)
#'    @param p order of the PDC (defaults is one)
#'    @param srate time serie sampling rate
#'    @param maxBoot number of bootstrap samples
#'    @param plot logical indicating if plot or not the PDC
#'    @return pvalue matrix containing the pvalues for PDC from graph i (i-th row) to graph j (j-th column)
PDC <- function(x, p=1, srate= 1, maxBoot=300, plot=FALSE) {
    T <- dim(x)[1]
    K <- dim(x)[2]

#    for (k in 1:K) {
#        x[,k] <- x[,k] - mean(x[,k])
#    }

    Y <- x[(1+p):T, ]

    pp <- 1
    Z <- x[(p-pp+1):(T-p+p-pp), ]
    if (p > 1) {
        for (pp in 2:p) {
            Z <- cbind(Z, x[(p-pp+1):(T-p+p-pp), ])
        }
    }

    B <- qr.solve(t(Z)%*%Z)%*%t(Z)%*%Y

    u <- Y - Z %*% B

    Sigma <- (t(u) %*% u) / (nrow(Z) - ncol(Z))

    res <- list()
    res$ar <- array(0, c(p, K, K))
    j <- 1
    for (r in 1:p) {
        res$ar[r,,] <- (B[j:(j+K-1), 1:K])
        j <- j + K
    }

    res$order <- p
    res$var.pred <- Sigma

    pdc.orig <- GPDC(x, res, plot=plot)

	sum.pdc <- matrix(0, K, K)
	for (i in 1:K) {
		for (j in 1:K) {
			sum.pdc[i,j] <- sum(pdc.orig[i,j,])
		}
	}

    pvalue <- matrix(0, K, K)

    for (source in 1:K) {
        for (target in 1:K) {
            if (source != target) {
                tmp <- zero <- source
                if (p > 1) {
                    for (r in 2:p) {
                        tmp <- tmp + K 
                        zero <- c(zero, tmp)
                    }
                }
                B.0 <- B
                B.0[zero,target] <- 0

                pdc.boot <- array(0, maxBoot)
                Y.boot <- Z %*% B.0
                for (boot in 1:maxBoot) {
                    Y.boot2 <- Y.boot +  u[sample(seq(1:nrow(u)), nrow(u), replace=TRUE), ]
                    B.boot <- qr.solve(t(Z)%*%Z)%*%t(Z)%*%Y.boot2

                    u.boot <- Y.boot2 - Z %*%  B.boot

                    Sigma.boot <- (t(u.boot) %*% u.boot) / (nrow(Z) - ncol(Z))

                    res.boot <- list()
                    res.boot$ar <- array(0, c(p, K, K))
                    j <- 1
                    for (r in 1:p) {
                        res.boot$ar[r,,] <- t(B.boot[j:(j+K-1), 1:K])
                        j <- j + K
                    }

                    res.boot$order <- p
                    res.boot$var.pred <- Sigma.boot

                    tmp <- GPDC(x, res.boot, srate = srate, plot=FALSE)
                    pdc.boot[boot] <- sum(tmp[target, source,])

                }

#                pvalue[target, source] <- length(which(pdc.boot > sum(pdc.orig[target,source,]))) / maxBoot
                pvalue[target, source] <- length(which(pdc.boot > sum.pdc[target,source])) / maxBoot

            }
        }
    }
    res <- list()
    res$p.value <- pvalue
    res$pdc <- sum.pdc
    return(res)
}



## Generalized Partial Directed Coherence
## x: NxK matrix of times series in the columns.
## model: object similar to output of ar function
GPDC <- function(x, model, srate= 1, plot=FALSE) {

    K <- ncol(x)
    T <- nrow(x)


#    maxFreq <- as.integer(floor(T/2)+1) 
    maxFreq = 128
    Apdc <- array(0,c(K,K,maxFreq))    
    for(lambda in 1:maxFreq){
        Apdc[,,lambda] <- GPDC1(model, lambda/(2*T), K)
    }

#    xaxis = seq(0, srate/2, length.out=as.integer(floor(T/2+1)))
    xaxis = xaxis[1:128]
    if(plot == TRUE) {
        par(mfrow=c(K,K))
        for(i in 1:K){
            for(j in 1:K){
#                if (j == 2 && i==1) {
#                	pdf(file="/Volumes/GoogleDrive/My Drive/Projeto-rank/figuras/xxx12-norank.pdf", width=4, height=4)
#                        plot(x=xaxis, y=Apdc[j,i,], xlim=c(0, 0.3), ylim=c(0,1), xlab="", ylab="", type="l", lwd=5)
#                    dev.off()
#                }
#                plot(x=xaxis, y=Apdc[j,i,], xlim=c(0, srate/2), ylim=c(0,1), xlab="", ylab="", type="l")
                ## Plote para o artigo do rank
                plot(x=xaxis, y=Apdc[j,i,], xlim=c(0, 0.3), ylim=c(0,1), xlab="", ylab="", type="l")
            }
        }
    }

    return(Apdc)
}



## Calculate PDC in ONE frequency lambda
GPDC1 <- function(model, lambda, K){

    p <- model$order
    Sigma <- model$var.pred #Residue Covariance Matrix

    ## Fourier Decomposition
    A <- matrix(0,K,K)
    for(i in 1:K){
        for(j in 1:K){
            for(r in 1:p){
                A[i,j] <- A[i,j]+model$ar[r,i,j]*exp(-1i*2*pi*r*lambda)
            }
        }
    }
    A <- diag(K)-A

    #Calculates GPDC
    Apdc <- matrix(0,K,K)
    for(i in 1:K){
        for(j in 1:K){
            Apdc[i,j] <- (1/sqrt(Sigma[i,i]))*abs(A[i,j])
            Z <- 0
            for(i2 in 1:K){
                Z <- Z+abs(A[i2,j])^2/Sigma[i2,i2]
            }
            Apdc[i,j] <- Apdc[i,j]/sqrt(Z)     
        }
    }
    return(Apdc^2)
}

