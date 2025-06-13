#May 28, 2025
#R code for simulations for Figure 3
#written by Daniel Shriner

png("Figure 3.png",height=6,width=6,units="in",res=600)
res <- vector("numeric",99)
q <- 1:99/100
n <- 100000
for (i in 1:99) {
	M <- rbinom(n,1,q[i])
	N <- 2*M
	F <- rbinom(n,2,q[i])
	X <- c(M,F)
	Y <- c(N,F)
	res[i] <- cor(X,Y)
}
plot(q,res^2,ylim=c(0,1),ylab=expression(italic(r)^2),type="l")

res <- vector("numeric",99)
q <- 1:99/100
n <- 100000
for (i in 1:99) {
	M <- rbinom(10*n,1,q[i])
	N <- 2*M
	F <- rbinom(n,2,q[i])
	X <- c(M,F)
	Y <- c(N,F)
	res[i] <- cor(X,Y)
}
lines(q,res^2,col="red")

res <- vector("numeric",99)
q <- 1:99/100
n <- 100000
for (i in 1:99) {
	M <- rbinom(n,1,q[i])
	N <- 2*M
	F <- rbinom(10*n,2,q[i])
	X <- c(M,F)
	Y <- c(N,F)
	res[i] <- cor(X,Y)
}
lines(q,res^2,col="blue")

legend("bottomleft",legend=c("Males=100,000 Females=100,000","Males=1,000,000 Females=100,000","Males=100,000 Females=1,000,000"),text.col=c("black","red","blue"))

dev.off()
