##### Part1: Life table
### Life table
LifeT <- scan() #read the following data
0 1.0000000 0
1 0.0000620 4600
2 0.0000340 8700
3 0.0000200 11600
4 0.0000155 12700
5 0.0000110 12700
6 0.0000065 12700
7 0.0000020 12700
8 0.0000020 12700
9 0 0

LifeT = data.frame(t(matrix(LifeT, nrow=3)))

LifeT[, 4] = LifeT[, 2]*LifeT[, 3]
LifeT[, 5] = LifeT[, 1]*LifeT[, 4]
colnames(LifeT) = c("x(Age)", "lx", "mx", "lx*mx", "x*lx*mx")

R0 <- sum(LifeT[, 4])
G <- sum(LifeT[, 5])/R0
app_r <- log(R0)/G

LifeT[, 6] = LifeT[, 4]*exp(-app_r*LifeT[, 1])
LifeT[, 7] <- 0

for (i in 3:length(LifeT[, 6])-1){
    LifeT[i, 7] = sum(LifeT[i:length(LifeT[, 6]), 6])/(LifeT[i, 2]*
                                                           exp(-app_r*LifeT[i, 1]))
}
LifeT[1, 7] = 1

colnames(LifeT) = c("x(Age)", "lx", "mx", "lx*mx", "x*lx*mx", "lx*mx*e^(-rt)", "Vx")

### Life expectancy
Age_range <- paste(seq(0, 8, 1), seq(1:9), sep="-")
LifeE <- data.frame(Age_range)
LifeE[, 2] <- 0

for (i in 1:length(LifeE[, 1])){
    LifeE[i, 2] = (LifeT[i, 2] + LifeT[(i+1), 2])/2
}

LifeE[, 3] <- 0

for (i in 1:length(LifeE[, 2])){
    LifeE[i, 3] = sum(LifeE[i:length(LifeE[, 2]), 2])/LifeT[i, 2]
}
colnames(LifeE) <- c("Age_range", "Lx", "e_x")

### true r
f <- function(r) {sum(LifeT[, 4]/exp(r*LifeT[, 1])) - 1}
sol <- uniroot(f=f, interval=c(0, 1), tol=1e-9)
sol ## root is root

LifeT[, 8] = LifeT[, 4]*exp(-(sol$root)*LifeT[, 1])
LifeT[, 9] <- 0

for (i in 3:length(LifeT[, 6])-1){
    LifeT[i, 9] = sum(LifeT[i:length(LifeT[, 6]), 6])/(LifeT[i, 2]*
                                                           exp(-(sol$root)*LifeT[i, 1]))
}
LifeT[1, 9] = 1
colnames(LifeT) = c("x(Age)", "lx", "mx", "lx*mx", "x*lx*mx", 
                    "lx*mx*e^(-rt)", "Vx", "T_lx*mx*e^(-rt)", "T_Vx")
head(LifeT)
