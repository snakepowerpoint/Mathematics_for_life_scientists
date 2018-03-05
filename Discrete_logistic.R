##### plot hat curve: Nt+1 = Nt + rNt(1-Nt/K)
hat <- data.frame(seq(from = 0, to = 1000, by = 0.1))

hat[, 2] = sapply(hat[, 1], FUN = function(x){x + 1.8*x*(1 - x/500)})
hat1 = hat[1:sum(hat[, 2]>0), ]  #drop negative data

plot(hat1[, 1], hat1[, 2], xlim=c(0, 1000), ylim=c(0, 1000), type="l", lwd=2,
     col="green", xlab="N_t", ylab="N_t+1", xaxs="i", yaxs="i", 
     main="time 1 return map with growth rate 1.8")
abline(a=0, b=1, col="red", lwd=2)

#####  plot trajactory of Nt
out <- data.frame(row.names=c("N_t", "N_t+1", "t"))
out[, 1] = c(100, 0, 0)

for (i in 1:1000){
    out[2, i] = out[1, i] + 1.8*out[1, i]*(1 - out[1, i]/500)
    out[, (i+1)] = c(out[2, i], 0, i)
}
out = t(out[, 1:1000])
out1 = round(out, digit=1)

#plot
for (i in 1:dim(out1)[1]){
    segments(out1[i, 1], out1[i, 1], out1[i, 1], out1[i, 2], col="blue", lwd=2)
}

for (i in 1:dim(out1)[1]){
    segments(out1[i, 1], out1[i, 2], out1[i, 2], out1[i, 2], col="blue", lwd=2)
}

##### plot time series of Nt
plot(out[1:50, 3], out[1:50, 1], main="discrete logistic population with growth 
     rate 1.8", xlab="time", ylab="N", col="blue", lwd=2, xaxs="i", yaxs="i",
     type="l", ylim=c(min(out[1:50, 1]), max(out[1:50, 1])+100))

##### results for cases in r=2.3, 2.45, 2.56, 2.8
#wrtie a function doing the same thing above
allout <- function(K=500, Nini=100, r=1.8, TimeS="F", Dynamic="F", report="F"){
    out <- data.frame(row.names=c("N_t", "N_t+1", "t"))
    out[, 1] = c(Nini, 0, 0)
    
    for (i in 1:1000){
        out[2, i] = out[1, i] + r*out[1, i]*(1 - out[1, i]/K)
        out[, (i+1)] = c(out[2, i], 0, i)
    }
    out = t(out[, 1:1000])
    
    if (report=="T"){
        return(out)
    }
   
    if (TimeS=="T"){
        plot(out[1:50, 3], out[1:50, 1], main=paste("discrete logistic population with 
             growth rate", r, sep=" "), xlab="time", ylab="N", col="blue", lwd=1, 
             xaxs="i", yaxs="i", type="l", 
             ylim=c(min(out[1:50, 1]), max(out[1:50, 1])+100))
    }
    
    if (Dynamic=="T"){
        hat <- data.frame(seq(from = 0, to = 1000, by = 0.1))
        hat[, 2] = sapply(hat[, 1], FUN = function(x){x + r*x*(1 - x/K)})
        hat1 = hat[1:sum(hat[, 2]>0), ]
        
        out1 = round(out, digit=1)
        
        plot(hat1[, 1], hat1[, 2], xlim=c(0, 1000), ylim=c(0, 1000), type="l", lwd=1,
             col="green", xlab="N_t", ylab="N_t+1", xaxs="i", yaxs="i", 
             main=paste("time 1 return map with growth rate", r, sep=" "))
        abline(a=0, b=1, col="red", lwd=1)
        
        for (i in 1:dim(out1)[1]){
            segments(out1[i, 1], out1[i, 1], out1[i, 1], out1[i, 2], col="blue", lwd=1)
        }
        
        for (i in 1:dim(out1)[1]){
            segments(out1[i, 1], out1[i, 2], out1[i, 2], out1[i, 2], col="blue", lwd=1)
        }        
    }    
}

allout(r=1.8, TimeS="T", Dynamic="T")
allout(r=2.3, TimeS="T", Dynamic="T")
allout(r=2.45, TimeS="T", Dynamic="T")
allout(r=2.56, TimeS="T", Dynamic="T")
allout(r=2.8, TimeS="T", Dynamic="T")

allout(Nini= 101, r=1.8, TimeS="T")
allout(Nini= 101, r=2.3, TimeS="T")
allout(Nini= 101, r=2.45, TimeS="T")
allout(Nini= 101, r=2.56, TimeS="T")
allout(Nini= 101, r=2.8, TimeS="T")

##### plot bifurcation diagram
r <- seq(from=1.9, to=3, by=0.01)
bi <- data.frame(rep(0, 1000))

for (i in 1:length(r)){
    bi[, i] = allout(r = r[i], report="T")[, 1]
}

plot(r, bi[1, ], xaxs="i", yaxs="i", xlim=c(1.8, 3), ylim=c(0, max(bi)+50), 
     type="n", xlab="Intrinsic growth rate r", ylab="Population size")
for (i in 500:1000){
    points(r, bi[i, ], pch=20, col="blue", cex=0.4)
}



