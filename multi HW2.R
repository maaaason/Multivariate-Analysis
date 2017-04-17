#1-a
data1 <- read.csv("D:\\a.csv", header = F)
S <- matrix(c(7476.45, 303.62, 303.62, 26.19),2,2)
S_eiva <- eigen(S)$values
S_eive <- eigen(S)$vectors
y1_S <- S_eive[1,1] * data1[,1] + S_eive[2,1] * data1[,2]
y2_S <- S_eive[1,2] * data1[,1] + S_eive[2,2] * data1[,2]
y1_S_var <- var(y1_S)
y2_S_var <- var(y2_S)

#1-b
Q1_b <- y1_S_var/(y1_S_var+y2_S_var)

#1-d
r_y1_x1 <- S_eive[1,1] * sqrt(S_eiva[1])/sqrt(S[1,1])
r_y1_x2 <- S_eive[2,1] * sqrt(S_eiva[1])/sqrt(S[2,2])

#2-a
corr <- 303.62/(sqrt(7476.45)*sqrt(26.19))
R <- matrix(c(1,corr,corr,1),2,2)
Z1 <- (data1[,1]-mean(data1[,1]))/sd(data1[,1])
Z2 <- (data1[,2]-mean(data1[,2]))/sd(data1[,2])
R_eiva <- eigen(R)$values
R_eive <- eigen(R)$vectors
y1_R <- R_eive[1,1] * Z1+R_eive[2,1] * Z2
y2_R <- R_eive[1,2] * Z1+R_eive[2,2] * Z2
y1_R_var <- var(y1_R)
y2_R_var <- var(y2_R)

#2-b
Q2_b <- y1_R_var/(y1_R_var + y2_R_var)

#2-c
r_y1_z1 <- R_eive[1,1] * sqrt(R_eiva[1])
r_y1_z2 <- R_eive[2,1] * sqrt(R_eiva[1])

#3-a(S)
data3 <- read.csv("D:\\b.csv", header = T)
S3 <- matrix(0,7,7)
for(i in 1:7){
  for(j in 1:7){
    S3[i,j] <- cov(data3[,i],data3[,j])
  }
}
S3_eiva <- eigen(S3)$values
S3_eive <- eigen(S3)$vectors
plot(x=1:7,y=S3_eiva)
## 2 components are enough

#3-b(S)
## The first principal component appears to put much more emphasis on YrHgt and the second principal component appears to contrast BkFat with SaleHt.

#3-d(S)
Y3_S <- matrix(0,76,2)
for(i in 1:7){
  Y3_S[,1] <- S3_eive[1,i] * data3[,1]
  Y3_S[,2] <- S3_eive[2,i] * data3[,2]
}
plot(y=Y3_S[,1],x=Y3_S[,2],xlab=expression(hat(y2)),ylab=expression(hat(y1)))
plot(y=Y3_S[1:32,1],x=Y3_S[1:32,2],xlab=expression(hat(y2)),ylab=expression(hat(y1)))
plot(y=Y3_S[33:49,1],x=Y3_S[33:49,2],xlab=expression(hat(y2)),ylab=expression(hat(y1)))
plot(y=Y3_S[50:76,1],x=Y3_S[50:76,2],xlab=expression(hat(y2)),ylab=expression(hat(y1)))
## Can not distinguish these three breeds.
## There are 3 outliers lie in lower right of the plot.

#3-e(S)
qqnorm(Y3_S[,1])
## The samples of the first principal component are almost following normal distribution.

#3-a(R)
data3_std <- matrix(0,76,7)
for(i in 1:7){
  data3_std[,i]=(data3[,i]-mean(data3[,i]))/sd(data3[,i])
}
R3 <- matrix(0,7,7)
for(j in 1:7){
  for(k in 1:7){
    R3[j,k] <- cor(data3_std[,j],data3_std[,k])
  }
}
R3_eiva <- eigen(R3)$values
R3_eive <- eigen(R3)$vectors
plot(x=1:7,y=R3_eiva)
## 3 components are enough

#3-b(R)
## The first principal component appears to contrast FtFrBody and PrctFFB with BkFat and the second principal component appears to be essentially a weighted average of YrHgt, PrctFFB, BkFat and SaleWt.

#3-d(R)
Y3_R <- matrix(0,76,2)
for(i in 1:7){
  Y3_R[,1] <- R3_eive[1,i] * data3[,1]
  Y3_R[,2] <- R3_eive[2,i] * data3[,2]
}
plot(y=Y3_R[,1],x=Y3_R[,2],xlab=expression(hat(y2)),ylab=expression(hat(y1)))
plot(y=Y3_R[1:32,1],x=Y3_R[1:32,2],xlab=expression(hat(y2)),ylab=expression(hat(y1)))
plot(y=Y3_R[33:49,1],x=Y3_R[33:49,2],xlab=expression(hat(y2)),ylab=expression(hat(y1)))
plot(y=Y3_R[50:76,1],x=Y3_R[50:76,2],xlab=expression(hat(y2)),ylab=expression(hat(y1)))
## Can not distinguish these three breeds.
## There are 3 outliers lie in upper left of the plot.

#3-e(R)
qqnorm(Y3_R[,1])
## The samples of the first principal component are not following normal distribution.

#4-a
data4 <- read.csv("D:\\c.csv", header = F)
data4_std <- matrix(0,19,12)
for(i in 1:12){
  data4_std[,i]=(data4[,i]-mean(data4[,i]))/sd(data4[,i])
}
R4 <- matrix(0,12,12)
for(j in 1:12){
  for(k in 1:12){
    R4[j,k] <- cor(data4_std[,j],data4_std[,k])
  }
}
R4_eiva <- eigen(R4)$values
plot(x=1:12,y=R4_eiva)
## 3 components are enough to effectively summarize the sample variability.

#4-b
R4_eive <- eigen(R4)$vectors
## The first principal component put very much emphasis on the X3 statement.
## The second principal component appears to reveal the negative effect of the X1, X6, X8, X11 and X12 statements.
## The third principal component appears to contrast the X2 and X12 with the X4 statements.
            