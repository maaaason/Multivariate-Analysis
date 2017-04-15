#1-a
data <- read.csv("D:\\a.csv", header = F)
S <- matrix(c(7476.45, 303.62, 303.62, 26.19),2,2)
S_eiva <- eigen(S)$values
S_eive <- eigen(S)$vectors
y1_S <- S_eive[1,1] * data[,1] + S_eive[2,1] * data[,2]
y2_S <- S_eive[1,2] * data[,1] + S_eive[2,2] * data[,2]
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
Z1 <- (data[,1]-mean(data[,1]))/sd(data[,1])
Z2 <- (data[,2]-mean(data[,2]))/sd(data[,2])
data_std <- matrix(c(Z1,Z2),10,2)
R_eiva <- eigen(R)$values
R_eive <- eigen(R)$vectors
y1_R <- R_eive[1,1] * Z_1+R_eive[2,1] * Z_2
y2_R <- R_eive[1,2] * Z_1+R_eive[2,2] * Z_2
y1_R_var <- var(y1_R)
y2_R_var <- var(y2_R)

#2-b
Q2_b <- y1_R_var/(y1_R_var + y2_R_var)

#2-c
r_y1_z1 <- R_eive[1,1] * sqrt(R_eiva[1])
r_y1_z2 <- R_eive[2,1] * sqrt(R_eiva[1])



            