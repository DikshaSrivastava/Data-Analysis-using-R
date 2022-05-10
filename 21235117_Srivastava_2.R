
#pre-processing
set.seed(100)
N=20
CX101 <- rnorm(N,45,8)
CX102 <- rnorm(N,65,8)
CX103 <- rnorm(N,85,10)
CX104 <- rnorm(N,45,10)
CX105 <- rnorm(N,70,1)

#Create the given matrix
res <- round(matrix(c(CX101, CX102, CX103, CX104, CX105), ncol = 5),2)
rownames(res) <- c("Student_1","Student_2","Student_3","Student_4",
                   "Student_5","Student_6","Student_7","Student_8",
                   "Student_9","Student_10","Student_11","Student_12",
                   "Student_13","Student_14","Student_15","Student_16",
                   "Student_17","Student_18","Student_19","Student_20")
colnames(res) <- c("CX101", "CX102", "CX103", "CX104", "CX105")
res

#Replace invalid values with NA
res1 <- apply(res, 2, function(val)ifelse(val<0 | val>100, NA, val))
res1

#Replace NA values with mean
res2 <- apply(res1, 2, function(val)ifelse(is.na(val), round(mean(val, na.rm = T),2), val))
res2

#Generate aggregate values and Rank
final <- cbind(res2, Min = apply(res2, 1, function(val)min(val)), 
               SD = apply(res2, 1, function(val)round(sd(val),2)), 
               Mean = apply(res2, 1, function(val)round(mean(val),2)))
#Calculating the rank using mean values
final <- cbind(final, Rank = rank(final[,"Mean"]))
final

#Student with highest average
rank_vector <- final[, "Rank"]
#Finding the maximum rank as a student with highest average would have highest rank
max_rank <- max(rank_vector)
#Finding the index of student having highest average
index <- which(rank_vector==max_rank)
highest_avg <- final[index, , drop = F]
highest_avg

#Subject Summary
subj_summ <- rbind(res2, Subject_Mean = apply(res2, 2, function(val)round(mean(val),2)),
                   Subject_Max = apply(res2, 2, function(val)max(val)),
                   Subject_Min = apply(res2, 2, function(val)min(val)))
subj_summ

