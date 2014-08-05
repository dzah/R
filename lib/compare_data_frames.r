# Найти строки, которые есть в 1 но нет в 2 и т.п.

# Простой способ - ниже, также см. 
# http://stackoverflow.com/questions/3171426/compare-two-data-frames-to-find-the-rows-in-data-frame-1-that-are-not-present-in


# Простой способ
daf1 <- data.frame(1:4, 5:8)
colnames(daf1) <- c("cname1", "cname2")
daf2 <- data.frame(c(1, 3, 5), c(5, 7, 9))
colnames(daf2) <- colnames(daf1)
daf1
daf2

daf1$in1 <- TRUE
daf2$in2 <- TRUE
daf1
daf2

res <- merge(daf1, daf2, all = T)
res

in1_in2 <- subset(res, in1 == T & in2 == T)
in1_in2

in1_not_in2 <- subset(res, in1 == T & is.na(in2))
in1_not_in2
