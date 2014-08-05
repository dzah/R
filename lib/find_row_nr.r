# Найти строки, содержащие NA в каком-то столбце
df <- data.frame(c(1:3), c(NA, 5, 6))
df
df1 <- df[is.na(rowSums(df)), ]
df1
# Чтобы найти номер строки:
which(is.na(rowSums(p)))
