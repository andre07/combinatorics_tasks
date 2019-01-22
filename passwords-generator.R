#generowanie haseł unikatowych składających się z liter małych i cyfr od 1 do 9
n = 1000  #ile haseł

A <- t(combn(c(letters, 1:9), 6)) #6 - liczba znaków w haśle
B <- A[sample(1:nrow(A), n),]
B <- as.data.frame(B)
rm(A)

passwords <- apply(B, 1, function(x) paste0(x, collapse = ""))

write.table(data.frame(passwords), "passwords.txt", 
            row.names = FALSE, quote = FALSE, col.names = FALSE)