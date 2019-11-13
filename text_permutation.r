x <- c("Posrać nas nie posrało. Po prostu dostarczamy sobie intelektualnej rozrywki.
       Darku, mam prośbę o podesłanie numeru konta, gdyż nie posiadam Waszego, choć zdawało mi się, że
       przelewaliście zaliczkę na Białogórę, ale okazało się, że zaliczka była wpłacana przez Was, ale bezpośrednio
       do właścicieli kwatery. Pozdrawiam")


x <- gsub(","," , ", x)
x <- gsub("."," . ", x, fixed = TRUE)
x <- gsub("?"," ? ", x, fixed = TRUE)
x <- gsub("!"," ! ", x)
x <- gsub("-"," - ", x)
x <- gsub("  "," ", x)

y <- strsplit(x, " ")
y <- stri_extract_all_words(x)

permuta <- function(x){
  if (length(x) > 3) 
    return(c(x[1], sample(x[2:(length(x)-1)]), x[length(x)])) else
      return(x)
}

library(stringi)
z <- lapply(y[[1]], stri_enc_toutf32)

writeLines(paste(unlist(lapply(lapply(1:length(z), 
                                      function(xx) permuta(as.numeric(unlist(z[[xx]])))),
                               stri_enc_fromutf32)), collapse = " "), "clipboard")
