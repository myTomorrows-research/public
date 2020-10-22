packages <- list('pdftools', 'RCurl', 'stringr', 'readxlsx2','reshape', 'reshape2', 'pheatmap', 'dplyr', 'progress','tictoc', 'tesseract')
lapply(packages, require, character.only = TRUE)
eng <- tesseract("eng")


review <- read.table("/Users/tobiaspolak/Desktop/review.csv", sep = ";")
items <- list('compassionate use', 'expanded access', 'early access', 'named-patient', 'pre-approval access')
for (term in items){
  review[,term] <- NA
}
for (i in 1:nrow(review)){
  drug_url = as.matrix(review$V2[i])
  download.file(drug_url, 'destfile') 
  pngfile <-pdf_convert('destfile', dpi = 300, format='png')
  text <- ocr(pngfile)
  
  for (word in items){ 
    if (sum (grepl(word, text, ignore.case=TRUE)) > 0){ 
      review[i, word] <- paste(which(grepl( word,text,ignore.case=TRUE)), collapse = ",")
    }else{ 
      review[i, word] <- FALSE
    } 
  }
}

for (j in 1:10)
{
  print(j)
}  
  
filename <- paste0('FDA_review', Sys.time(), '.csv')
write.table(review, filename)
write.csv2(review, filename)
