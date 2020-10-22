install.packages('rvest')
install.packages('stringr')
library(dplyr)
library(rvest)
library(stringr)
library(xml2)
library(rvest)
install.packages("rvest")

install.packages('XML')
library(XML)

pdf_url_matrix <- matrix(c("hoi", "hoi", "hoi"), nrow = 1, ncol = 3)

for (i in 1:nrow(data)){ 
  url <- as.matrix(data$ApplicationDocsURL[i])
  
  tryCatch(page <- xml2::read_html(url), error = function(e){'empty page'})
  if (page == 'empty page'){
    next
  }
  possible_pdfs <- html_nodes(page, xpath = './/a[text()="Medical Review(s)"]')
  if (length(possible_pdfs) == 0 ) {
    possible_pdfs = html_nodes(page, xpath = '..//li[contains(.,"Medical Review(s)")]/ul/li/a[contains(.,"Part")]') 
    if (length(possible_pdfs) == 0 ) {
      possible_pdfs = html_nodes(page, xpath = './/a[text()="Multi-Discipline Review/Summary, Clinical, Non-Clinical"]')
      if (length(possible_pdfs) == 0) {
        possible_pdfs = html_nodes(page, xpath = '..//li[contains(.,"Multi-Discipline Review/Summary, Clinical, Non-Clinical")]/ul/li/a[contains(.,"Part")]') 
        if (length(possible_pdfs) == 0 ) {
          possible_pdfs = html_nodes(page, xpath = './/a[text()="Summary Review"]')
          if (length(possible_pdfs) == 0) {
            possible_pdfs = html_nodes(page, xpath = '..//li[contains(.,"Summary Review")]/ul/li/a[contains(.,"Part")]') 
          }
        }
      }
    }
  }
  
  pdfs_url_link <- c()
  if (length(possible_pdfs) == 0) {
    pdf_url_matrix <- rbind(pdf_url_matrix, c(data$ApplNo[i], as.matrix(data$DrugName[i]), "No Medical/Summary/Multi-disciplinary documents found"))
  } else {
    pdf_matrix <- matrix(, nrow <- length(possible_pdfs), ncol = 2)
    
    pdf_link_expr <- regexpr("\\/[^\\/]*$", url )
    
    # LOOP through the links we found
    for (doc_element in possible_pdfs) {
      # Generate the url for medical/bla/bla
      pdfs_url_link <- c( paste(substring(url, 0, pdf_link_expr),html_attr(doc_element, "href"), sep = "/"))
      pdf_url_matrix <- rbind(pdf_url_matrix, c(data$ApplNo[i], as.matrix(data$DrugName[i]),pdfs_url_link))
    }
  }
  print(i/nrow(data) * 100) 
}


#########################################################################################################
#########################################################################################################
#########################################################################################################
#########################################################################################################
#########################################################################################################


# 1. Installing and loading packages
packages <- list('pdftools', 'RCurl', 'stringr', 'readxl','reshape', 'reshape2', 'dplyr', 'progress','tictoc', 'tesseract')
lapply(packages, require, character.only = TRUE)
lapply(packages, install.packages, character.only=TRUE)
eng <- tesseract("eng")

# 2. Downloading data and 
temp <- tempfile() # initialize temp

download.file("https://www.fda.gov/media/89850/download",temp) #download from drugs@FDA

ApplicationsDocsType_Lookup <- read.delim(unz(temp, 'ApplicationsDocsType_Lookup.txt'))
ApplicationDocs <- read.delim(unz(temp,'ApplicationDocs.txt'))
Applications <- read.delim(unz(temp,"Applications.txt"))
Products <- read.delim(unz(temp,"Products.txt"))
MarketingStatus <- read.delim(unz(temp, "MarketingStatus.txt"))
MarketingStatus_Lookup <- read.delim(unz(temp, "MarketingStatus_Lookup.txt"))

unlink(temp)


cat('number of all unique drug names: ', length(unique(Products$DrugName)), 
    '\nnumber of all unique application documents: ', length(ApplicationDocs$ApplicationDocsID),
    '\nnumber of all labels: ', sum(ApplicationDocs$ApplicationDocsTypeID==2),
    '\nnumber of all reviews: ', sum(ApplicationDocs$ApplicationDocsTypeID==3),
    '\nnumber of all summaries: ', sum(ApplicationDocs$ApplicationDocsTypeID==21))

# Applicationdoctype = 2 => 19611 documents
# Applicationdoctype = 3 => 6413 documents
# Applicationdoctype = 21 => 744 documents
# numer  7244 unique drugse

# Select all Products that have are Approved
Authorized <- MarketingStatus[MarketingStatus$MarketingStatusID ==1 | MarketingStatus$MarketingStatusID == 2,]
Authorized_Products <- merge(Authorized, Products)

# Select only ApplNo, Drug Name and Ingredient
Authorized_Products <- Authorized_Products[,c(1,7,8)]

# Aggregate by ApplNo
prod <- aggregate(Authorized_Products,by = list(Authorized_Products$ApplNo), FUN = last)
cat('unique Application Numbers for Authorized Products: ', length(prod$ApplNo))


# Select all .cfm documents from ApplicationDocs
ApplicationCFM <- drug_applications[grepl('*.cfm$', drug_applications$ApplicationDocsURL),]
cat('Applicationdocs total ', length(ApplicationDocs$ApplicationDocsID),  'drug_applications total: ', length(drug_applications$ApplicationDocsID), 'Total PDFs :', length(ApplicationDocs_pdf$ApplicationDocsID))


