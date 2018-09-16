library(stringr)
raw.data <-"555-1239Moe Szyslak(636) 555-0113Burns, C. Montgomery555-6542Rev. Timothy Lovejoy555 8904Ned Flanders636-555-3226Simpson, Homer5553642Dr. Julius Hibbert"
raw.data

name <- unlist(str_extract_all(raw.data, "[[:alpha:]., ]{2,}"))
name

## 1. Rearrange vector so that all elements conform to the standard: First_Name Last_Name

notconfflag <- str_detect(name, ",")
notconfnames <- name[notconfflag]
notconfnames
confnames <- name[notconfflag==FALSE]
confnames

splitnames <- unlist(str_split(notconfnames, ","))
firstnamesflag <- str_detect(splitnames, "[[:blank:]]+")
firstnames <- splitnames[firstnamesflag]
firstnames <- str_trim(firstnames)
lastnames <- splitnames[firstnamesflag==FALSE]
fixednames <- str_c(str_c(firstnames, " "), lastnames)

finalnames <- c(confnames, fixednames)
finalnames

## 2.Construct a logical vector indicating whether a character has a title (i.e. Rev. and Dr.)

hastitleflag <- str_detect(finalnames, "([:alpha:]{2,})(\\.)")
hastitle <- finalnames[hastitleflag]
hastitle 

## 3.Construct a logical vector indicating whether a character has a second name

has2ndflag <- str_detect(finalnames, "[A-Z](\\.)")
has2ndname <- finalnames[has2ndflag]
has2ndname

## 4. Describe the types of strings that conform to the following regex and construct an example that is matches by the regex

  ## 1. [0-9]+\\$ - Extracts digits ending with with the "$" sign
    ex_dollar <- "Price: $50, Tax: $5, Total: 55$, Fees$"
    ex_dollar
    dollar_tot <- str_extract_all(ex_dollar, "[0-9]+\\$")
    dollar_tot

  ## 2. \\b[a-z]{1,4}\\b - Extracts whole strings that have up to 4 lowercase letters
    ex_lowerc <- "abcd abc ABCD abcdefg 1234 abcd1 xy xy2z zyxw lmnopq"
    ex_lowerc
    lowerc <- str_extract_all(ex_lowerc, "\\b[a-z]{1,4}\\b")
    lowerc

  ## 3. .*?\\.txt$ - Extracts any strings (i.e files) that end with the ".txt" extension
    ex_files <- c("File1.doc", "File2.xls", "File3.txt", "File4.ppt", "File5.txt")
    ex_files
    txtfiles <- unlist(str_extract_all(ex_files, ".+?\\.txt$"))
    txtfiles

  ## 4. \\d{2}/\\d{2}/\\d{4} - Extracts dates conforming the format: 2 digits for month, 2 digits for day and 4 digits for year
    ex_date <- "IssueDate 01/01/2018, ExpDate 12/31/2020, OtherDate 04/02/18"
    ex_date
    dates <- str_extract_all(ex_date, "\\d{2}/\\d{2}/\\d{4}")
    dates
  
  ## 5. <(.+?)>.+?\\1 - Extracts the first string enclosed by "<>" then extracts the next occurrence 
  ## of that substring regardless of what is in between
    ex_backref <- "<1234> abcde <1234> xyz<1234>"
    ex_backref
    backref <- str_extract_all(ex_backref, "(<.+?>).+?\\1")
    backref
