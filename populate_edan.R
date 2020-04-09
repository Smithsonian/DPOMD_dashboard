#Script to populate/update object data from edan
#It assumes all id's are already in the table

library(DBI)
library(EDANr)
library(stringr)

source("settings.R")



#Connect to the database ----
db <- dbConnect(odbc::odbc(),
                driver = "PostgreSQL Unicode",
                database = pg_db,
                uid = pg_user,
                pwd = pg_pass,
                server = pg_host,
                port = 5432)



query_edan <- dbGetQuery(db, "SELECT * from projects_edan")

for (i in seq(1, dim(query_edan)[1])){
  
  this_item_search <- EDANr::searchEDAN(q = query_edan[i,]$edan_id, AppID = AppID, AppKey = AppKey)
  
  if (this_item_search$rowCount == 1){
    edan_id <- this_item_search$rows$url
    
    this_item <- EDANr::getContentEDAN(itemID = edan_id, AppID = AppID, AppKey = AppKey)
    
    IDSid1 <- this_item$content$descriptiveNonRepeating$online_media$media$idsId[1]
    if (!is.null(IDSid1)){
      title1 <- this_item$content$descriptiveNonRepeating$title$content[1]
      if (title1 == "Untitled Object"){
        title1 <- ""
      }
      
      link1 <- this_item$content$descriptiveNonRepeating$record_link[1]
      if (is.null(link1)){
        link1 <- paste0("https://collections.si.edu/search/detail/", edan_id)
      }
      
      credit1 <- this_item$content$freetext$creditLine$content[1]
      if (is.null(credit1)){
        credit1 <- ""
      }
      
      notes1 <- paste(this_item$content$freetext$notes$content[1], collapse = ". ")
      if (is.null(notes1)){
        notes1 <- ""
      }
      
      if (substr(IDSid1, 0, 4) != 'http'){
        IDSid1 <- paste0('https://ids.si.edu/ids/deliveryService?id=', IDSid1)
      }
      
      img_file <- paste0(query_edan[i,]$edan_id, ".jpg")
      
      img_file <- str_replace_all(img_file, ":", "_")
      
      img_res <- try(download.file(paste0(IDSid1, "&max=160"), destfile = paste0("www/", img_file)))
      
      if (class(img_res) == "try-error"){
        #try again
        download.file(paste0(IDSid1, "&max=160"), destfile = paste0("www/", img_file))
      }
      
      
      insert_q <- paste0("UPDATE projects_edan SET title = '", str_replace_all(title1, "'", "''"), "', link = '", link1, "', credit = '", credit1, "', notes = '", notes1, "', idsid = '", IDSid1, "', img_file = '", img_file, "' WHERE edan_id = '", query_edan[i,]$edan_id, "'")
      
      n <- dbSendQuery(db, insert_q)
      
      dbClearResult(n)
    }
  }
}
