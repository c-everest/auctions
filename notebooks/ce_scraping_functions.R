library(tidyverse)
library(dplyr)
library(plotly)
library(ggplot2)
library(httr)
library(stringr)
library(rvest)
library(xml2)  
library(XML)
library(tidyjson)
library(reticulate)
library(jsonlite)
library(DBI)
library(RPostgres)
library(odbc)


#LISTING ESSENTIALS
scrape_lot_number <- function(x) {
  lot_number <- x %>%
    html_nodes(xpath='//ul[@class="listing-essentials-items"]/li[position()=1]')%>%
    html_text()%>%
    str_extract("\\d+")
  
  if(length(lot_number)==0)
  {
    lot_number<-NA
  }
  else lot_number
  ;lot_number
}

scrape_seller_name <- function(x) {
  seller_name <- x%>%
    html_nodes(xpath='//ul[@class="listing-essentials-items"]/li[position()=2]')%>%
    html_text()%>%
    str_remove("Seller: ")
  
  if(length(seller_name)==0)
  {
    seller_name<-NA
  }
  else seller_name
  ;seller_name
}

scrape_seller_profile <- function(x) {
  seller_profile <- x %>%
    html_nodes(xpath='//ul[@class="listing-essentials-items"]/li[position()=2]//a')%>%
    html_node(xpath='@href')%>%
    html_text()
  
  if(nzchar(scrape_seller_profile)==FALSE)
  {
    is.na(scrape_seller_profile)<-(nzchar(scrape_seller_profile)==FALSE)
  }
  ;seller_profile
}

scrape_location <- function(x) {
  location <- x %>%
    html_nodes(xpath='//ul[@class="listing-essentials-items"]/li[position()=3]')%>%
    html_text()%>%
    str_remove("Location: ")
  
  if(length(location)==0)
  {
    location<-NA
  }
  else location
  ;location
}

scrape_location_google_maps <- function(x) {
  location_google_maps <- x %>%
    html_nodes(xpath='//ul[@class="listing-essentials-items"]/li[position()=3]//a')%>%
    html_node(xpath='@href')%>%
    html_text()
  
  if(nzchar(location_google_maps)==FALSE)
  {
    is.na(location_google_maps)<-(nzchar(location_google_maps)==FALSE)
  }
  ;location_google_maps
}

scrape_chassis <- function(x) {
  chassis <- x %>%
    html_nodes(xpath='//ul[@class="listing-essentials-items"]/li[position()=4]')%>%
    html_text()%>%
    str_remove("Chassis: ")
  
  if(length(chassis)==0)
  {
    chassis<-NA
  }
  else chassis
  ;chassis
}

scrape_odometer <- function(x) {
  odometer <- x %>%
    html_nodes(xpath='//ul[@class="listing-essentials-items"]/li[contains(text(), "Miles") or contains(text(),"Kilometers") or contains(text(), "Km") or contains(text(), "km") or contains(text(),"miles") or contains(text(),"TMU")]')%>% #
    html_text()%>%
    tolower()
  
  if(length(odometer)==0)
  {
    odometer<-NA
  }
  else odometer
  ;odometer
}

scrape_tmu <- function(x) {
  tmu <- x %>%
    html_nodes(xpath='//ul[@class="listing-essentials-items"]/li[contains(text(), "Miles") or contains(text(),"Kilometers") or contains(text(), "Km") or contains(text(), "km") or contains(text(), "miles") or contains(text(),"TMU")]')%>% #
    html_text()%>%
    tolower()%>%
    str_detect("tmu")
  
  if(length(tmu)==0)
  {
    tmu<-NA
  }
  else tmu
  ;tmu
}

scrape_engine <- function(x) {
  engine <- x %>%
    html_nodes(xpath='//ul[@class="listing-essentials-items"]/li[position()=6]')%>%
    html_text()
  if(length(engine)==0)
  {
    engine<-NA
  }
  else engine
  ;engine
}

scrape_transmission <- function(x) {
  transmission <- x %>%
    html_nodes(xpath='//ul[@class="listing-essentials-items"]/li[contains(text(), "Transmission") or contains(text(),"Transaxle") or contains(text(), "Gearbox") or contains(text(), "-Speed") or contains(text(),"Sequential")]')%>%
    html_text()%>%
    tolower()
  
  if(length(transmission)==0)
  {
    transmission<-NA
  }
  else transmission
  ;transmission
}

scrape_seller_type <- function(x) {
  seller_type <- x %>%
    html_nodes(xpath='//ul[@class="listing-essentials-items"]//li[contains(text(),"Private Party Or Dealer") or contains(text(),"Private Party or Dealer")]')%>%
    html_text()%>%
    tolower()%>%
    str_remove("private party or dealer: ")
  
  if(length(seller_type)==0)
  {
    seller_type<-NA
  }
  else seller_type
  ; seller_type
}

scrape_dealer_charges <- function(x) {
  dealer_charges <- x %>%
    html_nodes(xpath='//ul[@class="listing-essentials-items"]//li[contains(text(),"Additional Charges From This Dealer: USD")]')%>%
    html_text()%>%
    parse_number()
  ;dealer_charges
}

scrape_model_tag <- function(x) {
  model_tag <- x %>%
    html_nodes(xpath='//ul[@class="listing-essentials-items"]')%>%
    html_nodes(xpath='li[@class="listing-essentials-item listing-essentials-item-categories"]//a[not(@rel)]')%>%
    html_text()%>%
    str_remove("Premium")
  
  if(length(model_tag)==0)
  {
    model_tag<-NA
  }
  else model_tag
  
  ;model_tag
}

scrape_category_tag <- function(x) {
  category_tag <- x%>%
    html_nodes(xpath='//ul[@class="listing-essentials-items"]')%>%
    html_nodes(xpath='li[@class="listing-essentials-item listing-essentials-item-categories"]//a[@rel="category tag"]')%>%
    html_text()
  
  if(length(category_tag)==0)
  {
    category_tag<-NA
  }
  else category_tag
  ;category_tag
}

scrape_premium <- function(x) {
  premium <- x%>%
    html_nodes(xpath='//ul[@class="listing-essentials-items"]')%>%
    html_nodes(xpath='li[@class="listing-essentials-item listing-essentials-item-categories"]//a[not(@rel)][last()]')%>%
    html_text()%>%
    str_detect("Premium")
  
  if(length(premium)==0)
  {
    premium<-NA
  }
  else premium
  ;premium
}

#LISTING DETAILS

scrape_vehicle_description_text <- function(x) {
    vehicle_description_text <- x%>%
      html_nodes(xpath='//div[@class="column column-left"]')%>%
      html_nodes(xpath='div/div[@class="post-excerpt"]//p[text()]')%>%
      html_text()
    
    #merge into one text chunk
    paste(unlist(vehicle_description_text), collapse =" ")
    
    if(length(vehicle_description_text)==0)
    {
      vehicle_description_text<-NA
    }
    else vehicle_description_text
  ;vehicle_description_text
}

scrape_video_count <- function(x) {
  video_count <- x%>%
    html_nodes(xpath='//div[@class="column column-left"]')%>%
    html_nodes(xpath='div/div[@class="post-excerpt"]//p/iframe[@src]')%>%
    length()
  
  if(length(video_count)==0)
  {
    video_count<-NA
  }
  else video_count
  ;video_count
}

scrape_listing_nicknames <- function(x) {
  listing_nicknames <- x%>%
    html_nodes(xpath='//div[@class="column column-left"]//div//div[@class="listing-nicknames"]')%>%#[@data-gallery-items]
    #  html_children()
    html_text()%>%
    str_remove("Filed under: ")

  ;listing_nicknames
}

scrape_photo_count <- function(x) {
  photos <- x%>%
    #navigate to the photo gallery node
    html_nodes(xpath='//div[@class="column column-left"]//div[@data-gallery-limit]')%>%
    #access the contents of the attribute @data-gallery-items
    html_attr('data-gallery-items')%>%
    #remove backslashes that are escape characters
    str_replace("\\\\","")%>%
    #remove the rest of the escape characters
    str_replace_all("\\\\","")%>%
    #enter the object and collapse the JSON array into index-value pairs at this level
    gather_array("levels.2")%>%
    #spread all scalar elements into new columns
    spread_all()%>%
    as.tbl_json()
  photo_count <- n_distinct(photos$id)
  ;photo_count
}

#AUCTION RESULTS

scrape_auction_outcome <- function(x) {
  auction_outcome <- x%>%
    html_nodes(xpath='//div[@class="listing-available-info"]//span[@class="data-label" and position()=1]')%>%
    html_text()%>%
    tolower()%>%
    switch("sold for" = "sold"
           , "bid to" = "reserve not met"
           , "withdrawn on" = "withdrawn"
           , "current bid:" = "ongoing"
    )
  
  if(length(auction_outcome)==0)
  {
    auction_outcome<-NA
  }
  else auction_outcome
  
  ;auction_outcome
}

scrape_auction_live <- function(x) {
  auction_live <- x%>%
    html_nodes(xpath='//div[@class="listing-available-info"]//span[@class="data-label" and position()=1]')%>%
    html_text()%>%
    tolower()%>%
    str_detect("current bid:")
  
  if(length(auction_live)==0)
  {
    auction_live<-NA
  }
  else auction_live
  
  ;auction_live
}

scrape_final_bid_type <- function(x) {
  final_bid_type <- x%>%
    html_nodes(xpath='//div[@class="column column-left"]//div[@id="listing-bid-container"]/table[@class="listing-stats ended"]/tbody//tr[position()=1]//td[@class="listing-stats-label"]')%>%
    html_text()%>%
    tolower()%>%
    str_remove(" bid")
  
  if(length(final_bid_type)==0)
  {
    final_bid_type<-NA
  }
  else final_bid_type
  
  ;final_bid_type
}

scrape_final_bid_amount <- function(x) {
  final_bid_amount <- x%>%
    html_nodes(xpath='//div[@id="listing-bid-container"]/table[@class="listing-stats ended"]')%>%
    #from table body select first table row that also has the specified class
    html_nodes(xpath='//tbody//tr[position()=1 and @class="listing-stats-stat"]')%>%
    #from this row, select second table data item with the specified class
    html_nodes(xpath='td[position()=2 and @class="listing-stats-value"]')%>%
    #select the <span> tag that does NOT have the high bidder link attribute
    html_nodes(xpath='span[not(@data-listing-high-bidder-link)]')%>%
    html_text()%>%
    parse_number()
  
  if(length(final_bid_amount)==0)
  {
    final_bid_amount<-NA
  }
  else final_bid_amount
  
  ;final_bid_amount
}

scrape_winner_username <- function(x) {
  winner_username <- x%>%
    html_nodes(xpath='//div[@id="listing-bid-container"]/table[@class="listing-stats ended"]')%>%
    #from table body select first table row that also has the specified class
    html_nodes(xpath='//tbody//tr[position()=1 and @class="listing-stats-stat"]')%>%
    #from this row, select second table data item with the specified class
    html_nodes(xpath='td[position()=2 and @class="listing-stats-value"]')%>%
    #select the <a> tag that contains the relevant text
    html_nodes(xpath='span[@data-listing-high-bidder-link]/a')%>%
    html_text()
  
  if(length(winner_username)==0)
  {
    winner_username<-NA
  }
  else winner_username
  
  ;winner_username
}

scrape_winner_profile <- function(x) {
  winner_profile <- x%>%
    #bid container and table with auction stats
    html_nodes(xpath='//div[@id="listing-bid-container"]/table[@class="listing-stats ended"]')%>%
    #from table body select first table row that also has the specified class
    html_nodes(xpath='//tbody//tr[position()=1 and @class="listing-stats-stat"]')%>%
    #from this row, select second table data item with the specified class
    html_nodes(xpath='td[position()=2 and @class="listing-stats-value"]')%>%
    #select the <a> tag that contains the url
    html_nodes(xpath='span[@data-listing-high-bidder-link]/a')%>%
    #extract url as text
    html_nodes(xpath='@href')%>%
    html_text()
  
  if(length(winner_profile)==0)
  {
    winner_profile<-NA
  }
  else winner_profile
  
  ;winner_profile
}

scrape_auction_ended <- function(x) {
  auction_ended <- x%>%
    #bid container and table with auction stats
    html_nodes(xpath='//div[@id="listing-bid-container"]/table[@class="listing-stats ended"]')%>%
    #from table body select first table row that also has the specified class
    html_nodes(xpath='//tbody//tr[@class="listing-stats-stat"]')%>%
    #from this row, select second table data item with the specified class
    html_nodes(xpath='td[@class="listing-stats-value"]//span[@data-ends]')%>%
    html_text()%>%
    str_remove_all("at")%>%
    #  str_replace("PT","US/Pacific")%>%
    parse_datetime("%B %d, %Y %H:%M %p %Z")
  
  if(length(auction_ended)==0)
  {
    auction_ended<-NA
  }
  else auction_ended
  
  ;auction_ended
}

scrape_auction_bids <- function(x) {
  auction_bids <- x%>%
    #bid container and table with auction stats
    html_nodes(xpath='//div[@id="listing-bid-container"]/table[@class="listing-stats ended"]')%>%
    #tables rows that contain the specified class
    html_nodes(xpath='//tbody//tr[@class="listing-stats-stat"]')%>%
    #from this row, select second table data item with the specified class
    html_nodes(xpath='td[@class="listing-stats-value number-bids-value"]')%>%
    html_text()%>%
    as.numeric()
  
  if(length(auction_bids)==0)
  {
    auction_bids<-NA
  }
  else auction_bids
  
  ;auction_bids
}

scrape_auction_views <- function(x) {
  auction_views <- x %>%
    html_nodes(xpath = '//div[@id="listing-bid-container"]/table[@id="listing-actions-stats"]') %>%
    #tables rows that contain the specified attribute
    html_nodes(xpath = 'tbody//tr//td//span[@data-stats-item="views"]') %>%
    html_text() %>%
    #parse for only digits
    parse_number()
  
  if(length(auction_views)==0)
  {
    auction_views<-NA
  }
  else auction_views
  
  ;auction_views
}

scrape_auction_watchers <- function(x) {
  auction_watchers <- x %>%
  #bid container and table with auction stats
  html_nodes(xpath='//div[@id="listing-bid-container"]/table[@id="listing-actions-stats"]')%>%
    #tables rows that contain the specified attribute
    html_nodes(xpath='tbody//tr//td//span[@data-stats-item="watchers"]')%>%
    html_text()%>%
    #parse for only digits
    parse_number()
  
  if(length(auction_watchers)==0)
  {
    auction_watchers<-NA
  }
  else auction_watchers
  
  ;auction_watchers
}
scrape_auction_comments <- function(x) {
  auction_comments <-  x %>%
    #bid container and table with auction stats
    html_nodes(xpath='//div//h2[@class="comments-title"]')%>%
    #tables rows that contain the specified attribute
    html_text()%>%
    #parse for only digits
    parse_number()
  
  if(length(auction_comments)==0)
  {
    auction_comments<-NA
  }
  else auction_comments
  
  ;auction_comments
}
