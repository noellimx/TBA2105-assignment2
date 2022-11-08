library(tidyverse)
library(RSelenium)
library(seleniumPipes)
library(netstat)
library(data.table)

print("[system] terminating existing browser")

system("kill -9 $(pgrep Chrome)")

chrome_vers = c("106.0.5249.61", "107.0.5304.18", "107.0.5304.62")

print("[rs_server] starting")
rs_server <- RSelenium::rsDriver(browser="chrome", chromever=chrome_vers[1], verbose=FALSE, port=netstat::free_port())
client <- rs_server$client
client$open()

maximizeWindowSize <- function () {
  client$maxWindowSize()
}

scrollToBottom <- function () {
  print("[scrollToBottom]")
  client$executeScript("window.scrollTo({left: 0, top: document.body.scrollHeight, behavior: 'smooth'});")
  
}

scrollToTop <- function () {
  print("[scrollToTop]")
  client$executeScript("window.scrollTo({left: 0, top: 0, behavior: 'smooth'});")
  
}

goToPageWithFullWindowSize <- function(url) {
  print(paste("[goToPageWithFullWindowSize]",url))
  client$navigate(url)
  maximizeWindowSize()
  scrollToBottom()
}

URL_HOME_PAGE <- "https://www.carousell.sg"

changeToEnglish <- function() {
  client$findElement(using = 'xpath', "//select[@aria-label='Language select']/option[@value='en']")$clickElement()
}




showCategoryModal <- function () {
  
  textOfElement <- 'All Categories'
  xpath_value <- paste('//*[text()="',textOfElement,'"]', sep="")
  collapsibleButton <- client$findElement(using = 'xpath', xpath_value)
  
  collapsibleButton$clickElement()
  
  return(collapsibleButton)
}



# The ancestor element containing all categories
getCategoryModal <- function () {
  id_of_modal <- "ReactModalPortal-CategoriesSidebar"
  element <- client$findElement(using="id",id_of_modal)
  return (element)
}


resetToHomePage_EN_WithModal <- function () {
  
  goToPageWithFullWindowSize( URL_HOME_PAGE)
  changeToEnglish()
  showCategoryModal()
}

class_name_category_list_items <- ".D_B_"
class_name_category_item_label = ".D_BB.D_hW" # i.e the label beside desc in the modal
class_name_category_item_desc = ".D_iy.D_gr.D_iz.D_iB.D_iF.D_iI.D_iK.D_iu" #i.e "Cars"

getCategories <- function () {
  print("[getCategories]")

  val_ <- paste("*[@class = '", class_name_category_list_items, "']", sep="")
  
  cats <- client$findElements(using = "css",class_name_category_list_items)
  
  print("[getCategories] End")
  
  return (cats)
  
}

class_name_category_clickable_to_expand_sub_categories = ".D_BC" # i.e The arrow beside Category Description "Cars"
classname_sub_cat <- ".D_BE.D_hW"

getElements_SubCategoriesOfCategory <- function (target_category_desc,cats) {
  
  for (cat in cats) {

    elementDesc <- cat$findChildElement(using = "css",  class_name_category_item_desc)
    cat_text <- elementDesc$getElementText()
    print(paste("current cat -- > ",cat_text))
    
    if (cat_text == target_category_desc)  {
      
      print(paste("category found",target_category_desc))
      elementLabel <- cat$findChildElement(using = "css",  class_name_category_clickable_to_expand_sub_categories)
      elementLabel$clickElement()
      
      subcats <- getCategoryModal()$findChildElements(using = "css",  classname_sub_cat)
      
      print(paste("Length of sub categories", length(subcats)))
      return (subcats)
    }
  }
}



extractSubCatsHrefAndText = function (sub_cats) {
  print("[extractSubCatsHrefAndText] link_to_sub_cat: ")
  
    sub_cats_href_text_and_href <- sub_cats %>%  lapply(function(sb) {
      link_to_sub_cat <- sb$getElementAttribute("href")
      text_of_sub_cat <- sb$getElementText()
      return(list(text_of_sub_cat,link_to_sub_cat))
    })
    return(sub_cats_href_text_and_href)
}


### START



getHrefAndText_SubCategoriesOfCategories <- function (target_categories_desc, cats_) {
  mapping_raw_subcats_of_cats <- target_categories_desc %>% lapply(function (target_cat) {
    
    sub_cats_of_target <- getElements_SubCategoriesOfCategory(target_cat,cats_)
    sub_cats_text_and_href <- extractSubCatsHrefAndText(sub_cats_of_target)
    log_text <- paste("[mapping_raw_subcats_of_cats] target_category: " , target_cat," no. of subcats: ", as.numeric(length(sub_cats_text_and_href)) )
    result <- list(target_cat, list(sub_cats_text_and_href))
    return (result)
    
  })
  
  return (mapping_raw_subcats_of_cats)
  
  
}





class_name_page_subcat_listing_card_product_name <- ".D_iy.D_gr.D_iz.D_iB.D_iF.D_iI.D_iK.D_iG.D_iv"
class_name_page_subcat_listing_card_condition <- ".D_iy.D_gp.D_iz.D_iB.D_iE.D_iI.D_iK.D_iv"
class_name_page_subcat_listing_card_price <- ".D_iy.D_gr.D_iz.D_iB.D_iE.D_iI.D_iL.D_iu"

class_name_page_subcat_listing_cards <- ".D_Jh.D_Ng"
class_name_page_subcat_sort_dropdown <- ".D__x.M_sB"
class_name_page_subcat_sort_dropdown <- ".D_iy.M_gQ.D_iz.M_gR.D_iB.M_gU.D_iF.M_gX.D_iI.M_ha.D_iK.M_hc.D__w.M_sA.D_iu"

class_name_page_subcat_sort_dropdown_radios <- ".D_yI"

collectData <- function (href_, category_, sub_category_, listing_cards_count_want) {
  Sys.sleep(1)
  log_text <- paste("[collectData] url: ", href_, "category: ", category_, "sub_category:" , sub_category_ , "count: " ,listing_cards_count_want )
  print(log_text)
  client$navigate(href_)
  maximizeWindowSize()
  scrollToBottom()
  scrollToTop()
  client$executeScript("document.elementFromPoint(1, 1).click();")
  
  Sys.sleep(1)
  clickable_filter_dropdowns <- client$findElements(using = 'css', class_name_page_subcat_sort_dropdown)
  
  drop_down_primary_sort <- function(){
    Sys.sleep(1)
    
    print("[drop_down_primary_sort]")
    for (cd in clickable_filter_dropdowns) {
      text <- cd$getElementText()
      if (text == "Sort:Best Match"){
        print("[drop_down_primary_sort Sort:Best Match] Found")
        return (cd)
      }

    }
    
    print("[Sort:Best Match] NOT FOUND IN dropdowns, attempting from window client.")
    
    textOfElement <- "Best Match"
    xpath_value <- paste('//*[text()="',textOfElement,'"]', sep="")
  
    ele <- client$findElement(using='xpath',xpath_value)
    return (ele)
    
  }
  
  drop_down_primary_sort()$clickElement()
  
  
  clickable_filter_radios <- client$findElements(using = 'css', class_name_page_subcat_sort_dropdown_radios)
  
  drop_down_primary_sort_radio_recent <- (function(){
    
    print("[drop_down_primary_sort_radio_recent] ")
    for (r in clickable_filter_radios){
      text <- r$getElementText()
      if (text == "Recent"){
        print("[drop_down_primary_sort_radio_recent] FOUND")
        return (r)
      }
    }
    
    print("[drop_down_primary_sort_radio_recent] NOT FOUND by class. attempting to find from client")
    textOfElement <- "Recent"
    xpath_value <- paste('//*[text()="',textOfElement,'"]', sep="")
    
    ele <- client$findElement(using='xpath',xpath_value)
    
    return (ele)
  })()
  
  print("[sort by recency]")
  drop_down_primary_sort_radio_recent$clickElement()
  
  
  
  class_name_page_subcat_show_more_results <- ".D_hl.D_hc.D_hy.D_ht.D_hh.D_aYz"
  
  getButtonShowMoreResults <- function () {
    ele <- client$findElement(using = 'css', class_name_page_subcat_show_more_results)
    return (ele)
  }
  
  clickButtonShowMoreResults <- function () {
    print("[clickButtonShowMoreResults]")
    getButtonShowMoreResults()$clickElement()
  }
  # returns the cards shown.
  
  showListingCards <- function (count_want_) {
    
    print("[showListingCards]")
    print(paste("want count", as.character(count_want_)))
    
    scrollToBottom()
    scrollToTop()
    listing_cards <- client$findElements(using = 'css', class_name_page_subcat_listing_cards)
    
    listing_cards_count <- length(listing_cards)
    while (listing_cards_count < count_want_){
      scrollToBottom()
      scrollToTop()
      
      Sys.sleep(1)
      clickButtonShowMoreResults()
      listing_cards <- client$findElements(using = 'css', class_name_page_subcat_listing_cards)
      listing_cards_count <- length(listing_cards)
      print(paste("[showListingCards] count:  ",listing_cards_count))
      
    }
    return (listing_cards[1:count_want_])
  }
  
  listing_cards <- showListingCards(listing_cards_count_want)
  
  print(paste("Collected ", as.character(length(listing_cards)), "Cards"))

  
  print("getting card details.... ")
  
  df_subcat <- data.frame(matrix(ncol = 5, nrow= 0))
  colnames(df_subcat) <- c("Category","Subcategory","Product Name", "Price", "Condition")
  
  
  for (lc in listing_cards) {
    element_product_name <- lc$findChildElement(using = 'css', class_name_page_subcat_listing_card_product_name)
    product_name <- element_product_name$getElementText()
    element_product_condition <- lc$findChildElement(using = 'css', class_name_page_subcat_listing_card_condition)
    product_condition <- element_product_condition$getElementText()
    element_product_price <- lc$findChildElement(using = 'css', class_name_page_subcat_listing_card_price)
    product_price <- element_product_price$getElementText()
    
    row_lc <- c(category_, sub_category_, unlist(product_name), unlist(product_price), unlist(product_condition))
    df_subcat <- rbind(df_subcat,row_lc)
  }
  return (df_subcat)
}




crawlMrc <- function (listing_cards_per_sub_cat_want) {
  
  return (function (mrc_) {
    
    print("[crawlMrc]")
    mrc_ <- unlist(mrc_, recursive=FALSE)
    
    category_name <- unlist(mrc_[1])
    
    print(paste("[crawlMrc:mapping] Category: #",category_name))
    
    subcats <- unlist(mrc_[2],recursive=FALSE)
    
    
    df_cat <- lapply(subcats, function(sc) {
      sc_ <- unlist(sc)
      sc_desc <- sc_[1]
      sc_href <- sc_[2]
      
      df_sc <- collectData(sc_href,category_name,sc_desc, listing_cards_per_sub_cat_want )
      print(paste("[crawl] collected data for ",sc_desc))
      return (df_sc)
    }) %>% data.table::rbindlist(use.names=FALSE)
    
    return (df_cat)
    
  })
}

preprocessAll <- function (all)
{
  
  colnames(all) <- c("Category","Subcategory","Product Name", "Price", "Condition")
  # all <- all[,1:5]
  
  return (all)
}


resetToHomePage_EN_WithModal()
showCategoryModal()

cats <- getCategories()

target_categories_desc <- c("Luxury","Men's Fashion","Women's Fashion")

mapping_raw_subcats_of_cats <- getHrefAndText_SubCategoriesOfCategories(target_categories_desc, cats)

listing_cards_per_sub_cat_want <- 200

all <-  lapply(mapping_raw_subcats_of_cats,crawlMrc(listing_cards_per_sub_cat_want)) %>% data.table::rbindlist(use.names=FALSE) %>% preprocessAll()

write.csv(all, "data_dryrun.csv", row.names = TRUE)
