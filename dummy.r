library(tidyverse)
library(RSelenium)
library(netstat)

#library(binman)
#library(wdman)

#binman::rm_platform("phantomjs")
#wdman::selenium(retcommand = TRUE)


chrome_vers = c("106.0.5249.61", "107.0.5304.18", "107.0.5304.62")

print("[rs_server] starting")
rs_server <- RSelenium::rsDriver(browser="chrome", chromever=chrome_vers[1], verbose=FALSE, port=netstat::free_port())

client <- rs_server$client


client$open()

goToPageWithFullWindowSize <- function(url) {
  print(cat("[gotopage]",url))
  client$navigate(url)
  client$maxWindowsSize()
  client$executeScript("window.scrollTo(0, document.body.scrollHeight);")

}

goToPageWithFullWindowSize("https://www.ebay.com")









