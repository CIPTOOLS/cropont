# The app should be started in a separate process: maybe in same? To check.
# shiny::runApp(paste0(find.package("cropont"), "/app"), port = 12301)

# The Selenium tool is also a server; needs to be started using:
# RSelenium::startServer()

  remDr <- RSelenium::remoteDriver()
  remDr$open(silent = TRUE)
  appURL <- "http://127.0.0.1:12301"
  
  test_that("can connect to app", {  
    remDr$navigate(appURL)
    appTitle <- remDr$getTitle(appURL)[[1]]
    expect_equal(appTitle, "")  
  })
  
  test_that("Title is present", {  
    webElems <- remDr$findElements("css selector", ".logo")
    appCtrlLabels <- sapply(webElems, function(x){x$getElementText()})
    expect_equal(appCtrlLabels[[1]], "Demo Dictionary")  
  })
  
  remDr$close()  

