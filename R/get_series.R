#First release of the packagage.

ceic.series <- function(seriesId) {
  library("xml2")
  server <- getOption("ceic.server")
  if (is.null(server)) {
    server = "http://cisearch-dev.ceicdata.com"
  }

  url <- paste(server,
              "/xml/request?uid=",
              getOption("ceic.username"),
              "&pass=",
              getOption("ceic.pass"),
              "&req=CEIC_SERIES&series=",
              seriesId,
              "&withOriginalTPValue=Y&pcode=test", sep = "")

  xmlResponse <- read_xml(url)

  if(length(xml_find_all(xmlResponse, "/isearch_response/error_response")) != 0) {

    stop(xml_text(xml_find_all(xmlResponse, "/isearch_response/error_response/error_msg")))

  }

  freq <- xml_text(xml_find_all(xmlResponse, "/isearch_response/extendedSeriesList/seriesInfo/frequencyCode"))
  freq <- frequencyToNumeric(freq)
  records <- xml_find_all(xmlResponse, "/isearch_response/extendedSeriesList/timePoints")
  values <- xml_double(xml_find_all(records, "value"))
  dates <- xml_text(xml_find_all(records, "date"))
  startDate <- dates[length(dates)]
  endDate <- dates[1]
  splitDate <- strsplit(startDate, "-")
  startYear <- as.numeric(splitDate[[1]][1])
  stratMonth <- as.numeric(splitDate[[1]][2])

  ts(values, start=c(startYear, stratMonth), frequency = freq)
}

ceic.auth <- function(u, p) {
  options(ceic.username = u)
  options(ceic.pass = p)
}

ceic.getUser <- function() {
  print(getOption("ceic.username"))
  print(getOption("ceic.pass"))
}

ceic.server <- function(name) {
  if (name == "dev") {
    options(ceic.server = "http://cisearch-dev.ceicdata.com")
  }
  else if (name == "prod") {
    options(ceic.server = "http://cisearch.ceicdata.com")
  }
}

ceic.checkServer <- function() {
  print(getOption("ceic.server"))
}

frequencyToNumeric <- function (char) {
  if (char == "Y")
    return (1)
  else if (char == "S")
    return (2)
  else if (char == "Q")
    return (4)
  else if (char == "M")
    return (12)
  else if (char == "W")
    return (52)
  else if (char == "D")
    return (365)
  else
    return (1)
}

