library(rvest)

extract_year_data <- function(year) {
  url <- paste0("https://www.numbeo.com/quality-of-life/rankings_by_country.jsp?title=", year)
  webpage <- read_html(url)
  
  rows <- html_nodes(webpage, "tbody tr")
  
  data <- lapply(rows, function(row) {
    cells <- html_nodes(row, "td")
    list(
      Country = html_text(cells[2]),
      Quality_of_Life_Index = html_text(cells[3]),
      Purchasing_Power_Index = html_text(cells[4]),
      Safety_Index = html_text(cells[5]),
      Health_Care_Index = html_text(cells[6]),
      Cost_of_Living_Index = html_text(cells[7]),
      Property_Price_to_Income_Ratio = html_text(cells[8]),
      Traffic_Commute_Time_Index = html_text(cells[9]),
      Pollution_Index = html_text(cells[10]),
      Climate_Index = html_text(cells[11]),
      Year = year
    )
  })
  
  Filter(Negate(is.null), data)
}

years <- 2014:2021
all_data <- do.call(rbind, lapply(years, function(year) {
  year_data <- extract_year_data(year)
  do.call(rbind, lapply(year_data, as.data.frame, stringsAsFactors = FALSE))
}))


countries <- c("China", "Russia", "United States", "Canada", "Japan")
filtered_data <- subset(all_data, Country %in% countries)









indicators <- c(
  "Quality_of_Life_Index", "Purchasing_Power_Index", "Safety_Index",
  "Health_Care_Index", "Cost_of_Living_Index", "Property_Price_to_Income_Ratio",
  "Traffic_Commute_Time_Index", "Pollution_Index", "Climate_Index"
)

plot_indicator <- function(data, indicator, title) {
  plot(NULL, 
       xlim = range(data$Year, na.rm = TRUE), 
       ylim = c(0, 200),
       xlab = "Year", ylab = indicator, main = title)
  
  colors <- c("red", "blue", "green", "orange", "purple")
  countries <- unique(data$Country)
  
  for (i in seq_along(countries)) {
    country_data <- subset(data, Country == countries[i])
    lines(country_data$Year, country_data[[indicator]], 
          type = "o", col = colors[i], pch = 16, lwd = 2)
  }
  
  legend("topright", legend = countries, col = colors, pch = 16, cex = 0.8)
}

for (ind in indicators) {
  plot_indicator(filtered_data, ind, paste("Тенденция показателя:", ind))
}















library(rvest)
extract_museum_data <- function() {
  url <- "https://kudago.com/spb/list/33-luchshih-muzeya-peterburga/"
  webpage <- read_html(url, encoding = "UTF-8")
  big_item <- html_node(webpage, "div.post-list-big")
  
  items <- html_nodes(big_item, "article.post-list-item")

  
  data <- lapply(items, function(item) {
    name <- html_text(html_node(item, "h3.post-list-item-title a span"), trim = TRUE)
    Encoding(name) <- "UTF-8"
    
    address <- html_text(html_node(item, "address.post-list-item-info"), trim = TRUE)
    if (is.na(address))
      address <- html_text(html_node(item, "span.post-list-item-info--event-place"), trim = TRUE)
    Encoding(address) <- "UTF-8"
    
    description <- html_text(html_node(item, "div.post-list-item-description div p"), trim = TRUE)
    Encoding(description) <- "UTF-8"
    
    link <- html_attr(html_node(item, "a.post-list-item-title-link"), "href")
    
    age_restriction <- html_text(html_node(item, "span.list-age-restriction"), trim = TRUE)
    Encoding(age_restriction) <- "UTF-8"
    
    list(
      Название = name,
      Адрес = address,
      Описание = description,
      Ссылка = link,
      Возрастное_ограничение = age_restriction
    )
  })
  
  Filter(Negate(is.null), data)
}

museums_data <- extract_museum_data()

museums_df <- do.call(rbind, lapply(museums_data, as.data.frame, stringsAsFactors = FALSE))