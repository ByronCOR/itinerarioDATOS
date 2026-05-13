################################
#WEB SCRAPING
################################
install.packages("rvest")
install.packages("dplyr")
library("rvest")
library("dplyr")
url<- "https://books.toscrape.com/catalogue/page-1.html"
pagina<- read_html(url)
nodos<- pagina%>%
  html_elements(".product_pod h3 a")
nodos[[1]]
titulos<- nodos%>%
  html_attr("title")
enlaces<- nodos%>%html_attr("title")
