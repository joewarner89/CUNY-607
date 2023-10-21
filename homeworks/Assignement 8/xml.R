library(tidyverse)
library(XML)
library(minixml)
library(xml2)

# creating XmLfiles 
books <- xml_elem('books')


# adding elements of th first book and then
books$add('Book_Name','Data Science for Business: 
          What You Need to Know about Data Mining and Data-Analytic Thinking')$
  add('Author','Tom Fawcett','Foster Provost')$
  add('ISB_Number','9781449374280')$
  add('Page_counts','414','123')$
  add('Published','08/27/2023')$
  #adding the second book
  
  add('Book_name','The Miseducation of the Negro')$
  add('Author','Carter Godwin Woodson','H. Khalif Khalifah')$
  add('ISB_Number','9781564110411')$
  add('Page_counts','215')$
  add('Published','01/01/1992')$
  # adding the thrid book 
  add('Book_name','Twilights')$
  add('Author','Stephenie Meyer')$
  add('ISB_Number','9780316007443')$
  add('Page_counts','544')$
  add('Published','08/18/2007')


books$print()
xmlParse(books)
xml_structure(books)
library(methods)
xmlToDataFrame(books)

parse_xml_elem(books)
mxmlGetFirstChild(mxml_node_t *node)
