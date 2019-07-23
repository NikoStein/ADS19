# Problem Set 2
# Question 1


#Zusaetzlicher Inhalt (ganz unten): Data Transformation with dplyr, Kommentare zu JOINS, Rvest package basics

require(tidyverse)
require(rvest)

url = "http://quotes.toscrape.com/"

#a) Use rvest to scrape the first 10 quotes. Return a data frame with 3 columns (author,tags,quotes).

read_html(url) -> rawData #Damit ich nicht staendig auf die Internetseite zu greife

rawData %>%
  html_nodes('.author') %>%
  html_text() -> author

rawData %>%
  html_nodes('.text') %>%
  html_text() -> quotes

rawData %>%
  html_nodes('.tags') %>%
  html_text() %>%
  str_replace_all("\\s+", " ") %>% #\\s ersetzt alle Zeilenumbrueche und Whitespaces mit einem Space
  str_replace_all("Tags:","") -> tags #ersetzt Tags:

firstTen = data.frame(author, quotes, tags)




#b) Use your function to collect all 100 quotes from the website and store them in a data frame (quotes).

##Seite: http://quotes.toscrape.com/page/10/
page <- c(1:10)


getQuotes = function(page)
{

  quoteURL = paste0(url,'page/',page)

  read_html(quoteURL) -> rawData

  rawData %>%
    html_nodes('.author') %>%
    html_text() -> author

  rawData %>%
    html_nodes('.text') %>%
    html_text() -> quotes

  #NA's verschwinden lassen
  rawData %>%
    html_nodes('.tags') %>%
    html_text() %>%
    str_replace_all("\\s+", " ") %>%
    str_replace_all("Tags:","") -> tags

  #Zeilen ohne Tags mit NA auffuellen
  tags = ifelse(tags == " ", NA, tags)

  data.frame(Author = author, Quotes = quotes, Tags = tags)
}

quotes <- map_df(page, getQuotes)





#c) Additionally, we want to collect more information on the authors. Therefore, your next task is to scrape the URLs of each author’s about-page.

##Seite: http://quotes.toscrape.com/page/10/

getAuthorURL = function(page)
{
  quoteURL = paste0(url,'page/',page)

  read_html(quoteURL) -> rawData

  rawData %>%
    html_nodes(".quote span a") %>%
    html_attr("href") -> authorURL

  data.frame(Authorpages = authorURL)
}

authorpages <- map_df(page, getAuthorURL)




#d) Write a function to scrape the content of an about-page returning a data frame (authorDetails) with 3 columns (author, description, bornDate). Apply the function to scrape all about-pages.

#Seite: http://quotes.toscrape.com/author/Albert-Einstein/

authorURL <- authorpages$Authorpages

getAuthorDetails = function(authorURL)
{
  authorURL = paste0(url,authorURL)

  read_html(authorURL) -> rawData

  rawData %>%
    html_nodes('.author-title') %>%
    html_text() %>%
    str_replace_all("\\n", "") %>% #Zeilenumbrueche entfernen und viele Spaces, sonst bekommt man Probleme beim join.
    str_trim()-> author

  rawData %>%
    html_nodes('.author-description') %>%
    html_text() -> description

  rawData %>%
    html_nodes('.author-born-date') %>%
    html_text() -> bornDate

  data.frame(Author = author, Description = description, bornDate = bornDate)
}

authorDetails <- map_df(authorURL, getAuthorDetails)

#Alle Duplikate loeschen
authorDetails %>%
  distinct() -> authorDetails




#e) Next, your task is to analyze the collected data. Leverage your data wrangling skills to perform the following tasks
  #i) The authorDetails data frame stores the information on the birth data in one column (bornDate). Transform the data frame to store the information on the day, month and year in distinct columns. How many authors where born in the 19th century (1800-1899)?

        #Date Format: March 14, 1879

        authorDetails %>%
          mutate(Day = as.numeric(str_extract(bornDate, "\\d{2}"))) %>%
          mutate(Month = str_extract(bornDate, "\\w+")) %>%
          mutate(Year = as.numeric(str_extract(bornDate, "\\d{4}"))) -> authorDetails


        authorDetails %>%
          filter(between(Year, 1800, 1900)) %>%
          summarise(Count = n())

        #A: 20 authors where born in the 19th century

        #weitere Befehle für date time waeren: as.Date(authorDetails$bornDate) as.POSIXct(authorDetails$bornDate, format = "%m %d %Y")


  #ii) Transform and summarize the quotes data set to answer the following questions:

      # 1. Which author has the most quotes on the website?

          #1. Variante
          quotes %>%
            count(Author, sort = TRUE) %>%
            head(1)

          #2. Variante
          quotes %>%
            group_by(Author) %>%
            summarise(Count = n()) %>%
            arrange(-Count) %>%
            head(1)

          #A: Albert Einstein 10

      #2. How many quotes does an author have on average

          quotes %>%
            group_by(Author) %>%
            summarise(Count = n()) %>%
            summarise(Mean = mean(Count))
          #A: 2 quotes does an author have on average

      #3. Find all quotes that use the tag “life”

      quotes %>%
        filter(str_detect(Tags, "life")) -> lifeQuotes


  #iii) Join both data frames (you may need to transform keys first)

      #Problem bei Schriftsteller Alexandre Dumas fils beheben, da er mit verschiedener Schreibweise (Alexandre Dumas fils vs. Alexandre Dumas-fils) vertreten ist
      authorDetails[30, 1] = "Alexandre Dumas fils"

      #1. Variante
      joined_df <- merge(quotes, authorDetails, by = "Author")

      #2. Variante
      joined_df <- left_join(quotes, authorDetails, by = 'Author')





#########Data Transformation with dplyr####################################################################

      #select() extracts variables/columns as a table

      #filter() extracts rows that meet logical criteria

      #group_by() creates a "grouped" copy of a table. dplyr functions will manipulate each "group" separately and then combine the results

      #summarise() applies summary functions to columns to create a new table of summary statistics based on grouping.

      #arrange() orders rows by values of a column or columns

      #mutate() computes new columns/variables




#########Kommentar zu JOINS####################################################################

      # By using the merge function and its optional parameters:
      #
      # Inner join: merge(df1, df2) will work for these examples because R
      # automatically joins the frames by common variable names, but you would
      # most likely want to specify merge(df1, df2, by = "CustomerId") to make
      # sure that you were matching on only the fields you desired. You can also
      # use the by.x and by.y parameters if the matching variables have
      # different names in the different data frames.
      #
      # Outer join: merge(x = df1, y = df2, by = "CustomerId", all = TRUE)
      #
      # Left outer: merge(x = df1, y = df2, by = "CustomerId", all.x = TRUE)
      #
      # Right outer: merge(x = df1, y = df2, by = "CustomerId", all.y = TRUE)
      #
      # Cross join: merge(x = df1, y = df2, by = NULL)
      #
      # Just as with the inner join, you would probably want to explicitly pass
      # "CustomerId" to R as the matching variable. I think it's almost always
      # best to explicitly state the identifiers on which you want to merge;
      # it's safer if the input data.frames change unexpectedly and easier to
      # read later on.
      #
      # You can merge on multiple columns by giving by a vector, e.g., by =
      # c("CustomerId", "OrderId").
      #
      # If the column names to merge on are not the same, you can specify, e.g.,
      # by.x = "CustomerId_in_df1", by.y = "CustomerId_in_df2" where
      # CustomerId_in_df1 is the name of the column in the first data frame and
      # CustomerId_in_df2 is the name of the column in the second data frame.
      # (These can also be vectors if you need to merge on multiple columns.)





########rvest Basics#####################################################################


      #read_html(url) : scrape HTML content from a given URL
      #html_nodes(): identifies HTML wrappers.
      #html_nodes(“.class”): calls node based on CSS class
      #html_nodes(“#id”): calls node based on <div> id
      #html_nodes(xpath=”xpath”): calls node based on xpath (we’ll cover this later)
      #html_attrs(): identifies attributes (useful for debugging)
      #html_table(): turns HTML tables into data frames
      #html_text(): strips the HTML tags and extracts only the text

