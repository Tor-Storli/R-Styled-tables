library(quantmod)
library(tidyverse)
library(gt)
library(scales)

startdate = as.Date("2014-01-01")
enddate = as.Date("2023-10-01")

# Get daily stock prices for AbbVie Inc.
getSymbols("ABBV", from=startdate, to=enddate)






















#  Convert Daily stock prices to monthly stock prices
monthly_prices <- to.period(ABBV, period = 'months', OHLC = FALSE)


# View sample results
head(monthly_prices)
tail(monthly_prices)





















#  Convert data to a tibble data object
#  Also convert the date index into its own column
ABBV_data <- as_tibble(monthly_prices) |>
  mutate(Date = index(monthly_prices))




















# Clean up the ABBV tibble table 
ABBV_tibble = ABBV_data |>
  rename(Open = ABBV.Open,
         High = ABBV.High,
         Low = ABBV.Low,
         Close = ABBV.Close,
         Volume = ABBV.Volume,
         Adjusted = ABBV.Adjusted) |>
  dplyr::select(Date, Open, High, Low, Close, Volume, Adjusted)


# Create a gt table
ABBV_gt <-  gt(ABBV_tibble)























# Let us style the gt table by adding a title and a sub-title
ABBV_tab <- ABBV_gt |>
  tab_header(
  title = "AbbVie Inc. Daily Stock Prices",
  subtitle = "From: 1/1/2014   To: 10-1-2023"
  ) |>
  cols_width(c(Date:Adjusted) ~px(120))

ABBV_tab
  

  

#  We can also use markdown syntax to style the table further 
#  using Markdown function in the gt package:
  ABBV_tab |> 
  tab_header(
    title = md("**<font color=blue>AbbVie Inc. Daily Stock Prices</font>**"),
    subtitle = md("**From:** 1/1/2014 **To:** 10-1-2023")
  )

#  Find the min and max Adjusted price values along with their row and column position
row_with_min_value <- as.integer(which.min(ABBV_tibble$Adjusted))
row_with_max_value <- as.integer(which.max(ABBV_tibble$Adjusted))

column_with_min_value <- which.min(ABBV_tibble[row_with_min_value, ])
column_with_min_value <- which.max(ABBV_tibble[row_with_max_value, ])


min_price_volume <- ABBV_tibble$Volume[row_with_min_value]
max_price_volume <- ABBV_tibble$Volume[row_with_max_value]

min_value <- min(ABBV_tibble$Adjusted)
max_value <- max(ABBV_tibble$Adjusted)

#  Generate Min and Max footnote strings using markdow-html syntax
footnoteL <- "<b><font color=blue>Lowest Adjusted Price: </font></b>"
lowprice <- paste("<b><font color=red>$", as.character(round(min_value, 2)), "</font></b>")
footnoteL <- paste(footnoteL, lowprice)

footnoteH <- "<b><font color=blue>Highest Adjusted Price: </font></b>"
highprice <- paste("<b><font color=Darkgreen>$", as.character(round(max_value, 2)), "</font></b>")
footnoteH <- paste(footnoteH, highprice)

footnoteLV <- "<b><font color=blue>Volume at the Lowest Adjusted Price: </font></b>"
lowV <- paste("<b><font color=red>", as.character(round(min_price_volume, 2)), "</font></b>")
footnoteLV <- paste(footnoteLV, lowV)

footnoteHV <- "<b><font color=blue>Volume at the Highest Adjusted Price: </font></b>"
highV <- paste("<b><font color=Darkgreen>", as.character(round(max_price_volume, 2)), "</font></b>")
footnoteHV <- paste(footnoteHV, highV)

#  Create a table that displays the Min and Max Adjusted price 
ABBV_tab |>
    tab_footnote(
           footnote = md(footnoteL),
           locations = (cells_body(columns=Adjusted, rows = row_with_min_value))
         ) |>
  tab_footnote(
    footnote = md(footnoteH),
    locations = (cells_body(columns=Adjusted, rows = row_with_max_value))
  )

#  Create a table that displays the Min and Max Adjusted price 
# along with the trading volume on those dates
ABBV_m <- 
  ABBV_gt |>
  tab_header(
    title = md("**<font color=blue>AbbVie Inc. Daily Stock Prices</font>**"),
    subtitle = md("**From:** 1/1/2014 **To:** 10-1-2023")
  ) |>
  tab_source_note(md("<b><i><font color=purple>Source: Yahoo Finance</font></i></b>")) |>
  tab_footnote(
    footnote = md(footnoteL),
    locations = (cells_body(columns=Adjusted, rows = row_with_min_value))
  ) |>
  tab_footnote(
    footnote = md(footnoteLV),
    locations = cells_body(
      columns = Volume, rows = row_with_min_value
    )
  ) |>
  tab_footnote(
    footnote = md(footnoteH),
    locations = (cells_body(columns=Adjusted, rows = row_with_max_value))
  ) |>
  tab_footnote(
    footnote = md(footnoteHV),
    locations = cells_body(
      columns = Volume, rows = row_with_max_value
    ) ) |>
      cols_width(c(Date:Adjusted) ~px(120))
ABBV_m



#  Get some information about how we can use different currency symbols
info_currencies()

#  Format the table with currency symbols - EURO
ABBV_m |>
  fmt_currency(rows= everything(), 
               columns = c(Open:Close, Adjusted), 
               # columns = c(Open, High, Low, Close, Adjusted), 
               use_seps = FALSE, 
               currency= "EUR"
  )


#  Format the table with currency symbols - USD
#  Also change the font
ABBV_tab_font <- ABBV_m |>
  fmt_currency(rows= everything(), 
               columns = c(Open, High, Low, Close, Adjusted), 
               use_seps = FALSE, 
               currency= "USD") |>
  opt_table_font(font= list(
                            google_font(name="roboto"),
                            "verdana",
                            "calibri",
                           "Arial"
                           )
                )
ABBV_tab_font


ABBV_tab_font |>
  cols_align(
    align = "center",  # align = "left"
    columns = c(Date:Adjusted) 
  )



ABBV_tibble |>
  gt() |>
  data_color(
    method = "numeric",
    palette = c("red", "green")
  )



ABBV_gt |>
  data_color(
    columns = Adjusted,
    rows = Adjusted > 70 & Adjusted < 170,
    method = "numeric",
    palette = "viridis",
    domain = c(70, 170)
  )

??data_color


ABBV_m |>
  data_color(
    columns = c(Open:Close), #everything(),     method = "numeric",
    palette = "RdYlGn",
    domain = c(min_value, max_value)
  ) |>
  tab_options(
    table.border.top.color = "black",
    table.border.bottom.color = "black",
    heading.border.bottom.color = "black",
    column_labels.border.bottom.color = "black",
    table_body.border.bottom.color = "black"
  ) |>
  cols_align(
    align = "center",  # align = "left"
    columns = c(Date:Adjusted) 
  )
