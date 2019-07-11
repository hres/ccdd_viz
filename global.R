library(shiny)
library(shinyjs)
library(shinythemes)
library(pool)
library(DBI)
library(dbplyr)
library(dplyr)
library(networkD3)
library(DT)
library(purrr)

# establish database connection

ccdd <- dbPool(drv      = RPostgreSQL::PostgreSQL(),
               host     = "#####",
               dbname   = "#####",
               user     = "####",
               password = "#####" )

tm_list<-tbl(ccdd,sql("select distinct ds.tm_formal_name
                       from public.ccdd_tm_release_candidate as ds"))%>%collect()
  
