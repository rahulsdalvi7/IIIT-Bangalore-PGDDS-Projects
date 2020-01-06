library(SparkR)
library(ggplot2)
library(magrittr)
library(gridExtra)
library(XML)
library(RCurl)

# initiating the spark session
sparkR.session(master='local')
#sparkR.session.stop()


#Load 2015 parking violation dataframe
parking_violation_2015 <- read.df("s3://nyc-parking-tickets-spark/Parking_Violations_Issued_-_Fiscal_Year_2015.csv",
                                  source = "CSV", inferSchema = "true", header = "true")

#Load 2016 parking violation dataframe
parking_violation_2016 <- read.df("s3://nyc-parking-tickets-spark/Parking_Violations_Issued_-_Fiscal_Year_2016.csv",
                                  source = "CSV", inferSchema = "true", header = "true")
#Load 2016 parking violation dataframe
parking_violation_2017 <- read.df("s3://nyc-parking-tickets-spark/Parking_Violations_Issued_-_Fiscal_Year_2017.csv",
                                  source = "CSV", inferSchema = "true", header = "true")



#understanding 2015 data
nrow(parking_violation_2015)
ncol(parking_violation_2015)

#2015 Total Rows - 11809233
#2015 Total Cols - 51

str(parking_violation_2015)
# 'SparkDataFrame': 51 variables:
# $ Summons Number                   : num 8002531292 8015318440 7611181981 7445908067 7037692864 7704791394
# $ Plate ID                         : chr "EPC5238" "5298MD" "FYW2775" "GWE1987" "T671196C" "JJF6834"
# $ Registration State               : chr "NY" "NY" "NY" "NY" "NY" "PA"
# $ Plate Type                       : chr "PAS" "COM" "PAS" "PAS" "PAS" "PAS"
# $ Issue Date                       : chr "10/01/2014" "03/06/2015" "07/28/2014" "04/13/2015" "05/19/2015" "11/20/2014"
# $ Violation Code                   : int 21 14 46 19 19 21
# $ Vehicle Body Type                : chr "SUBN" "VAN" "SUBN" "4DSD" "4DSD" "4DSD"
# $ Vehicle Make                     : chr "CHEVR" "FRUEH" "SUBAR" "LEXUS" "CHRYS" "NISSA"
# $ Issuing Agency                   : chr "T" "T" "T" "T" "T" "T"
# $ Street Code1                     : int 20390 27790 8130 59990 36090 74230
# $ Street Code2                     : int 29890 19550 5430 16540 10410 37980
# $ Street Code3                     : int 31490 19570 5580 16790 24690 38030
# $ Vehicle Expiration Date          : chr "01/01/20150111 12:00:00 PM" "01/01/88888888 12:00:00 PM" "01/01/20160524 12:0
# $ Violation Location               : int 7 25 72 102 28 67
# $ Violation Precinct               : int 7 25 72 102 28 67
# $ Issuer Precinct                  : int 7 25 72 102 28 67
# $ Issuer Code                      : int 345454 333386 331845 355669 341248 357104
# $ Issuer Command                   : chr "T800" "T103" "T302" "T402" "T103" "T302"
# $ Issuer Squad                     : chr "A2" "B" "L" "D" "X" "A"
# $ Violation Time                   : chr "0011A" "0942A" "1020A" "0318P" "0410P" "0839A"
# $ Time First Observed              : chr "NA" "NA" "NA" "NA" "NA" "NA"
# $ Violation County                 : chr "NY" "NY" "K" "Q" "NY" "K"
# $ Violation In Front Of Or Opposite: chr "F" "F" "F" "F" "F" "F"
# $ House Number                     : chr "133" "1916" "184" "120-20" "66" "1013"
# $ Street Name                      : chr "Essex St" "Park Ave" "31st St" "Queens Blvd" "W 116th St" "Rutland Rd"
# $ Intersecting Street              : chr "NA" "NA" "NA" "NA" "NA" "NA"
# $ Date First Observed              : chr "01/05/0001 12:00:00 PM" "01/05/0001 12:00:00 PM" "01/05/0001 12:00:00 PM" "01
# $ Law Section                      : int 408 408 408 408 408 408
# $ Sub Division                     : chr "d1" "c" "f1" "c3" "c3" "d1"
# $ Violation Legal Code             : chr "NA" "NA" "NA" "NA" "NA" "NA"
# $ Days Parking In Effect           : chr "Y Y Y" "YYYYY" "NA" "YYYYY" "YYYYYYY" "Y"
# $ From Hours In Effect             : chr "1200A" "0700A" "NA" "0300P" "NA" "0830A"
# $ To Hours In Effect               : chr "0300A" "1000A" "NA" "1000P" "NA" "0900A"
# $ Vehicle Color                    : chr "BL" "BROWN" "BLACK" "GY" "BLACK" "WHITE"
# $ Unregistered Vehicle?            : int NA NA NA NA NA NA
# $ Vehicle Year                     : int 2005 0 2010 2015 0 0
# $ Meter Number                     : chr "NA" "NA" "NA" "NA" "NA" "NA"
# $ Feet From Curb                   : int 0 0 0 0 0 0
# $ Violation Post Code              : chr "A 77" "CC3" "J 32" "01 4" "19 7" "C 32"
# $ Violation Description            : chr "21-No Parking (street clean)" "14-No Standing" "46A-Double Parking (Non-COM)"
# $ No Standing or Stopping Violation: chr "NA" "NA" "NA" "NA" "NA" "NA"
# $ Hydrant Violation                : chr "NA" "NA" "NA" "NA" "NA" "NA"
# $ Double Parking Violation         : chr "NA" "NA" "NA" "NA" "NA" "NA"
# $ Latitude                         : chr "NA" "NA" "NA" "NA" "NA" "NA"
# $ Longitude                        : chr "NA" "NA" "NA" "NA" "NA" "NA"
# $ Community Board                  : chr "NA" "NA" "NA" "NA" "NA" "NA"
# $ Community Council                : chr "NA" "NA" "NA" "NA" "NA" "NA"
# $ Census Tract                     : chr "NA" "NA" "NA" "NA" "NA" "NA"
# $ BIN                              : chr "NA" "NA" "NA" "NA" "NA" "NA"
# $ BBL                              : chr "NA" "NA" "NA" "NA" "NA" "NA"
# $ NTA                              : chr "NA" "NA" "NA" "NA" "NA" "NA"

printSchema(parking_violation_2015)

#understanding 2016 data
nrow(parking_violation_2016)
ncol(parking_violation_2016)

#2016 Total Rows - 10626899
#2016 Total Cols - 51

str(parking_violation_2016)
# 'SparkDataFrame': 51 variables:
#   $ Summons Number                   : num 1363745270 1363745293 1363745438 1363745475 1363745487 1363745517
# $ Plate ID                         : chr "GGY6450" "KXD355" "JCK7576" "GYK7658" "GMT8141" "GYK3760"
# $ Registration State               : chr "99" "SC" "PA" "NY" "NY" "NY"
# $ Plate Type                       : chr "PAS" "PAS" "PAS" "OMS" "PAS" "PAS"
# $ Issue Date                       : chr "07/09/2015" "07/09/2015" "07/09/2015" "07/09/2015" "07/09/2015" "07/09/2015"
# $ Violation Code                   : int 46 21 21 21 21 21
# $ Vehicle Body Type                : chr "SDN" "SUBN" "SDN" "SUBN" "P-U" "SUBN"
# $ Vehicle Make                     : chr "HONDA" "CHEVR" "ME/BE" "NISSA" "LINCO" "HONDA"
# $ Issuing Agency                   : chr "P" "P" "P" "P" "P" "P"
# $ Street Code1                     : int 0 55730 42730 58130 58130 46730
# $ Street Code2                     : int 40404 67030 26730 18630 67030 58730
# $ Street Code3                     : int 40404 58730 26830 67030 58730 85730
# $ Vehicle Expiration Date          : int 20170602 20160288 0 0 20160206 20160709
# $ Violation Location               : int 74 79 79 79 79 79
# $ Violation Precinct               : int 74 79 79 79 79 79
# $ Issuer Precinct                  : int 301 301 0 301 301 301
# $ Issuer Code                      : int 358160 358160 358114 358114 358114 358114
# $ Issuer Command                   : chr "T301" "T301" "TEBN" "T301" "T301" "T301"
# $ Issuer Squad                     : chr "0000" "0000" "0000" "0000" "0000" "0000"
# $ Violation Time                   : chr "1037A" "1206P" "0820A" "0918A" "0925A" "0948A"
# $ Time First Observed              : chr "NA" "NA" "NA" "NA" "NA" "NA"
# $ Violation County                 : chr "K" "K" "K" "K" "K" "K"
# $ Violation In Front Of Or Opposite: chr "F" "F" "F" "F" "F" "F"
# $ House Number                     : chr "142" "331" "1087" "207" "237" "201"
# $ Street Name                      : chr "MACDOUNGH ST" "LEXINGTON AVE" "FULTON ST" "MADISON ST" "MADISON ST" "HALSEY S
# $ Intersecting Street              : chr "NA" "NA" "NA" "NA" "NA" "NA"
# $ Date First Observed              : int 0 0 0 0 0 0
# $ Law Section                      : int 408 408 408 408 408 408
# $ Sub Division                     : chr "D1" "F1" "D1" "D1" "D1" "D1"
# $ Violation Legal Code             : chr "NA" "NA" "NA" "NA" "NA" "NA"
# $ Days Parking In Effect           : chr "BBBBBBB" "YBBYBBB" "YBBYBBB" "YBBYBBB" "YBBYBBB" "YBBYBBB"
# $ From Hours In Effect             : chr "ALL" "1100A" "0800A" "0900A" "0900A" "0900A"
# $ To Hours In Effect               : chr "ALL" "1230P" "0930A" "1030" "1030A" "1030A"
# $ Vehicle Color                    : chr "WHITE" "RED" "WHITE" "BK" "BLK" "OTHER"
# $ Unregistered Vehicle?            : chr "0" "0" "0" "0" "0" "0"
# $ Vehicle Year                     : int 2010 0 0 2015 2006 2006
# $ Meter Number                     : chr "-" "-" "-" "-" "-" "-"
# $ Feet From Curb                   : int 0 0 0 0 0 0
# $ Violation Post Code              : chr "NA" "NA" "NA" "NA" "NA" "NA"
# $ Violation Description            : chr "NA" "NA" "NA" "NA" "NA" "NA"
# $ No Standing or Stopping Violation: chr "NA" "NA" "NA" "NA" "NA" "NA"
# $ Hydrant Violation                : chr "NA" "NA" "NA" "NA" "NA" "NA"
# $ Double Parking Violation         : chr "NA" "NA" "NA" "NA" "NA" "NA"
# $ Latitude                         : chr "NA" "NA" "NA" "NA" "NA" "NA"
# $ Longitude                        : chr "NA" "NA" "NA" "NA" "NA" "NA"
# $ Community Board                  : chr "NA" "NA" "NA" "NA" "NA" "NA"
# $ Community Council                : chr "NA" "NA" "NA" "NA" "NA" "NA"
# $ Census Tract                     : chr "NA" "NA" "NA" "NA" "NA" "NA"
# $ BIN                              : chr "NA" "NA" "NA" "NA" "NA" "NA"
# $ BBL                              : chr "NA" "NA" "NA" "NA" "NA" "NA"
# $ NTA                              : chr "NA" "NA" "NA" "NA" "NA" "NA"

printSchema(parking_violation_2016)

#understanding 2017 data
nrow(parking_violation_2017)
ncol(parking_violation_2017)

#2017 Total Rows - 10803028
#2017 Total Cols - 43

str(parking_violation_2017)
# 'SparkDataFrame': 43 variables:
#   $ Summons Number                   : num 5092469481 5092451658 4006265037 8478629828 7868300310 5096917368
# $ Plate ID                         : chr "GZH7067" "GZH7067" "FZX9232" "66623ME" "37033JV" "FZD8593"
# $ Registration State               : chr "NY" "NY" "NY" "NY" "NY" "NY"
# $ Plate Type                       : chr "PAS" "PAS" "PAS" "COM" "COM" "PAS"
# $ Issue Date                       : chr "07/10/2016" "07/08/2016" "08/23/2016" "06/14/2017" "11/21/2016" "06/13/2017"
# $ Violation Code                   : int 7 7 5 47 69 7
# $ Vehicle Body Type                : chr "SUBN" "SUBN" "SUBN" "REFG" "DELV" "SUBN"
# $ Vehicle Make                     : chr "TOYOT" "TOYOT" "FORD" "MITSU" "INTER" "ME/BE"
# $ Issuing Agency                   : chr "V" "V" "V" "T" "T" "V"
# $ Street Code1                     : int 0 0 0 10610 10510 0
# $ Street Code2                     : int 0 0 0 34330 34310 0
# $ Street Code3                     : int 0 0 0 34350 34330 0
# $ Vehicle Expiration Date          : int 0 0 0 20180630 20170228 0
# $ Violation Location               : int NA NA NA 14 13 NA
# $ Violation Precinct               : int 0 0 0 14 13 0
# $ Issuer Precinct                  : int 0 0 0 14 13 0
# $ Issuer Code                      : int 0 0 0 359594 364832 0
# $ Issuer Command                   : chr "NA" "NA" "NA" "T102" "T102" "NA"
# $ Issuer Squad                     : chr "NA" "NA" "NA" "J" "M" "NA"
# $ Violation Time                   : chr "0143A" "0400P" "0233P" "1120A" "0555P" "0852P"
# $ Time First Observed              : chr "NA" "NA" "NA" "NA" "NA" "NA"
# $ Violation County                 : chr "BX" "BX" "BX" "NY" "NY" "QN"
# $ Violation In Front Of Or Opposite: chr "NA" "NA" "NA" "O" "F" "NA"
# $ House Number                     : chr "NA" "NA" "NA" "330" "799" "NA"
# $ Street Name                      : chr "ALLERTON AVE (W/B) @" "ALLERTON AVE (W/B) @" "SB WEBSTER AVE @ E 1" "7th Ave"
# $ Intersecting Street              : chr "BARNES AVE" "BARNES AVE" "94TH ST" "NA" "NA" "@ MARATHON PKWY"
# $ Date First Observed              : int 0 0 0 0 0 0
# $ Law Section                      : int 1111 1111 1111 408 408 1111
# $ Sub Division                     : chr "D" "D" "C" "l2" "h1" "D"
# $ Violation Legal Code             : chr "T" "T" "T" "NA" "NA" "T"
# $ Days Parking In Effect           : chr "NA" "NA" "NA" "Y" "Y" "NA"
# $ From Hours In Effect             : chr "NA" "NA" "NA" "0700A" "0700A" "NA"
# $ To Hours In Effect               : chr "NA" "NA" "NA" "0700P" "0700P" "NA"
# $ Vehicle Color                    : chr "GY" "GY" "BK" "WH" "WHITE" "WH"
# $ Unregistered Vehicle?            : int NA NA NA NA NA NA
# $ Vehicle Year                     : int 2001 2001 2004 2007 2007 2012
# $ Meter Number                     : chr "NA" "NA" "NA" "NA" "NA" "NA"
# $ Feet From Curb                   : int 0 0 0 0 0 0
# $ Violation Post Code              : chr "NA" "NA" "NA" "04" "31 6" "NA"
# $ Violation Description            : chr "FAILURE TO STOP AT RED LIGHT" "FAILURE TO STOP AT RED LIGHT" "BUS LANE VIOLAT
# $ No Standing or Stopping Violation: chr "NA" "NA" "NA" "NA" "NA" "NA"
# $ Hydrant Violation                : chr "NA" "NA" "NA" "NA" "NA" "NA"
# $ Double Parking Violation         : chr "NA" "NA" "NA" "NA" "NA" "NA"
printSchema(parking_violation_2017)

#changing the type of issue date from chr to date
#for year 2015
head(select(parking_violation_2015, parking_violation_2015$`Summons Number`, parking_violation_2015$`Issue Date`),10)
parking_violation_2015$issue_date_formatted = to_date(parking_violation_2015$`Issue Date`,"MM/dd/yyyy")
head(select(parking_violation_2015, parking_violation_2015$`Summons Number`, parking_violation_2015$issue_date_formatted, parking_violation_2015$`Issue Date`),10)

#for year 2016
head(select(parking_violation_2016, parking_violation_2016$`Summons Number`, parking_violation_2016$`Issue Date`),10)
parking_violation_2016$issue_date_formatted = to_date(parking_violation_2016$`Issue Date`,"MM/dd/yyyy")
head(select(parking_violation_2016, parking_violation_2016$`Summons Number`, parking_violation_2016$issue_date_formatted, parking_violation_2016$`Issue Date`),10)

#for year 2017
head(select(parking_violation_2017, parking_violation_2017$`Summons Number`, parking_violation_2017$`Issue Date`),10)
parking_violation_2017$issue_date_formatted = to_date(parking_violation_2017$`Issue Date`,"MM/dd/yyyy")
head(select(parking_violation_2017, parking_violation_2017$`Summons Number`, parking_violation_2017$issue_date_formatted, parking_violation_2017$`Issue Date`),10)

#dropping duplicate rows from the data frames
parking_violation_2015 = distinct(parking_violation_2015)
parking_violation_2016 = distinct(parking_violation_2016)
parking_violation_2017 = distinct(parking_violation_2017)

# Create table view for sql queries
createOrReplaceTempView(parking_violation_2015,'parking_violation_2015')
createOrReplaceTempView(parking_violation_2016,'parking_violation_2016')
createOrReplaceTempView(parking_violation_2017,'parking_violation_2017')

head(SparkR::sql("select year(issue_date_formatted) year, COUNT(`Summons Number`) from parking_violation_2015 group by year(issue_date_formatted)"),100)
head(SparkR::sql("select year(issue_date_formatted) year, COUNT(`Summons Number`) from parking_violation_2016 group by year(issue_date_formatted)"),100)
head(SparkR::sql("select year(issue_date_formatted) year, COUNT(`Summons Number`) from parking_violation_2017 group by year(issue_date_formatted)"),100)
# we can see that there is data of year other than the mentioned in the file name
# we are assuming that all year wise file contains only data for that particular year and does not contain other year data. Data from other years are ignored as per our assumptions

#dropping duplicate rows from the data frames
dropDuplicates(parking_violation_2015)
dropDuplicates(parking_violation_2016)
dropDuplicates(parking_violation_2017)

# removing the years data for the year mentioned in the file name
parking_violation_2015 = filter(parking_violation_2015, year(parking_violation_2015$issue_date_formatted) == 2015)
parking_violation_2016 = filter(parking_violation_2016, year(parking_violation_2016$issue_date_formatted) == 2016)
parking_violation_2017 = filter(parking_violation_2017, year(parking_violation_2017$issue_date_formatted) == 2017)

dim(parking_violation_2015)
dim(parking_violation_2016)
dim(parking_violation_2016)

# Create table view for sql queries
createOrReplaceTempView(parking_violation_2015,'parking_violation_2015')
createOrReplaceTempView(parking_violation_2016,'parking_violation_2016')
createOrReplaceTempView(parking_violation_2017,'parking_violation_2017')

################################################################################################################
#### Examine the data.
# 1.Find total number of tickets for each year.
ticket_count_2015 = head(SparkR::sql("select year(issue_date_formatted) year, COUNT(DISTINCT `Summons Number`) no_of_tickets from parking_violation_2015 group by year(issue_date_formatted)"))
ticket_count_2016 = head(SparkR::sql("select year(issue_date_formatted) year, COUNT(DISTINCT `Summons Number`)  no_of_tickets from parking_violation_2016 group by year(issue_date_formatted)"))
ticket_count_2017 = head(SparkR::sql("select year(issue_date_formatted) year, COUNT(DISTINCT `Summons Number`)  no_of_tickets from parking_violation_2017 group by year(issue_date_formatted)"))

#combining all the data in a single data frame
ticket_count_final <- rbind(ticket_count_2015,ticket_count_2016,ticket_count_2017)
head(ticket_count_final)
#   year       no_of_tickets
# 1 2015       5373971
# 2 2016       4872621
# 3 2017       5431918
# the tickets in 2017 has maximum tickets and 2015 has least number of tickets

ggplot(ticket_count_final, aes(x = year, y = no_of_tickets)) + 
  geom_bar(stat = "identity") + 
  scale_y_continuous(name="Ticket Count", labels = scales::comma)


# 2.Find out how many unique states the cars which got parking tickets came from.
colnames(parking_violation_2015)
states_count_2015 = head(SparkR::sql("select year(issue_date_formatted) year, COUNT(DISTINCT `Registration State`) no_of_states from parking_violation_2015 group by year(issue_date_formatted)"),500)
states_count_2016 = head(SparkR::sql("select year(issue_date_formatted) year, COUNT(DISTINCT `Registration State`)  no_of_states from parking_violation_2016 group by year(issue_date_formatted)"),500)
states_count_2017 = head(SparkR::sql("select year(issue_date_formatted) year, COUNT(DISTINCT `Registration State`)  no_of_states from parking_violation_2017 group by year(issue_date_formatted)"),500)

#combining all the data in a single data frame
states_count_final <- rbind(states_count_2015,states_count_2016,states_count_2017)
head(states_count_final)
#   year           no_of_states
# 1 2015           68
# 2 2016           67
# 3 2017           65
# the tickets in 2017 has maximum tickets and 2016 has least number of tickets

ggplot(states_count_final, aes(x = year, y = no_of_states)) + 
  geom_bar(stat = "identity") + 
  scale_y_continuous(name="Registration States Count")


# 3. Some parking tickets don't have addresses on them, which is cause for concern. Find out how many such tickets there are.
#Assuming Violation location as the address, since we are talking about the address on the parking ticket only 
violation_location_count_2015 = head(SparkR::sql("select year(issue_date_formatted) year, COUNT(DISTINCT `Summons Number`) no_of_ticktes_without_address from parking_violation_2015 where `Violation Location` IS NULL group by year(issue_date_formatted)"),500)
violation_location_count_2016 = head(SparkR::sql("select year(issue_date_formatted) year, COUNT(DISTINCT `Summons Number`) no_of_ticktes_without_address from parking_violation_2016 where `Violation Location` IS NULL group by year(issue_date_formatted)"),500)
violation_location_count_2017 = head(SparkR::sql("select year(issue_date_formatted) year, COUNT(DISTINCT `Summons Number`) no_of_ticktes_without_address from parking_violation_2017 where `Violation Location` IS NULL group by year(issue_date_formatted)"),500)

heads(violation_location_count_2017)
#combining all the data in a single data frame
violation_location_count_final <- rbind(violation_location_count_2015,violation_location_count_2016,violation_location_count_2017)
head(violation_location_count_final)
#   year                        no_of_ticktes_without_address
# 1 2015                        721275
# 2 2016                        828348
# 3 2017                        925596

ggplot(violation_location_count_final, aes(x = year, y = no_of_ticktes_without_address)) + 
  geom_bar(stat = "identity") + 
  scale_y_continuous(name="No. of Tickets without address")

################################################################################################################
## Aggregation tasks
# 1. How often does each violation code occur? (frequency of violation codes - find the top 5)
violation_code_counts_2015 <- summarize(groupBy(parking_violation_2015, parking_violation_2015$`Violation Code`),
                                        count = n(parking_violation_2015$`Violation Code`))
violation_code_counts_2015 = head(arrange(violation_code_counts_2015, desc(violation_code_counts_2015$count)), n=5)
violation_code_counts_2015$year = 2015
head(violation_code_counts_2015)
#   Violation Code  count year
# 1             21 720902 2015
# 2             38 663904 2015
# 3             14 466488 2015
# 4             36 406249 2015
# 5             37 373229 2015


violation_code_counts_2016 <- summarize(groupBy(parking_violation_2016, parking_violation_2016$`Violation Code`),
                                        count = n(parking_violation_2016$`Violation Code`))
violation_code_counts_2016 = head(arrange(violation_code_counts_2016, desc(violation_code_counts_2016$count)), n=5)
violation_code_counts_2016$year = 2016
head(violation_code_counts_2016)
#   Violation Code  count year
# 1             21 664947 2016
# 2             36 615242 2016
# 3             38 547080 2016
# 4             14 405885 2016
# 5             37 330489 2016

violation_code_counts_2017 <- summarize(groupBy(parking_violation_2017, parking_violation_2017$`Violation Code`),
                                        count = n(parking_violation_2017$`Violation Code`))
violation_code_counts_2017 = head(arrange(violation_code_counts_2017, desc(violation_code_counts_2017$count)), n=5)
violation_code_counts_2017$year = 2017
head(violation_code_counts_2017)
#   Violation Code  count year
# 1             21 768087 2017
# 2             36 662765 2017
# 3             38 542079 2017
# 4             14 476664 2017
# 5             20 319646 2017



# plotting the top 5 violation codes on year basis
y_2015 = ggplot(violation_code_counts_2015, aes(x = reorder(as.factor(`Violation Code`), -count), y = count)) + 
  geom_bar(stat= "identity" ,position = "dodge") + 
  scale_y_continuous(name="Violation code counts", labels = scales::comma) + 
  ggtitle("Top 5 violation codes for 2015 ")  + 
  xlab("Violation Codes")

y_2016 = ggplot(violation_code_counts_2016, aes(x = reorder(as.factor(`Violation Code`), -count), y = count)) + 
  geom_bar(stat= "identity" ,position = "dodge") + 
  scale_y_continuous(name="Violation code counts", labels = scales::comma)+ 
  ggtitle("Top 5 violation codes for 2016 ") + 
  xlab("Violation Codes")

y_2017 = ggplot(violation_code_counts_2017, aes(x = reorder(as.factor(`Violation Code`), -count), y = count)) + 
  geom_bar(stat= "identity" ,position = "dodge") + 
  scale_y_continuous(name="Violation code counts", labels = scales::comma) + 
  ggtitle("Top 5 violation codes for 2017 ") + 
  xlab("Violation Codes")

grid.arrange(y_2015, y_2016,y_2017, nrow=3)

#-----------------------------------------------------------------------------------------------------------------
# 2. How often does each vehicle body type get a parking ticket? How about the vehicle make? (find the top 5 for both)
vehicle_body_type_counts_2015 <- summarize(groupBy(parking_violation_2015, parking_violation_2015$`Vehicle Body Type`),
                                           count = n_distinct(parking_violation_2015$`Summons Number`))
vehicle_body_type_counts_2015 = head(arrange(vehicle_body_type_counts_2015, desc(vehicle_body_type_counts_2015$count)), n=5)
head(vehicle_body_type_counts_2015)
#   Vehicle Body Type   count
# 1              SUBN 1715517
# 2              4DSD 1514580
# 3               VAN  795457
# 4              DELV  419548
# 5               SDN  209381


vehicle_body_type_counts_2016 <- summarize(groupBy(parking_violation_2016, parking_violation_2016$`Vehicle Body Type`),
                                           count = n_distinct(parking_violation_2016$`Summons Number`))
vehicle_body_type_counts_2016 = head(arrange(vehicle_body_type_counts_2016, desc(vehicle_body_type_counts_2016$count)), n=5)
head(vehicle_body_type_counts_2016)
#   Vehicle Body Type   count
# 1              SUBN 1596326
# 2              4DSD 1354001
# 3               VAN  722234
# 4              DELV  354388
# 5               SDN  178954

vehicle_body_type_counts_2017 <- summarize(groupBy(parking_violation_2017, parking_violation_2017$`Vehicle Body Type`),
                                           count = n_distinct(parking_violation_2017$`Summons Number`))
vehicle_body_type_counts_2017 = head(arrange(vehicle_body_type_counts_2017, desc(vehicle_body_type_counts_2017$count)), n=5)
head(vehicle_body_type_counts_2017)
#   Vehicle Body Type   count
# 1              SUBN 1883954
# 2              4DSD 1547312
# 3               VAN  724029
# 4              DELV  358984
# 5               SDN  194197

# plotting the top 5 Vehicle Body Type on year basis
y_2015 = ggplot(vehicle_body_type_counts_2015, aes(x = reorder(as.factor(`Vehicle Body Type`), -count), y = count)) + 
  geom_bar(stat= "identity" ,position = "dodge") + 
  scale_y_continuous(name="No. of Tickets", labels = scales::comma) + 
  ggtitle("Top 5 Vehicle Body Type by parking tickets for 2015 ")  + 
  xlab("Vehicle Body Type")

y_2016 = ggplot(vehicle_body_type_counts_2016, aes(x = reorder(as.factor(`Vehicle Body Type`), -count), y = count)) + 
  geom_bar(stat= "identity" ,position = "dodge") + 
  scale_y_continuous(name="No. of Tickets", labels = scales::comma) + 
  ggtitle("Top 5 Vehicle Body Type by parking tickets for 2016 ")  + 
  xlab("Vehicle Body Type")

y_2017 = ggplot(vehicle_body_type_counts_2017, aes(x = reorder(as.factor(`Vehicle Body Type`), -count), y = count)) + 
  geom_bar(stat= "identity" ,position = "dodge") + 
  scale_y_continuous(name="No. of Tickets", labels = scales::comma) + 
  ggtitle("Top 5 Vehicle Body Type by parking tickets for 2017 ")  + 
  xlab("Vehicle Body Type")

grid.arrange(y_2015, y_2016,y_2017, nrow=3)


# checking for top 5 vehicle make for year on tear
vehicle_make_counts_2015 <- summarize(groupBy(parking_violation_2015, parking_violation_2015$`Vehicle Make`),
                                      count = n_distinct(parking_violation_2015$`Summons Number`))
vehicle_make_counts_2015 = head(arrange(vehicle_make_counts_2015, desc(vehicle_make_counts_2015$count)), n=5)
head(vehicle_make_counts_2015)
#   Vehicle Make  count
# 1         FORD 685900
# 2        TOYOT 554392
# 3        HONDA 498858
# 4        NISSA 411857
# 5        CHEVR 404841

vehicle_make_counts_2016 <- summarize(groupBy(parking_violation_2016, parking_violation_2016$`Vehicle Make`),
                                      count = n_distinct(parking_violation_2016$`Summons Number`))
vehicle_make_counts_2016 = head(arrange(vehicle_make_counts_2016, desc(vehicle_make_counts_2016$count)), n=5)
head(vehicle_make_counts_2016)
#   Vehicle Make  count
# 1         FORD 612276
# 2        TOYOT 529115
# 3        HONDA 459469
# 4        NISSA 382082
# 5        CHEVR 339466


vehicle_make_counts_2017 <- summarize(groupBy(parking_violation_2017, parking_violation_2017$`Vehicle Make`),
                                      count = n_distinct(parking_violation_2017$`Summons Number`))
vehicle_make_counts_2017 = head(arrange(vehicle_make_counts_2017, desc(vehicle_make_counts_2017$count)), n=5)
head(vehicle_make_counts_2017)
#   Vehicle Make  count
# 1         FORD 636844
# 2        TOYOT 605291
# 3        HONDA 538884
# 4        NISSA 462017
# 5        CHEVR 356032

# plotting the top 5 Vehicle Make on year basis
y_2015 = ggplot(vehicle_make_counts_2015, aes(x = reorder(as.factor(`Vehicle Make`), -count), y = count)) + 
  geom_bar(stat= "identity" ,position = "dodge") + 
  scale_y_continuous(name="No. of tickets count", labels = scales::comma) + 
  ggtitle("Top 5 Vehicle Make for 2015 ")  + 
  xlab("Vehicle Make")

y_2016 = ggplot(vehicle_make_counts_2016, aes(x = reorder(as.factor(`Vehicle Make`), -count), y = count)) + 
  geom_bar(stat= "identity" ,position = "dodge") + 
  scale_y_continuous(name="No. of tickets count", labels = scales::comma) + 
  ggtitle("Top 5 Vehicle Make for 2016 ")  + 
  xlab("Vehicle Make")

y_2017 = ggplot(vehicle_make_counts_2017, aes(x = reorder(as.factor(`Vehicle Make`), -count), y = count)) + 
  geom_bar(stat= "identity" ,position = "dodge") + 
  scale_y_continuous(name="No. of tickets count", labels = scales::comma) + 
  ggtitle("Top 5 Vehicle Make for 2017 ")  + 
  xlab("Vehicle Make")

grid.arrange(y_2015, y_2016,y_2017, nrow=3)

#-----------------------------------------------------------------------------------------------------------------
# 3. A precinct is a police station that has a certain zone of the city under its command. Find the (5 highest) frequencies of:
# I. Violating Precincts (this is the precinct of the zone where the violation occurred)
violation_precinct_counts_2015 <- summarize(groupBy(parking_violation_2015, parking_violation_2015$`Violation Precinct`),
                                            count = n(parking_violation_2015$`Violation Precinct`))
violation_precinct_counts_2015 = head(arrange(violation_precinct_counts_2015, desc(violation_precinct_counts_2015$count)), n=5)
head(violation_precinct_counts_2015)
#   Violation Precinct  count
# 1                  0 721275
# 2                 19 287403
# 3                 14 197011
# 4                 18 193593
# 5                  1 152040

violation_precinct_counts_2016 <- summarize(groupBy(parking_violation_2016, parking_violation_2016$`Violation Precinct`),
                                            count = n(parking_violation_2016$`Violation Precinct`))
violation_precinct_counts_2016 = head(arrange(violation_precinct_counts_2016, desc(violation_precinct_counts_2016$count)), n=5)
head(violation_precinct_counts_2016)
#   Violation Precinct  count
# 1                  0 828348
# 2                 19 264299
# 3                 13 156144
# 4                  1 152231
# 5                 14 150637

violation_precinct_counts_2017 <- summarize(groupBy(parking_violation_2017, parking_violation_2017$`Violation Precinct`),
                                            count = n(parking_violation_2017$`Violation Precinct`))
violation_precinct_counts_2017 = head(arrange(violation_precinct_counts_2017, desc(violation_precinct_counts_2017$count)), n=5)
head(violation_precinct_counts_2017)
#   Violation Precinct  count
# 1                  0 925596
# 2                 19 274445
# 3                 14 203553
# 4                  1 174702
# 5                 18 169131

# plotting the top 5 Violating Precincts on year basis
y_2015 = ggplot(violation_precinct_counts_2015, aes(x = reorder(as.factor(`Violation Precinct`), -count), y = count)) + 
  geom_bar(stat= "identity" ,position = "dodge") + 
  scale_y_continuous(name="No. of tickets count", labels = scales::comma) + 
  ggtitle("Top 5 Violating Precincts for 2015 ")  + 
  xlab("Violation Precinct")

y_2016 = ggplot(violation_precinct_counts_2016, aes(x = reorder(as.factor(`Violation Precinct`), -count), y = count)) + 
  geom_bar(stat= "identity" ,position = "dodge") + 
  scale_y_continuous(name="No. of tickets count", labels = scales::comma) + 
  ggtitle("Top 5 Violating Precincts for 2016 ")  + 
  xlab("Violation Precinct")

y_2017 = ggplot(violation_precinct_counts_2017, aes(x = reorder(as.factor(`Violation Precinct`), -count), y = count)) + 
  geom_bar(stat= "identity" ,position = "dodge") + 
  scale_y_continuous(name="No. of tickets count", labels = scales::comma) + 
  ggtitle("Top 5 Violating Precincts for 2017 ")  + 
  xlab("Violation Precinct")

grid.arrange(y_2015, y_2016,y_2017, nrow=3)



# II. Issuing Precincts (this is the precinct that issued the ticket)
issuer_precinct_counts_2015 <- summarize(groupBy(parking_violation_2015, parking_violation_2015$`Issuer Precinct`),
                                         count = n(parking_violation_2015$`Issuer Precinct`))
issuer_precinct_counts_2015 = head(arrange(issuer_precinct_counts_2015, desc(issuer_precinct_counts_2015$count)), n=5)
head(issuer_precinct_counts_2015)
#    Issuer Precinct  count
# 1               0 828570
# 2              19 279931
# 3              14 190403
# 4              18 190337
# 5             114 149532


issuer_precinct_counts_2016 <- summarize(groupBy(parking_violation_2016, parking_violation_2016$`Issuer Precinct`),
                                         count = n(parking_violation_2016$`Issuer Precinct`))
issuer_precinct_counts_2016 = head(arrange(issuer_precinct_counts_2016, desc(issuer_precinct_counts_2016$count)), n=5)
head(issuer_precinct_counts_2016)
#   Issuer Precinct  count
# 1               0 948438
# 2              19 258049
# 3              13 153478
# 4               1 146987
# 5              14 146165

issuer_precinct_counts_2017 <- summarize(groupBy(parking_violation_2017, parking_violation_2017$`Issuer Precinct`),
                                         count = n(parking_violation_2017$`Issuer Precinct`))
issuer_precinct_counts_2017 = head(arrange(issuer_precinct_counts_2017, desc(issuer_precinct_counts_2017$count)), n=5)
head(issuer_precinct_counts_2017)
#   Issuer Precinct   count
# 1               0 1078406
# 2              19  266961
# 3              14  200495
# 4               1  168740
# 5              18  162994

# plotting the top 5 Issuer Precincts on year basis
y_2015 = ggplot(issuer_precinct_counts_2015, aes(x = reorder(as.factor(`Issuer Precinct`), -count), y = count)) + 
  geom_bar(stat= "identity" ,position = "dodge") + 
  scale_y_continuous(name="No. of tickets count", labels = scales::comma) + 
  ggtitle("Top 5 Issuer Precincts for 2015 ")  + 
  xlab("Issuer Precinct")

y_2016 = ggplot(issuer_precinct_counts_2016, aes(x = reorder(as.factor(`Issuer Precinct`), -count), y = count)) + 
  geom_bar(stat= "identity" ,position = "dodge") + 
  scale_y_continuous(name="No. of tickets count", labels = scales::comma) + 
  ggtitle("Top 5 Issuer Precincts for 2016 ")  + 
  xlab("Issuer Precinct")

y_2017 = ggplot(issuer_precinct_counts_2017, aes(x = reorder(as.factor(`Issuer Precinct`), -count), y = count)) + 
  geom_bar(stat= "identity" ,position = "dodge") + 
  scale_y_continuous(name="No. of tickets count", labels = scales::comma) + 
  ggtitle("Top 5 Issuer Precincts for 2017 ")  + 
  xlab("Issuer Precinct")

grid.arrange(y_2015, y_2016,y_2017, nrow=3)

#-----------------------------------------------------------------------------------------------------------------

# 4. Find the violation code frequency across 3 precincts which have issued the most number of tickets - do these precinct zones have an exceptionally high frequency of certain violation codes? Are these codes common across precincts? 
# for 2015
# lets see for Issuer Precinct 0,9,14
# for Issuer Precinct 0
issuer_violation_code0_counts_2015 <- summarize(groupBy(filter(parking_violation_2015, parking_violation_2015$`Issuer Precinct` == 0), parking_violation_2015$`Violation Code`),
                                                count = n(parking_violation_2015$`Summons Number`))
issuer_violation_code0_counts_2015 <-  SparkR::collect(arrange(issuer_violation_code0_counts_2015, desc(issuer_violation_code0_counts_2015$count)))
head(issuer_violation_code0_counts_2015,5)
#   Violation Code  count
# 1             36 406249
# 2              7 253730
# 3             21  96218
# 4              5  55192
# 5             66   2343

# for Issuer Precinct 19
issuer_violation_code19_counts_2015 <- summarize(groupBy(filter(parking_violation_2015, parking_violation_2015$`Issuer Precinct` == 19), parking_violation_2015$`Violation Code`),
                                                 count = n(parking_violation_2015$`Summons Number`))
issuer_violation_code19_counts_2015 <-  SparkR::collect(arrange(issuer_violation_code19_counts_2015, desc(issuer_violation_code19_counts_2015$count)))
head(issuer_violation_code19_counts_2015,5)
#   Violation Code count
# 1             38 45647
# 2             37 40665
# 3             14 31295
# 4             16 29738
# 5             46 27049

# for Issuer Precinct 14
issuer_violation_code14_counts_2015 <- summarize(groupBy(filter(parking_violation_2015, parking_violation_2015$`Issuer Precinct` == 14), parking_violation_2015$`Violation Code`),
                                                 count = n(parking_violation_2015$`Summons Number`))
issuer_violation_code14_counts_2015 <-  SparkR::collect(arrange(issuer_violation_code14_counts_2015, desc(issuer_violation_code14_counts_2015$count)))
head(issuer_violation_code14_counts_2015,5)
#   Violation Code count
# 1             69 41004
# 2             14 38696
# 3             31 20676
# 4             47 14480
# 5             42 14446


# for 2016
# lets see for Issuer Precinct 0,9,13
# for Issuer Precinct 0
issuer_violation_code0_counts_2016 <- summarize(groupBy(filter(parking_violation_2016, parking_violation_2016$`Issuer Precinct` == 0), parking_violation_2016$`Violation Code`),
                                                count = n(parking_violation_2016$`Summons Number`))
issuer_violation_code0_counts_2016 <-  SparkR::collect(arrange(issuer_violation_code0_counts_2016, desc(issuer_violation_code0_counts_2016$count)))
head(issuer_violation_code0_counts_2016,5)
#   Violation Code  count
# 1             36 615242
# 2              7 165111
# 3             21 104351
# 4              5  43467
# 5             66   3821

# for Issuer Precinct 19
issuer_violation_code19_counts_2016 <- summarize(groupBy(filter(parking_violation_2016, parking_violation_2016$`Issuer Precinct` == 19), parking_violation_2016$`Violation Code`),
                                                 count = n(parking_violation_2016$`Summons Number`))
issuer_violation_code19_counts_2016 <-  SparkR::collect(arrange(issuer_violation_code19_counts_2016, desc(issuer_violation_code19_counts_2016$count)))
head(issuer_violation_code19_counts_2016,5)
#   Violation Code count
# 1             37 38052
# 2             38 37855
# 3             46 36442
# 4             14 28772
# 5             21 25588

# for Issuer Precinct 13
issuer_violation_code13_counts_2016 <- summarize(groupBy(filter(parking_violation_2016, parking_violation_2016$`Issuer Precinct` == 13), parking_violation_2016$`Violation Code`),
                                                 count = n(parking_violation_2016$`Summons Number`))
issuer_violation_code13_counts_2016 <-  SparkR::collect(arrange(issuer_violation_code13_counts_2016, desc(issuer_violation_code13_counts_2016$count)))
head(issuer_violation_code13_counts_2016,5)

#   Violation Code count
# 1             69 23356
# 2             47 17532
# 3             38 16447
# 4             14 15812
# 5             37 13589



# for 2017
# lets see for Issuer Precinct 0,9,14
# for Issuer Precinct 0
issuer_violation_code0_counts_2017 <- summarize(groupBy(filter(parking_violation_2017, parking_violation_2017$`Issuer Precinct` == 0), parking_violation_2017$`Violation Code`),
                                                count = n(parking_violation_2017$`Summons Number`))
issuer_violation_code0_counts_2017 <-  SparkR::collect(arrange(issuer_violation_code0_counts_2017, desc(issuer_violation_code0_counts_2017$count)))
head(issuer_violation_code0_counts_2017,5)
head(issuer_violation_code0_counts_2017,5)
#   Violation Code  count
# 1             36 662765
# 2              7 210175
# 3             21 126053
# 4              5  48076
# 5             66   5258

# for Issuer Precinct 19
issuer_violation_code19_counts_2017 <- summarize(groupBy(filter(parking_violation_2017, parking_violation_2017$`Issuer Precinct` == 19), parking_violation_2017$`Violation Code`),
                                                 count = n(parking_violation_2017$`Summons Number`))
issuer_violation_code19_counts_2017 <-  SparkR::collect(arrange(issuer_violation_code19_counts_2017, desc(issuer_violation_code19_counts_2017$count)))
head(issuer_violation_code19_counts_2017,5)
#   Violation Code count
# 1             46 48445
# 2             38 36386
# 3             37 36056
# 4             14 29797
# 5             21 28415

# for Issuer Precinct 13
issuer_violation_code14_counts_2017 <- summarize(groupBy(filter(parking_violation_2017, parking_violation_2017$`Issuer Precinct` == 14), parking_violation_2017$`Violation Code`),
                                                 count = n(parking_violation_2017$`Summons Number`))
issuer_violation_code14_counts_2017 <-  SparkR::collect(arrange(issuer_violation_code14_counts_2017, desc(issuer_violation_code14_counts_2017$count)))
head(issuer_violation_code14_counts_2017,5)
#   Violation Code count
# 1             14 45036
# 2             69 30464
# 3             31 22555
# 4             47 18364
# 5             42 10027

# We can see that there is no violation code common across top 3 issuer percinct
#-----------------------------------------------------------------------------------------------------------------

# 5. You’d want to find out the properties of parking violations across different times of the day:

# I. The Violation Time field is specified in a strange format. Find a way to make this into a time attribute that you can use to divide into groups.


incorrect_violation_time_2015 <- SparkR::collect(SparkR::sql("select `Violation Time` from parking_violation_2015 where `Violation Time` is null or length(`Violation Time`)<>5 or upper(substr(`Violation Time`,-1)) not in ('A','P') or substr(`Violation Time`,1,2) not in ('00','01','02','03','04','05','06','07','08','09','10','11','12')"))
length(incorrect_violation_time_2015[is.na(incorrect_violation_time_2015)])
#664

incorrect_violation_time_2016 <- SparkR::collect(SparkR::sql("select `Violation Time` from parking_violation_2016 where `Violation Time` is null or length(`Violation Time`)<>5 or upper(substr(`Violation Time`,-1)) not in ('A','P') or substr(`Violation Time`,1,2) not in ('00','01','02','03','04','05','06','07','08','09','10','11','12')"))
length(incorrect_violation_time_2016[is.na(incorrect_violation_time_2016)])
#74 

incorrect_violation_time_2017 <- SparkR::collect(SparkR::sql("select `Violation Time` from parking_violation_2017 where `Violation Time` is null or length(`Violation Time`)<>5 or upper(substr(`Violation Time`,-1)) not in ('A','P') or substr(`Violation Time`,1,2) not in ('00','01','02','03','04','05','06','07','08','09','10','11','12')"))
length(incorrect_violation_time_2017[is.na(incorrect_violation_time_2017)])
#16


# II. Find a way to deal with missing values, if any.


head(dplyr::filter(incorrect_violation_time_2015, !is.na(stringr::str_trim(incorrect_violation_time_2015$`Violation Time`))))

#  Violation Time
#1          2859P
#2          3205P
#3          6113P
#4          1727P
#5          1.00P
#6          2015P


head(dplyr::filter(incorrect_violation_time_2016, !is.na(stringr::str_trim(incorrect_violation_time_2016$`Violation Time`))))

#  Violation Time
#1          2132P
#2          4721P
#3          3645P
#4          3200P
#5          1505P
#6          3720P

head(dplyr::filter(incorrect_violation_time_2017, !is.na(stringr::str_trim(incorrect_violation_time_2017$`Violation Time`))))

#  Violation Time
#1          4103P
#2          4605P
#3          7823P
#4          5620P
#5          5616P
#6           0557



# III. Divide 24 hours into 6 equal discrete bins of time. The intervals you choose are at your discretion. For each of these groups, find the 3 most commonly occurring violations


violation_code_time_bin_2015 <- SparkR::sql("SELECT `Summons Number`, `Violation Code` , case when substring(`Violation Time`,1,2) in ('00','01','02','03','12') and upper(substring(`Violation Time`,-1))='A' then 1
                                            when substring(`Violation Time`,1,2) in ('04','05','06','07') and upper(substring(`Violation Time`,-1))='A' then 2
                                            when substring(`Violation Time`,1,2) in ('08','09','10','11') and upper(substring(`Violation Time`,-1))='A' then 3
                                            when substring(`Violation Time`,1,2) in ('12','00','01','02','03' ) and upper(substring(`Violation Time`,-1))='P' then 4
                                            when substring(`Violation Time`,1,2) in ('04','05','06','07') and upper(substring(`Violation Time`,-1))='P' then 5
                                            when substring(`Violation Time`,1,2) in ('08','09','10','11') and upper(substring(`Violation Time`,-1))='P' then 6
                                            else null end as violation_time_bin from parking_violation_2015 where `Violation Time` is not null or (length(`Violation Time`)=5 and
                                            upper(substring(`Violation Time`,-1)) in ('A','P') and substring(`Violation Time`,1,2) in ('00','01','02','03','04','05','06','07', '08','09','10','11','12'))")

createOrReplaceTempView(violation_code_time_bin_2015, "violation_code_time_bin_2015_tbl")

head(SparkR::sql("SELECT `Violation Code`, count(*) count from violation_code_time_bin_2015_tbl where violation_time_bin = 1 group by `Violation Code` order by count DESC"),3)
#  Violation Code count
#1             21 30663
#2             40 20613
#3             78 17198

head(SparkR::sql("SELECT `Violation Code`, count(*) count from violation_code_time_bin_2015_tbl where violation_time_bin = 2 group by `Violation Code` order by count DESC"),3)
#  Violation Code count
#1             14 68994
#2             21 49098
#3             40 46783

head(SparkR::sql("SELECT `Violation Code`, count(*) count from violation_code_time_bin_2015_tbl where violation_time_bin = 3 group by `Violation Code` order by count DESC"),3)
#  Violation Code  count
#1             21 573741
#2             38 235956
#3             36 189347

head(SparkR::sql("SELECT `Violation Code`, count(*) count from violation_code_time_bin_2015_tbl where violation_time_bin = 4 group by `Violation Code` order by count DESC"),3)
#  Violation Code  count
#1             38 287564
#2             37 212536
#3             36 177439

head(SparkR::sql("SELECT `Violation Code`, count(*) count from violation_code_time_bin_2015_tbl where violation_time_bin = 5 group by `Violation Code` order by count DESC"),3)
#  Violation Code  count
#1             38 111178
#2             37  83676
#3             14  73424

head(SparkR::sql("SELECT `Violation Code`, count(*) count from violation_code_time_bin_2015_tbl where violation_time_bin = 6 group by `Violation Code` order by count DESC"),3)
#  Violation Code count
#1              7 29936
#2             38 27571
#3             40 22491

violation_code_time_bin_2016 <- SparkR::sql("SELECT `Summons Number`, `Violation Code` , case when substring(`Violation Time`,1,2) in ('00','01','02','03','12') and upper(substring(`Violation Time`,-1))='A' then 1
                                            when substring(`Violation Time`,1,2) in ('04','05','06','07') and upper(substring(`Violation Time`,-1))='A' then 2
                                            when substring(`Violation Time`,1,2) in ('08','09','10','11') and upper(substring(`Violation Time`,-1))='A' then 3
                                            when substring(`Violation Time`,1,2) in ('12','00','01','02','03' ) and upper(substring(`Violation Time`,-1))='P' then 4
                                            when substring(`Violation Time`,1,2) in ('04','05','06','07') and upper(substring(`Violation Time`,-1))='P' then 5
                                            when substring(`Violation Time`,1,2) in ('08','09','10','11') and upper(substring(`Violation Time`,-1))='P' then 6
                                            else null end as violation_time_bin from parking_violation_2016 where `Violation Time` is not null or (length(`Violation Time`)=5 and
                                            upper(substring(`Violation Time`,-1)) in ('A','P') and substring(`Violation Time`,1,2) in ('00','01','02','03','04','05','06','07', '08','09','10','11','12'))")

createOrReplaceTempView(violation_code_time_bin_2016, "violation_code_time_bin_2016_tbl")

head(SparkR::sql("SELECT `Violation Code`, count(*) count from violation_code_time_bin_2016_tbl where violation_time_bin = 1 group by `Violation Code` order by count DESC"),3)
#  Violation Code count
#1             21 31956
#2             40 19078
#3             78 14706

head(SparkR::sql("SELECT `Violation Code`, count(*) count from violation_code_time_bin_2016_tbl where violation_time_bin = 2 group by `Violation Code` order by count DESC"),3)
#   Violation Code count
#1             14 65347
#2             21 48240
#3             40 42306

head(SparkR::sql("SELECT `Violation Code`, count(*) count from violation_code_time_bin_2016_tbl where violation_time_bin = 3 group by `Violation Code` order by count DESC"),3)
#  Violation Code  count
#1             21 525280
#2             36 284279
#3             38 185395

head(SparkR::sql("SELECT `Violation Code`, count(*) count from violation_code_time_bin_2016_tbl where violation_time_bin = 4 group by `Violation Code` order by count DESC"),3)
#   Violation Code  count
#1             36 273581
#2             38 234221
#3             37 183854

head(SparkR::sql("SELECT `Violation Code`, count(*) count from violation_code_time_bin_2016_tbl where violation_time_bin = 5 group by `Violation Code` order by count DESC"),3)
#  Violation Code  count
#1             38 105657
#2             37  79991
#3             14  63778

head(SparkR::sql("SELECT `Violation Code`, count(*) count from violation_code_time_bin_2016_tbl where violation_time_bin = 6 group by `Violation Code` order by count DESC"),3)
#  Violation Code count
#1             38 20851
#2              7 20246
#3             40 20030

violation_code_time_bin_2017 <- SparkR::sql("SELECT `Summons Number`, `Violation Code` , case when substring(`Violation Time`,1,2) in ('00','01','02','03','12') and upper(substring(`Violation Time`,-1))='A' then 1
                                            when substring(`Violation Time`,1,2) in ('04','05','06','07') and upper(substring(`Violation Time`,-1))='A' then 2
                                            when substring(`Violation Time`,1,2) in ('08','09','10','11') and upper(substring(`Violation Time`,-1))='A' then 3
                                            when substring(`Violation Time`,1,2) in ('12','00','01','02','03' ) and upper(substring(`Violation Time`,-1))='P' then 4
                                            when substring(`Violation Time`,1,2) in ('04','05','06','07') and upper(substring(`Violation Time`,-1))='P' then 5
                                            when substring(`Violation Time`,1,2) in ('08','09','10','11') and upper(substring(`Violation Time`,-1))='P' then 6
                                            else null end as violation_time_bin from parking_violation_2017 where `Violation Time` is not null or (length(`Violation Time`)=5 and
                                            upper(substring(`Violation Time`,-1)) in ('A','P') and substring(`Violation Time`,1,2) in ('00','01','02','03','04','05','06','07', '08','09','10','11','12'))")

createOrReplaceTempView(violation_code_time_bin_2017, "violation_code_time_bin_2017_tbl")

head(SparkR::sql("SELECT `Violation Code`, count(*) count from violation_code_time_bin_2017_tbl where violation_time_bin = 1 group by `Violation Code` order by count DESC"),3)
#  Violation Code count
#1             21 36957
#2             40 25866
#3             78 15528

head(SparkR::sql("SELECT `Violation Code`, count(*) count from violation_code_time_bin_2017_tbl where violation_time_bin = 2 group by `Violation Code` order by count DESC"),3)
#  Violation Code count
#1             14 74114
#2             40 60652
#3             21 57897


head(SparkR::sql("SELECT `Violation Code`, count(*) count from violation_code_time_bin_2017_tbl where violation_time_bin = 3 group by `Violation Code` order by count DESC"),3)
#  Violation Code count
#1             21 598069
#2             36 348165
#3             38 176570

head(SparkR::sql("SELECT `Violation Code`, count(*) count from violation_code_time_bin_2017_tbl where violation_time_bin = 4 group by `Violation Code` order by count DESC"),3)
#  Violation Code count
#1             36 286284
#2             38 240721
#3             37 167026

head(SparkR::sql("SELECT `Violation Code`, count(*) count from violation_code_time_bin_2017_tbl where violation_time_bin = 5 group by `Violation Code` order by count DESC"),3)
#  Violation Code count
#1             38 102855
#2             14  75902
#3             37  70345

head(SparkR::sql("SELECT `Violation Code`, count(*) count from violation_code_time_bin_2017_tbl where violation_time_bin = 6 group by `Violation Code` order by count DESC"),3)
#  Violation Code count
#1              7 26293
#2             40 22337
#3             14 21045

# IV. Now, try another direction. For the 3 most commonly occurring violation codes, find the most common times of day (in terms of the bins from the previous part)

#2015 Data
violation_code_time_bin_count_2015 <- SparkR::sql("SELECT `Violation Code`, violation_time_bin, count(*) count from violation_code_time_bin_2015_tbl group by `Violation Code`, violation_time_bin order by count DESC")



head(violation_code_time_bin_count_2015)

# Violation Code violation_time_bin  count

#1             21                  3 573741

#2             38                  4 287564

#3             38                  3 235956

#4             37                  4 212536

#5             36                  3 189347

#6             36                  4 177439


filter(violation_code_time_bin_count_2015, violation_code_time_bin_count_2015$`Violation Code` == 21) %>% 
  
arrange(desc(.$count)) %>% 
  
head(3)


# 3, 4 and 2 are the bins where code 21 occurs most

#Violation Code violation_time_bin  count

#1             21                  3 573741

#2             21                  4  66820

#3             21                  2  49098



filter(violation_code_time_bin_count_2015, violation_code_time_bin_count_2015$`Violation Code` == 38) %>% 
  
arrange(desc(.$count)) %>% 
  
head(3)


#4, 3 and 5 are the binns where code 38 occurs most

#Violation Code violation_time_bin  count

#1             38                  4 287564

#2             38                  3 235956

#3             38                  5 111178



filter(violation_code_time_bin_count_2015, violation_code_time_bin_count_2015$`Violation Code` == 37) %>% 
  
arrange(desc(.$count)) %>% 
  
head(3)


#4, 5 and 3 are the binns where code 37 occurs most

#1             37                  4 212536

#2             37                  5  83676

#3             37                  3  68283



# 2016 data


violation_code_time_bin_count_2016 <- SparkR::sql("SELECT `Violation Code`, violation_time_bin, count(*) count from violation_code_time_bin_2016_tbl group by `Violation Code`, violation_time_bin order by count DESC")



head(violation_code_time_bin_count_2016)

#  Violation Code violation_time_bin  count
#1             21                  3 525280
#2             36                  3 284279
#3             36                  4 273581
#4             38                  4 234221
#5             38                  3 185395
#6             37                  4 183854

filter(violation_code_time_bin_count_2016, violation_code_time_bin_count_2016$`Violation Code` == 21) %>% 
  
arrange(desc(.$count)) %>% 
  
head(3)




#  Violation Code violation_time_bin  count
#1             21                  3 525280
#2             21                  4  58989
#3             21                  2  48240

filter(violation_code_time_bin_count_2016, violation_code_time_bin_count_2016$`Violation Code` == 36) %>% 
  
arrange(desc(.$count)) %>% 
  
head(3)



#  Violation Code violation_time_bin  count
#1             36                  3 284279
#2             36                  4 273581
#3             36                  2  39129


filter(violation_code_time_bin_count_2016, violation_code_time_bin_count_2016$`Violation Code` == 38) %>% 
  
arrange(desc(.$count)) %>% 
  
head(3)

#   Violation Code violation_time_bin  count
#1             38                  4 234221
#2             38                  3 185395
#3             38                  5 105657


#2017 
Data
violation_code_time_bin_count_2017 <- SparkR::sql("SELECT `Violation Code`, violation_time_bin, count(*) count from violation_code_time_bin_2017_tbl group by `Violation Code`, violation_time_bin order by count DESC")


head(violation_code_time_bin_count_2017)

#  Violation Code violation_time_bin  count
#1             21                  3 598069
#2             36                  3 348165
#3             36                  4 286284
#4             38                  4 240721
#5             38                  3 176570
#6             37                  4 167026


filter(violation_code_time_bin_count_2017, violation_code_time_bin_count_2017$`Violation Code` == 21) %>% 
  
arrange(desc(.$count)) %>% 
  
head(3)


#  Violation Code violation_time_bin  count
#1             21                  3 598069
#2             21                  4  74695
#3             21                  2  57897
 

filter(violation_code_time_bin_count_2017, violation_code_time_bin_count_2017$`Violation Code` == 36) %>% 
  
arrange(desc(.$count)) %>% 
  
head(3)

#  Violation Code violation_time_bin  count
#1             36                  3 348165
#2             36                  4 286284
#3             36                  2  14782


filter(violation_code_time_bin_count_2017, violation_code_time_bin_count_2017$`Violation Code` == 38) %>% 
  
arrange(desc(.$count)) %>% 
  
head(3)


#  Violation Code violation_time_bin  count
#1             38                  4 240721
#2             38                  3 176570
#3             38                  5 102855

#-----------------------------------------------------------------------------------------------------------------

# 6. Let's try and find some seasonality in this data
# I. First, divide the year into some number of seasons, and find frequencies of tickets for each season.

# For 2015
# head(SparkR::sql("SELECT  YEAR(issue_date_formatted) year, MONTH(issue_date_formatted) mnth,count(DISTINCT `Summons Number`) from parking_violation_2015 GROUP BY YEAR(issue_date_formatted),MONTH(issue_date_formatted)"),50)
# count(1)
# 1  5822402

# Dividing the year into quarters  of 3 months each 
violation_code_season_2015 <- SparkR::sql("SELECT `Summons Number`, `Violation Code`, case when MONTH(issue_date_formatted)  in (1,2,3) then 1
                                          when MONTH(issue_date_formatted) in (4,5,6) then 2
                                          when MONTH(issue_date_formatted)  in (7,8,9) then 3
                                          when MONTH(issue_date_formatted)  in (10,11,12) then 4
                                          else null end as season from parking_violation_2015")
# head(violation_code_season_2015,50)
season_frequency_2015 =  summarize(group_by(violation_code_season_2015, violation_code_season_2015$season), count = n(violation_code_season_2015$`Summons Number`)) 
head(arrange(season_frequency_2015,desc(season_frequency_2015$count)),50)  
#   season   count
# 1      2 2907331
# 2      1 2466640

### SparkR version
summarize(group_by(filter(violation_code_season_2015, violation_code_season_2015$season == 1), violation_code_season_2015$`Violation Code`), count = n(violation_code_season_2015$`Violation Code`))  %>%
  arrange(desc(.$count)) %>% 
  head(3)
#   Violation Code  count
# 1             38 336746
# 2             21 281386
# 3             14 219828

summarize(group_by(filter(violation_code_season_2015, violation_code_season_2015$season == 2), violation_code_season_2015$`Violation Code`), count = n(violation_code_season_2015$`Violation Code`))  %>%
  arrange(desc(.$count)) %>% 
  head(3)
#   Violation Code  count
# 1             21 439516
# 2             38 327158
# 3             14 246660

# since our assumptions consists of only 2015 data from 2015 file the data hase only data for 2 seasons(from Jan to Jun)
summarize(group_by(filter(violation_code_season_2015, violation_code_season_2015$season == 3), violation_code_season_2015$`Violation Code`), count = n(violation_code_season_2015$`Violation Code`))  %>%
  arrange(desc(.$count)) %>% 
  head(3)

summarize(group_by(filter(violation_code_season_2015, violation_code_season_2015$season == 4), violation_code_season_2015$`Violation Code`), count = n(violation_code_season_2015$`Violation Code`))  %>%
  arrange(desc(.$count)) %>% 
  head(3)

# For 2016

# head(SparkR::sql("SELECT  YEAR(issue_date_formatted) year, MONTH(issue_date_formatted) mnth,count(DISTINCT `Summons Number`) from parking_violation_2016 GROUP BY YEAR(issue_date_formatted),MONTH(issue_date_formatted)"),50)
# count(1)
# 1  5822402

# Dividing the year into quarters  of 3 months each 
violation_code_season_2016 <- SparkR::sql("SELECT `Summons Number`, `Violation Code`, case when MONTH(issue_date_formatted)  in (1,2,3) then 1
                                          when MONTH(issue_date_formatted) in (4,5,6) then 2
                                          when MONTH(issue_date_formatted)  in (7,8,9) then 3
                                          when MONTH(issue_date_formatted)  in (10,11,12) then 4
                                          else null end as season from parking_violation_2016")
# head(violation_code_season_2016,50)
season_frequency_2016 =  summarize(group_by(violation_code_season_2016, violation_code_season_2016$season), count = n(violation_code_season_2016$`Summons Number`)) 
head(arrange(season_frequency_2016,desc(season_frequency_2016$count)),50)  
#   season   count
# 1      1 2668423
# 2      2 2202295
# 3      3    1057
# 4      4     846
### SparkR version
summarize(group_by(filter(violation_code_season_2016, violation_code_season_2016$season == 1), violation_code_season_2016$`Violation Code`), count = n(violation_code_season_2016$`Violation Code`))  %>%
  arrange(desc(.$count)) %>% 
  head(3)
#   Violation Code  count
# 1             21 349297
# 2             36 341787
# 3             38 308987

summarize(group_by(filter(violation_code_season_2016, violation_code_season_2016$season == 2), violation_code_season_2016$`Violation Code`), count = n(violation_code_season_2016$`Violation Code`))  %>%
  arrange(desc(.$count)) %>% 
  head(3)
#   Violation Code  count
# 1             21 315234
# 2             36 273455
# 3             38 238083

# since our assumptions consists of only 2016 data from 2015 file the data hase only data for 2 seasons(from Jan to Jun)
summarize(group_by(filter(violation_code_season_2016, violation_code_season_2016$season == 3), violation_code_season_2016$`Violation Code`), count = n(violation_code_season_2016$`Violation Code`))  %>%
  arrange(desc(.$count)) %>% 
  head(3)
#   Violation Code count
# 1             21   249
# 2             46   214
# 3             40    89

summarize(group_by(filter(violation_code_season_2016, violation_code_season_2016$season == 4), violation_code_season_2016$`Violation Code`), count = n(violation_code_season_2016$`Violation Code`))  %>%
  arrange(desc(.$count)) %>% 
  head(3)
#   Violation Code count
# 1             21   167
# 2             46   164
# 3             40    80

# For 2017

# head(SparkR::sql("SELECT  YEAR(issue_date_formatted) year, MONTH(issue_date_formatted) mnth,count(DISTINCT `Summons Number`) from parking_violation_2017 GROUP BY YEAR(issue_date_formatted),MONTH(issue_date_formatted)"),50)
# count(1)
# 1  5822402

# Dividing the year into quarters  of 3 months each 
violation_code_season_2017 <- SparkR::sql("SELECT `Summons Number`, `Violation Code`, case when MONTH(issue_date_formatted)  in (1,2,3) then 1
                                          when MONTH(issue_date_formatted) in (4,5,6) then 2
                                          when MONTH(issue_date_formatted)  in (7,8,9) then 3
                                          when MONTH(issue_date_formatted)  in (10,11,12) then 4
                                          else null end as season from parking_violation_2017")
# head(violation_code_season_2017,50)
season_frequency_2017 =  summarize(group_by(violation_code_season_2017, violation_code_season_2017$season), count = n(violation_code_season_2017$`Summons Number`)) 
head(arrange(season_frequency_2017,desc(season_frequency_2017$count)),50)  
#   season   count
# 1      2 2760833
# 2      1 2669069
# 3      3    1046
# 4      4     970

### SparkR version
summarize(group_by(filter(violation_code_season_2017, violation_code_season_2017$season == 1), violation_code_season_2017$`Violation Code`), count = n(violation_code_season_2017$`Violation Code`))  %>%
  arrange(desc(.$count)) %>% 
  head(3)
#   Violation Code  count
# 1             21 373874
# 2             36 348240
# 3             38 287000

summarize(group_by(filter(violation_code_season_2017, violation_code_season_2017$season == 2), violation_code_season_2017$`Violation Code`), count = n(violation_code_season_2017$`Violation Code`))  %>%
  arrange(desc(.$count)) %>% 
  head(3)
#   Violation Code  count
# 1             21 393885
# 2             36 314525
# 3             38 255064

# since our assumptions consists of only 2016 data from 2015 file the data hase only data for 2 seasons(from Jan to Jun)
summarize(group_by(filter(violation_code_season_2017, violation_code_season_2017$season == 3), violation_code_season_2017$`Violation Code`), count = n(violation_code_season_2017$`Violation Code`))  %>%
  arrange(desc(.$count)) %>% 
  head(3)
#   Violation Code count
# 1             21   228
# 2             46   219
# 3             40   109

summarize(group_by(filter(violation_code_season_2017, violation_code_season_2017$season == 4), violation_code_season_2017$`Violation Code`), count = n(violation_code_season_2017$`Violation Code`))  %>%
  arrange(desc(.$count)) %>% 
  head(3)
#   Violation Code count
# 1             46   219
# 2             40   121
# 3             21   100

# 7. The fines collected from all the parking violation constitute a revenue source for the NYC police department. Let’s take an example of estimating that for the 3 most commonly occurring codes.
# I. Find total occurrences of the 3 most common violation codes
# Lets get the state with violation code
# Find out how many unique states the cars which got parking tickets came from.
#for 2015
#from question 1 we already have top 5 violation fro each year. The top violation codes can be found easily as below

head(violation_code_counts_2015,3)
head(violation_code_counts_2016,3)
head(violation_code_counts_2017,3)


# II. Then, search the internet for NYC parking violation code fines. You will find a website (on the nyc.gov URL) that lists these fines. They’re divided into two categories, one for the highest-density locations of the city, the other for the rest of the city. For simplicity, take an average of the two.
theurl <- getURL("https://www1.nyc.gov/site/finance/vehicles/services-violation-codes.page",.opts = list(ssl.verifypeer = FALSE) )
	          
violation_code_fine <- data.frame(readHTMLTable(theurl, which=1, stringsAsFactors=F))
for(i in seq(2,5,by=1)){
	   violation_code_fine <<- rbind(violation_code_fine, data.frame(readHTMLTable(theurl, which=i, stringsAsFactors=F)))
	          }

violation_code_fine <- violation_code_fine[violation_code_fine$V1 != "CODE",]
violation_fine_fee <-  violation_code_fine %>%
dplyr::mutate(fine = (as.numeric(substring(V3, 2)) + as.numeric(substring(V4, 2)))/2 ) 
violation_fine_fee <- rbind(violation_fine_fee, violation_fine_fee[36,])
violation_fine_fee[36, 1] <- 37
violation_fine_fee[91, 1] <- 38

# check which are na
which(is.na(violation_fine_fee$fine))
# [1]  4  6 86 90
	          
# assign the value manually to this violation
violation_fine_fee$fine[4] <- 115 
violation_fine_fee$fine[6] <- 390 
violation_fine_fee$fine[86] <- 150 
violation_fine_fee$fine[90] <- 0 
violation_fine_fee <- violation_fine_fee %>% dplyr::select("V1", "fine")
violation_fine_fee$V1 <- as.numeric(violation_fine_fee$V1) 
violation_fine_fee
	              
# III. Using this information, find the total amount collected for all the fines. State the code which has the highest total collection.
# get the highest fine paid for each violation code


state_ticket_violation_code_counts_2015<- SparkR::collect(summarize(groupBy(parking_violation_2015, parking_violation_2015$`Registration State`, parking_violation_2015$`Violation Code`),count = n(parking_violation_2015$`Registration State`)))
state_ticket_violation_code_fine_2015 <- dplyr::inner_join(state_ticket_violation_code_counts_2015, violation_fine_fee, by = c("Violation Code" = "V1")) %>%
dplyr::mutate(total_fine = count*fine) 

state_ticket_violation_code_counts_2016<- SparkR::collect(summarize(groupBy(parking_violation_2016, parking_violation_2016$`Registration State`, parking_violation_2016$`Violation Code`),count = n(parking_violation_2016$`Registration State`)))
state_ticket_violation_code_fine_2016 <- dplyr::inner_join(state_ticket_violation_code_counts_2016, violation_fine_fee, by = c("Violation Code" = "V1")) %>%
dplyr::mutate(total_fine = count*fine) 

state_ticket_violation_code_counts_2017<- SparkR::collect(summarize(groupBy(parking_violation_2017, parking_violation_2017$`Registration State`, parking_violation_2017$`Violation Code`),count = n(parking_violation_2017$`Registration State`)))
state_ticket_violation_code_fine_2017 <- dplyr::inner_join(state_ticket_violation_code_counts_2017, violation_fine_fee, by = c("Violation Code" = "V1")) %>%
dplyr::mutate(total_fine = count*fine) 
	
violation_fine_fee_SR<-createDataFrame(violation_fine_fee)

#2015

state_ticket_violation_code_counts_2015_SR<-createDataFrame(state_ticket_violation_code_counts_2015)
state_ticket_with_fine_2015<- join(state_ticket_violation_code_counts_2015_SR, violation_fine_fee_SR, state_ticket_violation_code_counts_2015_SR$`Violation Code` == violation_fine_fee_SR$`V1`, "inner") %>%
mutate(total_fine = .$count*.$fine) 


state_ticket_fine_violation_code_2015 <- state_ticket_with_fine_2015 %>%
groupBy(.$`Violation Code`) %>%
summarize(sum_fine = sum(state_ticket_with_fine_2015$total_fine), sum_count = sum(state_ticket_with_fine_2015$count)) %>%
arrange(desc(.$sum_fine))

head(state_ticket_fine_violation_code_2015)
#    Violation Code sum_fine sum_count
#1             14 53646120    466488
#2             21 39649610    720902
#3             38 33195200    663904
#4             46 32298440    280856
#5             40 28451920    247408
#6             36 20312450    406249


#2016
state_ticket_violation_code_counts_2016_SR<-createDataFrame(state_ticket_violation_code_counts_2016)

state_ticket_with_fine_2016<- join(state_ticket_violation_code_counts_2016_SR, violation_fine_fee_SR, state_ticket_violation_code_counts_2016_SR$`Violation Code` == violation_fine_fee_SR$`V1`, "inner") %>%
mutate(total_fine = .$count*.$fine) 


state_ticket_fine_violation_code_2016 <- state_ticket_with_fine_2016 %>%
groupBy(.$`Violation Code`) %>%
summarize(sum_fine = sum(state_ticket_with_fine_2016$total_fine), sum_count = sum(state_ticket_with_fine_2016$count)) %>%
arrange(desc(.$sum_fine))



head(state_ticket_fine_violation_code_2016)
#    Violation Code sum_fine sum_count
#1             14 46676775    405885
#2             21 36572085    664947
#3             46 31159710    270954
#4             36 30762100    615242
#5             38 27354000    547080
#6             40 24494080    212992

#2017
state_ticket_violation_code_counts_2017_SR<-createDataFrame(state_ticket_violation_code_counts_2017)

state_ticket_with_fine_2017<- join(state_ticket_violation_code_counts_2017_SR, violation_fine_fee_SR, state_ticket_violation_code_counts_2017_SR$`Violation Code` == violation_fine_fee_SR$`V1`, "inner") %>%
mutate(total_fine = .$count*.$fine) 


state_ticket_fine_violation_code_2017 <- state_ticket_with_fine_2017 %>%
groupBy(.$`Violation Code`) %>%
summarize(sum_fine = sum(state_ticket_with_fine_2017$total_fine), sum_count = sum(state_ticket_with_fine_2017$count)) %>%
arrange(desc(.$sum_fine))


head(state_ticket_fine_violation_code_2017)
# Violation Code sum_fine sum_count
#1             14 54816360    476664
#2             21 42244785    768087
#3             46 35917950    312330
#4             36 33138250    662765
#5             40 31876160    277184
#6             38 27103950    542079

# IV What can you intuitively infer from these findings?  

plot_2015 <- ggplot(as.data.frame(state_ticket_fine_violation_code_2015), aes(x = `Violation Code`, y = sum_fine)) +
  geom_bar(stat = "identity") +
  xlab("code") + ylab("Total fine") +  theme(axis.text.x = element_text(angle = 90, hjust = 1))

plot_2016 <- ggplot(as.data.frame(state_ticket_fine_violation_code_2016), aes(x = `Violation Code`, y = sum_fine)) +
  geom_bar(stat = "identity") +
  xlab("code") + ylab("Total fine") +  theme(axis.text.x = element_text(angle = 90, hjust = 1))

plot_2017 <- ggplot(as.data.frame(state_ticket_fine_violation_code_2017), aes(x = `Violation Code`, y = sum_fine)) +
  geom_bar(stat = "identity") +
  xlab("code") + ylab("Total fine") +  theme(axis.text.x = element_text(angle = 90, hjust = 1))

grid.arrange(plot_2015, plot_2016,plot_2017, nrow=3)

#insignts included in the document