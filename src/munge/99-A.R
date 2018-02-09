## Connection class for PostgreSQL Database
##

# install.packages("RPostgreSQL")
require("RPostgreSQL")

# create a connection
# save the password that we can "hide" it as best as we can by collapsing it
pw <- {"secret"}

# loads the PostgreSQL driver
drv <- dbDriver("PostgreSQL")
# creates a connection to the postgres database
# note that "con" will be used later in each connection to the database
con <- dbConnect(drv, dbname = "dedup",
                 host = "localhost", port = 5432,
                 user = "dddev", password = pw)
rm(pw) # removes the password

# check for the cartable
dbExistsTable(con, "userdetails")
# TRUE

# creates df, a data.frame with the necessary columns
data(mtcars)
df <- data.frame(carname = rownames(mtcars), 
                 mtcars, 
                 row.names = NULL)
df$carname <- as.character(df$carname)
rm(mtcars)

# writes df to the PostgreSQL database "postgres", table "cartable" 
dbWriteTable(con, "cartable", 
             value = df, append = TRUE, row.names = FALSE)

# query the data from postgreSQL 
df_postgres <- dbGetQuery(con, "SELECT * from userdetails")

# compares the two data.frames
identical(df, df_postgres)
# TRUE

# Basic Graph of the Data
require(ggplot2)
ggplot(df_postgres, aes(x = as.factor(cyl), y = mpg, fill = as.factor(cyl))) + 
  geom_boxplot() + theme_bw()

#If you want to create a table in R instead of pgAdmin III you can do that of course.
#The following creates the same table as the we did earlier in pgAdmin III:

# specifies the details of the table
sql_command <- "CREATE TABLE cartable
(
  carname character varying NOT NULL,
  mpg numeric(3,1),
  cyl numeric(1,0),
  disp numeric(4,1),  
  hp numeric(3,0),
  drat numeric(3,2),
  wt numeric(4,3),
  qsec numeric(4,2),
  vs numeric(1,0),
  am numeric(1,0),
  gear numeric(1,0),
  carb numeric(1,0),
  CONSTRAINT cartable_pkey PRIMARY KEY (carname)
)
WITH (
OIDS=FALSE
);
ALTER TABLE cartable
OWNER TO openpg;
COMMENT ON COLUMN cartable.disp IS '
';"
# sends the command and creates the table
dbGetQuery(con, sql_command)