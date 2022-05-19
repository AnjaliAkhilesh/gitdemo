// Databricks notebook source
//dbfs:/FileStore/shared_uploads/anjalibabu8991@gmail.com/Weblog.csv

// COMMAND ----------

// Spark Packages
import org.apache.log4j.{Level, Logger}
import org.apache.spark.sql.SparkSession
import org.apache.spark.sql.functions._

// COMMAND ----------

import spark.implicits._

// COMMAND ----------


val weblog_DF = spark.read.text("dbfs:/FileStore/shared_uploads/anjalibabu8991@gmail.com/Weblog.csv")
val header = weblog_DF.first() // Extract Header
val weblogs_DF = weblog_DF.filter(row => row != header)
weblogs_DF.printSchema()

 


// COMMAND ----------


weblogs_DF.show(5,false)


// COMMAND ----------


//a.Parsing the Log Files using RegExp & Pre-process Raw Log Data into Data frame with attributes. 
val weblog_DF = spark.read.text("dbfs:/FileStore/shared_uploads/anjalibabu8991@gmail.com/Weblog.csv")
val header = weblog_DF.first() // Extract Header
val weblogs_DF = weblog_DF.filter(row => row != header)
weblogs_DF.printSchema()

 


// COMMAND ----------

//timestamb
val timestamp = weblogs_DF.select(regexp_extract($"value", """\S(\d{2}/\w{3}/\d{4}:\d{2}:\d{2}:\d{2})""", 1).alias("Timestamp"))
timestamp.show(false)


// COMMAND ----------


// method-get
val method = weblogs_DF.select(regexp_extract($"value", """(\w+)\s(\S+)\s(\S)""", 1).alias("Method"))
method.show()


// COMMAND ----------

//request url
val url = weblogs_DF.select(regexp_extract($"value", """(\S+)\s(\S+)\s*(\S*)""", 2).alias("url"))
url.show()


// COMMAND ----------

//protocol
val protocol = weblogs_DF.select(regexp_extract($"value", """(\S+)\s(\S+)\s(\S+)(,)""", 3).alias("Protocol"))
protocol.show()



// COMMAND ----------


//status
val status = weblogs_DF.select(regexp_extract($"value", """,(\d{3})""", 1).alias("Status"))
status.show()


// COMMAND ----------


val weblog_df1 = weblogs_DF.select(regexp_extract($"value","""([^(\s|,)]+)""", 1).alias("host"),
                           regexp_extract($"value", """\S(\d{2}/\w{3}/\d{4}:\d{2}:\d{2}:\d{2})""", 1).alias("Timestamp"),
                           regexp_extract($"value", """(\S+)\s(\S+)\s*(\S*)""", 2).alias("url"),
                           regexp_extract($"value", """(\w+)\s(\S+)\s(\S)""", 1).alias("Method"),
                           regexp_extract($"value", """(\S+)\s(\S+)\s(\S+)(,)""", 3).alias("Protocol"),
                           regexp_extract($"value", """,(\d{3})""", 1).alias("Status"))
weblog_df1.show(40)
weblog_df1.printSchema()


// COMMAND ----------


//b)Use data cleaning: count null and remove null values. Fix rows with null status (Drop those rows).
import org.apache.spark.sql.functions.{col,when,count}
import org.apache.spark.sql.Column
def countNullCols (columns:Array[String]):Array[Column] = {
   columns.map(c => {
   count(when(col(c).isNull, c)).alias(c)
  })

}


// COMMAND ----------


//c)Pre-process and fix timestamp month name to month value. Convert Datetime (timestamp column) as Days, Month & Year.
weblog_df1.select(to_date($"Timestamp")).show(5)
val month_map = Map("Jan" -> 1, "Feb" -> 2, "Mar" -> 3, "Apr" -> 4, "May" -> 5, "Jun" -> 6, "Jul" -> 7, "Aug" -> 8, "Sep" -> 9,
"Oct" -> 10, "Nov" -> 11, "Dec" -> 12)
def parse_time(s : String):String = {
		"%3$s-%2$s-%1$s %4$s:%5$s:%6$s".format(s.substring(0,2), month_map(s.substring(3,6)), s.substring(7,11), 

                                             s.substring(12,14), s.substring(15,17), s.substring(18))
}

val toTimestamp = udf[String, String](parse_time(_))
val logsDF = weblog_df1.select($"*", to_timestamp(toTimestamp($"Timestamp")).alias("time")).drop("Timestamp")
logsDF.show()




// COMMAND ----------

val month_df=logsDF.withColumn("Day",dayofmonth($"time")).withColumn("Month",month($"time")).withColumn("Year",year($"time")).drop("time")
month_df.show()

// COMMAND ----------

//weblog_df1.write.parquet("dbfs:/FileStore/shared_uploads/anjalibabu8991@gmail.com/Weblog2/")

// COMMAND ----------

//d)Create new parquet file using cleaned Data Frame. Read the parquet file. 
val parquetLogs = spark.read.parquet("dbfs:/FileStore/shared_uploads/anjalibabu8991@gmail.com/Weblog2/")
parquetLogs.show()



// COMMAND ----------

//e)Show the summary of each column
parquetLogs.summary().show()


// COMMAND ----------

//f)Display frequency of 200 status code in the response for each month.
month_df.filter($"Status" === 200).groupBy("Month").count().sort(desc("count")).show(false)


// COMMAND ----------

//g)Frequency of Host Visits in November Month.

parquetLogs.filter($"month" === 11).groupBy("host").count().sort(desc("count")).show

// COMMAND ----------

//h)Display Top 15 Error Paths - status != 200.
parquetLogs.filter($"Status" =!= 200).groupBy("url").count().sort(desc("count")).show(15)

// COMMAND ----------

//i)Display Top 10 Paths with Error - with status equals 200.
parquetLogs.filter($"Status" === 200).groupBy("url").count().sort(desc("count")).show(10)


parquetLogs.createOrReplaceTempView("weblogs")
spark.sql("select * from weblogs").show()



// COMMAND ----------


//j)Exploring 404 status code. Listing 404 status Code Records. List Top 20 Host with 404 response status code (Query + Visualization).
spark.sql("select host,Status from weblogs where Status = 404 limit 20").show()


// COMMAND ----------

// MAGIC %sql
// MAGIC select host,Status from weblogs where Status = 404 limit 20

// COMMAND ----------


val month_df1=logsDF.withColumn("Day",dayofmonth($"time")).withColumn("Month",month($"time")).withColumn("Year",year($"time")).drop("time")
month_df1.createOrReplaceTempView("weblogsTable")
spark.sql("select * from weblogsTable").show



// COMMAND ----------


//k)Display the List of 404 Error Response Status Code per Day (Query + Visualization).
spark.sql("select Year, Day, count(*) as Count from weblogsTable where Status = 404 group by Year, Day order by Day limit 30").show(false)



// COMMAND ----------

// MAGIC %sql
// MAGIC select Year, Day, count(*) as Count from weblogsTable where Status = 404 group by Year, Day order by Day limit 30

// COMMAND ----------

//l)List Top 20 Paths (Endpoint) with 404 Response Status Code.
spark.sql("select url from WeblogsTable where Status = 404").show(false)


// COMMAND ----------

//m)Query to Display Distinct Path responding 404 in status error.
spark.sql("select distinct(url) from weblogsTable where Status = 404").show(false)


// COMMAND ----------


//n)Find the number of unique source IPs that have made requests to the webserver for each month

// COMMAND ----------

//o)Display the top 20 requested Paths in each Month (Query + Visualization).

// COMMAND ----------

//p)    Query to Display Distinct Path responding 404 in status error.
spark.sql("select distinct(url) from weblogsTable where Status = 404").show(false)

// COMMAND ----------


