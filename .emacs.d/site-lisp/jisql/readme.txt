;;
;; Original source and binaries obtained from http://www.xigole.com/software/jisql/jisql.jsp
;; The text below is copied from that page on 09/20/2010
;;
;; Revisions:
;;
;;   09/20/2010 - Modified Jiql.java to truncate the width of each columns' output 
;;                to 2000 characters.
;;                Modified DefaultFormatter.java to insert an newline prior to any
;;                header output.  
;;
;;
;;

isql is a Java based utility to provide a command line interactive session with a SQL server. This application is conceptually modeled on the Sybase 'isql' program with, obviously, strong similarities to Microsoft SQL/Server isql and osql (as Microsoft got SQL Server from Sybase).

The program can act in a similar way to Oracle's sqlplus and PostgreSQL's psql.

The complete Jisql package (source code, Javadoc, build environment) can be downloaded from:

http://www.xigole.com/software/jisql/build/jisql-2.0.7.tar.bz2 or
http://www.xigole.com/software/jisql/build/jisql-2.0.7.tar.gz or
http://www.xigole.com/software/jisql/build/jisql-2.0.7.zip
Windows users: There seem to be problems with WinZip and the tar.gz version of the archive if you open it directly without saving it to disk. The zip version works just fine.

About Java versions: These files have been compiled with Java 6. However, they do not take advantage of any Java 6 constructs so you can easily recompile for a lower version. I am lucky enough to work on platforms for which Java 6 is readily available but if you don't the code should still compile for at least Java 5.

These files contain a precompiled jar file along with all of the source code and an ant build file.

A simple command line might look like (this should be all on one line) is:
java -cp lib/jisql-2.0.7.jar:<file containing native driver>
          com.xigole.util.sql.Jisql
          -user scott -password blah -driver postgresql
          -cstring jdbc:postgresql://localhost:5432/scott -c \;
This logs into a PostgreSQL database as the user "scott", password "blah". It connects to the database named "scott". It uses the command terminator of ";", just like psql or sqlplus (which is escaped in the example so that it will not be interpreted by the Unix shell). If you do not use this the default is the term "go" on a single line like Sybase's isql or MS/SQL's isql/osql.

New in version 2.0.x
The biggest change in version 2 is the addition of separate output formatters. Basically this allows you to more easily customize the output for your application. There are three formatters that are included with the distribution:

Default (does not have to be specified on the command line). This is the default formatter and it behaves like a "normal" format command line interface. Output is wrapped in "pretty" formatters (a highly subjective term). If you've used a database command line tool then this format will very likely feel familar.
CSV The output follows a Comma Separated Values format. The default separator is a comma but any other character can be used. I like using the pipe character as it makes parsing the output very easy.
XML The output is a very simple XML tree. Basically there is the normal XML processing instruction at the top and then every row is output with the column names. There are command line options to include or exclude null columns.
There is now a dependency on JOpt Simple in for the base configuration. Additionally, if you are using the CSVFormatter then it is dependent on Java CSV.

New in version 2.0.5
There is a bit better versioning information available. The jar has been renamed to include the version so that you can quickly see what you're running. This same version information is included in the code (run with -help or with no arguments to see the version) and in the jar manifest.

Options:

-driver This option allows you to specify the JDBC driver class name of the driver. There are several shortcuts that can be used:
jconnect4 - short for com.sybase.jdbc.SybDriver
jconnect5 - short for com.sybase.jdbc2.jdbc.SybDriver
jconnect6 - short for com.sybase.jdbc3.jdbc.SybDriver
oraclethin - short for oracle.jdbc.driver.OracleDriver
db2app - the DB2 "App" driver - COM.ibm.db2.jdbc.app.DB2Driver
db2net - the DB2 "Net" driver - COM.ibm.db2.jdbc.net.DB2Driver
mssql - short for com.microsoft.jdbc.sqlserver.SQLServerDriver
cloudscape - short for COM.cloudscape.core.JDBCDriver
pointbase - short for com.pointbase.jdbc.jdbcUniversalDriver
postgresql - short for org.postgresql.Driver
mysqlconj - short for com.mysql.jdbc.Driver - the Connector/J driver for MySQL
mysqlcaucho - short for com.caucho.jdbc.mysql.Driver - the Caucho driver for MySQL
Alternatively, any class name can be specified here. The shortcuts only exist for those of us who generate more typos than real text :)
-cstring This option allows you to specify the connection string to the database. This string is driver specific but almost always starts with "jdbc:". Connection strings for the drivers I have tested look like:
jconnect4, jconnect5, jconnect6 - Sybase connection strings take the form "jdbc:sybase:Tds:[hostname]:[port]/[db_name]"
oraclethin - The Oracle "thin" driver connection string looks like "jdbc:oracle:thin:@[hostname]:[port]:[oracle sid]"
db2app - The DB2 "App" driver connection string looks like "jdbc:db2:[db_name]"
db2net - The DB2 "Net" driver connection string looks like "jdbc:db2://[hostname]:[port]/[db_name]"
mssql - The MS/SQL driver connection string looks like "jdbc:microsoft:sqlserver://[hostname]:[port]/[db_name]"
cloudscape - The Cloudscape driver connection string looks like "jdbc:cloudscape:[db_name];create=true;autocommit=false"
pointbase - The Pointbase driver connection string looks like "jdbc:pointbase:server://[hostname]:[port]/[db_name]"
postgresql - The PostgreSQL driver connection string looks like "jdbc:postgresql://[hostname]:[port]/[db_name]"
mysqlconj - The MySQL Connector/J driver connection string looks like "jdbc:mysql://[hostname]:[port]/[db_name]"
mysqlcaucho - The MySQL Cahcho driver connection string looks like "jdbc:mysql-caucho://[hostname]:[port]/[db_name]"
Important - each JDBC vendor has other flags and parameters that can be passed on the connection string. You should look at the documentation for your JDBC driver for more information. The strings listed are just a sample and may change with a new release of the driver. None of these strings are coded within the application - the list is provided for reference only.

-user or -u The user name to use to log into the database with.
-password or -p The password to use to log into the database with. If this option is missing then the program asks for the password.
-pf Optional file to specify the password. This prevents having to have it visible when looking at a process status. The first line of the file is read and used as the password. If both the command line password and this option are specified the command line password is used.
-c The "command terminator" to use. By default this application uses the string "go" (case insensitive) on a line by itself to determine when to send the string buffer to the database. You may specify something else with the -c option. For example, users of Oracle may prefer either the ";" (semi-colon) character or the "/" (forwardslash) character as that is what sqlplus uses. This string may occur as a standalone line or at the end of a particular line.
-input The name of a file to read commands from instead of System.in.
-query An optional single query to run instead of interacting with the command line or a file. Note - the command must have a command terminator. So, for example, your command line may be something like "-c \; -query "select * from blah;". If you do not include the command terminator then the command will hang, waiting for you to enter the default "go".
-debug This turns on some internal debugging code. Not generally useful.
-driverinfo Allows you to print some information that the driver returns. Generally not very useful in all but a few cases.
-formatter Optionally specify a class name or short cut to format the output. There are three built in short cuts:
csv output the data in CSV format.
xml output the data in XML format.
default (does not have to be specified) - output the format in the "normal" format.
Otherwise, this is a class name that implements com.xigole.util.sql.outputformatter.JisqlFormatter. See the code for more information on implementing your own output formatter.
 

The included default formatter supports the following command line options:
-noheader do not print the header column info.
-spacer The character to use for "empty" space. This defaults to the space character. From mrider - "I added the ability to specify the spacer for columns - which used to be the single char ' '. I did this because of brain-dead Windows' command line copy/paste. It seems that when a line of text ends in space, copy does not copy that space. Which makes it difficult to copy/paste into another program. This can probably be ignored most of the time."
-w Specifies the maximum field width for a column. By default jisql does not try to make the output look "better" By specifying a value for this jisql with truncate the output of columns that are wider than this parameter.
-delimiter Specify a single character delimiter for columns.
-trim trim the spaces from columns.
-nonull print an empty string instead of the word "NULL" when there is a null value.
-left left justify the output
-debug print debugging information about the result set.
 

The include CSV formatter supports the following command line options:
-delimiter specifies the delimiter to use. By default a comma is used
-colnames if included then column names are printed as the first line of output. By default they are not included
 

The included XML formatter does not have any additional output options.
 

New! - if you're having any issues with jisql, come and discuss it in our forums.

Copyright (C) 2004-2010 Scott Dunbar (scott@xigole.com)

Licensed under the Apache License, Version 2.0 (the "License"); you may not use this file except in compliance with the License. You may obtain a copy of the License at

http://www.apache.org/licenses/LICENSE-2.0

Unless required by applicable law or agreed to in writing, software distributed under the License is distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied. See the License for the specific language governing permissions and limitations under the License.
