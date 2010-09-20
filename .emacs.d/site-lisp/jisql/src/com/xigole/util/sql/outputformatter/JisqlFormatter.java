package com.xigole.util.sql.outputformatter;

import java.io.PrintStream;
import java.sql.ResultSet;
import java.sql.ResultSetMetaData;

import joptsimple.OptionParser;
import joptsimple.OptionSet;


/**
 * This is the definition of what a JisqlFormatter does.
 *
 */
public interface JisqlFormatter {
    
	/**
     * Sets a the option list for this formatter.
     * 
     * @param parser - the OptionParser to use.
     */
    public void setSupportedOptions( OptionParser parser );
    
    /**
     * Consumes any options that were specified on the command line.
     *
     * @param options the OptionSet that the main driver is using.  Implementing
     *                classes should add their supported parameters to the list.
     *
     * @throws Exception if there is a problem parsing the command line arguments.
     *                   Note that Jisql includes jopt-simple so you can use that
     *                   to parse your command line.  See
     *                   <a href="http://jopt-simple.sourceforge.net/">http://jopt-simple.sourceforge.net/</a>
     *                   for more information.
     *
     */
    public void consumeOptions( OptionSet options ) throws Exception;

    /**
     * Called to output a usage message to the command line window.  This
     * message should contain information on how to call the formatter.
     *
     * @param out where to put the usage message.
     *
     */
    public void usage( PrintStream out );

    
    /**
     * Outputs a header for a query.  This is called before any data is
     * output.
     *
     * @param out where to put header output.
     * @param metaData the ResultSetMetaData for the output.
     *  
     */
    public void formatHeader( PrintStream out, ResultSetMetaData metaData ) throws Exception;

    /**
     * Called to output the data.
     *
     * @param out where to put output data.
     * @param resultSet the ResultSet for the row.
     * @param metaData the ResultSetMetaData for the row.
     *
     */
    public void formatData( PrintStream out, ResultSet resultSet, ResultSetMetaData metaData ) throws Exception;

    /**
     * Outputs a footer for a query.  This is called after all data has been
     * exhausted.
     *
     * @param out where to put footer output.
     * @param metaData the ResultSetMetaData for the output.
     * 
     */
    public void formatFooter( PrintStream out, ResultSetMetaData metaData ) throws Exception;
}
