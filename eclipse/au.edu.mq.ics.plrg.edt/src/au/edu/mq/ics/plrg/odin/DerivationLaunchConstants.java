/*
 * Created on Apr 24, 2006
 *
 * Constants for Eli derivation launch attributes.
 */
package au.edu.mq.ics.plrg.odin;

import au.edu.mq.ics.plrg.eli.EDTPlugin;

/**
 * @author asloane
 * Copyright, 2006, Anthony Sloane.
 * 
 * Define the attributes for Eli derivation launches.
 */
public class DerivationLaunchConstants {
    
    // Configuration type identifier
    public static final String CONFIGTYPE = EDTPlugin.EDT + ".derivationLaunchConfigurationType";
    
    // Process factory
    public static final String PROCESS_FACTORY = EDTPlugin.EDT + ".derivationProcessFactory";

    // Source file 
    public static final String ATTR_FILE = EDTPlugin.EDT + ".file";
    
    // Derivation name
    public static final String ATTR_DERNAME = EDTPlugin.EDT + ".dername";
    
    // Optional arguments
    public static final String ATTR_ARGS = EDTPlugin.EDT + ".args";
    
    // Derivation mode
    public static final String ATTR_DERMODE = EDTPlugin.EDT + ".dermode";

    // Output file
    public static final String ATTR_OUTFILE = EDTPlugin.EDT + ".outfile";

}
