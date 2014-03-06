/*
 * Created on May 8, 2006
 *
 * Console for Eli plugin.
 */
package au.edu.mq.ics.plrg.eli;

import org.eclipse.jface.resource.ImageDescriptor;
import org.eclipse.ui.console.MessageConsole;

/**
 * @author asloane
 *
 * Eli consoles are message consoles with a distinguished type.
 */
public class EliConsole extends MessageConsole {

    /**
     * Construct an Eli console.
     */
    public EliConsole(String name, ImageDescriptor imageDescriptor) {
        super(name, imageDescriptor);
    }
    
    /**
     * Get the type of the console.  Needed so that we can use it in an
     * enablement test to get toolbar actions specialised.
     */
    public String getType() {
        return "eliConsole";
    }
    
}
