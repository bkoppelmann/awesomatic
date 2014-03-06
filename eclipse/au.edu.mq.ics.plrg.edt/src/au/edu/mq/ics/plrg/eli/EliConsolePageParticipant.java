/*
 * Created on May 8, 2006
 *
 * Console page participant for Eli plugin.
 */
package au.edu.mq.ics.plrg.eli;

import org.eclipse.jface.action.IToolBarManager;
import org.eclipse.ui.console.IConsole;
import org.eclipse.ui.console.IConsoleConstants;
import org.eclipse.ui.console.IConsolePageParticipant;
import org.eclipse.ui.console.actions.CloseConsoleAction;
import org.eclipse.ui.part.IPageBookViewPage;

/**
 * @author asloane
 *
 * Eli console page participant.  Modelled on: 
 * org.eclipse.jdt.internal.debug.ui.console.JavaStackTracePageParticipant
 * There apparently will be a better way to do this in later Eclipse versions.
 */
public class EliConsolePageParticipant implements IConsolePageParticipant {

    /**
     * Initialise the page participant.a
     */
    public void init(IPageBookViewPage page, IConsole console) {
        // Contribute a "Close Console" button to eli console toolbars, see
        // enablement in the plugin manifest.
        IToolBarManager manager = page.getSite().getActionBars().getToolBarManager();
        manager.appendToGroup(IConsoleConstants.LAUNCH_GROUP, new CloseConsoleAction(console));
    }
    
    /**
     * Clean up nothing.
     */
    public void dispose() {
    }

    /**
     * Adapt not at all.
     */
    public Object getAdapter(Class adapter) {
        return null;
    }

    /**
     * Changed to activated state.
     */
    public void activated() {
    }

    /**
     * Change to deactivated state.
     */
    public void deactivated() {
    }

}
