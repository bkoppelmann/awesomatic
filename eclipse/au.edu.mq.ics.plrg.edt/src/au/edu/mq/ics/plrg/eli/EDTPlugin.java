/*
 * Created on May 31, 2004
 *
 * Overall plugin class for Eli support.
 */
package au.edu.mq.ics.plrg.eli;

import java.io.BufferedReader;
import java.io.BufferedWriter;
import java.io.File;
import java.io.FileReader;
import java.io.FileWriter;
import java.io.InputStream;
import java.io.InputStreamReader;

import org.eclipse.core.resources.IProject;
import org.eclipse.core.resources.IProjectDescription;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.FileLocator;
import org.eclipse.core.runtime.IPath;
import org.eclipse.core.runtime.Path;
import org.eclipse.jface.preference.IPreferenceStore;
import org.eclipse.ui.console.ConsolePlugin;
import org.eclipse.ui.console.IConsole;
import org.eclipse.ui.console.IConsoleManager;
import org.eclipse.ui.console.MessageConsoleStream;
import org.eclipse.ui.plugin.AbstractUIPlugin;
import org.osgi.framework.BundleContext;

import au.edu.mq.ics.plrg.odin.Derivations;
import au.edu.mq.ics.plrg.odin.MainPreferencePage;
import au.edu.mq.ics.plrg.odin.VarPreferencePage;

/**
 * @author asloane
 *
 * General facilities for Eli support.
 */
public class EDTPlugin extends AbstractUIPlugin {

    // The EDT plugin ID.
    public static final String EDT = "au.edu.mq.ics.plrg.edt";

    // The shared instance.
    private static EDTPlugin plugin;
    
    // The derivations available to all Eli projects: in-memory, file path and file
    private Derivations derivations;
    private final IPath DERIVATIONS_FILEPATH = new Path("derivations");
    private File derivationsFile;
    
    // Process type for running Eli derivations
    public static final String PROCESS_TYPE = EDT + ".eliRun";

    /**
     * The constructor.
     */
    public EDTPlugin() {
        super();
        plugin = this;
    }
    
    /**
     * Returns the shared instance.
     */
    public static EDTPlugin getDefault() {
        return plugin;
    }

    /**
     * This method is called upon plug-in activation
     */
    public void start(BundleContext context) throws Exception {
        super.start(context);
        
        // Reader for the derivations file or null if none
        BufferedReader reader = null;
        
        // Look for the derivations file in the workspace.  If not there, look in the
        // plugin installation directory.
        IPath wspath = getDefault().getStateLocation().append(DERIVATIONS_FILEPATH);
        derivationsFile = wspath.toFile();
        if (derivationsFile.exists()) {
            reader = new BufferedReader(new FileReader(derivationsFile));
        } else { 
            InputStream istream = FileLocator.openStream(context.getBundle(), DERIVATIONS_FILEPATH, false);
            reader = new BufferedReader(new InputStreamReader(istream)); 
        }
        
        // If we found it read the derivations, otherwise there are none.
        if (reader == null)
            derivations = new Derivations();
        else {
            derivations = new Derivations(reader);
            reader.close();
        }            
    }

    /**
     * This method is called when the plug-in is stopped
     */
    public void stop(BundleContext context) throws Exception {
        // Ask the Eli model to clean up
        EliModel.finalise();
        
        // Save the derivations
        BufferedWriter writer = new BufferedWriter(new FileWriter(derivationsFile));
        derivations.write(writer);
        writer.close();
        
        super.stop(context);
    }
    
    /**
     * Make a Eli console returning its stream.  A console already existing with
     * the same name is reused.
     */
    public MessageConsoleStream getConsoleStream(String name) {
        IConsoleManager consoleManager = ConsolePlugin.getDefault().getConsoleManager();
        IConsole[] existing = consoleManager.getConsoles();
        EliConsole console = null;
        name = "Eli " + name;
        for (int i = 0; i < existing.length; i++)
            if (name.equals(existing[i].getName())) {
                console = (EliConsole) existing[i];
                if (getPreferenceStore().getBoolean(MainPreferencePage.CLEAR_CONSOLE_KEY))
                    console.clearConsole();
                console.activate();
                break;
            }
        if (console == null) {
            console = new EliConsole(name, null); 
            consoleManager.addConsoles(new IConsole[] { console });
        }
        return console.newMessageStream();
    }
    
    // Eli nature support
    
    public static final String ELI_NATURE = "au.edu.mq.ics.plrg.edt.eliNature";
    
    /**
     * Add the Eli nature to a project.
     */
    public static void addEliNature(IProject project) throws CoreException {
        if (project.hasNature(ELI_NATURE))
            return;
        IProjectDescription description = project.getDescription();
        String[] ids = description.getNatureIds();
        String[] newIds = new String[ids.length + 1];
        System.arraycopy(ids, 0, newIds, 0, ids.length);
        newIds[ids.length] = ELI_NATURE;
        description.setNatureIds(newIds);
        project.setDescription(description, null);
    }
    
    /**
     * Remove the Eli nature from a project.
     */
    public static void removeEliNature(IProject project) throws CoreException {
        IProjectDescription description = project.getDescription();
        String[] ids = description.getNatureIds();
        for (int i = 0; i < ids.length; i++) {
            if (ids[i].equals(ELI_NATURE)) {
                String[] newIds = new String[ids.length - 1];
                System.arraycopy(ids, 0, newIds, 0, i);
                System.arraycopy(ids, i+1, newIds, i, ids.length - i - 1);
                description.setNatureIds(newIds);
                project.setDescription(description, null);
                return;
            }
        }
    }
    
    /**
     * Set up the default preferences.
     */
    protected void initializeDefaultPreferences(IPreferenceStore store) {
        // Default Odin executable preference
        store.setDefault(MainPreferencePage.ODIN_KEY, "");
        
        // Default default cache preferences
        store.setDefault(MainPreferencePage.USE_DEFAULT_CACHE_KEY, false);
        store.setDefault(MainPreferencePage.DEFAULT_CACHE_KEY, "/");
        
        // Clear console
        store.setDefault(MainPreferencePage.CLEAR_CONSOLE_KEY, true);

        // Log and error level
        store.setDefault(VarPreferencePage.LOGLEVEL_KEY, "4");
        store.setDefault(VarPreferencePage.ERRLEVEL_KEY, "0");
        store.setDefault(VarPreferencePage.WARNLEVEL_KEY, "0");
    }
    
    /**
     * Return the current derivations.
     */
    public Derivations getDerivations() {
        return derivations;
    }
    
    /**
     * Set the derivations.
     */
    public void setDerivations(Derivations derivations) {
        this.derivations = derivations;
    }
    
}
