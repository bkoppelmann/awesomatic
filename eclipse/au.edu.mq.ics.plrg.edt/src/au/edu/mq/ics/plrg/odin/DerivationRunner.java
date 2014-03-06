/*
 * Created on Jun 11, 2004
 *
 * Run derivations.
 */
package au.edu.mq.ics.plrg.odin;

import java.io.BufferedReader;
import java.io.File;
import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.lang.reflect.InvocationTargetException;
import java.net.InetAddress;
import java.net.UnknownHostException;

import org.eclipse.core.resources.IFile;
import org.eclipse.core.resources.IMarker;
import org.eclipse.core.resources.IProject;
import org.eclipse.core.resources.IResource;
import org.eclipse.core.resources.IWorkspace;
import org.eclipse.core.resources.IWorkspaceRunnable;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IPath;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.Path;
import org.eclipse.jface.operation.IRunnableWithProgress;
import org.eclipse.jface.preference.IPreferenceStore;
import org.eclipse.swt.widgets.Display;
import org.eclipse.ui.IWorkbench;
import org.eclipse.ui.IWorkbenchPage;
import org.eclipse.ui.IWorkbenchWindow;
import org.eclipse.ui.PartInitException;
import org.eclipse.ui.PlatformUI;
import org.eclipse.ui.console.MessageConsoleStream;
import org.eclipse.ui.ide.IDE;

import au.edu.mq.ics.plrg.eli.EDTPlugin;

/**
 * @author asloane
 *
 * Support for running derivations.
 */
public class DerivationRunner implements IRunnableWithProgress {

    // The projects within which this derivation is to be run
    private OdinProject odinProject;
    private IProject project;
    
    // The file to which the derivation is to be applied
    private IResource src;
    
    // The file to which the otuput of the derivation goes if in file mode
    private IFile outfile;
    
    // The derivation to run
    private Derivation derivation;
    
    // Optional arguments to supply to the derivation
    private String args;
    
    // The mode in which to run the derivation
    private String mode;
    
    // The presentation name for this derivation
    private String name;
    
    // The console stream for output
    private MessageConsoleStream consoleStream;
    
    // The work associated with this derivation
    private int totalWork;
    
    // The multiplier to convert loglevel into total work
    private static final int WORK_MULTIPLIER = 200;
    
    // The first loglevel for which line by line progress monitor is to be used.
    private static final int BOUNDARY_LOGLEVEL = 4;
    
    // The work associated with the boundary log level.
    private static final int WORK_BOUNDARY = BOUNDARY_LOGLEVEL * WORK_MULTIPLIER;
    
    // Message for progress monitor
    private String monitorMessage; 

    // Cache of project location and cache location
    private String projectLoc;
    private String cacheLoc;
    
    // Eli problem marker name
    private String ELIPROBLEM = EDTPlugin.EDT + ".eliproblemmarker";
    
    // If we've seen any errors
    private boolean haserrors;
    
    /**
     * Create a runner given the project, file, derivation, mode name and output file.
     */
    public DerivationRunner(OdinProject odinProject, IResource src, Derivation derivation,
                            String args, String mode, String name, IFile outfile) {
        this.odinProject = odinProject;
        project = odinProject.getProject();
        this.src = src;
        this.derivation = derivation;
        this.args = args;
        this.mode = mode;
        this.name = name;
        this.outfile = outfile;
                
        // Save project location
        projectLoc = toOdinFilename(project.getLocation());
        
        // Save cache location.  We have to append the machine name because
        // the actual cache storage is at getCache()/machine/...
        // But ODINVIEW overrides this...
        String view = odinProject.getView();
        if (view.equals("")) {
            try {
                view = InetAddress.getLocalHost().getHostName();
            } catch (UnknownHostException e) {
                view = "";
            }
        }
        cacheLoc = odinProject.getCache() + "/" + view;
    }
    
    /**
     * Convert a path into a filename suitable for Odin.  Normally this just does
     * the usual OS conversion but on Windows it also needs to convert to a Cygwin
     * path.  We use Eclipse's toString() to do the slash conversion and replace
     * a drive specifier that remains. 
     */
    private String toOdinFilename(IPath path) {
        String name = path.toString();
        if ((name.length() >= 2) && (name.charAt(1) == ':')) {
            // Assume looks like C:/...
            name = "/cygdrive/" + name.charAt(0) + name.substring(2); 
        }
        return name;
    }
    
    /**
     * Convert an Odin filename into a native file name.  Normally this just does the usual
     * OS conversion, but on Windows it also needs to convert from a Cygwin path.
     * The Eclipse path separator is a slash, so no delimiter conversion is done.
     */
    private String fromOdinFileName(String name) {
        if (name.startsWith("/cygdrive/")) {
            name = name.charAt(10) + ":" + name.substring(11);
        }
        return name;
    }
    
    /**
     * Helper function to build an Odin command line.  The complication is that if the
     * project args are not null we need to insert them between the Eli executable and
     * the Odin cmd arguments.  This is needed to support Windows that can't invoke the
     * Eli script directly (since it's a sh script).  In this case the user must set
     * the executable to sh and the args to Eli executable.
     */
    static public String[] makeOdinCommand(OdinProject odinProject, String[] cmd) {
        String[] cmdarray;
        String args = odinProject.getArgs();
        int base = 1;
        int cmdlen = cmd.length;
        if (args.length() == 0)
            cmdarray = new String[cmdlen + 1];
        else {
            cmdarray = new String[cmdlen + 2];
            cmdarray[1] = args; 
            base = 2;
        }
        cmdarray[0] = odinProject.getOdin();
        for (int i = 0; i < cmd.length; i++)
            cmdarray[base+i] = cmd[i];
        return cmdarray;
    }
    
    /**
     * Run a derivation
     */
    public void run(IProgressMonitor monitor) throws InvocationTargetException,
            InterruptedException {

        // Get the console details
        EDTPlugin plugin = EDTPlugin.getDefault();
        consoleStream = plugin.getConsoleStream(name);
        
        // Get the values for the Odin variables
        IPreferenceStore preferenceStore = EDTPlugin.getDefault().getPreferenceStore();
        String loglevel = preferenceStore.getString(VarPreferencePage.LOGLEVEL_KEY);
        String errlevel = preferenceStore.getString(VarPreferencePage.ERRLEVEL_KEY);
        String warnlevel = preferenceStore.getString(VarPreferencePage.WARNLEVEL_KEY);

        // Setup the commands to establish the Odin variables
        String precmds =
            "LogLevel=" + loglevel + ";" +
            "ErrLevel=" + errlevel + ";" +
            "WarnLevel=" + warnlevel + ";";

        // Setup the complete derivation by substituting the placeholders:
        //    %a - optional arguments from launch config
        //    %d - root directory of the project
        //    %s - filename of the source specification
        String product = derivation.getProduct();
        product = product.replaceAll("%a", args);
        product = product.replaceAll("%d", projectLoc);
        String source = toOdinFilename(src.getLocation());
        product = product.replaceAll("%s", source);

        // Setup the Odin command
        String args = odinProject.getArgs();
        IPath cachePath = new Path(odinProject.getCache());
        String cache = toOdinFilename(cachePath);
        String[] cmdarray;
        String odin = odinProject.getOdin();
        int prodindex;
        if (args.length() == 0) {
            cmdarray = new String[] { odin, "-c", cache, precmds + product + ":warning>" };
            prodindex = 3;
        } else {
            cmdarray = new String[] { odin, args, "-c", cache, precmds + product + ":warning>" };
            prodindex = 4;
        }
                
        // Remove any markers and clear error status
        try {
            project.deleteMarkers(ELIPROBLEM, true, IResource.DEPTH_INFINITE);
        } catch (CoreException e) {
            e.printStackTrace();
        }
        haserrors = false;

        // Fake the amount of work required.  If loglevel is four or more then we
        // will get lots of output from Odin along the way.  Guess at a multiplier
        // of loglevel lines and count one unit of work for each line of output.
        // If loglevel is less just use a simple three-step process: exec, input, error.
        totalWork = Integer.parseInt(loglevel) * WORK_MULTIPLIER;
        
        // Set the message for the monitor.  We can't do monitor.beginTask(...) here
        // because Workspace.run(...) does one when it starts.  To get our message
        // and total work we defer this until the runnable runs in updateConsole().
        monitorMessage = "Deriving " + name;
        
        // Create a process to run the derivation
        runProcess(cmdarray, monitor, true);

        // If no errors then issue further Eli commands to properly deal with
        // the output:
        //     - ignore: just run the derivation without :warning so that ones
        //       that are executable (e.g. :run) will get done by Odin.  We react
        //       to these lines since a run may produce Eli-standard errors which
        //       we want to backtrack to the processor's input
        //     - show: append > so that we see the output in the console
        //     - edit: use :filename to get the name of the file holding the
        //       output and then open an editor on it
        //     - file: use >filename to send it to the requested file
        if (!haserrors) {
            if (mode.equals(Derivation.DERIVE)) {
                cmdarray[prodindex] = product;
                runProcess(cmdarray, monitor, true);
            } if (mode.equals(Derivation.SHOW)) {
                cmdarray[prodindex] = product + ">";
                runProcess(cmdarray, monitor, false);
            } else if (mode.equals(Derivation.EDIT)) {
                cmdarray[prodindex] = product + ":filename>";
                String name = grabOutput(cmdarray);
                if (name != null)
                    editFile(name);
            } else if (mode.equals(Derivation.FILE)) {
                String output = toOdinFilename(outfile.getLocation());
                cmdarray[prodindex] = product + ">" + output;
                runProcess(cmdarray, monitor, false);
                try {
                    outfile.refreshLocal(IResource.DEPTH_ONE, monitor);
                } catch (CoreException e) {
                    e.printStackTrace();
                }
            }
        }

        // Provide some feedback since Eli might not actually produce any
        consoleStream.println(monitorMessage + " complete");

        // All done
        monitor.done();
    }
    
    /**
     * Open an editor on the named file, linking into the workspace if necessary.
     */
    private void editFile(String name) {
        final IFile file;
        name = fromOdinFileName(name);
        if (name.startsWith(projectLoc)) {
            // If the file is in the project just open an editor on it directly
            name = name.substring(projectLoc.length() + 1);
            file = project.getFile(new Path(name));
            // Refresh so Eclipse can see the file
            try {
                file.refreshLocal(IResource.DEPTH_ZERO, null);
            } catch (CoreException e) {
                e.printStackTrace();
                return;
            }
        } else {
            try {
                // If the file to be linked already exists at the top of the project
                // remove it since it may be linked to the wrong thing
                IPath path = new Path(name);
                file = project.getFile(path.lastSegment());
                if (file.exists())
                    file.delete(true, false, null);
                file.createLink(path, 0, null); 
            } catch (CoreException e) {
                e.printStackTrace();
                return;
            }
        }

        Display.getDefault().asyncExec(new Runnable() {
            public void run() {
                try {
                    IWorkbench wb = PlatformUI.getWorkbench();
                    IWorkbenchWindow win = wb.getActiveWorkbenchWindow();
                    final IWorkbenchPage page = win.getActivePage();
                    IDE.openEditor(page, file);
                } catch (PartInitException e) {
                    e.printStackTrace();
                }
            }
        });
    }

    /**
     * Check the exit status of an eli invocation and pop a dialog if something went
     * wrong.
     */
    private void checkStatus(String[] cmdarray, int ret) {
        if (ret == 0) return;
        StringBuffer buffer = new StringBuffer();
        buffer.append("Error executing Eli: The command '");
        for (int i = 0; i < cmdarray.length; i++) {
            buffer.append(cmdarray[i]);
            buffer.append(' ');
        }
        buffer.append("' returned an exit status of ");
        buffer.append(ret);
        consoleStream.println(buffer.toString());
    }
    
    /**
     * Run the actual process.
     */
    private void runProcess(String[] cmdarray, IProgressMonitor monitor,
                            boolean reactToLines)
            throws InterruptedException {
        Runtime runtime = Runtime.getRuntime();
        try {
            File cwd = new File(projectLoc);
            Process process = runtime.exec(cmdarray, null, cwd);
            if (totalWork < WORK_BOUNDARY)
                monitor.worked(totalWork / 3);
            updateConsole(process, monitor, reactToLines);
            int ret = process.waitFor();
            checkStatus(cmdarray, ret);
        } catch (IOException e) {
            e.printStackTrace();
        }
    }

    /**
     * Update the console in a batch fashion.  Unfortunately the API is broken in
     * that Workspace.run() insists on calling beginTask on the progress monitor
     * whereas we would prefer to use our own settings.  We fake things by telling
     * the monitor that it is done and then calling beginTask again.  This seems
     * to ignore the taskname argument the second time so we use setTaskName which
     * appears to work. 
     */
    private void updateConsole(final Process process, IProgressMonitor monitor,
                   final boolean reactToLines) {
        IWorkspace workspace = project.getWorkspace();
        IWorkspaceRunnable runnable =
            new IWorkspaceRunnable() {
                public void run(IProgressMonitor monitor) throws CoreException {
                    try {
                        monitor.done();
                        monitor.beginTask(null, totalWork);
                        monitor.setTaskName(monitorMessage);
                        copyInputStream(process.getInputStream(), consoleStream, monitor, reactToLines);
                        copyInputStream(process.getErrorStream(), consoleStream, monitor, reactToLines);
                    } catch (IOException e) {
                        e.printStackTrace();
                    }
                }
            };
        try {
            workspace.run(runnable, project, IWorkspace.AVOID_UPDATE, monitor);
        } catch (CoreException e) {
            e.printStackTrace();
        }
    }

    /**
     * Get a single-string result from Eli
     */
    private String grabOutput(String[] cmdarray) {
        String result = null;
        try {
            Runtime runtime = Runtime.getRuntime();
            File cwd = new File(projectLoc);
            Process process = runtime.exec(cmdarray, null, cwd);
            InputStream stream = process.getInputStream();
            BufferedReader reader = new BufferedReader(new InputStreamReader(stream));
            String line = reader.readLine();
            while (line != null) {
                result = line;
                line = reader.readLine();
            }
        } catch (IOException e) {
            e.printStackTrace();
        }
        return result;
    }
    
    /**
     * Copy an input stream to a console stream.
     */
    private void copyInputStream(InputStream stream, MessageConsoleStream consoleStream,
                                 IProgressMonitor monitor, boolean reactToLines)
            throws IOException {
        if (totalWork < WORK_BOUNDARY)
            monitor.worked(totalWork / 3);
        
        // Process the output lines
        BufferedReader reader = new BufferedReader(new InputStreamReader(stream));
        String line = reader.readLine();
        while (line != null) {
            line = replace(line, "$ODINCACHE", cacheLoc);
            line = replace(line, projectLoc + "/", "");
            consoleStream.println(line);
            if (reactToLines)
                reactToLine(line);
            if (totalWork >= WORK_BOUNDARY)
                monitor.worked(1);
            line = reader.readLine();
        }
    }
    
    /**
     * Utility function to replace a non-regex pattern in a string with a string.
     */
    private String replace(String str, String pattern, String replace) {
        int s = 0;
        int e = 0;
        StringBuffer result = new StringBuffer();
    
        while ((e = str.indexOf(pattern, s)) >= 0) {
            result.append(str.substring(s, e));
            result.append(replace);
            s = e+pattern.length();
        }
        result.append(str.substring(s));
        return result.toString();
    }

    /**
     * React to a line of output in an appropriate manner.
     */
    private void reactToLine(String output) {
        if ((output == null) || (output.length() == 0))
            return;
        if (output.charAt(0) == '"')
            addMarkers(output);
    }
    
    /**
     * Add markers for the Eli message in this line.
     */
    private void addMarkers(String output) {
        // Format is:
        // "expr.con", line 6:11 ERROR: Syntax error
        // 1                5 6  7      10->
        
        // Break the line up
        String[] fields = output.split("[\", :]");
        
        // Extract the basic fields
        IFile file = project.getFile(fields[1]);
        int line;
        try {
            line = Integer.parseInt(fields[5]);
        } catch (NumberFormatException e) {
            // Ignore line, since number can't be parsed
            return;
        }
        String sev = fields[7];

        // If the file doesn't exist do nothing, refresh first in case it's
        // a file in the cache or somewhere that Eclipse doesn't know about
        try {
            file.refreshLocal(IResource.DEPTH_ZERO, null);
        } catch (CoreException e) {
            return;
        }
        if (!file.exists())
            return;
        
        // Set the marker type and note errors
        int severity = -1;
        if (sev.equals("NOTE")) {
            severity = IMarker.SEVERITY_INFO;
        } else if (sev.equals("WARNING")) {
            severity = IMarker.SEVERITY_WARNING;
        } else if (sev.equals("ERROR")) {
            severity = IMarker.SEVERITY_ERROR;
            haserrors = true;
        }
        
        // Catch case where format is not obeyed, and set up message
        int messageStart;
        if (severity == -1) {
            // Not in standard Eli format.  Now assumed to be one of:
            // "expr.c", line 6,11: Some error message (up to 4.4.0)
            // "expr.c", line 6:11 Some error message (later)
            // Assumed to be an error not warning etc
            severity = IMarker.SEVERITY_ERROR;
            haserrors = true;
            // Message is after the space after the colon
            messageStart = output.indexOf(":");
            do {
                messageStart++;
            } while (output.charAt(messageStart) != ' ');
            messageStart++;
        } else {
            // Standard format, message is after the severity
            messageStart = output.indexOf(sev) + sev.length() + 2;
        }
        String message = output.substring(messageStart);
        
        // Create a marker and attach to file
        try {
            IMarker marker = file.createMarker(ELIPROBLEM);
            marker.setAttribute(IMarker.SEVERITY, severity);
            marker.setAttribute(IMarker.MESSAGE, message);
            marker.setAttribute(IMarker.LINE_NUMBER, line);
        } catch (CoreException e) {
            e.printStackTrace();
        }
    }

}
