/*
 * Created on Jun 15, 2004
 *
 * Reset the Odin cache.
 */
package au.edu.mq.ics.plrg.odin;

import java.io.File;
import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.lang.reflect.InvocationTargetException;

import org.eclipse.core.resources.IProject;
import org.eclipse.core.resources.IResource;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.jface.action.IAction;
import org.eclipse.jface.dialogs.IDialogConstants;
import org.eclipse.jface.dialogs.MessageDialog;
import org.eclipse.jface.dialogs.ProgressMonitorDialog;
import org.eclipse.jface.operation.IRunnableWithProgress;
import org.eclipse.jface.viewers.ISelection;
import org.eclipse.jface.viewers.IStructuredSelection;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.ui.IObjectActionDelegate;
import org.eclipse.ui.IWorkbenchPart;
import org.eclipse.ui.IWorkbenchWindow;
import org.eclipse.ui.IWorkbenchWindowActionDelegate;

import au.edu.mq.ics.plrg.eli.EliModel;

/**
 * @author asloane
 *
 * Action to reset a project's cache.
 */
public class ResetCacheAction implements IWorkbenchWindowActionDelegate,
        IObjectActionDelegate, IRunnableWithProgress {

    // Cache of workbench shell
    private Shell workbenchShell;
    
    // Cache of selection
    private ISelection selection;
    
    // Cache of project
    private OdinProject odinProject;
    
    // Cache of Odin cache
    private String cache;
    
    // True if we want to reinstall the pkgs, false to just reset
    private boolean reinstall;
    
    /**
     * Initialise the action.
     */
    public void init(IWorkbenchWindow window) {
        workbenchShell = window.getShell();
    }
    
    /**
     * Adjust the actions based on the part that is active.
     */
    public void setActivePart(IAction action, IWorkbenchPart targetPart) {
    }
    
    /**
     * Get rid of anything we have allocated.
     */
    public void dispose() {
    }

    /**
     * Perform the actions.
     */
    public void run(IAction action) {
        // Find the project containing the selection
        IStructuredSelection structuredSelection = (IStructuredSelection) selection;
        Object object = structuredSelection.getFirstElement();
        if (object instanceof IProject) {
            IProject project = (IProject) object;
            odinProject = EliModel.create(project); 
        } else if (object instanceof IResource) {
            IResource rsrc = (IResource) object;
            odinProject = EliModel.create(rsrc.getProject());
        } else
            return;
        
        // Reset the cache if it exists
        cache = odinProject.getCache();
        if (!cache.equals("")) {
            // Confirm that this is really desired
            if (!MessageDialog.openQuestion(
                    workbenchShell, "Confirm Cache Reset",
                    "Are you sure you want to reset the cache at " + cache +"?"))
                return;
            File path = new File(cache);
            if (path.exists()) {
                MessageDialog dialog =
                    new MessageDialog(
                        workbenchShell, "Reset Cache", null, "Reinstall packages?",
                        MessageDialog.QUESTION,
                        new String[]{IDialogConstants.YES_LABEL, IDialogConstants.NO_LABEL},
                        1);
                reinstall = (dialog.open() == 0);

                try {
                    new ProgressMonitorDialog(workbenchShell).run(true, true, this);
                } catch (InterruptedException e) {
                    // Nothing to do for cancel
                } catch (InvocationTargetException e) {
                    e.printStackTrace();
                }
            }
        }
    }

    /**
     * Remember the selection.
     */
    public void selectionChanged(IAction action, ISelection selection) {
        this.selection = selection;
    }
    
    /**
     * Reset the cache with monitored progress.
     */
    public void run(IProgressMonitor monitor) {
        // Set up command
        String[] cmdarray =
            DerivationRunner.makeOdinCommand(
                odinProject,
                new String[] {
                    reinstall ? "-R" : "-r",
                    "-c",
                    cache,
                    "foo"    // This causes Odin to exit after reset
                }
            );
                        
        // Notify the monitor that we are beginning
        String prefix = reinstall ? "Reinstalling" : "Resetting";
        monitor.beginTask(prefix + " " + cache, 2);
        
        // Create a process to run the reset
        Runtime runtime = Runtime.getRuntime();
        try {
            Process process = runtime.exec(cmdarray);
            monitor.worked(1);
            // Read the output from odin but ignore it, otherwise it seems to
            // block when some output buffer is full, blocking the whole reset.
            InputStream inputStream = process.getInputStream();
            InputStreamReader reader = new InputStreamReader(inputStream);
            while (reader.read() != -1)
                ;
            process.waitFor();
        } catch (IOException e) {
            e.printStackTrace();
        } catch (InterruptedException e) {
            e.printStackTrace();
        }
        
        // All done
        monitor.done();
    }

}
