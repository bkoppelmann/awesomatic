/*
 * Created on April 24, 2006
 *
 * Launching Eli derivations.
 */
package au.edu.mq.ics.plrg.odin;

import java.io.File;
import java.lang.reflect.InvocationTargetException;

import org.eclipse.core.resources.IFile;
import org.eclipse.core.resources.IProject;
import org.eclipse.core.resources.IResource;
import org.eclipse.core.resources.IWorkspaceRoot;
import org.eclipse.core.resources.ResourcesPlugin;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.Path;
import org.eclipse.debug.core.ILaunch;
import org.eclipse.debug.core.ILaunchConfiguration;
import org.eclipse.debug.core.ILaunchManager;
import org.eclipse.debug.core.model.ILaunchConfigurationDelegate;
import org.eclipse.jface.dialogs.MessageDialog;
import org.eclipse.swt.widgets.Display;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.ui.PlatformUI;

import au.edu.mq.ics.plrg.eli.EDTPlugin;
import au.edu.mq.ics.plrg.eli.EliModel;

public class DerivationLaunchConfigurationDelegate implements
		ILaunchConfigurationDelegate {

	public void launch(final ILaunchConfiguration config, String mode,
			ILaunch launch, IProgressMonitor monitor) throws CoreException {
		
        if (monitor.isCanceled()) return;
        
        // We only know how to handle run mode
        if (mode.equals(ILaunchManager.RUN_MODE)) {
            
            // Get the launch configuration attributes
            final String fileName = config.getAttribute(DerivationLaunchConstants.ATTR_FILE, "");
            final String derivationName = config.getAttribute(DerivationLaunchConstants.ATTR_DERNAME, "");
            final String args = config.getAttribute(DerivationLaunchConstants.ATTR_ARGS, "");
            String derivationMode = config.getAttribute(DerivationLaunchConstants.ATTR_DERMODE, "");
            String outfileName = config.getAttribute(DerivationLaunchConstants.ATTR_OUTFILE, "");

            if (monitor.isCanceled()) return;
            
            // Get the project, file and derivation objects
            IWorkspaceRoot workspaceRoot = ResourcesPlugin.getWorkspace().getRoot();
            IResource src = workspaceRoot.findMember(new Path(fileName));
            if (src == null) {
                complain(config,
                         "This launch cannot proceed since the source file '" +
                         fileName + "' no longer exists.  Please fix the source file " +
                         "setting in the launch configuration or recreate the file.");
                return;
            }

            IFile outfile = null;
            if (derivationMode.equals(Derivation.FILE)) {
                outfile = workspaceRoot.getFile(new Path(outfileName));
                if (outfile.getLocation() == null) {
                    complain(config,
                            "This launch cannot proceed because the output file name '" +
                            outfileName + "' cannot be resolved in the workspace.  Please " +
                            "fix the output file setting in the launch configuration.");
                    return;
                }
            }
            IProject project = src.getProject();
            OdinProject odinProject = EliModel.create(project);
            
            Derivations derivations = EDTPlugin.getDefault().getDerivations();
            Derivation derivation = derivations.getDerivation(derivationName);
            if (derivation == null) {
                complain(config,
                        "This launch cannot proceed since the derivation '" +
                        derivationName + "' no longer exists.  Please fix the derivation " +
                        "setting in the launch configuration or recreate the derivation " +
                        "in the global preferences.");
               return;
            }
            
            if (monitor.isCanceled()) return;
            
            // Don't allow launch if the project is ill-formed.  These same
            // checks are made in the launch config dialog and reported to the user there.
            String odin = odinProject.getOdin();
            if (odin.equals("")) return;
            File odinFile = new File(odin);
            if (!odinFile.exists() || !odinFile.isFile()) return;
            
            if (monitor.isCanceled()) return;

            DerivationRunner runner = 
                new DerivationRunner(odinProject, src, derivation, args, derivationMode, 
                                     config.getName(), outfile);
            
            if (monitor.isCanceled()) return;
            
            try {
                runner.run(monitor);
            } catch (InvocationTargetException e) {
                // handle exception
                e.printStackTrace();
            } catch (InterruptedException e) {
                // handle cancelation, nothing to do
            }
            
        }

	}
    
    /**
     * General routine to complain about an error in the launch.
     */
	private void complain(final ILaunchConfiguration config, final String msg) {
        Display.getDefault().asyncExec(new Runnable() {
            public void run() {
                Shell workbenchShell = PlatformUI.getWorkbench().getActiveWorkbenchWindow().getShell();
                MessageDialog.openInformation(workbenchShell,
                                              "Error launching '" + config.getName() + "'",
                                              msg);
            }
        });   
    }
}
