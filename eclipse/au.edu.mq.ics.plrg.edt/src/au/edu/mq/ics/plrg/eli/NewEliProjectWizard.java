/*
 * Created on May 31, 2004
 *
 * Wizard for creating new Eli projects.
 */
package au.edu.mq.ics.plrg.eli;

import org.eclipse.core.resources.IProject;
import org.eclipse.jface.viewers.IStructuredSelection;
import org.eclipse.jface.wizard.IWizardContainer;
import org.eclipse.jface.wizard.IWizardPage;
import org.eclipse.jface.wizard.Wizard;
import org.eclipse.ui.INewWizard;
import org.eclipse.ui.IWorkbench;
import org.eclipse.ui.wizards.newresource.BasicNewProjectResourceWizard;

/**
 * @author asloane
 *
 * Support for creating a new Eli project. 
 */
public class NewEliProjectWizard extends Wizard implements INewWizard {

    // Project creation wizard
    private BasicNewProjectResourceWizard wizard; 
    
    /**
     * Create a new Eli project wizard.
     */
    public NewEliProjectWizard() {
        super();
        wizard = new BasicNewProjectResourceWizard();
    }
    
    /**
     * Initialise the wizard.
     */
    public void init(IWorkbench workbench, IStructuredSelection selection) {
         wizard.init(workbench, selection);
         setWindowTitle("New Eli Project");
    }
    
    /**
     * Set the container for this wizard.
     */
    public void setContainer(IWizardContainer wizardContainer) {
        wizard.setContainer(wizardContainer);
    }
    
    /**
     * Get the container for this wizard.
     */
    public IWizardContainer getContainer() {
        return wizard.getContainer();
    }
    
    /**
     * Return whether we can finish or not.
     */
    public boolean canFinish() {
        return wizard.canFinish();
    }
    
    /**
     * Create the wizard pages.
     */
    public void addPages() {
        // Pages from the basic new project resource wizard
        wizard.addPages();
        IWizardPage[] pages = wizard.getPages();
        pages[0].setTitle("Eli Project");
        pages[0].setDescription("Create a new empty Eli project.");
        for (int i = 0; i < pages.length; i++)
            addPage(pages[i]);
    }
    
    /**
     * Actually create the new Eli project.
     */
    public boolean performFinish() {
        // Do the hard work
        boolean ret = wizard.performFinish();
        if (!ret)
            return false;
        
        // Get the kernel project
        IProject newProject = wizard.getNewProject();
        
        // Create an Eli project based on the kernel project
        EliModel.create(newProject);

        return true;
    }
    
}
