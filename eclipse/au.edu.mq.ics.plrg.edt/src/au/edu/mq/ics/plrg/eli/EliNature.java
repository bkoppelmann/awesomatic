/*
 * Created on May 31, 2004
 *
 * Eli project nature.
 */
package au.edu.mq.ics.plrg.eli;

import org.eclipse.core.resources.IProject;
import org.eclipse.core.resources.IProjectNature;
import org.eclipse.core.runtime.CoreException;

/**
 * @author asloane
 *
 * Nature for Eli projects.
 */
public class EliNature implements IProjectNature {
    
    IProject project;

    public IProject getProject() {
        return project;
    }

    public void setProject(IProject project) {
        this.project = project;
    }

    public void configure() throws CoreException {
    }

    public void deconfigure() throws CoreException {
    }

}
