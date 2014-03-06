/*
 * Created on Jun 3, 2004
 *
 * Eli-specific project behaviour.
 */
package au.edu.mq.ics.plrg.eli;

import org.eclipse.core.resources.IProject;
import org.eclipse.core.runtime.CoreException;

import au.edu.mq.ics.plrg.odin.OdinProject;

/**
 * @author asloane
 *
 * Support Eli project behaviour.
 */
public class EliProject extends OdinProject {
    
    /**
     * Create an Eli project based on an underlying kernel project.
     */
    public EliProject(IProject project) {
        super(project);
        
        // Make the project into an Eli project
        try {
            EDTPlugin.addEliNature(project);
        } catch (CoreException e) {
                e.printStackTrace();
        }
    }
    
}
