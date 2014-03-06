/*
 * Created on Jun 3, 2004
 *
 * Overall model of Eli projects.
 */
package au.edu.mq.ics.plrg.eli;

import java.util.HashMap;
import java.util.Iterator;
import java.util.Map;

import org.eclipse.core.resources.IProject;

/**
 * @author asloane
 *
 * Maintain global workspace information about Eli projects.
 */
public class EliModel {
    
    // Map from kernel projects to Eli projects
    private static Map projects = new HashMap();

    /**
     * Finalise the model.
     */
    public static void finalise() {
        // Ask each project to finalise itself
        for (Iterator i = projects.values().iterator(); i.hasNext(); ) {
            EliProject eliProject = (EliProject) i.next();
            eliProject.finalise();
        }
    }

    /**
     * Return an Eli project corresponding to a kernel project.
     */
    public static EliProject create(IProject project) {
        if (project == null)
            return null;
        EliProject eliProject = (EliProject) projects.get(project);
        if (eliProject == null) {
            eliProject = new EliProject(project); 
            projects.put(project, eliProject);
        }
        return eliProject;
    }
    
    /**
     * Get the Eli project based on a kernel project (null if none).
     */
    public static EliProject getEliProject(IProject project) {
        return (EliProject) projects.get(project);
    }
    
}