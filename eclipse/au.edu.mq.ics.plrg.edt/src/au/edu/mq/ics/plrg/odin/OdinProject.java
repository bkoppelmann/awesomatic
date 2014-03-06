/*
 * Created on Jun 3, 2004
 *
 * Odin-specific project behaviour.
 */
package au.edu.mq.ics.plrg.odin;

import org.eclipse.core.resources.IProject;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.QualifiedName;
import org.eclipse.jface.preference.IPreferenceStore;

import au.edu.mq.ics.plrg.eli.EDTPlugin;

/**
 * @author asloane
 *
 * Support Odin project behaviour.
 */
public class OdinProject {

    // Cache of underlying kernel project
    private IProject project;
    
    // Persistent properties
    private static QualifiedName ODIN_KEY =
        new QualifiedName("au.edu.mq.ics.plrg.edt", "odin");
    private static QualifiedName ARGS_KEY =
        new QualifiedName("au.edu.mq.ics.plrg.edt", "args");
    private static QualifiedName CACHE_KEY =
        new QualifiedName("au.edu.mq.ics.plrg.edt", "cache");
    private static QualifiedName VIEW_KEY =
        new QualifiedName("au.edu.mq.ics.plrg.edt", "view");

    /**
     * Create an Odin project.
     */
    public OdinProject(IProject project) {
        this.project = project;
    }
    
    /**
     * Finalise the project.
     */
    public void finalise() {
    }
    
    /**
     * Return the underlying kernel project.
     */
    public IProject getProject() {
        return project;
    }
    
    /**
     * Return the current cache.
     */
    public String getCache() {
        String ret = "";
        try {
            ret = project.getPersistentProperty(CACHE_KEY);
            if (ret == null)
                    ret = getDefaultCache();
        } catch (CoreException e) {
            e.printStackTrace();
        }
        return ret;
    }
    
    /**
     * Return the default cache.
     */
    public String getDefaultCache() {
        IPreferenceStore preferenceStore = EDTPlugin.getDefault().getPreferenceStore();
        if (preferenceStore.getBoolean(MainPreferencePage.USE_DEFAULT_CACHE_KEY))
            return preferenceStore.getString(MainPreferencePage.DEFAULT_CACHE_KEY);
        else
            return project.getFolder(".ODIN").getLocation().toOSString();
    }
    
    /**
     * Set the cache.
     */
    public void setCache(String cache) {
        try { 
            project.setPersistentProperty(CACHE_KEY, cache);
        } catch (CoreException e) {
            e.printStackTrace();
        }
    }
    
    /**
     * Return the current view.
     */
    public String getView() {
        String ret = "";
        try {
            ret = project.getPersistentProperty(VIEW_KEY);
            if (ret == null)
                    ret = getDefaultView();
        } catch (CoreException e) {
            e.printStackTrace();
        }
        return ret;
    }
    
    /**
     * Return the default view.
     */
    public String getDefaultView() {
        IPreferenceStore preferenceStore = EDTPlugin.getDefault().getPreferenceStore();
        return preferenceStore.getString(MainPreferencePage.DEFAULT_VIEW_KEY);
    }
    
    /**
     * Set the view.
     */
    public void setView(String view) {
        try { 
            project.setPersistentProperty(VIEW_KEY, view);
        } catch (CoreException e) {
            e.printStackTrace();
        }
    }
    
    /**
     * Return the current Odin executable.
     */
    public String getOdin() {
        String ret = "";
        try {
            ret = project.getPersistentProperty(ODIN_KEY);
            if (ret == null)
                    ret = getDefaultOdin();
        } catch (CoreException e) {
            e.printStackTrace();
        }
        return ret;
    }
    
    /**
     * Return the default Odin executable.
     */
    public String getDefaultOdin() {
        IPreferenceStore preferenceStore = EDTPlugin.getDefault().getPreferenceStore();
        return preferenceStore.getString(MainPreferencePage.ODIN_KEY);
    }
    
    /**
     * Set the Odin executable.
     */
    public void setOdin(String exe) {
        try { 
            project.setPersistentProperty(ODIN_KEY, exe);
        } catch (CoreException e) {
            e.printStackTrace();
        }
    }
    
    /**
     * Return the current Odin arguments.
     */
    public String getArgs() {
        String ret = "";
        try {
            ret = project.getPersistentProperty(ARGS_KEY);
            if (ret == null)
                    ret = getDefaultArgs();
        } catch (CoreException e) {
            e.printStackTrace();
        }
        return ret;
    }
    
    /**
     * Return the default Odin arguments.
     */
    public String getDefaultArgs() {
        IPreferenceStore preferenceStore = EDTPlugin.getDefault().getPreferenceStore();
        return preferenceStore.getString(MainPreferencePage.ARGS_KEY);
    }
    
    /**
     * Set the Odin argumnets.
     */
    public void setArgs(String args) {
        try { 
            project.setPersistentProperty(ARGS_KEY, args);
        } catch (CoreException e) {
            e.printStackTrace();
        }
    }
    
}
