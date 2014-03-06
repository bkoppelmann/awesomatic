/*
 * Created on Jun 3, 2004
 *
 * Workspace-wide Odin preferences (main page).
 */
package au.edu.mq.ics.plrg.odin;

import org.eclipse.jface.preference.BooleanFieldEditor;
import org.eclipse.jface.preference.DirectoryFieldEditor;
import org.eclipse.jface.preference.FieldEditorPreferencePage;
import org.eclipse.jface.preference.FileFieldEditor;
import org.eclipse.jface.preference.IPreferenceStore;
import org.eclipse.jface.preference.StringFieldEditor;
import org.eclipse.ui.IWorkbench;
import org.eclipse.ui.IWorkbenchPreferencePage;

import au.edu.mq.ics.plrg.eli.EDTPlugin;

/**
 * @author asloane
 *
 * Support preferences for all Odin projects.
 */
public class MainPreferencePage extends FieldEditorPreferencePage
    implements IWorkbenchPreferencePage {
    
    // Preference keys
    public static final String ODIN_KEY = "odin_key";
    public static final String ARGS_KEY = "args_key";
    public static final String USE_DEFAULT_CACHE_KEY = "use_default_cache_key";
    public static final String DEFAULT_CACHE_KEY = "default_cache_key";
    public static final String DEFAULT_VIEW_KEY = "default_view";
    public static final String CLEAR_CONSOLE_KEY = "clear_console_key";
    
    /**
     * Create an Odin preference page.
     */
    public MainPreferencePage() {
        super(GRID);
        IPreferenceStore preferenceStore = EDTPlugin.getDefault().getPreferenceStore();
        setPreferenceStore(preferenceStore);
    }
    
    /**
     * Initialise the page.
     */
    public void init(IWorkbench workbench) {
    }
    
    /**
     * Create the contents of the page.
     */
    protected void createFieldEditors() {
        FileFieldEditor eliField =
            new FileFieldEditor(ODIN_KEY, "Default Eli executable:",
                                getFieldEditorParent());
        addField(eliField);
        
        StringFieldEditor argsField =
            new StringFieldEditor(ARGS_KEY, "Default Eli arguments:",
                                 getFieldEditorParent());
        addField(argsField);        
        
        BooleanFieldEditor useDefaultCacheField =
            new BooleanFieldEditor(USE_DEFAULT_CACHE_KEY, "Use default cache folder",
                                   getFieldEditorParent());
        addField(useDefaultCacheField);
        
        DirectoryFieldEditor defaultCacheField =
            new DirectoryFieldEditor(DEFAULT_CACHE_KEY, "Default cache folder:",
                                     getFieldEditorParent());
        addField(defaultCacheField);
        
        StringFieldEditor defaultViewField =
            new StringFieldEditor(DEFAULT_VIEW_KEY, "Default cache view:",
                                 getFieldEditorParent());
        addField(defaultViewField);        
        
        BooleanFieldEditor clearConsoleField =
            new BooleanFieldEditor(CLEAR_CONSOLE_KEY, "Clear console before each derivation",
                                   getFieldEditorParent());
        addField(clearConsoleField);
    }
    
}
