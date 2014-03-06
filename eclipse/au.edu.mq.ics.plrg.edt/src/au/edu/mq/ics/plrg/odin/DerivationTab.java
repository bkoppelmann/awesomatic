/*
 * Created on Apr 24, 2006
 *
 * Tab to select Eli derivation.
 */
package au.edu.mq.ics.plrg.odin;

import java.io.File;

import org.eclipse.core.resources.IFile;
import org.eclipse.core.resources.IProject;
import org.eclipse.core.resources.IWorkspace;
import org.eclipse.core.resources.IWorkspaceRoot;
import org.eclipse.core.resources.ResourcesPlugin;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IPath;
import org.eclipse.core.runtime.Path;
import org.eclipse.debug.core.DebugPlugin;
import org.eclipse.debug.core.ILaunchConfiguration;
import org.eclipse.debug.core.ILaunchConfigurationWorkingCopy;
import org.eclipse.debug.ui.AbstractLaunchConfigurationTab;
import org.eclipse.debug.ui.ILaunchConfigurationTab;
import org.eclipse.jface.viewers.ISelection;
import org.eclipse.jface.viewers.StructuredSelection;
import org.eclipse.swt.SWT;
import org.eclipse.swt.events.ModifyEvent;
import org.eclipse.swt.events.ModifyListener;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.events.SelectionListener;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Combo;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.FileDialog;
import org.eclipse.swt.widgets.Group;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.Text;
import org.eclipse.ui.IEditorInput;
import org.eclipse.ui.IEditorPart;
import org.eclipse.ui.IWorkbench;
import org.eclipse.ui.IWorkbenchPage;
import org.eclipse.ui.IWorkbenchWindow;
import org.eclipse.ui.PlatformUI;

import au.edu.mq.ics.plrg.eli.EDTPlugin;
import au.edu.mq.ics.plrg.eli.EliModel;
import au.edu.mq.ics.plrg.eli.EliProject;

/**
 * @author asloane
 * Copyright, 2006, Anthony Sloane.
 * 
 * Supporting for configuring the derivation to run in an Eli launch.
 */
public class DerivationTab extends AbstractLaunchConfigurationTab
            implements ILaunchConfigurationTab {
    
    // Cache of workspace root
    private IWorkspaceRoot workspaceRoot;
    
    // Cache of derivations
    private Derivations derivations;
    
    // Source file from which to perform the derivation
    private Text fileText;
    private Button fileBrowseButton;

    // List to select the derivation
    private Combo derCombo;
    
    // Optional derivation arguments
    private Text argsText;
    
    // Output direction selections
    private Button ignButton, conButton, edtButton, filButton;
    
    // Output file name
    private Text outfileText;
    private Button outfileBrowseButton;
    
    // Listener for changes in the file, derivation, output mode
    private class WidgetListener implements ModifyListener, SelectionListener {
        public void modifyText(ModifyEvent e) {
            Object source = e.getSource();
            setDirty(true);
            if (source == derCombo)
                setDefaultOutput();
            updateLaunchConfigurationDialog();
        }
        public void widgetSelected(SelectionEvent e) {
            Object source = e.getSource();
            if (source == fileBrowseButton) {
                handleFileBrowse(fileText, true);
            } else if (source == outfileBrowseButton) {
                handleFileBrowse(outfileText, false);
            } else {
                updateOutputFile();
                setDirty(true);
                updateLaunchConfigurationDialog();
            }
        }
        public void widgetDefaultSelected(SelectionEvent e) {
            setDirty(true);
            updateLaunchConfigurationDialog();
        }
    };
    private WidgetListener fListener = new WidgetListener();
    
    /**
     * Create an Eli derivation launch configuration tab.
     */
    public DerivationTab() {
        // Cache the workspace root
        IWorkspace workspace = ResourcesPlugin.getWorkspace();
        workspaceRoot = workspace.getRoot();
        
        // Cache the derivations
        derivations = new Derivations(EDTPlugin.getDefault().getDerivations());
    }
    
    /**
     * Return this tab's name.
     */
    public String getName() {
        return "Derivation";
    }
    
    /*
     * Create an Eli derivation launch configuration tab.
     */
    public void createControl(Composite parent) {
        Composite composite = new Composite(parent, SWT.NONE);
        setControl(composite);
        
        GridLayout layout = new GridLayout();
        layout.numColumns = 1;
        composite.setLayout(layout);
        composite.setLayoutData(new GridData(GridData.FILL_HORIZONTAL));
        
        // Group for the derivation selection
        Group derGroup = new Group(composite, SWT.NONE);
        derGroup.setText("File and derivation:");
        GridLayout derGroupLayout = new GridLayout();
        derGroupLayout.numColumns = 3;
        derGroup.setLayout(derGroupLayout);
        GridData derGroupData = new GridData(GridData.FILL_HORIZONTAL);
        derGroupData.horizontalSpan = 1;
        derGroup.setLayoutData(derGroupData);
        
        Label fileLabel = new Label(derGroup, SWT.NONE);
        fileLabel.setText("Source File:");
        
        fileText = new Text(derGroup, SWT.SINGLE | SWT.BORDER);
        fileText.setToolTipText("Source file from which to perform the derivation.");
        
        GridData fileData = new GridData();
        fileData.grabExcessHorizontalSpace = true;
        fileData.horizontalAlignment = SWT.FILL;
        fileText.setLayoutData(fileData);
        fileText.setFont(parent.getFont());
        fileText.addModifyListener(fListener);
        
        fileBrowseButton = new Button(derGroup, SWT.PUSH);
        fileBrowseButton.setText("Browse...");
        fileBrowseButton.addSelectionListener(fListener);
        
        Label derivationLabel = new Label(derGroup, SWT.NONE);
        derivationLabel.setText("Derivation:");
        derCombo = new Combo(derGroup, SWT.DROP_DOWN | SWT.READ_ONLY);
        derCombo.setToolTipText("List of available derivations.");
        GridData derData = new GridData();
        derData.horizontalSpan = 2;
        derData.widthHint = 200;
        derCombo.setLayoutData(derData);
        derCombo.setItems(derivations.getEnabledDerivationNames());
        derCombo.addModifyListener(fListener);
        
        Label argsLabel = new Label(derGroup, SWT.NONE);
        argsLabel.setText("Optional arguments:");
        
        argsText = new Text(derGroup, SWT.SINGLE | SWT.BORDER);
        argsText.setToolTipText("Optional arguments to supply to the derivation");
        argsText.setLayoutData(fileData);
        argsText.setFont(parent.getFont());
        argsText.addModifyListener(fListener);
        
        // Group for the output selection
        Group outGroup = new Group(composite, SWT.NONE);
        outGroup.setText("Output destination:");
        GridLayout outGroupLayout = new GridLayout();
        outGroupLayout.numColumns = 3;
        outGroup.setLayout(outGroupLayout);
        GridData outGroupData = new GridData(GridData.FILL_HORIZONTAL);
        outGroupData.horizontalSpan = 1;
        outGroup.setLayoutData(outGroupData);
        
        ignButton = new Button(outGroup, SWT.RADIO);
        ignButton.setText("Ignore the output");
        ignButton.addSelectionListener(fListener);
        GridData ignButtonData = new GridData();
        ignButtonData.horizontalSpan = 3;
        ignButton.setLayoutData(ignButtonData);
        
        conButton = new Button(outGroup, SWT.RADIO);
        conButton.setText("Send the output to the console view");
        conButton.addSelectionListener(fListener);
        GridData conButtonData = new GridData();
        conButtonData.horizontalSpan = 3;
        conButton.setLayoutData(conButtonData);
        
        edtButton = new Button(outGroup, SWT.RADIO);
        edtButton.setText("Send the output to a new editor view");
        edtButton.addSelectionListener(fListener);
        GridData edtButtonData = new GridData();
        edtButtonData.horizontalSpan = 3;
        edtButton.setLayoutData(edtButtonData);
        
        filButton = new Button(outGroup, SWT.RADIO);
        filButton.setText("Send the output to the following file:");
        filButton.addSelectionListener(fListener);
        GridData filButtonData = new GridData();
        filButtonData.horizontalSpan = 3;
        filButton.setLayoutData(filButtonData);
        
        outfileText = new Text(outGroup, SWT.SINGLE | SWT.BORDER);
        outfileText.setToolTipText("File where output is put in output mode.");
        outfileText.setEnabled(false);
        
        GridData outfileData = new GridData();
        outfileData.grabExcessHorizontalSpace = true;
        outfileData.horizontalAlignment = SWT.FILL;
        outfileText.setLayoutData(outfileData);
        outfileText.setFont(parent.getFont());
        outfileText.addModifyListener(fListener);
        
        outfileBrowseButton = new Button(outGroup, SWT.PUSH);
        outfileBrowseButton.setText("Browse...");
        outfileBrowseButton.addSelectionListener(fListener);
        outfileBrowseButton.setEnabled(false);
    }
    
    /*
     * Set up the default attributes for the configuration.
     */
    public void setDefaults(ILaunchConfigurationWorkingCopy config) {
        String fileName = null;
        IWorkbench wb = PlatformUI.getWorkbench();
        IWorkbenchWindow win = wb.getActiveWorkbenchWindow();
        IWorkbenchPage page = win.getActivePage();

        // Use the current selection if appropriate 
        ISelection selection = page.getSelection();
        if (selection instanceof StructuredSelection) {
            Object element = ((StructuredSelection)selection).getFirstElement();
            if (element instanceof IFile) {
                fileName = ((IFile)element).getFullPath().toOSString();
            } else if (element instanceof IProject) {
                fileName = ((IProject)element).getName();
            }
        }
        
        // No selection, so try the current editor
        if (fileName == null) {
            IEditorPart part = page.getActiveEditor();
            if (part != null) {
                IEditorInput input = part.getEditorInput();
                IFile file = (IFile) input.getAdapter(IFile.class);
                if (file == null)
                    fileName = "";
                else
                    fileName = file.getFullPath().toOSString();
            } else {
                // Otherwise just leave empty
                fileName = "";
            }
        }
        
        config.setAttribute(DebugPlugin.ATTR_PROCESS_FACTORY_ID, DerivationLaunchConstants.PROCESS_FACTORY);
        config.setAttribute(DerivationLaunchConstants.ATTR_FILE, fileName);
        config.setAttribute(DerivationLaunchConstants.ATTR_DERNAME, "Executable");
        config.setAttribute(DerivationLaunchConstants.ATTR_ARGS, "");
        config.setAttribute(DerivationLaunchConstants.ATTR_DERMODE, Derivation.DERIVE);
    }
    
    /**
     * Enable the output file selection widgets if in file mode.
     */
    private void updateOutputFile() {
        boolean enable = filButton.getSelection();
        outfileText.setEnabled(enable);
        outfileBrowseButton.setEnabled(enable);
    }
    
    /**
     * Set the output mode based on the default for the currently selected derivation.
     */
    private void setDefaultOutput() {
        String derivationName = derCombo.getText();
        Derivation derivation = derivations.getDerivation(derivationName);
        if (derivation == null)
            return;
        String defMode = derivation.getDefMode();
        ignButton.setSelection(defMode.equals(Derivation.DERIVE));
        conButton.setSelection(defMode.equals(Derivation.SHOW));
        edtButton.setSelection(defMode.equals(Derivation.EDIT));
        filButton.setSelection(defMode.equals(Derivation.FILE));
        updateOutputFile();
    }
    
    /**
     * Set the GUI from the config attributes. 
     */
    public void initializeFrom(ILaunchConfiguration config) {
        try {
            fileText.setText(config.getAttribute(DerivationLaunchConstants.ATTR_FILE, ""));
        } catch (CoreException e) {
            fileText.setText("");
        }
        try {
            derCombo.setText(config.getAttribute(DerivationLaunchConstants.ATTR_DERNAME, ""));
        } catch (CoreException e) {
            derCombo.setText("");
        }
        try {
            argsText.setText(config.getAttribute(DerivationLaunchConstants.ATTR_ARGS, ""));
        } catch (CoreException e) {
            argsText.setText("");
        }
        setOutput(config);
        try {
            outfileText.setText(config.getAttribute(DerivationLaunchConstants.ATTR_OUTFILE, ""));
        } catch (CoreException e) {
            outfileText.setText("");
        }
    }
    
    /**
     * Set the output mode buttons from the actual derivation mode.
     */
    private void setOutput(ILaunchConfiguration config) {
        try {
            String derMode = config.getAttribute(DerivationLaunchConstants.ATTR_DERMODE, "");
            ignButton.setSelection(derMode.equals(Derivation.DERIVE));
            conButton.setSelection(derMode.equals(Derivation.SHOW));
            edtButton.setSelection(derMode.equals(Derivation.EDIT));
            filButton.setSelection(derMode.equals(Derivation.FILE));
        } catch (CoreException e) {
            ignButton.setSelection(true);
            conButton.setSelection(false);
            edtButton.setSelection(false);
            filButton.setSelection(false);
        }
        updateOutputFile();
    }

    /**
     * Set the working copy attributes from the GUI.
     */
    public void performApply(ILaunchConfigurationWorkingCopy config) {
        config.setAttribute(DerivationLaunchConstants.ATTR_FILE, fileText.getText());
        config.setAttribute(DerivationLaunchConstants.ATTR_DERNAME, derCombo.getText());
        config.setAttribute(DerivationLaunchConstants.ATTR_ARGS, argsText.getText());
        if (ignButton.getSelection())
            config.setAttribute(DerivationLaunchConstants.ATTR_DERMODE, Derivation.DERIVE);
        else if (conButton.getSelection())
            config.setAttribute(DerivationLaunchConstants.ATTR_DERMODE, Derivation.SHOW);
        else if (edtButton.getSelection())
            config.setAttribute(DerivationLaunchConstants.ATTR_DERMODE, Derivation.EDIT);
        else if (filButton.getSelection())
            config.setAttribute(DerivationLaunchConstants.ATTR_DERMODE, Derivation.FILE);
        config.setAttribute(DerivationLaunchConstants.ATTR_OUTFILE, outfileText.getText());
    }
    
    /**
     * Are the current attribute settings valid?
     */
    public boolean isValid(ILaunchConfiguration launchConfig) {
        setErrorMessage(null);
        
        // Check file: non-empty, exists
        String fileName = fileText.getText();
        if (fileName.equals("")) {
            setErrorMessage("File name cannot be empty");
            return false;
        }
        
        // Check project: non-empty, exists, file exists within it
        IPath filePath = new Path(fileName);
        String projectName = filePath.segment(0);
        filePath = filePath.removeFirstSegments(1);
        if (projectName.equals("")) {
            setErrorMessage("Project name cannot be empty");
            return false;
        }
        IProject project = workspaceRoot.getProject(projectName);
        if (!project.exists()) {
            setErrorMessage("Project " + projectName + " does not exist");
            return false;
        }
        if (!project.exists(filePath)) {
            setErrorMessage("File " + fileName + " does not exist");
            return false;
        }

        // Complain if Odin hasn't been set for the project
        EliProject eliProject = EliModel.create(project);
        String odin = eliProject.getOdin();
        if (odin.equals("")) {
            setErrorMessage("No derivations can be performed in this project because " +
                            "the Eli executable property of this project is not set.");
            return false;
        }
        
        // Complain if Odin doesn't exist or isn't a regular file
        File odinFile = new File(odin);
        if (!odinFile.exists()) {
            setErrorMessage("No derivations can be performed in the " + project + " project because " +
                            "the Eli executable '" + odin + "' specified in the project " +
                            "properties does not exist.");
            return false;
        }
        if (!odinFile.isFile()) {
            setErrorMessage("No derivations can be performed in the " + project + " project because " +
                            "the Eli executable '" + odin + "' specified in the project " +
                            "properties is not a regular file.");
            return false;
        }
          
        // Check derivation name: non-empty
        String derName = derCombo.getText();
        if (derName.equals("")) {
            setErrorMessage("Derivation name cannot be empty");
            return false;
        }
    
        // Check: output file non-empty if usnig output mode
        if (filButton.getSelection()) {
            String outfileName = outfileText.getText();
            if (outfileName.equals("")) {
                setErrorMessage("Output file name cannot be empty in file mode");
                return false;
            }
            IFile outfile = workspaceRoot.getFile(new Path(outfileName));
            if (outfile.getLocation() == null) {
                setErrorMessage("Output file name must be relative to the workspace.");
                return false;
            }
        }
        
        return true;
    }

    /**
     * Handle browsing for files.  As the filter path use the folder of the
     * current value of the text field if it's a file that exists.  Otherwise
     * use the root of the workspace.  
     */
    protected void handleFileBrowse(Text fileText, boolean source) {
        // Get the workspace root path name
        IWorkspaceRoot workspaceRoot = ResourcesPlugin.getWorkspace().getRoot(); 
        String rootPathName = workspaceRoot.getLocation().toOSString();  
        
        // Set fileName and filterPath defaults from text field
        String fileName = fileText.getText();
        IPath filePath = new Path(fileName);
        String filterPath = rootPathName;
        IFile file = workspaceRoot.getFile(filePath);
        if (file.exists())
            filterPath = file.getLocation().removeLastSegments(1).toOSString();
    
        // Open the dialog
        FileDialog dialog = new FileDialog(getShell(), source ? SWT.OPEN : SWT.SAVE);
        dialog.setText(source ? "Select Source File" : "Select Output File");
        dialog.setFilterPath(filterPath);
        fileName = dialog.open();
        if (fileName != null)
            fileText.setText(fileName.substring(rootPathName.length()));
    }

}
