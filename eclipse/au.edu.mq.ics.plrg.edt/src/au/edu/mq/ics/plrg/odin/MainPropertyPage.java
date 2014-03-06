/*
 * Created on Jun 2, 2004
 *
 * Odin project properties.
 */
package au.edu.mq.ics.plrg.odin;

import java.io.File;

import org.eclipse.core.resources.IProject;
import org.eclipse.core.runtime.Path;
import org.eclipse.swt.SWT;
import org.eclipse.swt.events.SelectionAdapter;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.eclipse.swt.widgets.DirectoryDialog;
import org.eclipse.swt.widgets.FileDialog;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.Text;
import org.eclipse.ui.dialogs.PropertyPage;

import au.edu.mq.ics.plrg.eli.EliModel;

/**
 * @author asloane
 *
 * Handle Odin project properties.
 */
public class MainPropertyPage extends PropertyPage {
    
    // Local cache of kernel and Odin projects to which these properties apply
    private IProject project;
    private OdinProject odinProject;

    // Texts containing the executable, argument, cache location, odin view
    private Text odinText;
    private Text argsText;
    private Text cacheText;
    private Text viewText;
    
    /**
     * Create an Odin property page.
     */
    public MainPropertyPage() {
        super();
    }
    
    /**
     * Set up the page controls.
     */
    protected Control createContents(Composite parent) {
        project = (IProject) getElement().getAdapter(IProject.class);
        odinProject = (OdinProject) EliModel.create(project);

        Composite composite = new Composite(parent, SWT.NULL);

        GridLayout layout = new GridLayout();
        layout.numColumns = 3;
        composite.setLayout(layout);
        composite.setLayoutData(new GridData(GridData.FILL_HORIZONTAL));
                
        Label exeLabel = new Label(composite, SWT.NONE);
        exeLabel.setText("Eli executable:");

        odinText = new Text(composite, SWT.SINGLE | SWT.BORDER);
        odinText.selectAll();
        odinText.setToolTipText("Executable to use to perform Eli derivations.");
        odinText.setText(odinProject.getOdin());                
        
        GridData odinData = new GridData(GridData.FILL_HORIZONTAL);
        odinData.widthHint = 80;
        odinText.setLayoutData(odinData);
        odinText.setFont(parent.getFont());
        
        Button exeBrowseButton = new Button(composite, SWT.PUSH);
        exeBrowseButton.setText("Browse...");
        exeBrowseButton.addSelectionListener(new SelectionAdapter() {
            public void widgetSelected(SelectionEvent event) {
                handleOdinBrowseButtonPressed();
            }
        });

        Label argsLabel = new Label(composite, SWT.NONE);
        argsLabel.setText("Eli arguments:");

        argsText = new Text(composite, SWT.SINGLE | SWT.BORDER);
        argsText.setToolTipText("Arguments to supply to Eli executable.");
        argsText.setText(odinProject.getArgs());
        
        GridData argsData = new GridData(GridData.FILL_HORIZONTAL);
        argsData.widthHint = 80;
        argsData.horizontalSpan = 2;
        argsText.setLayoutData(argsData);
        argsText.setFont(parent.getFont());     
        
        Label cacheLabel = new Label(composite, SWT.NONE);
        cacheLabel.setText("Cache folder:");

        cacheText = new Text(composite, SWT.SINGLE | SWT.BORDER);
        cacheText.selectAll();
        cacheText.setToolTipText("Folder in which Eli should store derived objects.");
        cacheText.setText(odinProject.getCache());              
        
        GridData cacheTextData = new GridData(GridData.FILL_HORIZONTAL);
        cacheTextData.widthHint = 80;
        cacheText.setLayoutData(cacheTextData);
        cacheText.setFont(parent.getFont());
        
        Button cacheBrowseButton = new Button(composite, SWT.PUSH);
        cacheBrowseButton.setText("Browse...");
        cacheBrowseButton.addSelectionListener(new SelectionAdapter() {
            public void widgetSelected(SelectionEvent event) {
                handleCacheBrowseButtonPressed();
            }
        });

        Label viewLabel = new Label(composite, SWT.NONE);
        viewLabel.setText("Cache view:");

        viewText = new Text(composite, SWT.SINGLE | SWT.BORDER);
        viewText.setToolTipText("View for cache instead of machine name.");
        viewText.setText(odinProject.getView());
        
        GridData viewData = new GridData(GridData.FILL_HORIZONTAL);
        viewData.widthHint = 80;
        viewData.horizontalSpan = 2;
        viewText.setLayoutData(viewData);
        viewText.setFont(parent.getFont());     
        
        return composite;
    }
    
    /**
     *  Open a directory browser for cache location
     */
    void handleCacheBrowseButtonPressed() {
        DirectoryDialog dialog = new DirectoryDialog(cacheText.getShell());
        dialog.setMessage("Cache Directory:");

        String dirName = cacheText.getText().trim();
        if (!dirName.equals("")) {
            File path = new File(dirName);
            if (path.exists())
                dialog.setFilterPath(new Path(dirName).toOSString());
        }

        String selectedDirectory = dialog.open();
        if (selectedDirectory != null)
            cacheText.setText(selectedDirectory);
    }
    
    /**
     *  Open a directory browser for executable
     */
    void handleOdinBrowseButtonPressed() {
        FileDialog dialog = new FileDialog(odinText.getShell());

        String fileName = odinText.getText().trim();
        if (!fileName.equals("")) {
            File path = new File(fileName);
            if (path.exists())
                dialog.setFileName(new Path(fileName).toOSString());
        }

        String selectedFile = dialog.open();
        if (selectedFile != null)
            odinText.setText(selectedFile);
    }

    /**
     * Handle the OK of the property page.
     */
    public boolean performOk() {
        odinProject.setOdin(odinText.getText());
        odinProject.setArgs(argsText.getText());
        odinProject.setCache(cacheText.getText());
        odinProject.setView(viewText.getText());
        return true;
    }

    /**
     * Reset to the defaults.
     */
    public void performDefaults() {
        odinText.setText(odinProject.getDefaultOdin());
        argsText.setText(odinProject.getDefaultArgs());
        cacheText.setText(odinProject.getDefaultCache());
        viewText.setText(odinProject.getDefaultView());
    }
    
}
