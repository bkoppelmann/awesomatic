/*
 * Created on Jun 2, 2004
 *
 * Eli derivations preference.
 */
package au.edu.mq.ics.plrg.odin;

import org.eclipse.jface.dialogs.MessageDialog;
import org.eclipse.jface.preference.PreferencePage;
import org.eclipse.swt.SWT;
import org.eclipse.swt.events.SelectionAdapter;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Combo;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.eclipse.swt.widgets.Group;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.swt.widgets.Text;
import org.eclipse.ui.IWorkbench;
import org.eclipse.ui.IWorkbenchPreferencePage;
import org.eclipse.ui.PlatformUI;

import au.edu.mq.ics.plrg.eli.EDTPlugin;

/**
 * @author asloane
 *
 * Handle Eli derivations preference
 */
public class DerivationsPreferencePage extends PreferencePage implements IWorkbenchPreferencePage {
    
    // Local version of derivations
    private Derivations derivations;

    // Controls to select the category and derivation with that category
    private Combo catCombo, nameCombo;
    
    // Data about a derivation
    private Text catText, nameText, prodText, descText;
    private Combo modeCombo;
    private Button stateButton;
    
    /**
     * Create an Eli preference page.
     */
    public DerivationsPreferencePage() {
        super();
    }
    
    /**
     * Initialise the preference page.
     */
    public void init(IWorkbench workbench) {
        derivations = new Derivations(EDTPlugin.getDefault().getDerivations());
    }
    
    /**
     * Set up the page controls.
     */
    protected Control createContents(Composite parent) {
        noDefaultAndApplyButton();
        
        Composite composite = new Composite(parent, SWT.NULL);

        GridLayout layout = new GridLayout();
        layout.numColumns = 3;
        composite.setLayout(layout);
        composite.setLayoutData(new GridData(GridData.FILL_HORIZONTAL));
                
        // Category picker
        Label categoryLabel = new Label(composite, SWT.NONE);
        categoryLabel.setText("Category:");
        catCombo = new Combo(composite, SWT.DROP_DOWN | SWT.READ_ONLY);
        catCombo.setToolTipText("Derivation categories.");
        GridData catData = new GridData();
        catData.horizontalSpan = 2;
        catData.widthHint = 200;
        catCombo.setLayoutData(catData);
        catCombo.addSelectionListener(
                new SelectionAdapter() {
                    public void widgetSelected(SelectionEvent e) {
                        updateCombos(catCombo.getSelectionIndex());
                    }
                }
            );
        
        // Derivation picker
        Label derivationLabel = new Label(composite, SWT.NONE);
        derivationLabel.setText("Derivation:");
        nameCombo = new Combo(composite, SWT.DROP_DOWN | SWT.READ_ONLY);
        nameCombo.setToolTipText("List of derivations in the current category.");
        GridData derData = new GridData();
        derData.horizontalSpan = 2;
        derData.widthHint = 200;
        nameCombo.setLayoutData(derData);
        nameCombo.addSelectionListener(
                new SelectionAdapter() {
                    public void widgetSelected(SelectionEvent e) {
                        updateData();
                    }
                }
            );
        
        // Group for the derivation data
        Group dataGroup = new Group(composite, SWT.NONE);
        dataGroup.setText("Derivation data");
        GridLayout dataGroupLayout = new GridLayout();
        dataGroupLayout.numColumns = 2;
        dataGroup.setLayout(dataGroupLayout);
        GridData dataGroupData = new GridData(GridData.GRAB_HORIZONTAL);
        dataGroupData.horizontalSpan = 3;
        dataGroup.setLayoutData(dataGroupData);
        
        // Fields for derivation data
        catText = addField(dataGroup, "Category:", "The category in which this derivation is shown.", 400);
        nameText = addField(dataGroup, "Name:", "The name of this derivation.", 400);
        prodText = addField(dataGroup, "Product:", "The Eli product for this derivation.", 400);
        descText = addField(dataGroup, "Description:", "A one-line description of the derivation.", 400);

        // Pull-down for mode
        Label modeLabel = new Label(dataGroup, SWT.NONE);
        modeLabel.setText("Default mode:");
        modeCombo = new Combo(dataGroup, SWT.DROP_DOWN | SWT.READ_ONLY);
        modeCombo.setToolTipText("The default mode for this derivation.");
        modeCombo.setItems(new String[] {
                Derivation.DERIVE, Derivation.FILE, Derivation.EDIT, Derivation.SHOW
            } );
        GridData modeData = new GridData();
        modeData.widthHint = 200;
        modeCombo.setLayoutData(modeData);
        
        // State flag
        Label stateLabel = new Label(dataGroup, SWT.NONE);
        stateLabel.setText("State:");
        stateButton = new Button(dataGroup, SWT.CHECK);
        stateButton.setText("Enabled");
        
        // Add new derivation button
        Button newButton = new Button(composite, SWT.NONE);
        newButton.setText("New");
        newButton.setToolTipText("Add a new derivation in the current category.");
        newButton.addSelectionListener(
                new SelectionAdapter() {
                    public void widgetSelected(SelectionEvent e) {
                        newDerivation();
                    }
                }
            );
        
        // Delete derivation button
        Button applyButton = new Button(composite, SWT.NONE);
        applyButton.setText("Apply");
        applyButton.setToolTipText("Apply the data changes to the current derivation.");
        applyButton.addSelectionListener(
                new SelectionAdapter() {
                    public void widgetSelected(SelectionEvent e) {
                        applyData();
                    }
                }
            );
        
        // Delete derivation button
        Button deleteButton = new Button(composite, SWT.NONE);
        deleteButton.setText("Delete");
        deleteButton.setToolTipText("Delete the current derivation.");
        deleteButton.addSelectionListener(
                new SelectionAdapter() {
                    public void widgetSelected(SelectionEvent e) {
                        deleteDerivation();
                    }
                }
            );

        // Display the first category
        updateCombos(0);
        
        return composite;
    }

    /**
     * Add a label and text field pair returning the text field.
     */
    private Text addField(Composite composite, String labelText, String tooltipText, int width) {
        Label label = new Label(composite, SWT.NONE);
        label.setText(labelText);

        Text text = new Text(composite, SWT.SINGLE | SWT.BORDER);
        text.selectAll();
        text.setToolTipText(tooltipText);

        GridData data = new GridData(GridData.HORIZONTAL_ALIGN_FILL);
        data.widthHint = width;
        text.setLayoutData(data);
        
        return text;
    }
    
    /**
     * Update the combo boxes to reflect a change to a specific category index.
     */
    private void updateCombos(int catIndex) {
        String[] categories = derivations.getCategories();
        updateCombos(categories, categories[catIndex], null);
    }
    
    /**
     * Update the combo boxes to reflect a change in the category and derivation name. 
     */
    private void updateCombos   (String category, String derivation) {
        String[] categories = derivations.getCategories();
        updateCombos(categories, category, derivation);
    }
    
    /**
     * Update the combo boxes to reflect a change in the category and derivation given
     * the current categories.  If derivation name is null, use first derivation in the
     * category.
     */
    private void updateCombos(String[] categories, String category, String derivation) {
        catCombo.setItems(categories);
        String[] names = derivations.getDerivationNames(category);
        if (names == null) {
            // Nothing left in this category, select the first category (which must have one)
            int i = categories[0].equals(category) ? 1 : 0;
            updateCombos(categories, categories[i], null);
        } else {
            catCombo.setText(category);
            nameCombo.setItems(names);
            if (derivation == null)
                nameCombo.setText(names[0]);
            else
                nameCombo.setText(derivation);
        }
        updateData();
    }
    
    /**
     * Update the data fields to reflect the specified derivation.
     */
    private void updateData() {
        String name = nameCombo.getText();
        Derivation derivation = derivations.getDerivation(name);
        catText.setText(derivation.getCategory());
        nameText.setText(name);
        prodText.setText(derivation.getProduct());
        descText.setText(derivation.getDescription());
        modeCombo.setText(derivation.getDefMode());
        stateButton.setSelection(derivation.getEnabled());
    }
    
    /**
     * Apply the current data to the selected derivation.
     */
    private void applyData() {
        // Object if the category, name or product is empty
        String newCat = catText.getText();
        if (newCat.equals("")) { emptyDialog("Category"); return; }
        String newName = nameText.getText();
        if (newName.equals("")) { emptyDialog("Name"); return; }
        String newProd = prodText.getText();
        if (newProd.equals("")) { emptyDialog("Product"); return; }
        
        // Object if there is already a derivation with the new name and we're not replacing it
        String oldName = nameCombo.getText();
        if (!oldName.equals(newName) && (derivations.getDerivation(newName) != null)) {
            Shell workbenchShell = PlatformUI.getWorkbench().getActiveWorkbenchWindow().getShell();
            MessageDialog.openInformation(workbenchShell,
                    "Cannot create derivation",
                    "There is already a derivation called '" + newName + "'."); 
            return;
        }
        
        // Delete the current derivation
        derivations.deleteDerivation(oldName);

        // Create a new derivation and add it
        Derivation derivation =
            new Derivation(newCat, newName, newProd, descText.getText(), modeCombo.getText(),
                          stateButton.getSelection());
        derivations.addDerivation(derivation);
        
        // Update the combos to show the new cat and name
        updateCombos(newCat, newName);
    }
    
    /**
     * Display a dialog complaining about a given field being empty.
     */
    private void emptyDialog(String fieldName) {
        Shell workbenchShell = PlatformUI.getWorkbench().getActiveWorkbenchWindow().getShell();
        MessageDialog.openInformation(workbenchShell,
                "Cannot update derivation",
                "The " + fieldName + " field must not be empty."); 
        return;
    }
    
    /**
     * Setup the data for a new derivation in the current category.
     */
    private void newDerivation() {
        // Find a default category for the new derivation
        String category = catCombo.getText();
        
        // Find a default name for the new derivation
        String basename = "NewDerivation";
        String name = basename;
        if (derivations.getDerivation(name) != null) {
            int i = 2;
            while (true) {
                name = basename + " " + i; 
                if (derivations.getDerivation(name) == null)
                    break;
                i++;
            }
        }
        
        // Create a new derivation with default values and add it
        Derivation derivation = new Derivation(category, name, "", "", Derivation.DERIVE, true);
        derivations.addDerivation(derivation);
        
        // Update the combo to show the new derivation
        updateCombos(category, name);

        // Make it easy to change the default name
        nameText.selectAll();
        nameText.forceFocus();
    }
    
    /**
     * Delete the current derivation.  Don't let the derivations go empty because that
     * really complicates the display, change etc logic.
     */
    private void deleteDerivation() {
        Shell workbenchShell = PlatformUI.getWorkbench().getActiveWorkbenchWindow().getShell();

        // Don't let them delete the last one
        if (derivations.size() == 1) {
            MessageDialog.openInformation(workbenchShell,
                    "Cannot delete derivation",
                    "There must be at least one derivation and this is the only one, so you can't delete it.");
            return;
        }
        
        // Make sure they really want to delete it
        String name = nameCombo.getText();
        if (!MessageDialog.openQuestion(workbenchShell, "Confirm Derivation Deletion",
                "Are you sure you want to delete the " + name +" derivation?"))
            return;
        
        derivations.deleteDerivation(name);
        updateCombos(catCombo.getText(), null);
    }
    
    /**
     * Handle the OK of the property page.
     */
    public boolean performOk() {
        EDTPlugin.getDefault().setDerivations(derivations);
        return true;
    }

}
