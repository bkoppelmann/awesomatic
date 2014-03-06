/*
 * Created on Jun 8, 2004
 *
 * Odin derivations.
 */
package au.edu.mq.ics.plrg.odin;

import java.io.BufferedReader;
import java.io.BufferedWriter;
import java.io.IOException;


/**
 * @author asloane
 *
 * Represent a possible Odin derivation.
 */
public class Derivation implements Comparable {

    // Constants to represent possible derivation modes
    public static final String DERIVE = "Derive";    // Just derive the product
    public static final String SHOW   = "Show";      // Derive and display output in console
    public static final String EDIT   = "Edit";      // Derive and display output in editor
    public static final String FILE   = "File";      // Derive and put output in file
    
    // The category in which this derivation should be displayed
    private String category;
    
    // The name of this derivation
    private String name;
    
    // The product to derive
    private String product;
    
    // Description text
    private String description;
    
    // Default derivation mode
    private String defMode;
    
    // Is this derivation in play or not?
    private boolean enabled;
    
    /**
     * Create a derivation using a category, name, product and descriptio
     */
    public Derivation(String category, String name, String product,
                      String description, String mode, boolean enabled) {
        this.category = category;
        this.name = name;
        this.product = product;
        this.description = description;
        this.defMode = mode;
        this.enabled = enabled;
    }
    
    /**
     * Create a derivation that has the same information as an existing one.
     */
    public Derivation(Derivation derivation) {
        this.category = derivation.category;
        this.name = derivation.name;
        this.product = derivation.product;
        this.description = derivation.description;
        this.defMode = derivation.defMode;
        this.enabled = derivation.enabled;
    }
    
    /**
     * Factory method to create a derivation by reading the data from the specified
     * reader.  Returns null if the complete information could not be read.  Should
     * match the output code in Derivation.write(BufferedWriter).
     */
    public static Derivation read(BufferedReader reader) throws IOException {
        String category = reader.readLine();
        if (category == null) return null;
        String name = reader.readLine();
        if (name == null) return null;
        String product = reader.readLine();
        if (product == null) return null;
        String description = reader.readLine();
        if (description == null) return null;
        String mode = reader.readLine();
        if (mode == null) return null;
        String stateStr = reader.readLine();
        if (stateStr == null) return null;
        boolean enabled = stateStr.equals("Enabled");
        return new Derivation(category, name, product, description, mode, enabled);
    }

    /**
     * Write the derivation to the given writer.  Should match the reading code in
     * Derivation.read(BufferedReader).
     */
    public void write(BufferedWriter writer) throws IOException {
        writer.write(category);
        writer.newLine();
        writer.write(name);
        writer.newLine();
        writer.write(product);
        writer.newLine();
        writer.write(description);
        writer.newLine();
        writer.write(defMode);
        writer.newLine();
        writer.write(enabled ? "Enabled" : "Disabled");
        writer.newLine();
    }

    /**
     * Returns the category.
     */
    public String getCategory() {
        return category;
    }
    
    /**
     * Returns the derivation.
     */
    public String getProduct() {
        return product;
    }
    
    /**
     * Returns the description.
     */
    public String getDescription() {
        return description;
    }
    
    /**
     * Returns the name.
     */
    public String getName() {
        return name;
    }
    
    /**
     * Return the current mode setting.
     */
    public String getDefMode() {
        return defMode;
    }
    
    /**
     * Returns the enabled flag.
     */
    public boolean getEnabled() {
        return enabled;
    }
    
    /**
     * Compare with another derivation by name. 
     */
    public int compareTo(Object object) throws ClassCastException {
        if (object instanceof Derivation) {
            Derivation derivation = (Derivation) object;
            return name.compareTo(derivation.name);
        } else
            throw new ClassCastException("Can't compare with Derivation");
    }
    
    /**
     * Check for equality by name with another derivation.
     */
    public boolean equals(Object object) {
        if (object instanceof Derivation) {
            Derivation derivation = (Derivation) object;
            return name.equals(derivation.name);
        } else
            return false;
    }
    
    /**
     * Convert to a representative string.
     */
    public String toString() {
        return name;
    }
    
}
