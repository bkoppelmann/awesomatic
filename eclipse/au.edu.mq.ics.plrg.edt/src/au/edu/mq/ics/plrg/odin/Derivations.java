/*
 * Created on Jun 8, 2004
 *
 * Derivations.
 */
package au.edu.mq.ics.plrg.odin;

import java.io.BufferedReader;
import java.io.BufferedWriter;
import java.io.IOException;
import java.util.HashMap;
import java.util.Iterator;
import java.util.Map;
import java.util.SortedSet;
import java.util.TreeSet;

/**
 * @author asloane
 *
 * Repository of derivations available to the Odin projects.
 */
public class Derivations {

    // The category list
    private SortedSet categorySet;
    
    // The derivations keyed by category, each category maps to a set of derivations
    private Map categoryMap;
    
    // The derivations keyed by name, each name maps to a derivation
    private Map nameMap; 
    
    /**
     * Initialise a new derivation history.
     */
    private void setup() {
        categorySet = new TreeSet();
        categoryMap = new HashMap();
        nameMap = new HashMap();
    }
    
    /**
     * Build a new empty derivation repository.
     */
    public Derivations() {
        setup();
    }
    
    /**
     * Build a new derivation repository that clones an existing one.
     */
    public Derivations(Derivations derivations) {
        setup();
        Derivation[] derivationList = derivations.getDerivations();
        for (int i = 0; i < derivationList.length; i++)
            addDerivation(new Derivation(derivationList[i]));
    }

    /**
     * Build a derivation repository from the specified reader.
     */
    public Derivations(BufferedReader reader) {
        setup();
        try {
            Derivation derivation;
            String blank = null;
            do {
                derivation = Derivation.read(reader);
                if (derivation != null) {
                    addDerivation(derivation);
                    blank = reader.readLine();
                }
            } while ((derivation != null) && (blank != null));
            reader.close();
        } catch (IOException e) {
            e.printStackTrace();
        }
    }

    /**
     * Write the derivations using the specified writer.
     */
    public void write(BufferedWriter writer) throws IOException {
        for (Iterator i = nameMap.values().iterator(); i.hasNext(); ) {
            Derivation derivation = (Derivation) i.next();
            derivation.write(writer);
            writer.newLine();
        }
    }
    
    /**
     * Add a derivation to a category.
     */
    public void addDerivation(Derivation derivation) {
        String category = derivation.getCategory();
        categorySet.add(category);
        SortedSet set = (SortedSet) categoryMap.get(category);
        if (set == null)
            set = new TreeSet();
        set.add(derivation);
        categoryMap.put(category, set);
        nameMap.put(derivation.getName(), derivation);
    }

    /**
     * Delete a derivation by name (no effect if not there).
     */
    public void deleteDerivation(String name)
    {
        Derivation derivation = getDerivation(name);
        if (derivation == null)
            return;
        String category = derivation.getCategory();
        SortedSet derivations = (SortedSet) categoryMap.get(category);
        if (derivations != null) {
            derivations.remove(derivation);
            if (derivations.isEmpty()) {
                categoryMap.remove(category);
                categorySet.remove(category);
            }
        }
        nameMap.remove(name);
    }
    
    /**
     * Return the current categories.
     */
    public String[] getCategories() {
        int count = categorySet.size();
        if (count == 0)
            return null;
        String[] categories = new String[count];
        int j = 0;
        for (Iterator i = categorySet.iterator(); i.hasNext(); j++)
            categories[j] = (String) i.next();
        return categories;
    }
    
    /**
     * Return all of the derivations.
     */
    public Derivation[] getDerivations() {
        int count = nameMap.size();
        if (count == 0)
            return null;
        Derivation[] derivations = new Derivation[count];
        int j = 0;
        for (Iterator i = nameMap.values().iterator(); i.hasNext(); j++)
            derivations[j] = (Derivation) i.next();
        return derivations;
    }
    
    /**
     * Return the derivations in a category.
     */
    public Derivation[] getDerivations(String category) {
        SortedSet derivationSet = (SortedSet) categoryMap.get(category);
        if (derivationSet == null)
            return null;
        Derivation[] derivations = new Derivation[derivationSet.size()];
        int j = 0;
        for (Iterator i = derivationSet.iterator(); i.hasNext(); j++)
            derivations[j] = (Derivation) i.next();
        return derivations;
    }
    
    /**
     * Get the names of all enabled derivations.
     */
    public String[] getEnabledDerivationNames() {
        if (nameMap.size() == 0)
            return null;
        SortedSet names = new TreeSet();
        for (Iterator i = nameMap.values().iterator(); i.hasNext(); ) {
            Derivation derivation = (Derivation) i.next();
            if (derivation.getEnabled())
                names.add(derivation.getName());
        }
        return (String[]) names.toArray(new String[0]);
    }
    
    /**
     * Get the sorted names of the derivations in a category.
     */
    public String[] getDerivationNames(String category) {
        SortedSet derivations = (SortedSet) categoryMap.get(category);
        if (derivations == null)
            return null;
        int count = derivations.size();
        String[] names = new String[count];
        Iterator i = derivations.iterator();
        for (int j = 0; j < count; j++) {
            Derivation derivation = (Derivation) i.next();
            names[j] = derivation.getName();
        }
        return names;
    }
    
    /**
     * Return a derivation by name, null if not there.
     */
    public Derivation getDerivation(String derivationName) {
        return (Derivation) nameMap.get(derivationName);
    }
    
    /**
     * Return the number of derivations. 
     */
    public int size() {
        return nameMap.size();
    }
    
}
