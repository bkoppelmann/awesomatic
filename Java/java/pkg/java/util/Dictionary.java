package java.util;

public abstract class Dictionary {
  abstract public int size() { }
  abstract public boolean isEmpty() { }
  abstract public Object get(Object key) { }
  abstract public Object put(Object key, Object element) { }
  abstract public Object remove(Object key) { }
  abstract public Enumeration keys() { }
  abstract public Enumeration elements() { }
}
