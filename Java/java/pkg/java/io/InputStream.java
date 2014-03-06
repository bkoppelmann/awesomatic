package java.io;

public abstract class InputStream {
  public abstract int read() { }
  public void close() { }
  public void mark() { }
  public void reset() { }
}
