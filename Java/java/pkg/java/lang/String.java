package java.lang;

public final class String {
  public String() { }
  public String(String value) { }
  public String(StringBuffer buffer) { }
  public String(char[] value) { }
  public String(char[] value, int offset, int count) { }
  public String(byte[] ascii, int hibyte) { }
  public String(byte[] ascii, int hibyte, int offset, int count) { }
  public String toString() { }
  public int length() { }
  public char charAt(int index) { }
  public static String valueOf(Object obj) { }
  public static String valueOf(char[] data) { }
  public static String valueOf(char[] data, int offset, int count) { }
  public static String valueOf(boolean b) { }
  public static String valueOf(char c) { }
  public static String valueOf(int i) { }
  public static String valueOf(long l) { }
  public static String valueOf(float f) { }
  public static String valueOf(double d) { }
  public String intern() { }
}
