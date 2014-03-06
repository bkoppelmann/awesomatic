interface Colors {
  int RED = 1, GREEN = 2, BLUE = 4;
}

interface RainbowColors extends Colors {
  int YELLOW = 3, ORANGE = 5, INDIGO = 6, VIOLET = 7;
}

interface PrintColors extends Colors {
  int YELLOW = 8, CYAN = 16, MAGENTA = 32;
}

interface LotsOfColors extends RainbowColors, PrintColors {
  int FUSCHIA = 17, VERMILLION = 43, CHARTREUSE = RED+90;
}

class Test implements LotsOfColors {
  public static void main(String[] args) {
    System.out.println(RED, ORANGE, CYAN, VERMILLION);
  }
}
