import java.io.BufferedReader;
import java.io.FileReader;

public class Day09 {
  public static void main(String[] args) {
    try {
    BufferedReader bReader = new BufferedReader(new FileReader("input.txt"));
    String input = bReader.readLine();
    StreamProcessor p = StreamProcessor.processString(input);
    System.out.println(p.getScore());
    System.out.println(p.getGarbageCount());
    } catch (Exception e) {
      System.out.println("File not found.");
    }
  }
}
