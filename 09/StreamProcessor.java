import java.util.Stack;

public class StreamProcessor {
  private enum State { START, IN_GROUP, IN_GARBAGE, IGNORE_NEXT }

  private Stack<State> states;

  private int totalScore;
  private int openGroups;
  private int garbageCount;

  public StreamProcessor() {
    states = new Stack<State>();
    states.push(State.START);
    totalScore = 0;
    openGroups = 0;
    garbageCount = 0;
  }

  public int getScore() {
    return totalScore;
  }

  public int getGarbageCount() {
    return garbageCount;
  }

  public static final StreamProcessor processString(String s) {
    StreamProcessor p = new StreamProcessor();
    for (char c : s.toCharArray()) {
      p.processChar(c);
    }
    return p;
  }

  public void processChar(char c) {
    if (checkIgnore()) {
      return;
    }
    countGarbage();
    switch (c) {
      case '{':
        handleOpenCurly();
        break;
      case '}':
        handleClosingCurly();
        break;
      case '!':
        handleExclamationMark();
        break;
      case '<':
        handleOpenAngle();
        break;
      case '>':
        handleClosingAngle();
        break;
      default:
        break;
    }
  }
  
  private boolean stateIs(State s) {
    if (states.empty()) {
      throw new IllegalStateException("states are empty");
    }
    return states.peek() == s;
  }

  private boolean checkIgnore() {
    if (stateIs(State.IGNORE_NEXT)) {
      states.pop();
      return true;
    }
    return false;
  }

  private void handleOpenCurly() {
    if (stateIs(State.START) || stateIs(State.IN_GROUP)) {
      openGroups++;
      states.push(State.IN_GROUP);
    }
  }

  private void handleClosingCurly() {
    if (stateIs(State.IN_GROUP)) {
      totalScore += openGroups;
      openGroups--;
      states.pop();
    }
  }

  private void handleExclamationMark() {
    states.push(State.IGNORE_NEXT);
    garbageCount--;
  }

  private void handleOpenAngle() {
    if (!stateIs(State.IN_GARBAGE)) {
      states.push(State.IN_GARBAGE);
    }
  }
  
  private void handleClosingAngle() {
    states.pop();
    garbageCount--;
  }

  private void countGarbage() {
    if (stateIs(State.IN_GARBAGE)) {
      garbageCount++;
    }
  }
}
