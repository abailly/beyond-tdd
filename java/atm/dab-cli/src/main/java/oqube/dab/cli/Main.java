/**
 *
 */
package oqube.dab.cli;

import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStreamReader;
import java.io.PrintStream;

/**
 * Main CLI client application class. This implementation starts a loop, passing
 * user input to current state and displaying state output.
 *
 * @author nono
 *
 */
public class Main implements Runnable {

  private UIState initialState;

  private UIState state;

  private boolean debug;

  public UIState getInitialState() {
    return initialState;
  }

  public void setInitialState(UIState initialState) {
    this.initialState = initialState;
  }

  /*
   * (non-Javadoc)
   *
   * @see java.lang.Runnable#run()
   */
  public void run() {
    UIState old = null;
    state = initialState;
    BufferedReader in = new BufferedReader(new InputStreamReader(System.in));
    while (true) {
      // entering some state
      if (old != state) {
        if (old != null)
          old.exit(System.out);
        state.enter(System.out);
        System.out.println();
        old = state;
      }
      // wait for intput
      String input = null;
      try {
        input = in.readLine();
      } catch (IOException e) {
        e.printStackTrace();
        System.err.println("Caught exception, exiting");
      }
      // check special command
      process(input, System.out);
      if(Thread.currentThread().isInterrupted())
        break;
      // process input
      try {
        state = state.process(input, System.out);
        System.out.println();
      } catch (ProcessException e) {
        if (debug)
          e.printStackTrace();
        System.out.println(e.getMessage());
        UIState s = e.getState();
        if (s != null)
          state = s;
      }
    }
  }

  private void process(String input, PrintStream out) {
    if ("\\q".equals(input)) {
      out.println("Exiting");
      Thread.currentThread().interrupt();
    }
  }

  public boolean isDebug() {
    return debug;
  }

  public void setDebug(boolean debug) {
    this.debug = debug;
  }
}
