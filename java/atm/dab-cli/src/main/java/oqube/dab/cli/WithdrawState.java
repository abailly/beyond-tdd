/**
 * 
 */
package oqube.dab.cli;

import java.io.PrintStream;

import oqube.dab.BankException;
import oqube.dab.CardInvalidException;

/**
 * @author nono
 * 
 */
public class WithdrawState extends AbstractState {

  private UIState error;

  public UIState getError() {
    return error;
  }

  public void setError(UIState error) {
    this.error = error;
  }

  /*
   * (non-Javadoc)
   * 
   * @see oqube.dab.cli.UIState#enter(java.io.PrintStream)
   */
  public void enter(PrintStream output) {
    output.print(getMessage("msg.withdraw.enter"));
  }

  /*
   * (non-Javadoc)
   * 
   * @see oqube.dab.cli.UIState#exit(java.io.PrintStream)
   */
  public void exit(PrintStream output) {
  }

  /*
   * (non-Javadoc)
   * 
   * @see oqube.dab.cli.UIState#process(java.lang.String, java.io.PrintStream)
   */
  public UIState process(String input, PrintStream output)
      throws ProcessException {
    int m = Integer.parseInt(input);
    try {
      getDab().withdrawal(m);
      output.print(getMessage("msg.withdraw.deliver"));
      return getNext();
    } catch (Exception e) {
      throw new ProcessException(getMessage("msg.withdraw.error"), e, error);
    }
  }

}
