/**
 * 
 */
package oqube.dab.cli;

import java.io.PrintStream;

/**
 * @author nono
 * 
 */
public class CardRetainedState extends AbstractState {

	/*
	 * (non-Javadoc)
	 * 
	 * @see oqube.dab.cli.UIState#enter(java.io.PrintStream)
	 */
	public void enter(PrintStream output) {
		output.print(messages.getMessage("msg.cardretained.enter", null,
				"Card retained", null));
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
		return getNext();
	}

}
