/**
 * 
 */
package oqube.dab.cli;

import java.io.PrintStream;
import java.util.PropertyResourceBundle;
import java.util.ResourceBundle;

import oqube.dab.BankException;
import oqube.dab.CardInvalidException;
import oqube.dab.CardReader;
import oqube.dab.DAB;

/**
 * Initial state for application. 
 * The user is asked to enter its card, that is a path to a file
 * containing Card data (bank account number and pin code).
 * 
 * @author nono
 * 
 */
public class InsertCardState extends AbstractState {

	/*
	 * (non-Javadoc)
	 * 
	 * @see oqub.dab.cli.UIState#enter(java.io.PrintStream)
	 */
	public void enter(PrintStream output) {
		output.print(getMessage("msg.insert"));
	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see oqub.dab.cli.UIState#exit(java.io.PrintStream)
	 */
	public void exit(PrintStream output) {
	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see oqub.dab.cli.UIState#process(java.lang.String, java.io.PrintStream)
	 */
	public UIState process(String input, PrintStream output)
			throws ProcessException {
		try {
			// load data from card file
			getCardReader().loadCard(input);
			getDab().insert();
		} catch (BankException e) {
			throw new ProcessException(getMessage("msg.insert.error"), e);
		}
		return getNext();
	}

}
