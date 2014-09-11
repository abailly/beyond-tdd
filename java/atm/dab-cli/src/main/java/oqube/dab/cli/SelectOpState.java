/**
 * 
 */
package oqube.dab.cli;

import java.io.PrintStream;

import oqube.dab.BankException;
import oqube.dab.CardInvalidException;

/**
 * A state for selecting operations on an inserted card.
 * 
 * @author nono
 * 
 */
public class SelectOpState extends AbstractState {

	private UIState withdraw;

	public UIState getWithdraw() {
		return withdraw;
	}

	public void setWithdraw(UIState withdraw) {
		this.withdraw = withdraw;
	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see oqube.dab.cli.UIState#enter(java.io.PrintStream)
	 */
	public void enter(PrintStream output) {
		output.print(messages.getMessage("msg.select.enter", null,
				"Select ops", null));
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
		if ("1".equals(input)) {
			int solde;
			try {
				solde = getDab().balance();
			} catch (Exception e) {
				throw new ProcessException(getMessage("msg.select.error"), e,
						this);
			}
			output.print(messages.getMessage("msg.select.balance",
					new Object[] { new Integer(solde) }, "Solde", null));
			return this;
		} else if ("2".equals(input))
			return withdraw;
		else if ("0".equals(input)) {
			getCardReader().returnCard();
			return getNext();
		} else {
			output.print(getMessage("msg.select.menu"));
			return this;
		}
	}
}
