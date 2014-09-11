package oqube.dab.cli;

import java.io.PrintStream;

import oqube.dab.BankException;
import oqube.dab.CardInvalidException;
import oqube.dab.DAB;

/**
 * This state if for entering pin code.
 * 
 * @author nono
 * 
 */
public class EnterPinState extends AbstractState {

	private UIState cardRetained;
	private UIState insertCard;

	public void enter(PrintStream output) {
		output.print(getMessage("msg.pincode.enter"));
	}

	public void exit(PrintStream output) {
	}

	public UIState process(String input, PrintStream output)
			throws ProcessException {
		try {
			if (getDab().pinCode(input)) {
				output.print(getMessage("msg.pincode.ok"));
				return getNext();
			} else {
				output.print(getMessage("msg.pincode.ko"));
				return this;
			}
		} catch (CardInvalidException e) {
			throw new ProcessException(getMessage("msg.pincode.error"),e,insertCard);
		} catch (BankException e) {
			ProcessException pe = new ProcessException(getMessage("msg.pincode.failure"), e, cardRetained);
			throw pe;
		}

	}

	public void setCardRetained(UIState ret) {
		this.cardRetained = ret;
	}

	public UIState getCardRetained() {
		return cardRetained;
	}

	public UIState getInsertCard() {
		return insertCard;
	}

	public void setInsertCard(UIState insertCard) {
		this.insertCard = insertCard;
	}
}
