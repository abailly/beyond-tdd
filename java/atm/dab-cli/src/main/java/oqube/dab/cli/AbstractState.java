/**
 * 
 */
package oqube.dab.cli;

import java.io.PrintStream;
import java.util.PropertyResourceBundle;
import java.util.ResourceBundle;

import org.springframework.context.MessageSource;

import oqube.dab.DAB;
 
/**
 * @author nono
 * 
 */
public abstract class AbstractState implements UIState {

	protected MessageSource messages;

	/*
	 * next state for succesful processing
	 */
	private UIState next;

	private DAB dab;

	private CardFileReader cardReader;

	public DAB getDab() {
		return dab;
	}

	public void setDab(DAB dab) {
		this.dab = dab;
	}

	public UIState getNext() {
		return next;
	}

	public void setNext(UIState state) {
		this.next = state;
	}

	public CardFileReader getCardReader() {
		return cardReader;
	}

	public void setCardReader(CardFileReader cardReader) {
		this.cardReader = cardReader;
	}

	public MessageSource getMessages() {
		return messages;
	}

	public void setMessages(MessageSource messages) {
		this.messages = messages;
	}

	public String getMessage(String k) {
		return messages.getMessage(k, null, k, null);
	}
}
