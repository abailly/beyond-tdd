/**
 * 
 */
package oqube.dab.cli;

import java.io.PrintStream;

import oqube.dab.DAB;

/**
 * An interface for states of the interface. Each instance of this class
 * represents some conversational state in the application.
 * 
 * @author nono
 * 
 */
public interface UIState {

	/**
	 * Method for state entry.
	 * This method is called each time the state is entered.
	 * 
	 * @param output the output stream for user feedback.
	 */
	void enter(PrintStream output);
	
	/**
	 * Main method for states. 
	 * The state is asked to process some input and is given the opportunity
	 * to give some output.
	 * 
	 * @param input a string passed from the cli. maybe empty.
	 * @param output the stream for user feedback
	 * @return
	 */
	UIState process(String input, PrintStream output) throws ProcessException;
	
	/**
	 * Method for state exit.
	 * This method is called each time the state is exited.
	 *  
	 * @param output the stream for user notification.
	 */
	void exit(PrintStream output);

	/**
	 * Defines the standard next state.
	 * This is a convenience method that may be used by states handlers
	 * to define the standard next state.
	 * 
	 * @param state a state. May be null if not used.
	 */
	void setNext(UIState state);

	/**
	 * Sets the interface to DAB.
	 * 
	 * @param dab may not be null.
	 */
	void setDab(DAB dab);

	/**
	 * Sets the interface to card reader.
	 * @param reader
	 */
	void setCardReader(CardFileReader reader);
}
