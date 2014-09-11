/**
 * 
 */
package oqube.dab.cli;

import oqube.dab.CardInvalidException;
import oqube.dab.CardReader;

/**
 * A card reader that can read data from files.
 * 
 * @author nono
 *
 */
public interface CardFileReader extends CardReader {

	/**
	 * Try to initialize reader with data from given file.
	 * 
	 * @param input a path to some  file. May not be null.
	 * @throws CardInvalidException if file does not exist, is not readable
	 * or data is incorrect.
	 */
	void loadCard(String input)throws CardInvalidException;
}
