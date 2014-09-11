/**
 *
 */
package oqube.dab.cli;

import java.io.File;
import java.io.FileInputStream;
import java.io.FileNotFoundException;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.PrintStream;
import java.util.MissingResourceException;
import java.util.PropertyResourceBundle;

import oqube.dab.CardInvalidException;
import oqube.dab.CardReader;

/**
 * A card reader that loads card data from file. The data file is a properties
 * formatted file containing properties <code>card.account</code> and
 * <code>card.pincode</code>.
 *
 * @author nono
 *
 */
public class CLICardReader implements CardFileReader {

  private String accountNo;

  private String pinCode;

  private boolean cardInvalid = true;

  private int failedCode;

  private String cardfile;

  /*
   * (non-Javadoc)
   *
   * @see oqube.dab.CardReader#accountNo()
   */
  public String accountNo() throws CardInvalidException {
    if(cardInvalid)
      throw new CardInvalidException();
    return accountNo;
  }

  /*
   * (non-Javadoc)
   *
   * @see oqube.dab.CardReader#checkCode(java.lang.String)
   */
  public boolean checkCode(String code) throws CardInvalidException {
    if(cardInvalid)
      throw new CardInvalidException();
    if (!code.equals(pinCode)) {
      failedCode++;
      return false;
    } else
      return true;
  }

  /*
   * (non-Javadoc)
   *
   * @see oqube.dab.CardReader#failedCode()
   */
  public int failedCode() throws CardInvalidException {
    return failedCode;
  }

  /*
   * (non-Javadoc)
   *
   * @see oqube.dab.CardReader#retainCard()
   */
  public void retainCard() {
    // delete card
    if(cardfile != null){
      File toDelete = new File(cardfile);
      if(!toDelete.delete()) 
        throw new RuntimeException("cannot delete file "+ toDelete + " ??!!");
    }
    this.accountNo = null;
    this.pinCode = null;
    this.cardfile = null;
    this.cardInvalid = true;
  }

  /*
   * (non-Javadoc)
   *
   * @see oqube.dab.CardReader#returnCard()
   */
  public void returnCard() {
    // save data to card
    try {
      PrintStream ps = new PrintStream(new FileOutputStream(cardfile));
      ps.println("card.account=" + accountNo);
      ps.println("card.pincode=" + pinCode);
      ps.println("card.failedcode=" + failedCode);
      ps.flush();
      ps.close();
    } catch (FileNotFoundException e) {
      e.printStackTrace();
      throw new IllegalStateException(
        "Something bad happened to cardfile " + cardfile);
    }
    this.accountNo = null;
    this.pinCode = null;
    this.cardfile = null;
    this.cardInvalid = true;
  }

  /**
   * Asks this card reader to load data from the given input.
   *
   * @param input
   *            a relative or absolute path to a properties file containing
   *            card data.
   * @throws CardInvalidException
   */
  public void loadCard(String input) throws CardInvalidException {
    FileInputStream fis = null;
    try {
      fis = new FileInputStream(input);
      PropertyResourceBundle props = new PropertyResourceBundle(fis);
      this.accountNo = props.getString("card.account");
      this.pinCode = props.getString("card.pincode");
      try {
        this.failedCode = Integer.parseInt(props
                                           .getString("card.failedcode"));
      } catch (MissingResourceException e) {
        // failedcode is optional
      }
      this.cardInvalid = false;
      this.cardfile = input;
    } catch (Exception e) {
      this.cardInvalid = true;
      throw new CardInvalidException(e.getMessage());
    } finally {
      if(fis != null)
	try { 
          fis.close();
        } catch(IOException e) {
          e.printStackTrace();
        }
    }
  }

}
