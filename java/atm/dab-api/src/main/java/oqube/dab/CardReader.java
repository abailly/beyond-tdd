package oqube.dab;

/**
 * An interface representing the harware for reading cards.
 * This interface is used by ATM to communicate with the card.
 * 
 * @author nono
 */
public interface CardReader {
  /**
   * Ask the card to check some pin code.
   * 
   * @param code the pincode to check. a four-digit string.
   * @return true if code mathces, false otherwise.
   * @throws CardInvalidException if card cannot be read or is retained.
   */
  boolean checkCode(String code) throws CardInvalidException ;
  
  /**
   * 
   * @return the number of failed pincode attempt for current card.
   * @throws CardInvalidException if card cannot be read.
   */
  int failedCode()  throws CardInvalidException ;
  
  /**
   * 
   * @return the account number stored in card.
   * @throws CardInvalidException if card cannot be read.
   */
  String accountNo()   throws CardInvalidException ;
  
  /**
   * Ask the card reader to retain card.
   * Normally invoked when the user has 3 failed pincode attempts or some 
   * notification has been received from the bank.
   */
  void retainCard();
  
  /**
   * Ask the card reader to return card to user.
   * Normally invoked when the user ask for card withdrawal.
   */
  void returnCard();
}
