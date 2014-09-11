package oqube.dab;

/**
 * Main interface specifying behavior of ATM.
 * 
 * Note that the method {@link #insert()} should be called prior to any other 
 * operation with the DAB. No operation on this interface should be enabled if
 * no card is inserted. Equivalently, this meens that the behavior of this 
 * interface is unspecified if this basic contract is breached. <p/>
 * The method {@link #withdrawCard()} terminates a sequence of interaction with 
 * the user which can then insert another card. 
 * 
 * @author nono
 */
public interface DAB {

  /**
   * Initializes transaction.
   * This method is called by the system as soon as the card has been 
   * correctly inserted and the card reader is ready to operate. 
   * The DAB will ask the CardReader to retain card if more than 3 failed attempts are
   * stored in the card and will throw an exception.
   * 
   * @throws BankException if card has already 3 failed pincode attempts stored. 
   * @throws CardInvalidException if card is invalid.
   * @see CardReader#retainCard()
   */
  void insert() throws BankException, CardInvalidException;

  /**
   * Check pincode of card.
   * Max. number of failed tries is 3. Card is retained if 3 failed attempts are made and
   * an exception is thrown.
   *  
   * @param code the pincode. A 4-digit string.
   * @return true if pincode is ok, false otherwise.
   * @throws BankException if 3 invalid pincodes are typed. 
   * @throws CardInvalidException if card is invalid.
   * @see CardReader#retainCard()
   */
  boolean pinCode(String code) throws BankException, CardInvalidException;

  /**
   * Withdraw some money using card.
   * It should not be possible to withdraw money without a succesful authentification
   * of the user with {@link #pinCode(String)}. Withdrawal is possible (ie.returns true), 
   * if two conditions are met:
   * <ol>
   * <li>the Bank allows the withdrawal for the account associated with card,</li>
   * <li>the cash dispenser has enough money left to accomodate for the requested amount.</li>
   * </ol>
   * If this withdrawal is Ok, then the {@link Dispenser} is asked to deliver the amount
   * requested in some banknotes breakdown. 
   * 
   * @throws BankException  if something get wrong from the bank.
   * @throws CardInvalidException if card is invalid.
   * @see Dispenser#deliver(int[])
   * @see Dispenser#getReserve() 
   */
  void withdrawal(int amount) throws BankException, CardInvalidException;

  /**
   * Get balance for account.
   * It should not be possible to call balance without a succesful authentification
   * of the user with {@link #pinCode(String)}. The balance for the account stored 
   * in the card is asked to the Bank. 
   * 
   * @throws BankException if something gets wrong from the bank.
   * @throws CardInvalidException if card is invalid. 
   */
  int balance() throws BankException, CardInvalidException;

  /**
   * Terminates transaction.
   * This action should asked the CardReader to return the card to user.
   * @see CardReader#returnCard()
   */
  void withdrawCard(); 
}
