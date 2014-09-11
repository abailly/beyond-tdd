package oqube.dab;

import java.io.File;
import java.io.FileWriter;
import java.io.IOException;
import java.io.PrintWriter;
import java.util.HashMap;
import java.util.Map;

import oqube.dab.cli.CLICardReader;

/**
 * Interface for interacting with the ATM System.
 * 
 * @author nono
 *
 */
public class ATMFixture {

  private ATMImpl ATM = new ATMImpl();

  private Bank bank = new Bank() {

    private Map<String, Integer> bank = new HashMap<String, Integer>();

    public int balance(String accountNo) throws BankException {
      return bank.get(accountNo);
    }

    public void deposit(String accountNo, int amount) throws BankException {
      bank.put(accountNo, amount);
    }

    public boolean withdraw(String accountNo, int amount) throws BankException {
      int bal = bank.get(accountNo);
      if (bal >= amount)
        bank.put(accountNo, bal - amount);
      else
        throw new BankException("Not enough cash");
      return true;
    }

  };

  private Card card;

  private Dispenser dispenser = new Dispenser() {

    private int[] infiniteReserve = new int[] { Integer.MAX_VALUE,
        Integer.MAX_VALUE, Integer.MAX_VALUE, Integer.MAX_VALUE };

    public int[] getReserve() {
      return infiniteReserve;
    }

    public void deliver(int[] notes) throws BankException {
    }

  };

  private File cardFile;

  private CLICardReader reader;

  /**
   * Create a new ATMFixture.
   * The constructor initializes a complete system, consisting of an ATM implementation, 
   * an in-memory bank that can later be initialized with accounts ({@link #setAccountBalance(String, int)}),
   * a standard file-based card reader and a dispenser containing an unlimited amount of cash (well,
   * not infinite but containing {@link Integer#MAX_VALUE} notes). 
   */
  public ATMFixture() {
    reader = new CLICardReader();
    ATM.setBank(bank);
    ATM.setDispenser(dispenser);
    ATM.setCardReader(reader);
  }

  /**
   * Insert a card with given characteristics. 
   * 
   * @param pin
   * @param account
   * @param failedCode
   * @throws CardInvalidException
   * @throws BankException
   */
  public void insertCard(String pin, String account, int failedCode)
      throws CardInvalidException, BankException {
    initCardReader(pin, account, failedCode);
    ATM.insert();
  }

  private void initCardReader(String pin, String account, int failedCode)
      throws CardInvalidException {
    this.card = new Card(pin, account, failedCode);
    try {
      this.cardFile = createCardFile(this.card);
    } catch (IOException e) {
      throw new RuntimeException("Cannot write cardfile " + e + ", giving up");
    }
    reader.loadCard(this.cardFile.getAbsolutePath());
  }

  private File createCardFile(Card card2) throws IOException {
    File f = new File(new File(System.getProperty("java.io.tmpdir")), "card");
    PrintWriter pw = new PrintWriter(new FileWriter(f));
    pw.print("card.account=");
    pw.println(card.getAccount());
    pw.print("card.pincode=");
    pw.println(card.getPin());
    pw.print("card.failedcode=");
    pw.println(card.getFailedCode());
    pw.flush();
    pw.close();
    return f;
  }

  ATMImpl getATM() {
    return ATM;
  }

  public boolean enterPinCode(String code) throws CardInvalidException,
      BankException {
    return ATM.pinCode(code);
  }

  public Card getCard() {
    return card;
  }

  public void setAccountBalance(String accountNo, int balance)
      throws BankException {
    this.bank.deposit(accountNo, balance);
  }

  public void selectWithdraw() {}
  
  public void withdrawal(int amount) throws CardInvalidException, BankException {
    ATM.withdrawal(amount);
  }

  public int getBalance() throws BankException {
    return ATM.balance();
  }

  public void withdrawCard() {
    ATM.withdrawCard();
  }

  File getCardFile() {
    return cardFile;
  }
}
