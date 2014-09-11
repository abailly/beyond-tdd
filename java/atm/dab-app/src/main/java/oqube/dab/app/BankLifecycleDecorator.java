/**
 * 
 */
package oqube.dab.app;

import java.io.FileNotFoundException;

import org.springframework.context.Lifecycle;

import oqube.dab.Bank;
import oqube.dab.BankException;
import oqube.dab.BankImpl;
import oqube.dab.Dispenser;
import oqube.dab.cli.CLIDispenser;

/**
 * This adapter adds lifecycle management to the bank. Spring offer lifecycle
 * handling through {@link Lifecycle} interface: Methods are offered for
 * starting and stopping the container and all beans implementing the lifecycle
 * interface. Decorator patter wraps real implementation, adding some behavior
 * to the base code without the need to modify it.
 * 
 * @author nono
 * 
 */
public class BankLifecycleDecorator implements Lifecycle, Bank, Dispenser {

  private BankImpl realBank;

  private CLIDispenser realdispenser;

  private boolean running;

  /*
   * (non-Javadoc)
   * 
   * @see org.springframework.context.Lifecycle#isRunning()
   */
  public boolean isRunning() {
    return running;
  }

  /*
   * (non-Javadoc)
   * 
   * @see org.springframework.context.Lifecycle#start()
   */
  public void start() {
    this.running = true;
  }

  /*
   * (non-Javadoc)
   * 
   * @see org.springframework.context.Lifecycle#stop()
   */
  public void stop() {
    try {
      realBank.save();
      realdispenser.saveReserve();
    } catch (FileNotFoundException e) {
      e.printStackTrace();
    } finally {
      running = false;
    }
  }

  public int balance(String accountNo) throws BankException {
    return realBank.balance(accountNo);
  }

  public void deposit(String accountNo, int amount) throws BankException {
    realBank.deposit(accountNo, amount);
  }

  public boolean withdraw(String accountNo, int amount) throws BankException {
    return realBank.withdraw(accountNo, amount);
  }

  public BankImpl getRealBank() {
    return realBank;
  }

  public void setRealBank(BankImpl realBank) {
    this.realBank = realBank;
  }

  public void deliver(int[] notes) throws BankException {
    realdispenser.deliver(notes);
  }

  public int[] getReserve() {
    return realdispenser.getReserve();
  }

  public CLIDispenser getRealdispenser() {
    return realdispenser;
  }

  public void setRealdispenser(CLIDispenser realdispenser) {
    this.realdispenser = realdispenser;
  }

}
