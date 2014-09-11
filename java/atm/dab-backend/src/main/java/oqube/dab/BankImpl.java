/*______________________________________________________________________________
 *
 * Copyright 2004 Arnaud Bailly - NORSYS/LIFL
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions
 * are met:
 *
 * (1) Redistributions of source code must retain the above copyright
 *     notice, this list of conditions and the following disclaimer.
 *
 * (2) Redistributions in binary form must reproduce the above copyright
 *     notice, this list of conditions and the following disclaimer in
 *     the documentation and/or other materials provided with the
 *     distribution.
 *
 * (3) The name of the author may not be used to endorse or promote
 *     products derived from this software without specific prior
 *     written permission.
 *
 * THIS SOFTWARE IS PROVIDED BY THE AUTHOR ``AS IS'' AND ANY EXPRESS OR
 * IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
 * WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
 * ARE DISCLAIMED. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR ANY DIRECT,
 * INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES
 * (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR
 * SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION)
 * HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT,
 * STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE)
 * ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED
 * OF THE POSSIBILITY OF SUCH DAMAGE.
 *______________________________________________________________________________
 *
 * Created on 18 nov. 2004
 *
 */
package oqube.dab;

import java.io.File;
import java.io.FileInputStream;
import java.io.FileNotFoundException;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.PrintStream;
import java.util.Enumeration;
import java.util.Iterator;
import java.util.List;
import java.util.ArrayList;
import java.util.Map;
import java.util.HashMap;
import java.util.PropertyResourceBundle;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

/**
 * A version of bank managing accounts from file. A bank file is some property
 * file containing key-value pairs of the form: <code>account.xxx=yyy</code>
 * where xxx is the account number and yyy is the balance of the account.
 *
 * @author nono
 * @version $Id: /local/fidl/fidl/src/main/java/Banque/BankImpl.java 115
 *          2005-12-06T21:35:50.769357Z nono $
 */
public class BankImpl implements Bank {

  class Account {

    String number;

    List<Integer> moves = new ArrayList<Integer>();

    boolean cache;

    private int _bal;

    public Account(String anum, int balance) {
      this.number = anum;
      this._bal = balance;
    }

    int getBalance() {
      return _bal;
    }

    void withdraw(int amount) throws BankException {
      if (amount > getBalance())
        throw new BankException("Invalid Amount");
      _bal -= amount;
    }

    void deposit(int amount) {
      _bal += amount;
    }
  }

  private Map<String, Account> accounts;

  private Pattern keyPattern = Pattern.compile("account\\.(\\w+)");

  private File file;

  /**
   * @throws IOException
   * @throws
   *
   */
  public BankImpl(File file) throws IOException {
    // try loading data
    PropertyResourceBundle props = new PropertyResourceBundle(
      new FileInputStream(file));
    accounts = new HashMap<String, Account>();
    this.file = file;
    for (Enumeration<String> e = props.getKeys(); e.hasMoreElements();) {
      String s = e.nextElement();
      Matcher m = keyPattern.matcher(s);
      if (m.find()) {
        String anum = m.group(1);
        try {
          int balance = Integer.parseInt(props.getString(s));
          // create account
          accounts.put(anum, new Account(anum, balance));
        } catch (NumberFormatException nfe) {
          throw new IOException("Incorrect format for account "
                                + anum);
        }

      }
    }
  }

  /*
   * (non-Javadoc)
   *
   * @see Banque.Account#balance(int)
   */
  public int balance(String accountNo) throws BankException {
    Account a = accounts.get(accountNo);
    if (a == null)
      throw new BankException("Invalid account number");
    return a.getBalance();
  }

  /*
   * (non-Javadoc)
   *
   * @see Banque.Account#withdraw(int, int)
   */
  public boolean withdraw(String accountNo, int amount) throws BankException {
    if (amount <= 0)
      throw new BankException(
        "Withdrawn amount must be strictly positive");
    Account a = accounts.get(accountNo);
    if (a == null)
      throw new BankException("Invalid account number");
    a.withdraw(amount);
    return true;
  }

  /*
   * (non-Javadoc)
   *
   * @see Banque.Account#depot(int, int)
   */
  public void deposit(String accountNo, int amount) throws BankException {
    if (amount <= 0)
      throw new BankException(
        "Deposited amount must be strictly positive");
    Account a = accounts.get(accountNo);
    if (a == null)
      throw new BankException("Invalid account number");
    a.deposit(amount);
  }

  public void save() throws FileNotFoundException {
    PrintStream ps = new PrintStream(new FileOutputStream(file));
    for (Account a : accounts.values()) {
      ps.println("account." + a.number + "=" + a.getBalance());
    }
    ps.flush();
    ps.close();
  }
}

/*
 * $Log: BankImpl.java,v $ Revision 1.1 2005/02/02 14:24:04 bailly added Bank
 * implementations
 *
 */
