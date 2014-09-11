/*
 * ______________________________________________________________________________
 * 
 * Copyright 2003 Arnaud Bailly - NORSYS/LIFL
 * 
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions are met:
 * 
 * (1) Redistributions of source code must retain the above copyright notice,
 * this list of conditions and the following disclaimer.
 * 
 * (2) Redistributions in binary form must reproduce the above copyright notice,
 * this list of conditions and the following disclaimer in the documentation
 * and/or other materials provided with the distribution.
 * 
 * (3) The name of the author may not be used to endorse or promote products
 * derived from this software without specific prior written permission.
 * 
 * THIS SOFTWARE IS PROVIDED BY THE AUTHOR ``AS IS'' AND ANY EXPRESS OR IMPLIED
 * WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF
 * MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO
 * EVENT SHALL THE AUTHOR BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
 * SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO,
 * PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS;
 * OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY,
 * WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR
 * OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF
 * ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
 * ______________________________________________________________________________
 * 
 * Created on Jul 20, 2004
 * 
 */
package oqube.dab;

/**
 * An implementation of ATM for testing purpose
 * 
 * @author nono
 * @version $Id: /local/fidl/fidl/src/main/java/Banque/ATMImpl.java 115
 *          2005-12-06T21:35:50.769357Z nono $
 */
public class ATMImpl implements DAB {

  private CardReader cardReader;

  public CardReader getCardReader() {
    return cardReader;
  }

  public void setCardReader(CardReader cardReader) {
    this.cardReader = cardReader;
  }

  private Dispenser dispenser;

  public Dispenser getDispenser() {
    return dispenser;
  }

  public void setDispenser(Dispenser dispenser) {
    this.dispenser = dispenser;
  }

  private Bank bank;

  public Bank getBank() {
    return bank;
  }

  public void setBank(Bank bank) {
    this.bank = bank;
  }

  private boolean inserted;

  public boolean isInserted() {
    return inserted;
  }

  public void setInserted(boolean inserted) {
    this.inserted = inserted;
  }

  /*
   * (non-Javadoc)
   * 
   * @see Banque.DAB#insert()
   */
  public void insert() throws BankException, CardInvalidException {
    assert !this.inserted;
    if (cardReader.failedCode() > 2) {
      cardReader.retainCard();
      this.inserted = false;
      throw new BankException(
          "Too many failed attempts at pin code, keeping card");
    }
    this.inserted = true;
  }

  /*
   * (non-Javadoc)
   * 
   * @see Banque.DAB#pinCode(int)
   */
  public boolean pinCode(String code) throws BankException,
      CardInvalidException {
    assert this.inserted;
    if (cardReader.failedCode() > 2) {
      cardReader.retainCard();
      this.inserted = false;
      throw new BankException(
          "Too many failed attempts at pin code, keeping card");
    }
    if (cardReader.checkCode(code)) {
      return true;
    } else {
      return false;
    }
  }

  /*
   * (non-Javadoc)
   * 
   * @see Banque.DAB#withdrawCardReader()
   */
  public void withdrawCard() {
    assert this.inserted;
    cardReader.returnCard();
    this.inserted = false;
  }

  /*
   * (non-Javadoc)
   * 
   * @see Banque.DAB#withdrawal(int)
   */
  public void withdrawal(int amount) throws BankException, CardInvalidException {
    assert this.inserted;
    bank.withdraw(cardReader.accountNo(), amount);
    /* compute notes distribution */
    int[] notes = notes(dispenser.notes, amount);
    dispenser.deliver(notes);
  }

  /*
   * (non-Javadoc)
   * 
   * @see Banque.DAB#balance()
   */
  public int balance() throws BankException, CardInvalidException {
    assert this.inserted;
    return bank.balance(cardReader.accountNo());
  }

  /**
   * Compute distribution of notes using standard greedy algorithm. arrays ar 4
   * position wide with 10,20,50 and 100 values. note that available array is
   * modified
   * 
   * @throws BankException
   */
  public int[] notes(int[] available, int amount) throws BankException {
    int ln = available.length;
    int[] res = new int[ln];
    int i = ln - 1;
    while (i >= 0 && amount != 0) {
      if (amount >= available[i]) {
        res[i]++;
        amount -= available[i];
      } else {
        i--;
      }
    }
    if (amount != 0)
      throw new BankException("Cannot breakdown " + amount
          + " among available notes");
    else
      return res;
  }
}
